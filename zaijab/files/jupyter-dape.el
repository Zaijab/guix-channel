;;; jupyter-dape.el --- DAPE debug adapter for emacs-jupyter via TCP bridge -*- lexical-binding: t -*-

;;; Commentary:
;;
;; ipykernel does not expose debugpy on a TCP port.  All DAP traffic
;; is proxied through the Jupyter wire protocol:
;;
;;   debug_request / debug_reply   on the control channel
;;   debug_event                   on iopub
;;
;; This package creates a local TCP bridge that DAPE connects to, translating
;; between DAP wire protocol (Content-Length framing + JSON) and Jupyter debug
;; messages.

;;; Code:

(require 'dape)
(require 'jupyter)
(require 'jupyter-client)
(require 'jupyter-messages)
(require 'jupyter-monads)
(require 'json)

;;; ========================================================================
;;; Customization
;;; ========================================================================

(defgroup jupyter-dape nil
  "DAPE debug adapter bridging emacs-jupyter to debugpy."
  :group 'jupyter
  :group 'dape)

(defcustom jupyter-dape-debug nil
  "Non-nil enables verbose [jdape] logging to *Messages*."
  :type 'boolean
  :group 'jupyter-dape)

(defmacro jdape--log (fmt &rest args)
  "Emit a [jdape] message when `jupyter-dape-debug' is non-nil."
  `(when jupyter-dape-debug
     (message (concat "[jdape] " ,fmt) ,@args)))

;;; ========================================================================
;;; State
;;; ========================================================================

(defvar jupyter-dape--bridge-active nil
  "Non-nil when the TCP bridge is running and DAPE should receive messages.")

(defvar jupyter-dape--session-gen 0
  "Incremented each time the bridge is (re)started.
Event subscribers capture this at install time and refuse to forward
events once the generation has advanced, preventing stale events from
a previous session reaching the new DAPE connection.")


(defvar jupyter-dape--server-process nil)
(defvar jupyter-dape--client-process nil)
(defvar jupyter-dape--recv-buffer "")
(defvar jupyter-dape--jupyter-client nil)
(defvar jupyter-dape--seq-counter 0)
(defvar jupyter-dape--init-capabilities nil
  "Cached capabilities from the kernel's initialize response.")

;;; ========================================================================
;;; DAP wire protocol framing
;;; ========================================================================

(defun jupyter-dape--dap-frame (unibyte-json)
  "Wrap UNIBYTE-JSON in Content-Length framing."
  (let ((len (length unibyte-json)))
    (concat (format "Content-Length: %d\r\n\r\n" len)
            unibyte-json)))

(defun jupyter-dape--extract-frames (data)
  "Parse complete DAP frames from unibyte string DATA.
Return (JSON-STRINGS . REMAINDER)."
  (let ((messages nil)
        (pos 0))
    (while
        (let ((hdr-end (string-search "\r\n\r\n" data pos)))
          (when hdr-end
            (let* ((header (substring data pos hdr-end))
                   (body-start (+ hdr-end 4))
                   (len (when (string-match
                               "Content-Length: \\([0-9]+\\)" header)
                          (string-to-number (match-string 1 header))))
                   (body-end (and len (+ body-start len))))
              (when (and len (<= body-end (length data)))
                (push (decode-coding-string
                       (substring data body-start body-end)
                       'utf-8)
                      messages)
                (setq pos body-end)
                t)))))
    (cons (nreverse messages) (substring data pos))))

;;; ========================================================================
;;; Sending to DAPE
;;; ========================================================================

(defun jupyter-dape--message-content-json (msg)
  "Extract the DAP JSON for MSG's content.
Prefers the raw JSON cached in message-part to avoid
decode-then-re-encode round-trip issues with booleans."
  (let ((content-part (plist-get msg :content)))
    (jdape--log "message-content-json: content-part type=%S car=%S"
                (type-of content-part)
                (and (listp content-part) (car-safe content-part)))
    (if (and (listp content-part)
             (eq (car content-part) 'message-part)
             (stringp (cadr content-part)))
        (progn
          (jdape--log "message-content-json: using cached raw JSON, length=%d"
                      (length (cadr content-part)))
          (cadr content-part))
      (let* ((decoded (jupyter-message-content msg))
             (json-false :json-false)
             (json-null :json-null)
             (encoded (json-encode decoded)))
        (jdape--log "message-content-json: re-encoded from plist, length=%d"
                    (length encoded))
        encoded))))

(defun jupyter-dape--send-to-dape (json-string)
  "Send JSON-STRING to DAPE via the TCP bridge."
  (jdape--log "send-to-dape: client-alive=%S json-length=%d json-prefix=%.300s"
              (and jupyter-dape--client-process
                   (process-live-p jupyter-dape--client-process))
              (length json-string)
              json-string)
  (when (and jupyter-dape--client-process
             (process-live-p jupyter-dape--client-process))
    (let ((encoded (encode-coding-string json-string 'utf-8 t)))
      (process-send-string
       jupyter-dape--client-process
       (jupyter-dape--dap-frame encoded))
      (jdape--log "send-to-dape: sent %d bytes" (length encoded)))))

;;; ========================================================================
;;; Advice 1: Route debug_request to the control channel
;;; ========================================================================

(defun jupyter-dape--route-debug-to-control (orig type)
  "Route debug_request to control; delegate all other types to ORIG."
  (let ((result (if (equal type "debug_request") "control" (funcall orig type))))
    (when (equal type "debug_request")
      (jdape--log "route: type=%S -> channel=%S" type result))
    result))

;;; ========================================================================
;;; Advice 2: Intercept debug_reply and debug_event
;;; ========================================================================
;;
;; ALWAYS intercepts these when jupyter-dape-mode is on, preventing:
;;   - "Unhandled channel: control"   (no pcase branch in hook-allows-handler-p)
;;   - "Unhandled message type: ..."  (no entry in jupyter--client-handlers)

(defun jupyter-dape--intercept-debug-messages (orig client channel msg req)
  "Intercept debug_reply; delegate everything else to ORIG.
debug_event is handled by the kernel-IO subscriber instead."
  (let ((msg-type (jupyter-message-type msg)))
    (cond
     ((equal msg-type "debug_reply")
      (jdape--log "intercept: debug_reply on channel=%S bridge-active=%S"
                  channel jupyter-dape--bridge-active)
      (when jupyter-dape--bridge-active
        (jupyter-dape--send-to-dape
         (jupyter-dape--message-content-json msg)))
      nil)
     ((equal msg-type "debug_event")
      ;; Swallow to prevent "Unhandled message type" errors.
      ;; Forwarding to DAPE is done by the kernel-IO event subscriber.
      (jdape--log "intercept: swallowing debug_event (handled by subscriber)")
      nil)
     (t (funcall orig client channel msg req)))))

;;; ========================================================================
;;; Kernel-IO subscriber for debug_event
;;; ========================================================================

(defun jupyter-dape--install-event-subscriber (client)
  "Subscribe to CLIENT's kernel-IO to catch debug_event messages."
  (jdape--log "install-event-subscriber: installing gen=%d" jupyter-dape--session-gen)
  (let ((my-gen jupyter-dape--session-gen))
    (jupyter-run-with-io (jupyter-kernel-io client)
      (jupyter-subscribe
        (jupyter-subscriber
          (lambda (msg)
            (when (and jupyter-dape--bridge-active
                       (= jupyter-dape--session-gen my-gen)
                       (equal (jupyter-message-type msg) "debug_event")
                       (equal (plist-get msg :channel) "iopub"))
              (jdape--log "event-subscriber: forwarding debug_event gen=%d" my-gen)
              (jupyter-dape--send-to-dape
               (jupyter-dape--message-content-json msg)))))))))

;;; ========================================================================
;;; Parsing DAP JSON
;;; ========================================================================

(defun jupyter-dape--parse-dap-json (json-string)
  "Parse JSON-STRING into a keyword-keyed plist.
Uses :json-false and :json-null to match jupyter's conventions.
Arrays become vectors (via :array-type \\='array)."
  (let ((result (json-parse-string json-string
                                   :object-type 'plist
                                   :false-object :json-false
                                   :null-object :json-null
                                   ;; 'array = vectors, 'list = lists.
                                   ;; 'vector is NOT valid.
                                   :array-type 'array)))
    (jdape--log "parse-dap-json: parsed %d chars -> plist keys=%S"
                (length json-string)
                (cl-loop for k in result by #'cddr
                         when (keywordp k) collect k))
    result))

;;; ========================================================================
;;; Synchronous debug send/receive (for pre-flight)
;;; ========================================================================

(defun jupyter-dape--next-seq ()
  (cl-incf jupyter-dape--seq-counter))

(defun jupyter-dape--debug-send-sync (client command &optional arguments)
  "Send DAP COMMAND synchronously via CLIENT.  Return the reply content plist."
  (jdape--log "send-sync: command=%S arguments=%S" command arguments)
  (let (reply-content received)
    (let ((jupyter-inhibit-handlers t))
      (jupyter-run-with-client client
        (jupyter-mlet*
            ((req (apply #'jupyter-request "debug_request"
                         (append (list :type "request"
                                       :seq (jupyter-dape--next-seq)
                                       :command command)
                                 (when arguments
                                   (list :arguments arguments))))))
          (jdape--log "send-sync: req id=%S type=%S"
                      (jupyter-request-id req)
                      (jupyter-request-type req))
          (jupyter-run-with-io (jupyter-request-message-publisher req)
            (jupyter-subscribe
              (jupyter-subscriber
                (lambda (msg)
                  (jdape--log "send-sync subscriber: msg-type=%S channel=%S"
                              (jupyter-message-type msg)
                              (plist-get msg :channel))
                  (when (equal (jupyter-message-type msg) "debug_reply")
                    (setq reply-content (jupyter-message-content msg))
                    (setq received t)
                    (jdape--log "send-sync: got debug_reply, content keys=%S"
                                (and (listp reply-content)
                                     (cl-loop for k in reply-content by #'cddr
                                              when (keywordp k) collect k)))
                    (jupyter-unsubscribe))))))
          (jupyter-sent (jupyter-return req)))))
    (jdape--log "send-sync: waiting for reply (timeout=%S)..." jupyter-long-timeout)
    (jupyter-with-timeout (nil jupyter-long-timeout nil) received)
    (jdape--log "send-sync: received=%S reply-content=%S" received
                (and reply-content (truncate-string-to-width
                                    (format "%S" reply-content) 200)))
    reply-content))

;;; ========================================================================
;;; Inbound path: DAPE → kernel
;;; ========================================================================

(defun jupyter-dape--forward-to-kernel (json-string)
  "Forward a raw DAP JSON-STRING from DAPE to the Jupyter kernel.
Parses JSON into a plist (required by jupyter-encode-raw-message).
The reply flows through jupyter-handle-message → advice → send-to-dape.

Special case: if the command is \"initialize\" and we already cached
capabilities from the configure step, synthesize a success response
instead of forwarding (the kernel rejects duplicate initialize)."
  (unless jupyter-dape--bridge-active
    (jdape--log "forward-to-kernel: bridge not active, dropping message")
    (cl-return-from jupyter-dape--forward-to-kernel nil))
  (jdape--log "forward-to-kernel: json-length=%d json-prefix=%.100s"
              (length json-string) json-string)
  (condition-case err
      (let* ((dap-plist (jupyter-dape--parse-dap-json json-string))
             (command (plist-get dap-plist :command))
             (seq (plist-get dap-plist :seq)))
        (jdape--log "forward-to-kernel: command=%S type=%S caps-cached=%S"
                    command (type-of command) (not (null jupyter-dape--init-capabilities)))
        (cond
          ((equal command "terminate")
           ;; dape-quit semantics: the kernel IS the debuggee, so kill it.
           ;; Synthesize success so DAPE doesn't hang waiting for a response.
           (jdape--log "forward-to-kernel: intercepting terminate, killing kernel")
           (let* ((response `(:seq ,(jupyter-dape--next-seq)
                              :type "response"
                              :request_seq ,seq
                              :success t
                              :command ,command))
                  (json-str (json-encode response)))
             (jupyter-dape--send-to-dape json-str))
           ;; Close the socket so dape can't send follow-up messages
           ;; (e.g. a disconnect) that would hit the dead kernel's I/O.
           (jupyter-dape--stop-bridge)
           (let ((client jupyter-dape--jupyter-client))
             (ignore-errors (jupyter-shutdown-kernel client))
             ;; Deactivate interaction mode in all associated source buffers,
             ;; then kill the REPL buffer so it doesn't linger with stale state.
             (ignore-errors
               (cl-loop for buf in (buffer-list)
                        do (with-current-buffer buf
                             (when (and jupyter-repl-interaction-mode
                                        (eq jupyter-current-client client))
                               (jupyter-repl-interaction-mode -1)))))
             (ignore-errors
               (cl-loop for buf in (buffer-list)
                        do (with-current-buffer buf
                             (when (and (eq major-mode 'jupyter-repl-mode)
                                        (eq jupyter-current-client client))
                               (let ((kill-buffer-query-functions nil))
                                 (kill-buffer buf))))))))
          ((equal command "disconnect")
           ;; dape-disconnect-quit semantics: detach, keep kernel alive.
           ;; Synthesize success, then reset debugpy state for next session.
           (jdape--log "forward-to-kernel: intercepting disconnect, resetting debugpy state")
           (let* ((response `(:seq ,(jupyter-dape--next-seq)
                              :type "response"
                              :request_seq ,seq
                              :success t
                              :command ,command))
                  (json-str (json-encode response)))
             (jupyter-dape--send-to-dape json-str))
           (setq jupyter-dape--bridge-active nil)
           (ignore-errors
             (jupyter-dape--debug-send-sync
              jupyter-dape--jupyter-client "disconnect"
              (list :restart :json-false :terminateDebuggee :json-false))))
          (t
          (if (and (equal command "initialize")
                 jupyter-dape--init-capabilities)
            ;; Synthesize a success response from cached capabilities
            (let* ((json-false :json-false)
                   (json-null :json-null)
                   (response `(:seq ,(jupyter-dape--next-seq)
                               :type "response"
                               :request_seq ,seq
                               :success t
                               :command "initialize"
                               :body ,jupyter-dape--init-capabilities))
                   (json-str (json-encode response)))
              (jdape--log "forward-to-kernel: synthesizing initialize response from cache")
              (jupyter-dape--send-to-dape json-str)
              ;; DAP spec requires an "initialized" event after
              ;; the initialize response so the client proceeds.
              (let* ((evt `(:seq ,(jupyter-dape--next-seq)
                            :type "event"
                            :event "initialized"))
                     (evt-json (json-encode evt)))
                (jdape--log "forward-to-kernel: sending synthetic initialized event")
                (jupyter-dape--send-to-dape evt-json)))
          ;; Normal path: forward to kernel
          (let ((content-list (append dap-plist nil))
                (client jupyter-dape--jupyter-client)
                (jupyter-inhibit-handlers t))
            (jdape--log "forward-to-kernel: client=%S client-connected=%S"
                        (type-of client)
                        (and client (jupyter-connected-p client)))
            (jupyter-run-with-client client
              (jupyter-mlet*
                  ((req (apply #'jupyter-request "debug_request" content-list)))
                (jdape--log "forward-to-kernel: req created id=%S type=%S"
                            (jupyter-request-id req)
                            (jupyter-request-type req))
                (jupyter-sent (jupyter-return req)))))))))
    (error
     (message "[jdape] forward-to-kernel ERROR: %S" err))))

;;; ========================================================================
;;; TCP bridge
;;; ========================================================================

(defun jupyter-dape--bridge-filter (_proc data)
  "Process filter for the bridge client socket."
  (when jupyter-dape--bridge-active
    (jdape--log "bridge-filter: received %d bytes" (length data))
    (setq jupyter-dape--recv-buffer
          (concat jupyter-dape--recv-buffer
                  (if (multibyte-string-p data)
                      (encode-coding-string data 'utf-8 t)
                    data)))
    (condition-case err
        (pcase-let ((`(,frames . ,rest)
                     (jupyter-dape--extract-frames
                      jupyter-dape--recv-buffer)))
          (setq jupyter-dape--recv-buffer rest)
          (jdape--log "bridge-filter: extracted %d frames, %d bytes remaining"
                      (length frames) (length rest))
          (dolist (json-str frames)
            (jupyter-dape--forward-to-kernel json-str)))
      (error
       (message "[jdape] bridge-filter ERROR: %S" err)))))

(defun jupyter-dape--bridge-sentinel (proc event)
  "Sentinel for the bridge client socket."
  (jdape--log "bridge-sentinel: event=%S alive=%S" event (process-live-p proc))
  (unless (process-live-p proc)
    ;; Only clear state if this is still the active client process.
    ;; A new client may have connected before the old sentinel fires.
    (when (eq proc jupyter-dape--client-process)
      (setq jupyter-dape--client-process nil)
      (setq jupyter-dape--recv-buffer ""))))

(defun jupyter-dape--start-bridge ()
  "Start the TCP bridge server.  Return the port number."
  (jupyter-dape--stop-bridge)
  (cl-incf jupyter-dape--session-gen)
  (setq jupyter-dape--recv-buffer "")
  (setq jupyter-dape--server-process
        (make-network-process
         :name "jupyter-dape-bridge"
         :server t
         :host "127.0.0.1"
         :service 0
         :family 'ipv4
         :coding 'no-conversion
         :noquery t
         :log (lambda (_server client _message)
                (jdape--log "bridge-log: new client connection %S" client)
                (setq jupyter-dape--client-process client)
                (set-process-coding-system client 'no-conversion 'no-conversion)
                (set-process-filter client #'jupyter-dape--bridge-filter)
                (set-process-sentinel client #'jupyter-dape--bridge-sentinel))))
  (let ((port (process-contact jupyter-dape--server-process :service)))
    (jdape--log "start-bridge: listening on port %d" port)
    port))

(defun jupyter-dape--stop-bridge ()
  "Tear down the TCP bridge."
  (jdape--log "stop-bridge: tearing down")
  (setq jupyter-dape--bridge-active nil)
  (when (and jupyter-dape--client-process
             (process-live-p jupyter-dape--client-process))
    (delete-process jupyter-dape--client-process))
  (setq jupyter-dape--client-process nil)
  (when (and jupyter-dape--server-process
             (process-live-p jupyter-dape--server-process))
    (delete-process jupyter-dape--server-process))
  (setq jupyter-dape--server-process nil)
  (setq jupyter-dape--recv-buffer ""))

;;; ========================================================================
;;; co_filename wrapping
;;; ========================================================================

(defun jupyter-dape--compile-wrap (code filepath)
  "Wrap CODE so that co_filename is set to FILEPATH."
  (format
   (concat "exec(compile("
           "__import__('base64').b64decode(b'%s').decode(),"
           "%s,'exec'),globals(),locals())")
   (base64-encode-string (encode-coding-string code 'utf-8 t) t)
   (prin1-to-string (expand-file-name filepath))))

(defun jupyter-dape--wrap-eval-string (orig str &optional insert beg end)
  "Wrap STR in exec(compile(...,filepath,...)) when a DAPE session is active."
  (let* ((live-conns (and (fboundp 'dape--live-connections)
                          (dape--live-connections)))
         (has-client (bound-and-true-p jupyter-current-client))
         (lang (and has-client
                    (ignore-errors
                      (jupyter-kernel-language jupyter-current-client))))
         (filepath (and live-conns
                        has-client
                        (eq lang 'python)
                        (buffer-file-name (current-buffer)))))
    (jdape--log "wrap-eval-string: live-conns=%S has-client=%S lang=%S filepath=%S"
                (not (null live-conns)) (not (null has-client)) lang filepath)
    (if filepath
        (progn
          (jdape--log "wrap-eval-string: wrapping for %s" filepath)
          (funcall orig (jupyter-dape--compile-wrap str filepath) insert beg end))
      (funcall orig str insert beg end))))

;;; ========================================================================
;;; DAPE config callbacks
;;; ========================================================================

(defun jupyter-dape--ensure (config)
  "Verify that a Python Jupyter client is active."
  (ignore config)
  (let* ((has-client (bound-and-true-p jupyter-current-client))
         (lang (and has-client
                    (ignore-errors
                      (jupyter-kernel-language jupyter-current-client)))))
    (jdape--log "ensure: has-client=%S lang=%S" has-client lang)
    (unless (eq lang 'python)
      (user-error
       "No active Python Jupyter client; M-x jupyter-repl-associate-buffer"))))

(defun jupyter-dape--configure (config)
  "Initialize the kernel debugger and start the TCP bridge.
Return CONFIG with the bridge port."
  (let ((client jupyter-current-client))
    (jdape--log "configure: client=%S" (type-of client))
    ;; Pre-flight: ensure the kernel-side debugpy is running.
    (let* ((info (jupyter-dape--debug-send-sync client "debugInfo"))
           (body (and info (plist-get info :body)))
           (was-started (and body (eq (plist-get body :isStarted) t))))
      (jdape--log "configure: debugInfo body=%S was-started=%S" body was-started)
      ;; Always send initialize to reset the kernel's DAP session state.
      ;; First session: this starts debugpy. Subsequent sessions: TCP disconnect
      ;; resets the kernel's DAP state machine even though isStarted stays t,
      ;; so we must re-initialize before attach will be accepted.
      (jdape--log "configure: sending initialize to kernel")
      (let ((init-reply (jupyter-dape--debug-send-sync
                         client "initialize"
                         (list :adapterID "jupyter-dape"
                               :clientID "emacs-dape"
                               :linesStartAt1 t
                               :columnsStartAt1 t
                               :pathFormat "path"))))
        (jdape--log "configure: initialize reply=%S"
                    (and init-reply
                         (truncate-string-to-width
                          (format "%S" init-reply) 200)))
        ;; Cache the capabilities so we can replay them when DAPE
        ;; sends its own initialize (kernel rejects duplicates).
        (when init-reply
          (setq jupyter-dape--init-capabilities
                (plist-get init-reply :body))
          (jdape--log "configure: cached capabilities=%S"
                      (not (null jupyter-dape--init-capabilities)))))
      ;; Only verify debugpy started if it wasn't running before.
      (unless was-started
        (let* ((info2 (jupyter-dape--debug-send-sync client "debugInfo"))
               (body2 (and info2 (plist-get info2 :body)))
               (started2 (and body2 (eq (plist-get body2 :isStarted) t))))
          (jdape--log "configure: post-init debugInfo body=%S started=%S" body2 started2)
          (unless started2
            (user-error
             "Kernel debugger failed to start; ensure ipykernel >= 6")))))
    ;; Start bridge
    (setq jupyter-dape--jupyter-client client)
    (let ((port (jupyter-dape--start-bridge)))
      (jdape--log "configure: bridge started on port %d" port)
      (setq jupyter-dape--bridge-active t)
      (jupyter-dape--install-event-subscriber client)
      (jdape--log "configure: returning config with port=%d" port)
      (plist-put config 'port port))))

;;; ========================================================================
;;; Config registration
;;; ========================================================================

(defun jupyter-dape--register-config ()
  (setq dape-configs
        (cons '(jupyter-dape
                modes (python-mode python-ts-mode)
                ensure jupyter-dape--ensure
                host "localhost"
                fn jupyter-dape--configure
                :request "attach"
                :type "python"
                :justMyCode nil
                :showReturnValue t
                :stopOnEntry nil)
              (cl-remove 'jupyter-dape dape-configs :key #'car)))
  (jdape--log "register-config: jupyter-dape registered"))

;;; ========================================================================
;;; Minor mode
;;; ========================================================================

;;;###autoload
(define-minor-mode jupyter-dape-mode
  "Global minor mode enabling the emacs-jupyter DAPE debug adapter."
  :global t
  :lighter " JDape"
  (cond
   (jupyter-dape-mode
    (jdape--log "mode: enabling")
    (advice-add 'jupyter-channel-from-request-type :around
                #'jupyter-dape--route-debug-to-control)
    (advice-add 'jupyter--run-handler-maybe :around
                #'jupyter-dape--intercept-debug-messages)
    (advice-add 'jupyter-eval-string :around
                #'jupyter-dape--wrap-eval-string)
    (jupyter-dape--register-config)
    (jdape--log "mode: enabled, advice installed"))
   (t
    (jdape--log "mode: disabling")
    (jupyter-dape--stop-bridge)
    (setq jupyter-dape--init-capabilities nil)
    (advice-remove 'jupyter-channel-from-request-type
                   #'jupyter-dape--route-debug-to-control)
    (advice-remove 'jupyter--run-handler-maybe
                   #'jupyter-dape--intercept-debug-messages)
    (advice-remove 'jupyter-eval-string
                   #'jupyter-dape--wrap-eval-string)
    (setq dape-configs
          (cl-remove 'jupyter-dape dape-configs :key #'car))
    (jdape--log "mode: disabled"))))

(provide 'jupyter-dape)

;;; jupyter-dape.el ends here
