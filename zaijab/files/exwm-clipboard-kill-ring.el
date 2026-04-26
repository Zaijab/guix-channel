;;; exwm-clipboard-kill-ring.el --- Mirror X CLIPBOARD into kill-ring via xelb -*- lexical-binding: t; -*-

;; Listens for XFixesSelectionNotify events on the X CLIPBOARD selection
;; using EXWM's existing xelb connection.  No subprocess, no polling: we
;; piggy-back on the X socket Emacs already holds open as the window
;; manager.  Whenever any X client takes ownership of CLIPBOARD (i.e.
;; someone copied something in a browser, terminal, etc.), the X server
;; pushes us a SelectionNotify; we read the contents synchronously and
;; push them onto `kill-ring'.
;;
;; Usage from a running Emacs session:
;;   M-x load-file RET ~/code/guix-channel/zaijab/files/exwm-clipboard-kill-ring.el RET
;;   M-x exwm-clipboard-kill-ring-enable
;;
;; To also mirror the PRIMARY selection (mouse-highlight):
;;   (setq exwm-clipboard-kill-ring-include-primary t) ; before enable

(require 'xcb)
(require 'xcb-xfixes)

(defgroup exwm-clipboard-kill-ring nil
  "Push X CLIPBOARD changes into Emacs `kill-ring'."
  :group 'exwm)

(defcustom exwm-clipboard-kill-ring-include-primary nil
  "If non-nil, also mirror the PRIMARY (mouse-highlight) selection."
  :type 'boolean)

(defvar exwm-clipboard-kill-ring--connection nil)
(defvar exwm-clipboard-kill-ring--root nil)
(defvar exwm-clipboard-kill-ring--clipboard-atom nil)
(defvar exwm-clipboard-kill-ring--primary-atom nil)
(defvar exwm-clipboard-kill-ring--last nil
  "Last string we pushed; guards against echo loops and duplicates.")

(defun exwm-clipboard-kill-ring--intern (conn name)
  "Synchronously intern atom NAME on CONN, returning its id."
  (let* ((req (make-instance 'xcb:InternAtom
                             :only-if-exists 0
                             :name-len (length name)
                             :name name))
         (reply (xcb:+request-unchecked+reply conn req)))
    (slot-value reply 'atom)))

(defun exwm-clipboard-kill-ring--subscribe (conn atom)
  "Ask the server to send us SelectionNotify whenever ATOM's owner changes."
  (xcb:+request conn
      (make-instance 'xcb:xfixes:SelectSelectionInput
                     :window exwm-clipboard-kill-ring--root
                     :selection atom
                     :event-mask
                     xcb:xfixes:SelectionEventMask:SetSelectionOwner))
  (xcb:flush conn))

(defun exwm-clipboard-kill-ring--on-notify (data _synthetic)
  "Handler invoked by xelb on each XFixesSelectionNotify event."
  (let ((obj (make-instance 'xcb:xfixes:SelectionNotify)))
    (xcb:unmarshal obj data)
    (let* ((sel (slot-value obj 'selection))
           (kind (cond
                  ((eq sel exwm-clipboard-kill-ring--clipboard-atom)
                   'CLIPBOARD)
                  ((and exwm-clipboard-kill-ring-include-primary
                        (eq sel exwm-clipboard-kill-ring--primary-atom))
                   'PRIMARY))))
      (when kind
        (condition-case err
            (let ((text (gui-get-selection kind 'UTF8_STRING)))
              (when (and (stringp text)
                         (> (length text) 0)
                         (not (equal text exwm-clipboard-kill-ring--last)))
                (setq exwm-clipboard-kill-ring--last text)
                ;; Avoid feeding the new entry straight back into
                ;; CLIPBOARD, which would re-trigger this handler.
                (let ((interprogram-cut-function nil))
                  (kill-new text))))
          (error
           (message "exwm-clipboard-kill-ring: %S" err)))))))

;;;###autoload
(defun exwm-clipboard-kill-ring-enable ()
  "Begin mirroring X CLIPBOARD changes into `kill-ring'."
  (interactive)
  (unless (and (boundp 'exwm--connection) exwm--connection)
    (user-error "EXWM connection not available"))
  (let ((conn exwm--connection))
    (setq exwm-clipboard-kill-ring--connection conn
          exwm-clipboard-kill-ring--root
          (slot-value (car (slot-value (xcb:get-setup conn) 'roots)) 'root))
    ;; Make sure the XFIXES extension is queried & enabled on this
    ;; connection.  xelb requires `prefetch-extension-data' before any
    ;; xfixes opcode is sent, and the X server requires a QueryVersion
    ;; handshake before it will dispatch xfixes requests.
    (xcb:prefetch-extension-data conn 'xcb:xfixes)
    (xcb:get-extension-data conn 'xcb:xfixes)
    (xcb:+request-unchecked+reply conn
        (make-instance 'xcb:xfixes:QueryVersion
                       :client-major-version 5
                       :client-minor-version 0))
    (setq exwm-clipboard-kill-ring--clipboard-atom
          (exwm-clipboard-kill-ring--intern conn "CLIPBOARD")
          exwm-clipboard-kill-ring--primary-atom
          (exwm-clipboard-kill-ring--intern conn "PRIMARY"))
    (xcb:+event conn 'xcb:xfixes:SelectionNotify
                #'exwm-clipboard-kill-ring--on-notify)
    (exwm-clipboard-kill-ring--subscribe
     conn exwm-clipboard-kill-ring--clipboard-atom)
    (when exwm-clipboard-kill-ring-include-primary
      (exwm-clipboard-kill-ring--subscribe
       conn exwm-clipboard-kill-ring--primary-atom))
    (message "exwm-clipboard-kill-ring: listening on CLIPBOARD%s"
             (if exwm-clipboard-kill-ring-include-primary " + PRIMARY" ""))))

(defun exwm-clipboard-kill-ring-disable ()
  "Stop mirroring (unsubscribes selection input on the X server)."
  (interactive)
  (when exwm-clipboard-kill-ring--connection
    (let ((conn exwm-clipboard-kill-ring--connection))
      (dolist (atom (list exwm-clipboard-kill-ring--clipboard-atom
                          exwm-clipboard-kill-ring--primary-atom))
        (when atom
          (xcb:+request conn
              (make-instance 'xcb:xfixes:SelectSelectionInput
                             :window exwm-clipboard-kill-ring--root
                             :selection atom
                             :event-mask 0))))
      (xcb:flush conn))
    (setq exwm-clipboard-kill-ring--connection nil)
    (message "exwm-clipboard-kill-ring: stopped")))

(provide 'exwm-clipboard-kill-ring)
;;; exwm-clipboard-kill-ring.el ends here
