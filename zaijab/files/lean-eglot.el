;;; lean-eglot.el --- eglot backend for lean4-mode  -*- lexical-binding: t; -*-

;; lean4-mode ships hard-wired to lsp-mode: `lean4-mode-setup' calls
;; `lean4-create-lsp-workspace' directly, and the *Lean Goal* buffer talks
;; to the server exclusively through `lsp-request-async' /
;; `lsp--get-buffer-diagnostics'.  This redefines just those functions (by
;; name, after the package loads) to go through eglot/jsonrpc/flymake
;; instead.  Nothing in the installed package is patched; magit-section
;; rendering, the debounce timer, keymaps and syntax are all reused as-is.

(require 'cl-lib)
(require 'eglot)
(require 'lean4-mode)
(require 'lean4-info)
(require 'flymake)

;; The Lean server itself exposes $/lean/plainGoal etc. regardless of which
;; client is talking to it, so `lean4--server-cmd' (picks `lake serve' vs
;; `lean --server') needs no changes.
(add-to-list 'eglot-server-programs
             (cons 'lean4-mode
                   (lambda (_interactive _project) (lean4--server-cmd))))

;; Something in the installed package set (not this repo's config -
;; confirmed by grepping it) adds `lsp' to `lean4-mode-hook' directly, so
;; both clients would otherwise attach to every buffer at once.
(remove-hook 'lean4-mode-hook #'lsp)

(defun lean4-create-lsp-workspace ()
  "Attach eglot instead of registering an lsp-mode workspace folder."
  (eglot-ensure))

;; `lean4-info-buffer-refresh' used to run off `lsp-on-idle-hook', which
;; only fires inside an active lsp-mode session.  A plain idle timer, gated
;; on major mode, is the eglot-agnostic equivalent.
(defvar lean4-eglot--idle-timer nil)

(defun lean4-eglot--maybe-refresh ()
  (when (derived-mode-p 'lean4-mode)
    (lean4-info-buffer-refresh)))

(unless lean4-eglot--idle-timer
  (setq lean4-eglot--idle-timer
        (run-with-idle-timer lean4-info-buffer-debounce-delay-sec t
                              #'lean4-eglot--maybe-refresh)))

;; lsp-mode discards out-of-order responses itself (`:mode 'tick' plus a
;; cancel-token per request name).  jsonrpc-async-request has no such
;; option, so a rapid pair of refreshes (e.g. the cursor moving twice
;; within one round trip) can have the older request's response land after
;; the newer one and clobber it with stale goal state.  A per-buffer tick
;; guards against that: a response is only applied if it's still the most
;; recent request issued.
(defvar-local lean4-eglot--refresh-tick 0)

(defun lean4-info-buffer-refresh ()
  "Refresh the *Lean Goal* buffer via the buffer's eglot server."
  (when-let ((server (and (lean4-info-buffer-active lean4-info-buffer-name)
                           (eglot-current-server))))
    ;; jsonrpc-async-request's SUCCESS-FN does not run with the requesting
    ;; buffer current (it fires in an internal temp buffer), unlike
    ;; lsp-mode's callbacks, so rebind explicitly.
    (let* ((buf (current-buffer))
           (tick (cl-incf lean4-eglot--refresh-tick)))
      (jsonrpc-async-request
       server '$/lean/plainGoal (eglot--TextDocumentPositionParams)
       :success-fn (lambda (result)
                     (when (buffer-live-p buf)
                       (with-current-buffer buf
                         (when (= tick lean4-eglot--refresh-tick)
                           (setq lean4-goals
                                 (unless (eq result :null)
                                   (let ((g (plist-get result :goals)))
                                     (unless (eq g :null) g))))
                           (lean4-info-buffer-redisplay-debounced)))))
       :error-fn #'ignore)
      (jsonrpc-async-request
       server '$/lean/plainTermGoal (eglot--TextDocumentPositionParams)
       :success-fn (lambda (result)
                     (when (buffer-live-p buf)
                       (with-current-buffer buf
                         (when (= tick lean4-eglot--refresh-tick)
                           (setq lean4-term-goal
                                 (unless (eq result :null)
                                   (let ((g (plist-get result :goal)))
                                     (unless (eq g :null) g))))
                           (lean4-info-buffer-redisplay-debounced)))))
       :error-fn #'ignore))))

;; `lsp--fontlock-with-mode' is a private lsp-mode helper; this is the same
;; two lines without the dependency.
(defun lean4-eglot--fontlock-with-mode (str mode)
  (with-temp-buffer
    (delay-mode-hooks (funcall mode))
    (insert str)
    (font-lock-ensure)
    (buffer-string)))

(defun lean4--insert-goal-text (text delimiter)
  (lean4-info--insert-highlight-inaccessible-names
   (lean4-eglot--fontlock-with-mode text 'lean4-info-mode)
   delimiter))

(defun lean4-eglot--diag-start-line (diag)
  (line-number-at-pos (flymake-diagnostic-beg diag)))

(defun lean4-eglot--diag-end-line (diag)
  (line-number-at-pos (flymake-diagnostic-end diag)))

(defun lean4-info--error-button-action (data)
  (let ((buffer (car data))
        (pos (cdr data)))
    (when (buffer-live-p buffer)
      (pop-to-buffer buffer)
      (goto-char pos))))

(defun lean4-info--mk-message-section (value caption diags buffer)
  "Add a section with id VALUE, caption CAPTION and contents DIAGS."
  (when-let (msgs diags)
    (magit-insert-section (magit-section value)
      (magit-insert-heading caption)
      (magit-insert-section-body
        (dolist (d msgs)
          (let* ((beg (flymake-diagnostic-beg d))
                 (ln (with-current-buffer buffer (line-number-at-pos beg)))
                 (col (with-current-buffer buffer
                        (save-excursion (goto-char beg) (current-column)))))
            (insert-text-button (format "%d:%d:" ln col)
                                 'action #'lean4-info--error-button-action
                                 'button-data (cons buffer beg)
                                 'face 'magit-section-heading
                                 'help-echo "mouse-2: visit this file, line and column")
            (lean4-info--insert-highlight-inaccessible-names
             "\n" (flymake-diagnostic-text d) "\n")))))))

(defun lean4-info-buffer-redisplay ()
  (when (lean4-info-buffer-active lean4-info-buffer-name)
    (let* ((deactivate-mark deactivate-mark)
           (inhibit-read-only t)
           (buffer (current-buffer))
           (line (line-number-at-pos))
           (diags (sort (flymake-diagnostics)
                        (lambda (a b) (< (lean4-eglot--diag-end-line a)
                                          (lean4-eglot--diag-end-line b)))))
           (errors-above (seq-filter
                          (lambda (d) (< (lean4-eglot--diag-end-line d) line))
                          diags))
           (rest (seq-remove
                  (lambda (d) (< (lean4-eglot--diag-end-line d) line))
                  diags))
           (errors-here (seq-filter
                         (lambda (d) (<= (lean4-eglot--diag-start-line d) line))
                         rest))
           (errors-below (seq-remove
                          (lambda (d) (<= (lean4-eglot--diag-start-line d) line))
                          rest)))
      (with-current-buffer lean4-info-buffer-name
        (erase-buffer)
        (magit-insert-section (magit-section 'root)
          (when-let ((goals lean4-goals))
            (magit-insert-section (magit-section 'goals)
              (magit-insert-heading "Goals:")
              (magit-insert-section-body
                (if (> (length goals) 0)
                    (seq-doseq (g goals)
                      (magit-insert-section (magit-section)
                        (lean4--insert-goal-text g "\n\n")))
                  (insert "goals accomplished\n\n")))))
          (when-let ((term-goal lean4-term-goal))
            (magit-insert-section (magit-section 'term-goal)
              (magit-insert-heading "Expected type:")
              (magit-insert-section-body
                (lean4--insert-goal-text term-goal "\n"))))
          (lean4-info--mk-message-section 'errors-here "Messages here:" errors-here buffer)
          (lean4-info--mk-message-section 'errors-below "Messages below:" errors-below buffer)
          (lean4-info--mk-message-section 'errors-above "Messages above:" errors-above buffer))))))

(provide 'lean-eglot)
;;; lean-eglot.el ends here
