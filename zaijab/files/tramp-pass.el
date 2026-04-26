;;; tramp-pass.el --- TRAMP password + OTP via auth-source-pass -*- lexical-binding: t; -*-

;; Looks up TRAMP password prompts in `~/.password-store' using
;; `auth-source-pass'.  Per host, the pass entry at HOST/USER (or
;; HOST) holds the password as its first line; an optional `otp:'
;; field names another pass entry whose `otpauth://' URI is used to
;; compute a one-time code when a 2FA prompt arrives.
;;
;; Why we don't just rely on TRAMP's built-in auth-source path:
;; `tramp-read-passwd' tries auth-source first, but only on the very
;; first password request and only inside an `ignore-errors' wrapper.
;; Any quirk along that path silently falls through to `read-passwd',
;; which `pselect's on stdin and wedges a daemon that has no terminal.
;; Handling the prompt ourselves is more reliable.
;;
;; Setup:
;;   (require 'auth-source-pass)
;;   (auth-source-pass-enable)
;;   (require 'tramp-pass)
;;
;; Per-host pass entry layout (e.g. `koa.its.hawaii.edu/zjabbar'):
;;   <password>
;;   otp: <name-of-pass-entry-with-otpauth-uri>

(require 'tramp)
(require 'tramp-sh)
(require 'auth-source-pass)
(require 'password-store-otp)
(require 'rx)

(defgroup tramp-pass nil
  "TRAMP password + OTP via `auth-source-pass'."
  :group 'tramp)

(defcustom tramp-pass-otp-regexp
  (rx (or "Duo two-factor"
          "Passcode or option"
          "Verification code" "verification code"
          "Passcode" "passcode"
          "One-time" "one-time"
          "OTP"))
  "Regexp distinguishing a 2FA prompt from a regular password prompt.
Both share `tramp-password-prompt-regexp'; we disambiguate by
scanning the live prompt text for these markers."
  :type 'regexp)

(defcustom tramp-pass-otp-field "otp"
  "Field inside a pass entry naming the OTP `pass' entry to use."
  :type 'string)

(defun tramp-pass--otp-prompt-p (proc)
  "Return non-nil when PROC's recent output looks like a 2FA prompt."
  (with-current-buffer (process-buffer proc)
    (save-excursion
      (goto-char (point-max))
      (re-search-backward tramp-pass-otp-regexp nil t))))

(defun tramp-pass--entry-path (host user)
  "Return the existing pass entry name for HOST/USER, or nil.
Tries HOST/USER then HOST, mirroring `auth-source-pass' lookup order."
  (cl-find-if
   (lambda (p) (and p (auth-source-pass-parse-entry p)))
   (list (and user host (concat host "/" user))
         host)))

(defun tramp-pass--parsed-for (vec)
  "Return the parsed pass-entry alist for VEC's host/user, or nil."
  (when-let* ((path (tramp-pass--entry-path
                     (tramp-file-name-host vec)
                     (tramp-file-name-user vec))))
    (auth-source-pass-parse-entry path)))

(defun tramp-pass-action-password (proc vec)
  "TRAMP action: answer a password / 2FA prompt from `pass'.
On a 2FA-style prompt, sends a fresh OTP if the entry has an
`otp:' field.  Otherwise sends the entry's first-line password.
Returns nil when the host has no pass entry, letting TRAMP's
default handler take over."
  (when (tramp-file-name-p vec)
    ;; The active connection's default-directory is the TRAMP path
    ;; itself, so any nested `expand-file-name' (notably the one
    ;; epa/gpg performs while decrypting the pass entry) re-enters
    ;; TRAMP and deadlocks the half-open connection.  Pin
    ;; default-directory to a local path while we do pass lookups.
    (let ((default-directory tramp-compat-temporary-file-directory))
      (when-let* ((parsed (tramp-pass--parsed-for vec)))
        (let* ((reply
                (if (tramp-pass--otp-prompt-p proc)
                    (when-let* ((otp-entry (cdr (assoc tramp-pass-otp-field
                                                      parsed))))
                      (password-store-otp-token otp-entry))
                  (let ((s (cdr (assoc 'secret parsed))))
                    (cond ((functionp s) (funcall s))
                          ((stringp s) s))))))
          (when (and reply (> (length reply) 0))
            (process-send-string proc (concat reply "\n"))
            ;; Hide the answered prompt, mirroring the default
            ;; `tramp-action-password'.  Without this, the action
            ;; loop's next iteration sees the same prompt text and
            ;; can deadlock waiting on output that already arrived.
            (with-current-buffer (process-buffer proc)
              (narrow-to-region (point-max) (point-max)))
            t))))))

;; Prepend so we beat TRAMP's default password handler.
(add-to-list 'tramp-actions-before-shell
             '(tramp-password-prompt-regexp tramp-pass-action-password))

(provide 'tramp-pass)
;;; tramp-pass.el ends here
