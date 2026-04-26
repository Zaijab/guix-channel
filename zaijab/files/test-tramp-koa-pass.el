;;; test-tramp-koa-pass.el --- Validate the OTP-noise filter -*- lexical-binding: t; -*-

;; Run from a -Q daemon so the active session isn't disturbed:
;;   emacsclient -s tramp-test -e '(load-file "/home/zjabbar/code/guix-channel/zaijab/files/test-tramp-koa-pass.el")'

(require 'cl-lib)

(defun test-tramp-koa-pass--filter (raw)
  "Local copy of the :filter-return advice body, for testing in isolation."
  (or (and (stringp raw)
           (catch 'found
             (dolist (line (nreverse (split-string raw "\n" t "[ \t\r]+")))
               (when (string-match-p "\\`[0-9]\\{6,8\\}\\'" line)
                 (throw 'found line)))))
      raw))

(defun test-tramp-koa-pass-live ()
  "Install the advice and call `password-store-otp-token' once for real.
Burns one HOTP counter value.  Returns a plist describing the result
without revealing the actual code."
  (require 'password-store-otp)
  (advice-add 'password-store-otp-token :filter-return
              #'test-tramp-koa-pass--filter)
  (unwind-protect
      (let ((result (password-store-otp-token "hawaii_edu_otp")))
        (list :type (type-of result)
              :len (and (stringp result) (length result))
              :all-digits (and (stringp result)
                               (numberp (string-match "^[0-9]+$" result)))
              :has-newline (and (stringp result)
                                (string-match-p "[\n\r]" result))
              :first-char-class (and (stringp result)
                                     (> (length result) 0)
                                     (let ((c (aref result 0)))
                                       (cond ((and (>= c ?0) (<= c ?9)) 'digit)
                                             ((or (and (>= c ?a) (<= c ?z))
                                                  (and (>= c ?A) (<= c ?Z))) 'alpha)
                                             (t 'other))))))
    (advice-remove 'password-store-otp-token
                   #'test-tramp-koa-pass--filter)))

(defun test-tramp-koa-pass-run ()
  "Return a list of (label PASS-OR-FAIL got expected)."
  (let ((cases
         `(("noisy HOTP output"
            ,(concat "[master 0a1b2c3] Increment HOTP counter for x.\n"
                     " 1 file changed, 1 insertion(+), 1 deletion(-)\n"
                     "654321\n")
            "654321")
           ("clean code only"
            "654321"
            "654321")
           ("clean code with trailing newline"
            "654321\n"
            "654321")
           ("eight-digit HOTP"
            "12345678\n"
            "12345678")
           ("only noise (passthrough)"
            "[master abc] commit only\n"
            "[master abc] commit only\n")
           ("empty string"
            ""
            "")
           ("nil input"
            nil
            nil))))
    (mapcar (lambda (c)
              (let* ((label    (nth 0 c))
                     (input    (nth 1 c))
                     (expected (nth 2 c))
                     (got      (test-tramp-koa-pass--filter input)))
                (list label (if (equal got expected) 'PASS 'FAIL) got expected)))
            cases)))
