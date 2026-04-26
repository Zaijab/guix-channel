;;; tramp-pass-bootstrap.el --- Test daemon setup -*- lexical-binding: t; -*-

(add-to-list 'load-path
             "/home/zjabbar/.guix-home/profile/share/emacs/site-lisp/password-store-2.3.2")
(add-to-list 'load-path
             "/home/zjabbar/.guix-home/profile/share/emacs/site-lisp/password-store-otp-0.1.5")
(require 'password-store)
(require 'password-store-otp)
(require 'auth-source-pass)
(auth-source-pass-enable)
(advice-add 'password-store-otp-token :filter-return
            (lambda (raw)
              (or (and (stringp raw)
                       (catch 'found
                         (dolist (line (nreverse (split-string raw "\n" t "[ \t\r]+")))
                           (when (string-match-p "\\`[0-9]\\{6,8\\}\\'" line)
                             (throw 'found line)))))
                  raw)))
(load-file "/home/zjabbar/code/guix-channel/zaijab/files/tramp-pass.el")
