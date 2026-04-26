;;; test-tramp-pass.el --- Synthetic tests for tramp-pass -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'tramp)
(load-file "/home/zjabbar/code/guix-channel/zaijab/files/tramp-pass.el")

(defun test-tramp-pass--run (prompt-text parsed)
  "Drive the action with PROMPT-TEXT and a mocked PARSED alist."
  (let* ((buf (generate-new-buffer " *tpp-test*"))
         (proc (make-pipe-process :name "tpp" :buffer buf :noquery t))
         (vec (make-tramp-file-name :method "ssh" :user "zjabbar"
                                    :host "koa.its.hawaii.edu"
                                    :localname "/"))
         (sent nil))
    (unwind-protect
        (cl-letf* (((symbol-function 'tramp-pass--parsed-for)
                    (lambda (_v) parsed))
                   ((symbol-function 'password-store-otp-token)
                    (lambda (entry) (format "OTP-%s" entry)))
                   ((symbol-function 'process-send-string)
                    (lambda (_p s) (setq sent s) nil)))
          (with-current-buffer buf (insert prompt-text))
          (let ((r (tramp-pass-action-password proc vec)))
            (list :returned r :sent sent)))
      (delete-process proc) (kill-buffer buf))))

(defun test-tramp-pass-cases ()
  (list
   (cons "password prompt + entry with secret"
         (test-tramp-pass--run
          "zjabbar@koa's password: "
          '((secret . "PW") ("otp" . "hawaii_edu_otp"))))
   (cons "duo prompt + entry with otp"
         (test-tramp-pass--run
          "Passcode or option (1-3): "
          '((secret . "PW") ("otp" . "hawaii_edu_otp"))))
   (cons "duo prompt + entry without otp (should fall through)"
         (test-tramp-pass--run
          "Passcode or option (1-3): "
          '((secret . "PW"))))
   (cons "password prompt + secret as closure"
         (test-tramp-pass--run
          "Password: "
          `((secret . ,(lambda () "CLOSURE-PW")))))
   (cons "no entry at all (should fall through)"
         (test-tramp-pass--run
          "Password: "
          nil))))

(provide 'test-tramp-pass)
;;; test-tramp-pass.el ends here
