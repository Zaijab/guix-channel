;;; tramp-pass-livetest.el --- Drive a real TRAMP login w/ deep instrumentation -*- lexical-binding: t; -*-

;; This file is loaded into a fresh -Q daemon AFTER tramp-pass-bootstrap.el.
;; Goals:
;;   - Log every entry/exit of our action AND of TRAMP's default action,
;;     synchronously to disk, so the trace survives even when TRAMP
;;     suspends timers during connection waits.
;;   - Refuse to call the wedge-prone default `tramp-action-password' if
;;     it ever fires: log + return nil instead.  This protects the daemon
;;     and tells us our narrow-after-send fix worked.
;;   - Run `file-exists-p' on a remote path (the non-interactive
;;     primitive that `find-file' eventually calls) and capture the
;;     final outcome to .out, with a generous timeout for 2FA.

(setq tramp-verbose 3
      tramp-connection-timeout 60)

(defconst tpp-log
  "/home/zjabbar/code/guix-channel/zaijab/files/tramp-pass-livetest.log")
(defconst tpp-out
  "/home/zjabbar/code/guix-channel/zaijab/files/tramp-pass-livetest.out")

(defun tpp-log (fmt &rest args)
  "Synchronously append a timestamped line to `tpp-log'."
  (let ((line (apply #'format fmt args)))
    (with-temp-buffer
      (insert (format "[%s] %s\n"
                      (format-time-string "%H:%M:%S.%3N")
                      line))
      (write-region (point-min) (point-max) tpp-log t 'silent))))

(defun tpp--tail (proc &optional n)
  "Return the last N (default 300) chars of PROC's buffer, single-line."
  (with-current-buffer (process-buffer proc)
    (let* ((n (or n 300))
           (s (buffer-substring-no-properties
               (max (point-min) (- (point-max) n))
               (point-max))))
      (replace-regexp-in-string "\n" "\\\\n" s))))

;; --- Instrument our action -----------------------------------------
(advice-add 'tramp-pass-action-password :around
            (lambda (orig proc vec)
              (tpp-log "OURS-IN host=%s otp?=%s tail=<<%s>>"
                       (tramp-file-name-host vec)
                       (and (tramp-pass--otp-prompt-p proc) t)
                       (tpp--tail proc))
              (let ((ret
                     (condition-case err
                         (funcall orig proc vec)
                       (error (tpp-log "OURS-ERR %S" err) nil))))
                (tpp-log "OURS-OUT ret=%S" ret)
                ret)))

;; --- Intercept the default action so it cannot wedge the daemon ----
;; If our narrow-after-send fix works, this wrapper should never log a
;; "DEFAULT-FIRED" line.  If it does, the inner orig is NOT called
;; (read-passwd would pselect on stdin and hang us).
(advice-add 'tramp-action-password :around
            (lambda (_orig proc _vec)
              (tpp-log "DEFAULT-FIRED tail=<<%s>>" (tpp--tail proc))
              nil))

;; --- Instrument the action loop iterations -------------------------
(advice-add 'tramp-process-one-action :around
            (lambda (orig proc vec actions)
              (tpp-log "POA-IN n-actions=%d" (length actions))
              (let ((ret (condition-case err
                             (funcall orig proc vec actions)
                           (error (tpp-log "POA-ERR %S" err) (signal (car err) (cdr err))))))
                (tpp-log "POA-OUT ret=%S" ret)
                ret)))

;; --- Drive the test ------------------------------------------------
(tpp-log "TEST-START")
(let ((result
       (condition-case err
           (let ((default-directory "/ssh:zjabbar@koa.its.hawaii.edu:/home/zjabbar/"))
             (tpp-log "calling file-exists-p on remote .")
             (let ((exists (file-exists-p ".")))
               (tpp-log "TEST-OK exists=%S" exists)
               (format "ok exists=%S\n" exists)))
         (error
          (tpp-log "TEST-ERR %S" err)
          (format "error=%S\n" err)))))
  (with-temp-file tpp-out (insert result)))

(tpp-log "TEST-END")
