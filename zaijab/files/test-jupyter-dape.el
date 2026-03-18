;;; test-jupyter-dape.el --- Automated test for jupyter-dape -*- lexical-binding: t -*-
;;; Run with: emacs --batch -l ~/.config/emacs/init.el -l this-file.el

(defvar jdape-test-log "/tmp/jdape-test.log")

(defun jt (fmt &rest args)
  (let ((msg (apply #'format fmt args)))
    (append-to-file (concat msg "\n") nil jdape-test-log)
    (message "%s" msg)))

(defun jt-wait (seconds)
  "Spin the event loop for SECONDS."
  (let ((end (+ (float-time) seconds)))
    (while (< (float-time) end)
      (accept-process-output nil 0.1)
      (sit-for 0.05 t))))

(defun jt-wait-until (pred timeout)
  "Wait up to TIMEOUT seconds for PRED to return non-nil."
  (let ((end (+ (float-time) timeout)))
    (while (and (< (float-time) end) (not (funcall pred)))
      (accept-process-output nil 0.1)
      (sit-for 0.05 t))
    (funcall pred)))

(defun jt-dump-buffer (name file)
  (let ((buf (get-buffer name)))
    (when buf
      (with-temp-file file
        (insert-buffer-substring buf))
      (jt "  Dumped %s -> %s" name file))))

(defun jt-dump-all ()
  (jt "--- Dumping buffers ---")
  (jt-dump-buffer "*Messages*" "/tmp/jdape-test-messages.log")
  (dolist (buf (buffer-list))
    (let ((n (buffer-name buf)))
      (when (string-match-p "dape" n)
        (jt-dump-buffer n (format "/tmp/jdape-test-%s.log"
                                  (replace-regexp-in-string "[^a-zA-Z0-9_-]" "_" n)))))))

;; Clear old logs
(ignore-errors (delete-file jdape-test-log))
(dolist (f (directory-files "/tmp" t "jdape-test.*\\.log$"))
  (ignore-errors (delete-file f)))

(condition-case top-err
    (progn
      (jt "=== Step 1: Load jupyter-dape.el ===")
      (load-file "/home/zjabbar/code/guix-channel/zaijab/files/jupyter-dape.el")
      (jt "Loaded OK")

      (jt "=== Step 2: Open test.py ===")
      (find-file "/home/zjabbar/code/guix-channel/zaijab/files/test.py")
      (jt "Opened test.py, buffer=%s" (buffer-name))

      (jt "=== Step 3: Enable jupyter-dape-mode ===")
      (jupyter-dape-mode 1)
      (jt "Mode enabled")

      (jt "=== Step 4: Start jupyter REPL ===")
      (jupyter-run-repl "python3")
      (jt "jupyter-run-repl called, waiting for kernel...")

      ;; Wait for a repl buffer to appear
      (let ((ok (jt-wait-until
                 (lambda ()
                   (cl-find-if (lambda (b) (string-match-p "jupyter-repl" (buffer-name b)))
                               (buffer-list)))
                 15)))
        (if ok
            (jt "REPL buffer found: %s" (buffer-name ok))
          (jt "FAIL: no REPL buffer after 15s")
          (jt-dump-all)
          (kill-emacs 1)))

      ;; Give kernel time to fully initialize
      (jt "Waiting 5s for kernel to settle...")
      (jt-wait 5)

      (jt "=== Step 5: Associate buffer ===")
      ;; Find the client from the REPL buffer's local variable
      (let* ((repl-buf (cl-find-if (lambda (b) (string-match-p "jupyter-repl" (buffer-name b)))
                                   (buffer-list)))
             (client (and repl-buf
                          (with-current-buffer repl-buf
                            (bound-and-true-p jupyter-current-client)))))
        (jt "REPL buffer: %S client: %S" (and repl-buf (buffer-name repl-buf)) (type-of client))
        (if client
            (with-current-buffer "test.py"
              (jupyter-repl-associate-buffer client)
              (jt "Associated. jupyter-current-client type=%S" (type-of jupyter-current-client)))
          (jt "FAIL: no client found in REPL buffer")
          (jt-dump-all)
          (kill-emacs 1)))

      (jt "=== Step 6: Set breakpoint on line 10 ===")
      (with-current-buffer "test.py"
        (goto-char (point-min))
        (forward-line 9)
        (jt "Point at line %d" (line-number-at-pos))
        (dape-breakpoint-toggle)
        (jt "Breakpoint toggled"))

      (jt "=== Step 7: Start dape ===")
      (with-current-buffer "test.py"
        (let ((config-entry (assoc 'jupyter-dape dape-configs)))
          (jt "Config entry found: %S" (not (null config-entry)))
          (if config-entry
              (progn
                (dape (dape--config-eval 'jupyter-dape (cdr config-entry)))
                (jt "dape called"))
            (jt "FAIL: jupyter-dape not in dape-configs")
            (jt-dump-all)
            (kill-emacs 1))))

      ;; Wait for dape to connect
      (jt "Waiting for dape connection (up to 15s)...")
      (let ((conn (jt-wait-until
                   (lambda ()
                     (and (fboundp 'dape--live-connections)
                          (car (dape--live-connections))))
                   15)))
        (if conn
            (jt "SUCCESS: dape connected! conn=%S" (type-of conn))
          (jt "FAIL: no dape connection after 15s")))

      (jt "=== Step 8: Evaluate test.py ===")
      (with-current-buffer "test.py"
        (jupyter-eval-string
         (buffer-substring-no-properties (point-min) (point-max)))
        (jt "jupyter-eval-string called"))

      ;; Wait for a stopped thread (breakpoint hit)
      (jt "Waiting for breakpoint hit (up to 10s)...")
      (let ((stopped (jt-wait-until
                      (lambda ()
                        (let ((conns (and (fboundp 'dape--live-connections)
                                          (dape--live-connections))))
                          (when conns
                            (let ((threads (ignore-errors (dape--threads (car conns)))))
                              (cl-find-if (lambda (th) (plist-get th :stopped)) threads)))))
                      10)))
        (if stopped
            (progn
              (jt "SUCCESS: breakpoint hit! thread=%S" stopped)
              (jt "=== Step 9: Try stepping (next) ===")
              (condition-case err
                  (progn
                    (dape-next)
                    (jt "dape-next called")
                    (jt-wait 3)
                    (jt "Step completed"))
                (error (jt "Step error: %S" err))))
          (jt "No breakpoint hit within 10s")))

      (jt "=== FINAL STATE ===")
      (let ((live (and (fboundp 'dape--live-connections) (dape--live-connections))))
        (jt "Live connections: %d" (length live)))
      (jt "bridge-active: %S" jupyter-dape--bridge-active)
      (jt "client-process alive: %S" (and jupyter-dape--client-process
                                          (process-live-p jupyter-dape--client-process)))
      (jt "init-capabilities cached: %S" (not (null jupyter-dape--init-capabilities)))
      (jt-dump-all)
      (jt "=== TEST COMPLETE ==="))
  (error
   (jt "TOP-LEVEL ERROR: %S" top-err)
   (jt-dump-all)
   (jt "=== TEST COMPLETE (with error) ===")))

(kill-emacs 0)
