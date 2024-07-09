(define-module (zaijab channel-from-lock)
  #:use-module (guix ci)
  #:use-module (rnrs io ports)
  #:use-module (guix channels))

(define raw-data
  (let* ((input+output (pipe))
	 (pid (spawn "guix" '("guix" "describe" "-f" "channels")
                     #:output (cdr input+output))))
    (close-port (cdr input+output))
    (read (open-string-input-port (get-string-all (car input+output))))))

(define master-zaijab-channel-lock
  (cons*
   '(channel (name 'zaijab)
	     (url "/home/zjabbar/code/guix-channel/"))
   (cddr raw-data)))

master-zaijab-channel-lock
