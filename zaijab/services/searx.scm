(define-module (zaijab services searx)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages search)
  #:use-module (guix gexp))

(define (searx-profile-service config)
  (list searx))

(define (searx-shepherd-service config)
  (list (shepherd-service
	 (provision '(searx))
	 (documentation "Run and control searx daemon.")
	 (start #~(make-forkexec-constructor (list #$(file-append searx "/bin/searx-run"))
					     #:log-file "/home/zjabbar/.local/var/log/searx.log" h))
	 (stop #~(make-kill-destructor)))))

(define-public searx-service-type
  (service-type (name 'searx)
                (extensions
                 (list (service-extension
                        profile-service-type
                        searx-profile-service)
		       (service-extension
			shepherd-service-type
			searx-shepherd-service)))
                (default-value #f)
                (description "Searx Time.")))

