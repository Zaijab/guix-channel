(define-module (zaijab services searx)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages search)
  #:use-module (guix gexp))

(define (home-searx-profile-service config)
  (list searx))

(define (home-searx-shepherd-service config)
  (list (shepherd-service
	 (provision '(searx))
	 (documentation "Run and control searx daemon.")
	 (start #~(make-forkexec-constructor (list #$(file-append searx "/bin/searx-run & disown"))))
	 (stop #~(make-kill-destructor)))))

(define-public home-searx-service-type
  (service-type (name 'home-searx)
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        home-searx-profile-service)
		       (service-extension
			home-shepherd-service-type
			home-searx-shepherd-service)))
                (default-value #f)
                (description "Searx Time.")))

