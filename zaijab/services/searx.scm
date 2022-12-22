(define-module (zaijab services searx)
  #:use-modules (gnu home services shepherd)
  #:use-modules (gnu packages search))

(define (home-searx-profile-service config)
  (list searx))

(define (home-searx-shepherd-service config)
  (list (shepherd-service
	 (provision '(searx))
	 (documentation "Run and control searx daemon.")
	 (start #~(make-forkexec-constructor '("searx-run")))
	 (stop #~(make-kill-destructor)))))

(define home-searx-service-type
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

