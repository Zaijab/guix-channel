(define-module (zaijab services iwd)
  ;#:use-module (rde serializers json)
  ;#:use-module (rde serializers ini)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services dbus)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages dns)
  #:use-module (gnu system shadow)

  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix modules)

  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

(define-configuration/no-serialization iwd-configuration
  (iwd (file-like iwd) "")
  (openresolv (file-like openresolv) "")
  (coreutils (file-like coreutils) "")
  (main-conf
   (ini-config '())
   "Will be serialized to /etc/iwd/main.conf"))

(define (iwd-shepherd-service config)
  "Return a shepherd service for iwd"
  (let* ((iwd (iwd-configuration-iwd config))
         (openresolv (iwd-configuration-openresolv config))
         (coreutils (iwd-configuration-coreutils config))
         (environment #~(list (string-append
                               "PATH="
                               (string-append #$openresolv "/sbin")
                               ":"
                               (string-append #$coreutils "/bin")))))
    (list
     (shepherd-service
      (documentation "Run iwd")
      (provision '(iwd))
      (requirement '(user-processes dbus-system loopback))
      (start #~(make-forkexec-constructor
                (list (string-append #$iwd "/libexec/iwd"))
                #:log-file "/var/log/iwd.log"
                #:environment-variables #$environment))
      (stop #~(make-kill-destructor))))))

(define (iwd-etc-service config)
  (let ((cfg (iwd-configuration-main-conf config)))
    `(("iwd/main.conf"
       ,(apply mixed-text-file
               "main.conf"
               (serialize-ini-config cfg))))))

(define iwd-package (compose list iwd-configuration-iwd))

(define-public iwd-service-type
  (service-type
   (name 'iwd)
   (extensions
    (list (service-extension
           shepherd-root-service-type
           iwd-shepherd-service)
          (service-extension
           dbus-root-service-type
           iwd-package)
          (service-extension
           etc-service-type
           iwd-etc-service)
          (service-extension
           profile-service-type
           iwd-package)))
   (default-value (iwd-configuration))
   (description "Configure and run iNet Wireless Daemon (iwd).")))
