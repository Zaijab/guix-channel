(define-module (zaijab packages browser)
  #:use-module (guix packages)
  #:use-module (gnu packages password-utils)
  #:use-module (nongnu packages chrome))

(define-public google-chrome-unstable-browserpass
  (package (inherit google-chrome-unstable)
    (inputs (append (list (list "browserpass-native" browserpass-native)) (package-inputs google-chrome-unstable)))))
