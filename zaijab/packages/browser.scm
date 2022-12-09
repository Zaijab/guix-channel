(define-module (zaijab packages browser)
  #:use-module (guix packages)
  #:use-module (gnu packages password-utils)
  #:use-module (nongnu packages chrome))

(define-public google-chrome-unstable-browserpass
  (package (inherit google-chrome-stable)
    (inputs (append browserpass-native (package-inputs google-chrome-stable)))))
