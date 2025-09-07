(define-module (zaijab packages lrslib)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages multiprecision)
  #:use-module ((guix licenses) #:prefix license:))

(define-public lrslib
  (package
    (name "lrslib")
    (version "071a")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://cgm.cs.mcgill.ca/~avis/C/lrslib/archive/"
                          "lrslib-" version ".tar.gz"))
       (sha256
        (base32 "0rr5s14gkdj5bds8kmi84x4pf4l4a5rqgllv6caxwy1ckzps1pnw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; No test suite
       #:make-flags (list (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)))) ; No configure script
    (inputs
     (list gmp))
    (home-page "https://www-cgrl.cs.mcgill.ca/~avis/C/lrs.html")
    (synopsis "Reverse search vertex enumeration program")
    (description
     "lrslib is a self-contained ANSI C implementation of the reverse search
algorithm for vertex enumeration/convex hull problems.  It is the fastest
available program for solving general vertex enumeration problems.")
    (license license:gpl2+)))