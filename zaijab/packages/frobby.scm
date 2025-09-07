(define-module (zaijab packages frobby)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)  
  #:use-module (gnu packages multiprecision)
  #:use-module ((guix licenses) #:prefix license:))

(define-public frobby
  (package
    (name "frobby")
    (version "0.9.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Macaulay2/frobby")
             (commit "ae88a0bd93af2d7819db719e51b74eae713d7739"))) ; latest 2024-04-06
       (file-name (git-file-name name version))
       (sha256
        (base32 "19ydfdvsir214b4zy4hkh2hfwlzb8h2glwxndg94zdalv2lsdi8p"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f ; No test suite in makefile
           #:make-flags 
           #~(list (string-append "PREFIX=" #$output)
                   (string-append "BIN_INSTALL_DIR=" #$output "/bin"))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure) ; No configure script
               (add-after 'unpack 'fix-env-and-bash-references
                 (lambda _
                   ;; Fix all /usr/bin/env references using gexp paths
                   (substitute* (find-files "." "(Makefile|.*\\.mk)$")
                     (("/usr/bin/env bash")
                      (string-append #$(this-package-native-input "bash") "/bin/bash"))
                     (("/usr/bin/env")
                      (string-append #$(this-package-native-input "coreutils") "/bin/env")))
                   #t))
               (add-after 'unpack 'copy-headers
                 (lambda _
                   ;; Copy headers to include directory (needed by Macaulay2)
                   (let ((out #$output))
                     (mkdir-p (string-append out "/include"))
                     (for-each (lambda (header)
                                (install-file header (string-append out "/include")))
                              (find-files "src" "\\.h$"))
                     #t)))
               (add-after 'build 'build-shared-library
                 (lambda _
                   ;; Build shared library as well
                   (invoke "make" "library" "MODE=shared")
                   #t)))))
    (native-inputs
     (list bash coreutils))
    (inputs
     (list gmp))
    (home-page "https://www.broune.com/frobby/")
    (synopsis "Software system for computations with monomial ideals")
    (description
     "Frobby is a software system and C++ library for computations with
monomial ideals.  Frobby is free software and it is intended as a vehicle for
computational and mathematical research on monomial ideals.")
    (license license:gpl2+)))