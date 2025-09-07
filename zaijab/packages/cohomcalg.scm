(define-module (zaijab packages cohomcalg)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:))

(define-public cohomcalg
  (package
    (name "cohomcalg")
    (version "0.32")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/BenjaminJurke/cohomCalg")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ffz71w2808jddy075wpfd74si5h220q35kqd2ij0khjpxyqlhpn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; No test suite
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; No configure script
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "bin/cohomcalg" (string-append out "/bin"))
               #t))))))
    (home-page "https://github.com/BenjaminJurke/cohomCalg")
    (synopsis "Cohomology ring calculator for line bundle cohomology")
    (description 
     "cohomCalg is a C++ program for calculating line bundle valued sheaf
cohomology groups on toric varieties.  It uses algorithms based on the
Koszul complex and provides efficient methods for computing cohomology
dimensions.")
    (license license:gpl3+)))