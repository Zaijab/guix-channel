(define-module (zaijab packages mathematics)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages version-control)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (macaulay2))

(define-public macaulay2
  (package
    (name "macaulay2")
    (version "1.24.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Macaulay2/M2")
             (commit (string-append "release-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0000000000000000000000000000000000000000000000000000"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DCMAKE_BUILD_TYPE=Release"
             "-DBUILD_NATIVE=OFF"
             "-DENABLE_DOWNLOAD=OFF")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir-to-build-directory
           (lambda _
             (chdir "M2")
             #t))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "make" "check"))
             #t)))))
    (native-inputs
     (list cmake
           ninja
           pkg-config
           texinfo
           python
           git))
    (inputs
     (list boost
           gmp
           mpfr
           ntl
           flint
           factory
           libffi
           readline
           tbb))
    (home-page "https://macaulay2.com")
    (synopsis "Computer algebra system for algebraic geometry and commutative algebra")
    (description
     "Macaulay2 is a software system devoted to supporting research in algebraic
geometry and commutative algebra, whose creation has been funded by the National
Science Foundation since 1992.  Features include Groebner bases and free resolutions
of modules over polynomial rings (in a non-commutative setting as well), Ext, Tor,
and local cohomology.  It also offers an embedded, high-level programming language
designed to support computations in algebraic geometry and commutative algebra.")
    (license license:gpl2+)))