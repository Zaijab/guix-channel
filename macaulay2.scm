(use-modules (guix packages)
             (guix download)
             (guix git-download)
             (guix gexp)
             (guix build-system cmake)
             (guix build-system gnu)
             (gnu packages)
             (gnu packages algebra)
             (gnu packages autotools)
             (gnu packages base)
             (gnu packages boost)
             (gnu packages build-tools)
             (gnu packages cmake)
             (gnu packages dbm)
             (gnu packages compression)
             (gnu packages cpp)
             (gnu packages documentation)
             (gnu packages gcc)
             (gnu packages libffi)
             (gnu packages maths)
             (gnu packages multiprecision)
             (gnu packages pkg-config)
             (gnu packages python)
             (gnu packages readline)
             (gnu packages tbb)
             (gnu packages texinfo)
             (gnu packages version-control)
             (gnu packages assembly)
             (gnu packages xml)
             (gnu packages bison)
             (gnu packages check)
             (gnu packages bdw-gc)
             ((guix licenses) #:prefix license:))

(package
  (name "macaulay2")
  (version "1.24.11")
  (source
   (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/Macaulay2/M2")
           (commit (string-append "release-" version))
           (recursive? #t)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "07vlm8cifycb3nxc2902bjg7j2g47r9g9llxiyzpjrc33cjhbqaq"))))
  (build-system cmake-build-system)
  (arguments
   (list #:configure-flags
         #~(list "-DCMAKE_BUILD_TYPE=Release"
                 "-DBUILD_NATIVE=OFF"
                 "-DENABLE_DOWNLOAD=OFF"
                 "-DWITH_TBB=OFF"
                 "-DWITH_XML=OFF"
                 "-DWITH_SQL=OFF"
                 (string-append "-DREADLINE_INCLUDE_DIR="
                               #$(this-package-input "readline") "/include")
                 (string-append "-DREADLINE_LIBRARY="
                               #$(this-package-input "readline") "/lib/libreadline.so")
                 (string-append "-DGDBM_INCLUDE_DIR="
                               #$(this-package-input "gdbm") "/include")
                 (string-append "-DGDBM_LIBRARY="
                               #$(this-package-input "gdbm") "/lib/libgdbm.so")
                 (string-append "-DGTEST_ROOT="
                               #$(this-package-native-input "googletest"))
                 "-DUSE_SYSTEM_GTEST=ON")
         #:tests? #f
         #:parallel-build? #f
         #:phases
         #~(modify-phases %standard-phases
             (add-after 'unpack 'enter-source-directory
               (lambda _
                 (chdir "M2")
                 #t))
             (replace 'build
               (lambda _
                 ;; First make run - generates signature files (may fail)
                 (system* "make" "-j" "1")
                 ;; Second make run - completes the build
                 (invoke "make" "-j" "1")
                 #t)))))
  (native-inputs
   (list cmake
         pkg-config
         texinfo
         python
         git
         yasm
         bison
         googletest))
  (inputs
   (list boost
         gmp
         mpfr
         ntl
         flint
         singular
         libffi
         readline
         openblas
         lapack
         gdbm
         libxml2
         libgc))
  (home-page "https://macaulay2.com")
  (synopsis "Computer algebra system for algebraic geometry and commutative algebra")
  (description
   "Macaulay2 is a software system devoted to supporting research in algebraic
geometry and commutative algebra, whose creation has been funded by the National
Science Foundation since 1992.  Features include Groebner bases and free resolutions
of modules over polynomial rings (in a non-commutative setting as well), Ext, Tor,
and local cohomology.  It also offers an embedded, high-level programming language
designed to support computations in algebraic geometry and commutative algebra.")
  (license license:gpl2+))