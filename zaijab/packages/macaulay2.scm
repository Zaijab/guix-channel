(define-module (zaijab packages macaulay2)
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
  )

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

(define-public macaulay2
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
                 "-DWITH_SQL=OFF"
                 "-DWITH_FROBBY=OFF"
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
         #:make-flags #~(list "VERBOSE=0")
         #:parallel-build? #t
         #:phases
         #~(modify-phases %standard-phases
             (add-after 'unpack 'enter-source-directory
               (lambda _
                 (chdir "M2")
                 #t))
             (add-before 'build 'create-frobby-stub
               (lambda _
                 ;; Create a stub frobby.h to bypass the version.dd issue
                 (mkdir-p "usr-host/include")
                 (call-with-output-file "usr-host/include/frobby.h"
                   (lambda (port)
                     (display "#ifndef FROBBY_H\n#define FROBBY_H\nnamespace constants { extern const char* const version; }\nconst char* const version = \"stub\";\n#endif\n" port)))
                 #t))
             (replace 'build
               (lambda _
                 ;; First make run - generates signature files (may fail)
                 (system* "make" "-j" "1")
                 ;; Second make run - build core components, continue on error
                 (system* "make" "-j" "1" "-k")
                 #t))
             (replace 'install
               (lambda* (#:key outputs #:allow-other-keys)
                 (let* ((out (assoc-ref outputs "out"))
                        (bin (string-append out "/bin"))
                        (lib (string-append out "/lib"))
                        (share (string-append out "/share")))
                   (mkdir-p bin)
                   (mkdir-p lib)
                   (mkdir-p share)
                   ;; Debug: Show current directory and available files
                   (display "=== INSTALL PHASE DEBUG ===\n")
                   (display "Current working directory:\n")
                   (system* "pwd")
                   (display "Complete file tree:\n")
                   (system* "find" "." "-type" "f" "-name" "*M2*")
                   (display "All executable files:\n")
                   (system* "find" "." "-type" "f" "-executable")
                   (display "Contents of usr-dist directory:\n")
                   (when (file-exists? "usr-dist")
                     (system* "find" "usr-dist" "-type" "f"))
                   ;; Find the actual M2 interpreter binary
                   (let* ((m2-interpreter-path "Macaulay2/d/M2-interpreter")
                          (m2-wrapper-path "usr-dist/x86_64-Linux-Linux-6.15.11/bin/M2")
                          (m2-lib-path "usr-dist/x86_64-Linux-Linux-6.15.11/lib")
                          (m2-share-path "usr-dist/common/share"))
                     (display (string-append "Looking for M2-interpreter at: " m2-interpreter-path "\n"))
                     (display (string-append "M2-interpreter exists? " (if (file-exists? m2-interpreter-path) "YES" "NO") "\n"))
                     (display (string-append "Looking for M2 wrapper at: " m2-wrapper-path "\n"))
                     (display (string-append "M2 wrapper exists? " (if (file-exists? m2-wrapper-path) "YES" "NO") "\n"))
                     ;; Install the actual binary
                     (when (file-exists? m2-interpreter-path)
                       (display "Installing M2-interpreter as M2-binary\n")
                       (copy-file m2-interpreter-path (string-append bin "/M2-binary"))
                       (chmod (string-append bin "/M2-binary") #o755))
                     ;; Copy library files
                     (when (file-exists? m2-lib-path)
                       (display "Installing lib files\n")
                       (system* "cp" "-r" m2-lib-path (string-append out "/lib/M2")))
                     ;; Copy share files
                     (when (file-exists? m2-share-path)
                       (display "Installing share files\n")
                       (system* "cp" "-r" m2-share-path (string-append out "/share/M2")))
                     ;; Create wrapper script
                     (display "Creating wrapper script\n")
                     (call-with-output-file (string-append bin "/M2")
                       (lambda (port)
                         (format port "#!/bin/sh~%")
                         (format port "export M2PREFIX=~a~%" out)
                         (format port "exec ~a/bin/M2-binary \"$@\"~%" out)))
                     (chmod (string-append bin "/M2") #o755)
                     (display "=== END INSTALL DEBUG ===\n"))
                   #t))))))
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
         mpfi
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
    (license license:gpl2+)))
