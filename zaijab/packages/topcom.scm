(define-module (zaijab packages topcom)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module ((guix licenses) #:prefix license:))

(define-public topcom
  (package
    (name "topcom")
    (version "0.17.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.wm.uni-bayreuth.de/de/team/rambau_joerg/"
                          "TOPCOM-Downloads/TOPCOM-0_17_8.tgz"))
       (sha256
        (base32 "1axav1xrhsrj533bd2l42i7qh99c2xxvzjms471rx1gfa67vk0rz"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f ; No test suite  
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-configure-scripts
                 (lambda _
                   ;; Patch the main TOPCOM configure script to fix hardcoded /bin/sh paths
                   (substitute* "configure"
                     (("/bin/sh") (which "sh")))
                   ;; Also patch the external makefile to handle bundled cddlib better
                   (substitute* "external/Makefile"
                     (("sh -c 'cd cddlib-0.94f-TOPCOM; \\./configure --prefix=`pwd`/\\.\\.  --libdir=`pwd`/../lib '")
                      (string-append "sh -c 'cd cddlib-0.94f-TOPCOM; chmod +x configure; " (which "sh") " ./configure --prefix=`pwd`/.. --libdir=`pwd`/../lib '"))
                     (("/bin/sh") (which "sh")))
                   #t))
               (add-after 'configure 'fix-cddlib-library-path
                 (lambda _
                   ;; Make sure the external/lib directory exists and build cddlib manually
                   (with-directory-excursion "external"
                     ;; First make sure the cddlib configure is executable
                     (when (file-exists? "cddlib-0.94f-TOPCOM/configure")
                       (chmod "cddlib-0.94f-TOPCOM/configure" #o755))
                     ;; Build cddlib manually  
                     (invoke "make" "cdd")
                     ;; Create the lib directory and copy the library
                     (mkdir-p "lib")
                     (when (file-exists? "cddlib-0.94f-TOPCOM/.libs/libcddgmp.a")
                       (copy-file "cddlib-0.94f-TOPCOM/.libs/libcddgmp.a" "lib/libcddgmp.a"))
                     ;; Also try other possible locations
                     (when (file-exists? "cddlib-0.94f-TOPCOM/lib-src-gmp/libcddgmp.a")
                       (copy-file "cddlib-0.94f-TOPCOM/lib-src-gmp/libcddgmp.a" "lib/libcddgmp.a")))
                   #t))
               (replace 'configure
                 (lambda* (#:key outputs #:allow-other-keys)
                   ;; Let TOPCOM handle its bundled cddlib but provide system headers too
                   (setenv "CPPFLAGS" (string-append "-I" #$(this-package-input "cddlib") "/include/cddlib"))
                   (invoke "sh" "./configure" (string-append "--prefix=" #$output)))))))
    (inputs
     (list m4 cddlib gmp))
    (home-page "https://www.wm.uni-bayreuth.de/de/team/rambau_joerg/TOPCOM/")
    (synopsis "Triangulations of point configurations and oriented matroids")
    (description
     "TOPCOM is a collection of clients to compute triangulations of point
configurations and oriented matroids, placements of points, and related
structures, like oriented matroids, convex hulls, and hyperplane arrangements.")
    (license license:gpl2+)))