(define-module (zaijab packages mathematics)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages check)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages bash)
  #:use-module ((guix licenses) #:prefix license:))

;; =============================================================================
;; MACAULAY2 - BASED ON WORKING NIX VERSION WITH CMAKE + NINJA
;; =============================================================================
;;
;; This package is based on the newer working Nix version from the instructions.
;; Key features:
;; 1. Uses CMake + Ninja for faster builds
;; 2. Skips install-packages target (massive time savings per maintainer)
;; 3. Builds only essential components for mathematical functionality
;; 4. Includes necessary helper packages (cohomCalg, TOPCOM)
;;
;; Reference: GitHub issue about removing install-packages for faster builds
;; "seems working with significantly shorter build time"

;; mpsolve dependency - not available in Guix, create custom package
;; Good so far
(define-public mpsolve
  (package
    (name "mpsolve")
    (version "3.2.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://macaulay2.com/Downloads/OtherSourceCode/mpsolve-3.2.1.tar.gz")
       (sha256
        (base32 "13p7a4fsx3gy8h6syw6gfb2jxcnrwkcvrgya4h7h4bmbx654449x"))))
    
    (build-system gnu-build-system)
    (arguments
     (list
      ;; #:configure-flags
      ;; #~(list "--disable-shared"
      ;;         "--disable-dependency-tracking"
      ;;         "--disable-examples"
      ;;         "--disable-debug"
      ;;         "--disable-silent-rules"
      ;;         "--disable-ui"
      ;;         "--disable-graphical-debugger"
      ;;         "--disable-documentation")
      #:phases
      #~(modify-phases %standard-phases
          (replace 'bootstrap
            (lambda _ (invoke "autoreconf" "-fvi"))))))
    
    (native-inputs (list autoconf automake libtool pkg-config bison flex))
    (inputs (list gmp mpfr))
    (home-page "https://numpi.dm.unipi.it/software/mpsolve")
    (synopsis "Multiprecision polynomial solver")
    (description "MPSolve solves polynomials and secular equations with arbitrary precision.")
    (license license:gpl3+)))

;; csdp dependency - not available in Guix, create custom package
(define-public csdp
  (package
    (name "csdp")
    (version "6.2.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://macaulay2.com/Downloads/OtherSourceCode/Csdp-6.2.0.tgz")
       (sha256
        (base32 "0nsw0n55z8xsz029lwxv0hb92x5xzmrhbgfgblhfx0rlycajl83z"))))
    
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f ; No test suite
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure) ; Uses plain Makefile
          (replace 'build
            (lambda _
              (invoke "make" 
                      "CC=gcc"
                      (string-append "LIBS=-L../lib -lsdp -L" #$(this-package-input "openblas") "/lib -lopenblas -lm")
                      "CFLAGS=-O3 -fopenmp -ansi -Wall -DBIT64 -DUSEOPENMP -DSETNUMTHREADS -DUSESIGTERM -DUSEUSEGETTIME -I../include")))
          (replace 'install
            (lambda _
              (install-file "solver/csdp" (string-append #$output "/bin")))))))
    
    (inputs (list lapack openblas))
    (home-page "https://github.com/coin-or/Csdp")
    (synopsis "Semidefinite programming solver")
    (description "CSDP solves semidefinite programming problems using interior-point methods.")
    (license license:epl2.0)))

;; cohomCalg dependency - needed by Macaulay2
;; Based on Nix: cohomCalg.nix
(define-public cohomcalg
  (package
    (name "cohomcalg")
    (version "0.32")
    (source
     (origin
       (method url-fetch)
       (uri "https://github.com/BenjaminJurke/cohomCalg/archive/refs/tags/v0.32.tar.gz")
       (sha256
        (base32 "0hhfsk217g4kswjyzlvji2cy9aslpwwi860mnaa4f2hbkjwm4z1n"))))
    
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f ; No test suite
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure) ; Uses plain Makefile
          (replace 'build
            (lambda _
              (invoke "make" "-j" (number->string (parallel-job-count)))))
          (replace 'install
            (lambda _
              (mkdir-p (string-append #$output "/bin"))
              (copy-recursively "bin" (string-append #$output "/bin")))))))
    
    (native-inputs (list gcc-toolchain gnu-make))
    (home-page "https://github.com/BenjaminJurke/cohomCalg")
    (synopsis "Cohomology calculation program for line bundle cohomologies")
    (description "cohomCalg computes line bundle valued cohomology classes on toric varieties.")
    (license license:gpl3+)))

;; TOPCOM dependency - needed by Macaulay2
;; Based on Nix: TOPCOM.nix
(define-public topcom
  (package
    (name "topcom")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
	     "https://www.wm.uni-bayreuth.de/de/team/rambau_joerg/TOPCOM-Downloads/TOPCOM-1_1_2.tgz"))
       (sha256
        (base32 "1dla6za9l8s97cl1932dk3rbcpgfiln8zagy85j0axjvxra0gcag"))))
    (build-system gnu-build-system)
    (arguments
     (list
        #:phases
	#~(modify-phases %standard-phases
	          (add-after 'unpack 'patch-cddlib-makefiles
		    (lambda _
		      ;; Patch source Makefile.am files to use system cddlib
		      (substitute* '("src-reg/Makefile.am" "src/Makefile.am")
			(("\\.\\./external/lib/libcddgmp\\.a") "-lcddgmp"))))
      
		  (add-after 'patch-cddlib-makefiles 'regenerate-build-system
		    (lambda _
		      ;; Regenerate autotools files to fix version mismatch
		      (invoke "autoreconf" "-fiv")))
		  
	    (add-before 'configure 'add-cddlib-include
	      (lambda* (#:key inputs #:allow-other-keys)
		(let ((cddlib-inc (assoc-ref inputs "cddlib")))
		  (setenv "CPPFLAGS"
			  (string-append "-I" cddlib-inc "/include/cddlib "
					 "-I" cddlib-inc "/include "
					 (or (getenv "CPPFLAGS") ""))))))
	    )
      ))
    (native-inputs (list autoconf automake m4 cddlib))
    (inputs (list gmp))
    (home-page "https://www.wm.uni-bayreuth.de/de/team/rambau_joerg/TOPCOM")
    (synopsis "Triangulations of point configurations and oriented matroids")
    (description
     "TOPCOM computes triangulations of point configurations and oriented matroids.")
    (license license:gpl2+)))

;; Macaulay2 main package - modern CMake approach
(define-public macaulay2
  (package
    (name "macaulay2")
    ;; Using version from newer working Nix
    (version "1.25.06")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Macaulay2/M2.git")
             ;; Commit from newer Nix version that works
             (commit "6fe351dcfe733f2a525d361d8585f9f2375e264d")
             ;; CRITICAL: get submodules like working Nix
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1960sa9936s37nassa7cz004ailx24iw9ckbn583i45xi6hl73pr"))))

    (build-system cmake-build-system)
    
    (arguments
     (list
      #:tests? #f ; Skip tests for speed
      #:configure-flags
      #~(list 
	 "-GNinja"
	 (string-append "-DPARALLEL_JOBS=" (number->string (parallel-job-count)))
	 "-DCMAKE_BUILD_TYPE=Release"
	 (string-append "-DREADLINE_INCLUDE_DIR=" #$(this-package-input "readline") "/include")
	 "-DCONFIGURE_COMMAND=${CONFIGURE_COMMAND};--disable-documentation")

      
      #:phases
      #~(modify-phases %standard-phases
          ;; Enter M2 directory like both Nix versions

	  ;; (add-before 'build 'fix-nauty-shebang
	  ;;   (lambda _
	  ;;     (substitute* "build.ninja"
	  ;; 	(("autoreconf -vif &&")
	  ;; 	 (string-append "autoreconf -vif && "
	  ;; 			"sed -i '1s|^#!.*|#!" #$(file-append bash-minimal "/bin/bash") "|' configure && "
	  ;; 			"sed -i 's|/bin/sh|" #$(file-append bash-minimal "/bin/bash") "|g' configure &&")))))
	  
	  ;; (add-before 'build 'set-shell
	  ;;   (lambda _
	  ;;     (setenv "SHELL" (string-append #$bash-minimal "/bin/bash"))))

	  (add-before 'build 'fix-shebangs-and-shell
	    (lambda _
	      ;; gfan: make sure make uses bash
	      (setenv "SHELL" (string-append #$bash-minimal "/bin/bash"))

	      ;; nauty: fix configure shebangs and /bin/sh references
	      (substitute* "build.ninja"
		(("autoreconf -vif &&")
		 (string-append
		  "autoreconf -vif && "
		  "sed -i '1s|^#!.*|#!" #$(file-append bash-minimal "/bin/bash") "|' configure && "
		  "sed -i 's|/bin/sh|" #$(file-append bash-minimal "/bin/bash") "|g' configure &&")))))

	  (add-before 'build 'force-make-shell
	    (lambda _
	      (setenv "MAKEFLAGS"
		      (string-append "SHELL=" #$bash-minimal "/bin/bash"))))


          (add-after 'unpack 'enter-m2-directory
            (lambda _
              (chdir "M2")))
          
          ;; Set up build directory following newer Nix approach
          (add-after 'enter-m2-directory 'setup-build-directory
            (lambda _
              ;; cd M2/BUILD/build from newer Nix
              (mkdir-p "BUILD/build")
              (chdir "BUILD/build")))
          
	  (add-after 'setup-build-directory 'setup-external-sources
	    (lambda* (#:key inputs #:allow-other-keys)
	      (mkdir-p "../tarfiles")
	      (let ((nauty-source (assoc-ref inputs "nauty-source"))
		    (gfan-source (assoc-ref inputs "gfan-source")))
		(when nauty-source
		  (copy-file nauty-source "../tarfiles/nauty2_8_9.tar.gz"))
		(when gfan-source
		  (copy-file gfan-source "../tarfiles/gfan0.6.2.tar.gz")))))
	  
          ;; Replicate exact Nix cmake call
          (replace 'configure
            (lambda* (#:key configure-flags #:allow-other-keys)
              ;; Nix: cmake -GNinja -S../.. -B. [flags]
              (apply invoke "cmake" "-S../.." "-B." configure-flags)))
          
          ;; Replace default build with selective build from newer Nix
          ;; (replace 'build
          ;;   (lambda _
          ;;     ;; Follow exact newer Nix sequence:
          ;;     ;; ninja build-libraries (build submodules first)
          ;;     (invoke "ninja" "build-libraries")
          ;;     ;; ninja M2-binary M2-core (core components)
          ;;     (invoke "ninja" "M2-binary" "M2-core")
          ;;     ;; ninja build-programs (supporting programs)
          ;;     (invoke "ninja" "build-programs")))

	  (replace 'build
	    (lambda _
	      (setenv "CheckDocumentation" "false")
	      (setenv "IgnoreExampleErrors" "true")
	      (setenv "RemakeAllDocumentation" "false")
	      (setenv "RerunExamples" "false")
	      
	      (invoke "ninja" "build-libraries")
	      (invoke "ninja" "M2-binary" "M2-core")
	      (invoke "ninja" "build-programs")))
          
          ;; Skip install-packages per maintainer recommendation
          ;; "you don't need to run the 'install-packages' target at all"
          ;; (replace 'install
          ;;   (lambda _
          ;;     ;; Selective package install as per newer Nix approach
          ;;     ;; Install core M2 without the problematic packages
          ;;     (invoke "ninja" "install")))

  ;; 	  (replace 'install
  ;; (lambda _
  ;;   ;; Use install/local to avoid package processing
  ;;   (invoke "ninja" "install/strip")))

	  (replace 'install
  (lambda* (#:key outputs #:allow-other-keys)
    (let ((out (assoc-ref outputs "out")))
      ;; Bypass ninja install entirely - just copy built artifacts
      ;; Everything is already built in usr-dist/ directory
      (copy-recursively "usr-dist" out))))

	  )))

    ;; Dependencies from newer Nix nativeBuildInputs

    (native-inputs
     `(("bash-minimal" ,bash-minimal)
       ("cmake" ,cmake)
       ("ninja" ,ninja)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gcc-toolchain" ,gcc-toolchain)
       ("gnu-make" ,gnu-make)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("bison" ,bison)
       ("python" ,python)
       ("texlive-bin" ,texlive-bin)
       ("cohomcalg" ,cohomcalg)
       ("topcom" ,topcom)
       ("nauty-source"
	,(origin
	   (method url-fetch)
	   (uri "https://pallini.di.uniroma1.it/nauty2_8_9.tar.gz")
	   (sha256
            (base32 "1vn4abz498h8fbh27z0l5jrs4z04d693xklbb5mai5l7yhmv8yn9"))))
       ("gfan-source"
	,(origin
	   (method url-fetch)
	   (uri "https://users-math.au.dk/jensen/software/gfan/gfan0.6.2.tar.gz")
	   (sha256
	    (base32 "02pihqb1lb76a0xbfwjzs1cd6ay3ldfxsm8dvsbl6qs3vkjxax56"))))
       ))
    
    ;; Runtime dependencies from newer Nix buildInputs
    (inputs
     (list
      ;; Mathematical libraries (order from newer Nix)
      singular
      boost
      gdbm
      tbb
      libffi
      readline
      gmp
      libxml2
      eigen
      googletest
      mpfr
      ;; `(,nauty "lib")
      ntl
      glpk
      mpfi
      normaliz
      libgc    ; boehmgc in Nix
      lrslib   ; lrs in Nix
      cddlib
      4ti2     ; _4ti2 in Nix
      flint
      fflas-ffpack
      givaro
      
      ;; Custom packages we created
      mpsolve
      csdp
      
      ;; Available in Guix (was missing in some versions)  
      ;; gfan ; Built by M2's build system, not needed as system dependency
      msolve   ; Available as msolve in Guix
      
      ;; Linear algebra
      lapack
      openblas)) ; blas in Nix
    
    (home-page "https://macaulay2.com")
    (synopsis "Software system for research in algebraic geometry and commutative algebra")
    (description
     "Macaulay2 is a software system devoted to supporting research in
algebraic geometry and commutative algebra.  This version uses the modern
CMake build system and skips package installation for significantly faster
build times while maintaining full mathematical functionality.  Features
include Groebner bases and free resolutions of modules over polynomial rings,
Ext, Tor, and local cohomology.")
    (license license:gpl2+)))


macaulay2
