(define-module (zaijab packages macaulay2)
  #:use-module (zaijab packages frobby)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
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
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages xml)
  #:use-module ((guix licenses) #:prefix license:))

;; =============================================================================
;; MACAULAY2 DEPENDENCY ANALYSIS (CORRECTED)
;; =============================================================================
;;
;; M2 has a three-tier dependency strategy:
;;
;; 1. SUBMODULES (always built from source in M2/submodules/, we use recursive? #t):
;;    ✓ bdwgc (Boehm-Demers-Weiser garbage collector) 
;;    ✓ fflas_ffpack (Finite field linear algebra subroutines)
;;    ✓ flint (Fast Library for Number Theory)
;;    ✓ frobby (Monomial ideal computations)  
;;    ✓ givaro (C++ library for arithmetic and algebraic computations)
;;    ✓ googletest (Google's C++ testing framework)
;;    ✓ mathic (Data structures for computational algebra)
;;    ✓ mathicgb (Groebner basis computations)
;;    ✓ memtailor (Memory pool manager)
;;
;; 2. SYSTEM DETECTION (M2 configure detects these, we provide as inputs):
;;    LIBRARIES: gc gdbm gmp mpfr mpfi normaliz readline ntl flint factory lapack 
;;               mpsolve frobby glpk cddlib fplll givaro fflas_ffpack linbox gtest 
;;               tbb memtailor mathic mathicgb eigen
;;    PROGRAMS:  4ti2 gfan csdp nauty cddplus lrslib topcom cohomcalg msolve
;;
;; 3. FALLBACK DOWNLOADS (only if --enable-download, IMPOSSIBLE in Guix):
;;    M2/libraries/ contains build instructions for missing dependencies
;;    Downloads from https://macaulay2.com/Downloads/OtherSourceCode
;;
;; *** GUIX STRATEGY ***
;; No network access during build, so we MUST provide all required dependencies
;; as inputs. Nix does NOT use --enable-download, proving M2 works without it.
;;
;; =============================================================================
;; DEPENDENCY STATUS (CORRECTED AFTER INVESTIGATION)
;; =============================================================================

;; MPSOLVE - Polynomial solver (MISSING from Guix, REQUIRED)
;; Source: https://github.com/robol/MPSolve or M2 fallback
;; Used by: M2's roots() function for finding polynomial roots
;; Status: MUST CREATE custom package

(define-public mpsolve
  (package
    (name "mpsolve")
    (version "3.2.1")  ; Version used by M2 libraries/mpsolve/
    (source
     (origin
       (method url-fetch)
       (uri "https://macaulay2.com/Downloads/OtherSourceCode/mpsolve-3.2.1.tar.gz")
       (sha256
        (base32 "13p7a4fsx3gy8h6syw6gfb2jxcnrwkcvrgya4h7h4bmbx654449x"))))
       
    ;; FALLBACK APPROACH: If GitHub fails, use M2's mirror:
    ;; (source
    ;;  (origin
    ;;    (method url-fetch)
    ;;    (uri "https://macaulay2.com/Downloads/OtherSourceCode/mpsolve-3.2.1.tar.gz") 
    ;;    (sha256 (base32 "..."))))
    
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "--disable-shared"      ; M2 uses static linking
              "--disable-dependency-tracking"
              "--disable-examples"
              "--disable-debug"
              "--disable-silent-rules"
              "--disable-ui"
              "--disable-graphical-debugger"
              "--disable-documentation")
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

;; CSDP - Semidefinite programming solver (MISSING from Guix, REQUIRED)  
;; Source: https://github.com/coin-or/Csdp or M2 fallback
;; Used by: M2's semidefinite programming functions
;; Status: MUST CREATE custom package

(define-public csdp
  (package
    (name "csdp")
    (version "6.2.0")  ; Version used by M2 libraries/csdp/
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

;; =============================================================================
;; CURRENT STATUS SUMMARY (as of this session)
;; =============================================================================
;;
;; ✅ COMPLETED SUCCESSFULLY:
;; - Comprehensive M2 dependency analysis and documentation
;; - mpsolve package: Built and tested
;;   Built: /gnu/store/9apchqcmflcvvsqfrv4ap41rmzvwipa3-mpsolve-3.2.1  
;;   Test: `guix shell mpsolve -L . -- mpsolve --version` ✓
;; - csdp package: Built and tested  
;;   Built: /gnu/store/c516ybyg1v80w8zp6a6gkbiy1zp3nc0i-csdp-6.2.0
;;   Test: `guix shell csdp -L . -- csdp` shows "CSDP 6.2.0" ✓
;; - factory dependency: Available in existing `singular` Guix package ✓
;; - All other major dependencies: Available in Guix (see analysis above) ✓
;; 
;; ⏳ READY FOR NEXT SESSION:
;; - Main Macaulay2 package build and test
;; - Betti coefficient calculation using working M2
;;
;; =============================================================================
;; REMAINING WORK PLAN
;; =============================================================================
;;
;; PHASE 1: ATTEMPT M2 BUILD WITH CURRENT DEPENDENCIES
;; 1. Try building main macaulay2 package with:
;;    - Our built mpsolve and csdp packages  
;;    - Existing Guix packages (singular for factory, normaliz, etc.)
;;    - All submodules via recursive? #t
;;    - Nix patches applied
;;
;; PHASE 2: COMPARE WITH NIX IMPLEMENTATION IF PHASE 1 FAILS  
;; The Nix package (cache/m2-nix-pure/default.nix) uses custom builds:
;; - my-normaliz: Builds Normaliz v3.10.2 with custom flags
;; - my-frobby: Builds frobby with M2-specific patches
;; - my-lrslib: Custom lrslib build  
;; - my-TOPCOM: Custom TOPCOM build
;; - my-singular-factory: Builds ONLY factory component from Singular
;;
;; If M2 fails with system packages, create these custom packages:
;; - Check versions: Normaliz v3.10.2, specific frobby/lrslib/TOPCOM commits
;; - Apply M2 patches from M2/libraries/*/patch-* files
;; - Use same build flags as Nix my-* packages
;;
;; PHASE 3: HANDLE PROGRAM LINKING  
;; Nix creates: $out/libexec/Macaulay2/bin/normaliz -> normaliz/bin/normaliz
;; Add similar program linking phase to M2 package if needed
;;
;; PHASE 4: VERSION COMPARISON
;; If build fails, consider downgrading from M2 1.24.11 to proven Nix 1.23:
;; - Change version to "1.23" 
;; - Use commit "ec65028f1527076b663279b1311188caa9e22b67"
;; - Verify hash matches Nix implementation
;;
;; PHASE 5: TESTING AND VALIDATION
;; 1. Build M2 successfully  
;; 2. Test basic M2 functionality: `guix shell macaulay2 -L . -- M2 --version`
;; 3. Test mathematical functions: load and run betti-computation.m2
;; 4. Verify output matches expected Betti table results
;;
;; PHASE 6: FALLBACK APPROACHES IF ALL ELSE FAILS
;; 1. Use M2's OtherSourceCode for all dependencies:
;;    - Download tarballs to zaijab/files/damn/ (already partially done)
;;    - Use local-file in M2 build process with --enable-download
;; 2. Simplify build system:
;;    - Switch from gnu-build-system to trivial-build-system  
;;    - Replicate Nix builder.sh approach more directly
;; 3. Create minimal M2 build:
;;    - Disable non-essential features to get basic functionality working
;;    - Add features incrementally once core works
;;
;; =============================================================================
;; TESTING COMMANDS FOR NEXT SESSION
;; =============================================================================
;; 
;; # Test current dependency packages:
;; guix build mpsolve csdp -L . --keep-going
;; guix shell mpsolve -L . -- mpsolve --version  
;; guix shell csdp -L . -- csdp
;;
;; # Try main M2 build (when ready):
;; guix build macaulay2 -L . --keep-going 
;;
;; # Test M2 functionality (when built):
;; guix shell macaulay2 -L . -- M2 --version
;; guix shell macaulay2 -L . -- M2 < zaijab/files/betti-computation.m2
;;
;; # Debug with build logs:
;; guix build macaulay2 -L . --keep-failed

(define-public macaulay2
  (package
    (name "macaulay2")
    (version "1.25.05") ; Updated to recent version without libatomic_ops submodule
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Macaulay2/M2")
             (commit (string-append "release-" version))
             (recursive? #t))) ; CRITICAL: Fetch submodules 
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lajihpski1siajgjp1zqdd6596s3aw58wg72s218s97cnzlla6p"))))

    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "--disable-documentation"     ; Nix uses this
              "--with-system-gc"           ; Nix uses this  
              ;; Exact Nix boost configuration:
              (string-append "--with-boost=" #$(this-package-input "boost") "/include")
              (string-append "--with-boost-libdir=" #$(this-package-input "boost") "/lib")
              (string-append "--with-gtest-source-path=" 
                           #$(this-package-native-input "googletest") "/src/googletest")
              ;; Point to system frobby
              (string-append "--with-frobby=" #$(this-package-input "frobby")))
      #:make-flags
      #~(list (string-append "CPPFLAGS=-I" #$(this-package-input "cddlib") "/include/cddlib"
                            " -I" #$(this-package-native-input "googletest") "/include"))
      #:tests? #f ; No test suite in standard build
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'enter-source-directory
            (lambda _ (chdir "M2") #t))
          (add-before 'bootstrap 'apply-nix-patches
            (lambda _
              ;; Apply same patches as Nix macaulay2.patch
              ;; Patch 1: Fix GNUmakefile.in git operations
              (when (file-exists? "GNUmakefile.in")
                (substitute* "GNUmakefile.in"
                  ;; Disable git submodule checkout that fails in isolated builds
                  (("git-checkout-in-\\$1:; cd @srcdir@/\\.\\..*") "git-checkout-in-$1:")
                  ;; Use sh explicitly for autogen.sh
                  (("cd @srcdir@/submodules/\\$1 && NOCONFIGURE=1 \\./autogen\\.sh")
                   "cd @srcdir@/submodules/$1 && NOCONFIGURE=1 sh ./autogen.sh")
                  ;; Remove git dependency rules  
                  (("@srcdir@/submodules/\\$1/configure\\.ac:.*git-checkout.*") "")))
              
              ;; Patch 2: Fix Macaulay2/bin/Makefile.in time command
              (when (file-exists? "Macaulay2/bin/Makefile.in")
                (substitute* "Macaulay2/bin/Makefile.in"
                  ;; Remove time command that may not be available
                  (("time @CXX@") "@CXX@")))
              
              ;; Patch 3: Fix configure.ac lsb_release issue
              (when (file-exists? "configure.ac")
                (substitute* "configure.ac"
                  ;; Remove double-quotes from ISSUE variable
                  (("\\s+ISSUE=\\$ISSUE_FLAVOR-\\$ISSUE_RELEASE") 
                   "\n     ISSUE=$ISSUE_FLAVOR-$ISSUE_RELEASE\n\n     # remove double-quotes produced by some versions of lsb_release\n     ISSUE=`echo $ISSUE | sed 's/\"//g'`")))
              #t))
          (add-before 'configure 'set-issue-fallback
            (lambda _
              ;; Ensure ISSUE environment variable is set as fallback
              (setenv "ISSUE" "GuixLinux-unknown")
              #t))
          (add-after 'configure 'fix-shell-paths
            (lambda _
              ;; Fix hardcoded /bin/sh paths in generated files
              (let ((shell (which "sh")))
                ;; Fix main Makefile
                (substitute* "Makefile"
                  (("/bin/sh") shell))
                ;; Fix any included makefiles that might have /bin/sh
                (for-each (lambda (file)
                  (when (file-exists? file)
                    (substitute* file
                      (("/bin/sh") shell))))
                  (find-files "." "\\.mk$|Makefile.*"))
                ;; Also set SHELL variable for make
                (setenv "SHELL" shell))
              #t)))))

    ;; Build-time dependencies (Nix buildInputs that don't get linked)
    (native-inputs
     (list autoconf
           automake
           bison       ; not checked by M2 autoconf but required late in build
           flex
           gfortran-toolchain
           libtool
           perl        ; not checked by M2 autoconf but required to build ntl
           pkg-config
           python      ; Required by configure script
           texinfo     ; surprise requirement at the very last step of build!
           googletest  ; gtest + gtest.dev + gtest.src combined
           which))     ; surprise requirement for _4ti2 (markov script)

    ;; Runtime dependencies
    (inputs
     (list
      ;; =======================================================================
      ;; LIBRARIES AVAILABLE IN GUIX (✓ = exists, ~ = different name/component)
      ;; =======================================================================
      
      ;; Core system libraries
      libgc                    ; ✓ "gc" -> libgc in Guix  
      gdbm                     ; ✓ GNU database manager
      gmp                      ; ✓ GNU Multiple Precision Arithmetic
      mpfr                     ; ✓ GNU MPFR Library  
      mpfi                     ; ✓ Multiple Precision Floating-point Interval
      readline                 ; ✓ GNU Readline
      
      ;; Mathematical libraries  
      ntl                      ; ✓ Number Theory Library
      flint                    ; ✓ Fast Library for Number Theory
      lapack                   ; ✓ Linear Algebra PACKage
      glpk                     ; ✓ GNU Linear Programming Kit
      cddlib                   ; ✓ Double Description Method (provides cdd programs)
      fplll                    ; ✓ Floating-point LLL
      givaro                   ; ✓ C++ arithmetic/algebraic computations
      linbox                   ; ✓ C++ template library for linear algebra  
      tbb                      ; ✓ Intel Threading Building Blocks
      eigen                    ; ✓ C++ template library for linear algebra
      
      ;; Mathematical programs
      4ti2                     ; ✓ Algebraic/geometric problems
      ;; gfan                  ; ✗ Temporarily disabled due to C++ compilation issues
      nauty                    ; ✓ Graph isomorphism testing
      lrslib                   ; ✓ Reverse search algorithm
      msolve                   ; ✓ Polynomial system solving
      
      ;; CORRECTED: Available as subcomponents of other packages
      singular                 ; ~ Provides factory (libfactory.so, factory.h)
      normaliz                 ; ✓ Convex geometry computations
      
      ;; =======================================================================
      ;; MISSING DEPENDENCIES (need custom packages above)
      ;; =======================================================================
      
      mpsolve                  ; ✗ Polynomial solver (custom package above)
      csdp                     ; ✗ Semidefinite programming (custom package above)
      frobby                   ; ✗ Monomial ideal computations (custom package to avoid submodule download)
      ;; topcom                ; ? Triangulations (may not be required)
      ;; cddplus               ; ? Alternative cdd (probably not needed, cddlib sufficient)
      
      ;; =======================================================================
      ;; SUBMODULE DEPENDENCIES (handled by recursive? #t)
      ;; =======================================================================
      ;; These are built from submodules, but M2 may also try system detection:
      ;; - fflas_ffpack (submodule preferred over Guix package)
      ;; - frobby (submodule preferred over our custom package) 
      ;; - memtailor, mathic, mathicgb (only available as submodules)
      
      ;; Standard C/C++ support libraries  
      boost                    ; ✓ Boost C++ Libraries
      libffi                   ; ✓ Foreign Function Interface
      libxml2                  ; ✓ XML parsing library
      zlib                     ; ✓ Compression library
      xz                       ; ✓ LZMA compression
      ncurses))                ; ✓ Terminal control library

    (home-page "https://macaulay2.com")
    (synopsis "Software system for research in algebraic geometry and commutative algebra")
    (description
     "Macaulay2 is a software system devoted to supporting research in
algebraic geometry and commutative algebra.  Features include Groebner bases
and free resolutions of modules over polynomial rings, Ext, Tor, and local
cohomology.  It also offers an embedded, high-level programming language
designed to support computations in algebraic geometry and commutative algebra.")
    (license license:gpl2+)))

;;;; Old Macaulay build. It was confusing you when building.

;; (define-module (zaijab packages macaulay2-autotools)
;;   #:use-module (zaijab packages cohomcalg)
;;   #:use-module (zaijab packages frobby)
;;   ;; #:use-module (zaijab packages topcom)
;;   ;; 
;;   ;; COMPARISON WITH NIX IMPLEMENTATION:
;;   ;; 
;;   ;; Key differences found between Nix (cache/macaulay2-build/default.nix) and this Guix implementation:
;;   ;; 
;;   ;; 1. VERSIONING:
;;   ;;    - Nix uses: rev = "ec65028f1527076b663279b1311188caa9e22b67" (1.23 release)
;;   ;;    - Guix uses: version "1.24.11" with commit (string-append "release-" version)
;;   ;;    - NOTE: We're targeting a newer version than the working Nix implementation
;;   ;;
;;   ;; 2. BUILD SYSTEM APPROACH:
;;   ;;    - Nix uses: custom builder.sh script with stdenv.mkDerivation
;;   ;;    - Guix uses: gnu-build-system with modify-phases
;;   ;;    - Nix builder.sh: simple sequence (copy src, patch, make, configure, make, install)
;;   ;;    - Guix phases: complex multi-phase approach with bootstrap, configure, build phases
;;   ;;
;;   ;; 3. SOURCE HANDLING:
;;   ;;    - Nix: fetchSubmodules = true (gets all submodules)  
;;   ;;    - Guix: recursive? #f (disabled recursive submodule fetch)
;;   ;;    - This is likely a MAJOR difference - Nix gets submodules, we don't
;;   ;;
;;   ;; 4. CUSTOM DEPENDENCIES:
;;   ;;    - Nix defines: my-cohomCalg, my-normaliz, my-frobby, my-lrslib, my-TOPCOM, my-singular-factory
;;   ;;    - Guix uses: cohomcalg, frobby (plus system normaliz, lrslib, singular)
;;   ;;    - Nix builds these from scratch with specific versions and build scripts
;;   ;;    - We try to use existing Guix packages where possible (normaliz, singular)
;;   ;;
;;   ;; 5. DEPENDENCY DIFFERENCES:
;;   ;;    - Nix has: lsb-release, texinfoInteractive, gtest+gtest.dev+gtest.src, csdp, mpsolve, blas
;;   ;;    - Guix missing: lsb-release, csdp, mpsolve, separate gtest components
;;   ;;    - Nix uses: boehmgc, _4ti2, libatomic_ops
;;   ;;    - Guix uses: libgc, 4ti2, libatomic-ops (naming differences)
;;   ;;
;;   ;; 6. CONFIGURE FLAGS:  
;;   ;;    - Both use similar flags but Nix has explicit boost paths:
;;   ;;      Nix: "--with-boost=${pkgs.boost.dev}" "--with-boost-libdir=${pkgs.boost}/lib"
;;   ;;      Guix: (string-append "--with-boost=" #$(this-package-input "boost"))
;;   ;;
;;   ;; 7. COMPILATION FLAGS:
;;   ;;    - Nix: cppflags = "-I${pkgs.cddlib}/include/cddlib -I${pkgs.gtest.dev}/include"
;;   ;;    - Guix: CPPFLAGS in make-flags with similar paths but different syntax
;;   ;;
;;   ;; 8. PROGRAM LINKING:
;;   ;;    - Nix: link_programs that creates symbolic links to normaliz
;;   ;;    - Guix: create-program-links phase (but currently incomplete, topcom commented out)
;;   ;;
;;   ;; 9. PATCHING APPROACH:
;;   ;;    - Nix: Simple patch application with patch -p0 < $patch
;;   ;;    - Guix: Complex multi-phase patching with substitute* in multiple phases
;;   ;;    - Both apply the same base patch content but differently
;;   ;;
;;   ;; 10. VERSION MISMATCH IMPLICATIONS:
;;   ;;     - Nix proven working with 1.23, we're attempting 1.24.11
;;   ;;     - May have introduced new build complexities or changed dependencies
;;   ;;     - Should consider downgrading to 1.23 to match working Nix version first
;;   #:use-module (guix packages)
;;   #:use-module (guix git-download)
;;   #:use-module (guix build-system gnu)
;;   #:use-module (guix gexp)
;;   #:use-module (gnu packages)
;;   #:use-module (gnu packages algebra)
;;   #:use-module (gnu packages autotools)
;;   #:use-module (gnu packages bison)
;;   #:use-module (gnu packages flex)
;;   #:use-module (gnu packages boost)
;;   #:use-module (gnu packages check)
;;   #:use-module (gnu packages compression)
;;   #:use-module (gnu packages cpp)
;;   #:use-module (gnu packages documentation)
;;   #:use-module (gnu packages gcc)
;;   #:use-module (gnu packages commencement)
;;   #:use-module (gnu packages libffi)
;;   #:use-module (gnu packages linux)
;;   #:use-module (gnu packages maths)
;;   #:use-module (gnu packages algebra)
;;   #:use-module (gnu packages ncurses)
;;   #:use-module (gnu packages multiprecision)
;;   #:use-module (gnu packages perl)
;;   #:use-module (gnu packages pkg-config)
;;   #:use-module (gnu packages python)
;;   #:use-module (gnu packages readline)
;;   #:use-module (gnu packages tbb)
;;   #:use-module (gnu packages texinfo)
;;   #:use-module (gnu packages version-control)
;;   #:use-module (gnu packages xml)
;;   #:use-module (gnu packages bdw-gc)
;;   #:use-module (gnu packages dbm)
;;   #:use-module (gnu packages base)
;;   #:use-module (gnu packages web)
;;   #:use-module (gnu packages wget)
;;   #:use-module ((guix licenses) #:prefix license:))

;; (define-public macaulay2-autotools
;;   (package
;;     (name "macaulay2-autotools")
;;     (version "1.24.11")
;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;              (url "https://github.com/Macaulay2/M2")
;;              (commit (string-append "release-" version))
;;              (recursive? #f)))  ; Disable recursive submodule fetch
;;        ;; *** CRITICAL DIFFERENCE FROM NIX ***
;;        ;; Nix source configuration:
;;        ;; src = pkgs.fetchFromGitHub {
;;        ;;   owner = "Macaulay2"; repo = "M2";
;;        ;;   rev = "ec65028f1527076b663279b1311188caa9e22b67"; # 1.23 release  
;;        ;;   hash = "sha256-uD1fpz9Awa+4W55C5BRPxIIxrLXffQx4b3JvpJ+QMc0=";
;;        ;;   fetchSubmodules = true;  <<<--- KEY DIFFERENCE!
;;        ;; };
;;        ;;
;;        ;; We set recursive? #f, Nix sets fetchSubmodules = true
;;        ;; This means Nix gets ALL submodules (memtailor, mathic, mathicgb, etc.)
;;        ;; We don't get submodules, which may be why our builds fail
;;        ;;
;;        ;; Also: Nix uses specific commit hash for 1.23, we use "release-1.24.11"
;;        ;; The version bump might have introduced incompatibilities
;;        (file-name (git-file-name name version))
;;        (sha256
;;         (base32 "0xgiy02rg523fky40qfghvrakp8svamcyvn3qzjzk5xcd27j7rs0"))))
;;     (build-system gnu-build-system)
;;     ;; NIX BUILD COMPARISON: Nix uses stdenv.mkDerivation with custom builder.sh
;;     ;; instead of gnu-build-system. The Nix approach is simpler:
;;     ;; 1. Copy source (cp --no-preserve=mode -r $src src)
;;     ;; 2. Apply patch (patch -p0 < $patch)  
;;     ;; 3. Export CPPFLAGS
;;     ;; 4. make (bootstraps autotools)
;;     ;; 5. ./configure --prefix=$out $configureArgs
;;     ;; 6. make 
;;     ;; 7. make install
;;     ;;
;;     ;; Our Guix approach uses gnu-build-system which runs:
;;     ;; 1. unpack -> extract-source-directory -> patch-template-files -> bootstrap -> configure -> build -> install
;;     ;; This is more complex but follows Guix patterns
;;     (arguments
;;      (list
;;       #:configure-flags
;;       #~(list "--disable-documentation"    ; Critical: Prevents build failures
;;               "--with-system-gc"
;;               "--enable-download"           ; Allow downloading missing dependencies
;;               ;; NIX CONFIGURE COMPARISON:
;;               ;; Nix configureArgs = [
;;               ;;   "--with-boost=${pkgs.boost.dev}"
;;               ;;   "--with-boost-libdir=${pkgs.boost}/lib"  
;;               ;;   "--with-system-gc"
;;               ;;   "--disable-documentation"
;;               ;;   "--with-gtest-source-path=${pkgs.gtest.src}/googletest"
;;               ;; ];
;;               ;; 
;;               ;; Key differences:
;;               ;; - Nix uses boost.dev for headers, we use boost directly
;;               ;; - Nix has explicit boost lib path, we derive it
;;               ;; - Nix points to gtest.src/googletest, we use googletest/src/googletest
;;               ;; - We added --enable-download which Nix doesn't have (but maybe should?)
;;               (string-append "--with-boost=" #$(this-package-input "boost"))
;;               (string-append "--with-boost-libdir=" #$(this-package-input "boost") "/lib")
;;               (string-append "--with-gtest-source-path=" 
;;                            #$(this-package-native-input "googletest") 
;;                            "/src/googletest"))
;;       #:make-flags
;;       #~(list (string-append "CPPFLAGS=-I" #$(this-package-input "cddlib") "/include/cddlib"
;;                             " -I" #$(this-package-native-input "googletest") "/include"))
;;       ;; NIX CPPFLAGS COMPARISON: 
;;       ;; Nix: cppflags = "-I${pkgs.cddlib}/include/cddlib -I${pkgs.gtest.dev}/include";
;;       ;; Guix: CPPFLAGS=-I.../cddlib/include/cddlib -I.../googletest/include
;;       ;;
;;       ;; Key difference: Nix uses gtest.dev for include headers, we use googletest directly
;;       ;; This might be significant - gtest.dev might have different header layout
;;       #:tests? #f  ; Tests require additional setup
;;       #:phases
;;       #~(modify-phases %standard-phases
;;           (add-after 'unpack 'enter-source-directory
;;             (lambda _
;;               (chdir "M2")
;;               #t))
;;           (add-before 'bootstrap 'patch-template-files
;;             (lambda _
;;               ;; Apply equivalent of macaulay2.patch manually to template files
;;               (when (file-exists? "GNUmakefile.in")
;;                 (substitute* "GNUmakefile.in"
;;                   ;; Remove git checkout commands that fail in isolated builds
;;                   (("git-checkout-in-\\$1:; cd @srcdir@/\\.\\..*") "git-checkout-in-$1:")
;;                   ;; Use sh explicitly for autogen.sh
;;                   (("cd @srcdir@/submodules/\\$1 && NOCONFIGURE=1 \\./autogen\\.sh")
;;                    "cd @srcdir@/submodules/$1 && NOCONFIGURE=1 sh ./autogen.sh")
;;                   ;; Fix Makefile dependencies that require git
;;                   (("@srcdir@/submodules/\\$1/configure\\.ac:.*git-checkout.*") "")))
;;               ;; Fix Macaulay2/bin/Makefile.in 
;;               (when (file-exists? "Macaulay2/bin/Makefile.in")
;;                 (substitute* "Macaulay2/bin/Makefile.in"
;;                   ;; Remove time command that may not be available
;;                   (("time @CXX@") "@CXX@")))
;;               #t))
;;           (add-before 'configure 'set-issue-env
;;             (lambda _
;;               ;; Workaround for the "issue not found" configure error
;;               (setenv "ISSUE" "GNU/Linux")
;;               #t))
;;           (add-before 'bootstrap 'apply-known-good-patches
;;             (lambda _
;;               ;; Apply exact patches from the working Nix implementation
;;               ;; Patch 1: Fix GNUmakefile.in git operations
;;               (when (file-exists? "GNUmakefile.in")
;;                 (substitute* "GNUmakefile.in"
;;                   ;; Disable git submodule checkout that fails in isolated builds
;;                   (("git-checkout-in-\\$1:; cd @srcdir@/\\.\\..*") "git-checkout-in-$1:")
;;                   ;; Use sh explicitly for autogen.sh 
;;                   (("cd @srcdir@/submodules/\\$1 && NOCONFIGURE=1 \\./autogen\\.sh")
;;                    "cd @srcdir@/submodules/$1 && NOCONFIGURE=1 sh ./autogen.sh")
;;                   ;; Remove git dependency in Makefile rules
;;                   (("@srcdir@/submodules/\\$1/configure\\.ac:.*git-checkout.*") "")))
              
;;               ;; Patch 2: Fix Macaulay2/bin/Makefile.in time command
;;               (when (file-exists? "Macaulay2/bin/Makefile.in")
;;                 (substitute* "Macaulay2/bin/Makefile.in"
;;                   ;; Remove time command that may not be available
;;                   (("time @CXX@") "@CXX@")))
;;               #t))
;;           (add-after 'configure 'fix-generated-shell-paths
;;             (lambda _
;;               ;; Fix /bin/sh references in generated Makefile
;;               (let ((shell (which "sh")))
;;                 (substitute* "Makefile"
;;                   (("/bin/sh") shell))
;;                 ;; Also fix any other generated files that might have shell references
;;                 (when (file-exists? "include/config.Makefile")
;;                   (substitute* "include/config.Makefile"
;;                     (("/bin/sh") shell))))
;;               #t))
;;           (add-after 'install 'create-program-links
;;             (lambda* (#:key outputs #:allow-other-keys)
;;               (let* ((out (assoc-ref outputs "out"))
;;                      (libexec-bin (string-append out "/libexec/Macaulay2/bin")))
;;                 ;; Create required program links that M2 expects
;;                 (mkdir-p libexec-bin)
;;                 ;; Skip topcom for now due to bundled dependency complexity
;;                 ;; (symlink (string-append #$topcom "/bin/points2triangs")
;;                 ;;          (string-append libexec-bin "/points2triangs"))
;;                 #t))))))
;;     (native-inputs
;;      (list autoconf
;;            automake
;;            bison
;;            flex
;;            gfortran-toolchain
;;            libtool
;;            perl        ; Required to build ntl
;;            pkg-config
;;            python      ; Required by configure script
;;            texinfo     ; Required at final build step
;;            googletest
;;            wget))      ; Required for downloading missing dependencies
;;     ;; NIX NATIVE-INPUTS (buildInputs) COMPARISON:
;;     ;; Nix buildInputs includes these that we have:
;;     ;;   autoconf, automake, bison, boost, eigen, gcc, gfortran, libffi, libtool,
;;     ;;   libxml2, libz, lzma, ncurses, perl, pkg-config, tbb, texinfoInteractive,
;;     ;;   gmp, gtest+gtest.dev+gtest.src, readline, lapack, boehmgc, gdbm, _4ti2,
;;     ;;   cddlib, csdp, flint, gfan, glpk, libatomic_ops, mpfi, mpfr, mpsolve,
;;     ;;   nauty, ntl, which, givaro, fflas-ffpack, blas
;;     ;;
;;     ;; Nix buildInputs includes these we DON'T have:
;;     ;;   - lsb-release (might be important for ISSUE detection)
;;     ;;   - texinfoInteractive vs texinfo (interactive version)
;;     ;;   - gtest.dev + gtest.src separate packages (vs our single googletest)
;;     ;;   - csdp, mpsolve (missing math libraries)
;;     ;;   - blas (separate from fflas-ffpack)
;;     ;;
;;     ;; Plus all the custom packages: my-cohomCalg, my-normaliz, my-frobby, 
;;     ;; my-lrslib, my-TOPCOM, my-singular-factory
;;     (inputs
;;      (list boost
;;            eigen
;;            libffi
;;            libxml2
;;            zlib
;;            xz
;;            ncurses
;;            tbb
           
;;            ;; Math libraries normally downloaded during build
;;            gmp
;;            readline
;;            lapack
;;            libgc    ; Boehm GC
;;            gdbm
;;            4ti2
;;            cddlib
;;            flint
;;            ;; gfan  ; Skip for now due to int32_t compilation issues
;;            glpk
;;            libatomic-ops
;;            mpfi
;;            mpfr
;;            nauty
;;            ntl
           
;;            ;; Linear algebra libraries  
;;            givaro
;;            fflas-ffpack
;;            openblas
           
;;            ;; Our custom packages not in Guix
;;            cohomcalg
;;            frobby  ; Now working with gexp-based shebang fixes
;;            ;; topcom  ; Skip for now due to bundled dependency complexity
           
;;            ;; Use existing packages from Guix
;;            lrslib
;;            normaliz
;;            singular
           
;;            ;; *** CRITICAL CUSTOM PACKAGE DIFFERENCES ***
;;            ;; 
;;            ;; NIX USES CUSTOM BUILDS FOR ALL THESE:
;;            ;; - my-cohomCalg: builds from BenjaminJurke/cohomCalg v0.32
;;            ;; - my-normaliz: builds from Normaliz/Normaliz v3.10.2 with custom script
;;            ;; - my-frobby: builds from Macaulay2/frobby specific commit with custom flags 
;;            ;; - my-lrslib: builds from external tarball with custom script
;;            ;; - my-TOPCOM: builds from external tarball TOPCOM-0.17.8
;;            ;; - my-singular-factory: builds ONLY factory subcomponent from Singular Release-4-2-1
;;            ;;
;;            ;; WE USE EXISTING GUIX PACKAGES:
;;            ;; - cohomcalg: our custom package (matches Nix approach)
;;            ;; - frobby: our custom package (matches Nix approach)  
;;            ;; - lrslib: existing Guix package (different from Nix custom build)
;;            ;; - normaliz: existing Guix package (different from Nix custom build)
;;            ;; - singular: existing Guix package (different from Nix factory-only build)
;;            ;;
;;            ;; This mismatch might be causing compatibility issues!
;;            ;; Nix builds specific versions with custom configurations,
;;            ;; we use whatever Guix provides which might have different versions,
;;            ;; build flags, or include different components.
           
;;            ;; Utility required for 4ti2
;;            which))
;;     (home-page "https://macaulay2.com")
;;     (synopsis "Computer algebra system for algebraic geometry and commutative algebra")
;;     (description
;;      "Macaulay2 is a software system devoted to supporting research in algebraic
;; geometry and commutative algebra, whose creation has been funded by the National
;; Science Foundation since 1992.  Features include Groebner bases and free resolutions
;; of modules over polynomial rings (in a non-commutative setting as well), Ext, Tor,
;; and local cohomology.  It also offers an embedded, high-level programming language
;; designed to support computations in algebraic geometry and commutative algebra.")
;;     (license license:gpl2+)))

;; ============================================================================
;; COMPREHENSIVE NIX VS GUIX ANALYSIS SUMMARY
;; ============================================================================
;;
;; CRITICAL ISSUES IDENTIFIED:
;;
;; 1. **SUBMODULE ISSUE** (Highest Priority)
;;    Problem: Nix fetches submodules (fetchSubmodules = true), we don't (recursive? #f)
;;    Impact: Missing essential submodules like memtailor, mathic, mathicgb
;;    Solution: Change recursive? #f to recursive? #t, or handle submodules manually
;;
;; 2. **VERSION MISMATCH** (High Priority) 
;;    Problem: Nix uses proven 1.23 (ec65028f...), we use untested 1.24.11
;;    Impact: Version 1.24.11 may have new build requirements or incompatibilities
;;    Solution: Downgrade to 1.23 initially to match working Nix implementation
;;
;; 3. **CUSTOM PACKAGE VERSIONS** (High Priority)
;;    Problem: We use Guix packages, Nix builds specific versions with custom flags
;;    Impact: Version/configuration mismatches causing runtime failures
;;    Examples: 
;;      - Nix normaliz v3.10.2 vs Guix normaliz (unknown version)
;;      - Nix singular-factory (factory-only) vs Guix singular (full package)
;;    Solution: Create custom packages matching Nix versions exactly
;;
;; 4. **BUILD SYSTEM COMPLEXITY** (Medium Priority)
;;    Problem: Nix uses simple 7-step builder.sh, we use complex multi-phase approach
;;    Impact: More opportunity for phase interactions and failures
;;    Solution: Consider simplifying phases or switching to trivial-build-system
;;
;; 5. **MISSING DEPENDENCIES** (Medium Priority)
;;    Problem: Missing lsb-release, csdp, mpsolve, blas, gtest.dev components
;;    Impact: Configure or runtime failures
;;    Solution: Add missing dependencies to inputs
;;
;; RECOMMENDED IMPLEMENTATION STRATEGY:
;;
;; Phase 1: Minimal Changes for Quick Success
;; - Change version from 1.24.11 to 1.23 with Nix commit hash
;; - Change recursive? #f to recursive? #t
;; - Add missing dependencies: lsb-release, csdp, mpsolve
;; - Try build with minimal changes first
;;
;; Phase 2: Custom Package Alignment  
;; - Create custom normaliz package matching Nix v3.10.2
;; - Create custom singular-factory package (factory component only)
;; - Create custom lrslib, TOPCOM packages matching Nix versions
;; - Update inputs to use custom packages
;;
;; Phase 3: Build System Simplification
;; - Consider switching to trivial-build-system with simple build script
;; - Replicate Nix builder.sh approach more closely
;; - Reduce complex phase interactions
