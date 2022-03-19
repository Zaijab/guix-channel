(define-module (zaijab packages lisp-xyz)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system asdf)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages lisp-xyz))

(define-public sbcl-rtg-math-zain
  (let ((commit "29fc5b3d0028a4a11a82355ecc8cca62662c69e0")
        (revision "1"))
    (package
      (name "sbcl-rtg-math-zain")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/cbaggers/rtg-math")
               (commit commit)))
         (file-name (git-file-name "rtg-math" version))
         (sha256
          (base32 "0bhxxnv7ldkkb18zdxyz2rj2a3iawzq2kcp7cn5i91iby7n0082x"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria sbcl-documentation-utils sbcl-glsl-spec sbcl-varjo))
      (home-page "https://github.com/cbaggers/rtg-math")
      (synopsis "Common Lisp library of game-related math functions")
      (description
       "RTG-MATH provides a selection of the math routines most commonly needed
for making realtime graphics in Lisp.")
      (license license:bsd-2))))

(define-public sbcl-cepl.sdl2
  (let ((commit "6da5a030db5e3579c5a1c5350b1ffb8fc9950e9a")
        (revision "1"))
    (package
      (name "sbcl-cepl.sdl2")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
	 (method git-fetch)
	 (uri (git-reference
               (url "https://github.com/cbaggers/cepl.sdl2")
               (commit commit)))
	 (file-name (git-file-name "cepl.sdl2" version))
	 (sha256
          (base32 "0lz8yxm1g2ch0w779lhrs2xkfciy3iz6viz7cdgyd2824isvinjf"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:asd-files '("cepl.sdl2.asd")))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("bordeaux-threads" ,sbcl-bordeaux-threads)
         ("cffi" ,sbcl-cffi)
         ("cl-opengl" ,sbcl-cl-opengl)
         ("cl-ppcre" ,sbcl-cl-ppcre)
         ("documentation-utils" ,sbcl-documentation-utils)
         ("float-features" ,sbcl-float-features)
         ("ieee-floats" ,sbcl-ieee-floats)
         ("split-sequence" ,sbcl-split-sequence)
         ("varjo" ,sbcl-varjo)
	 ("cepl" ,sbcl-cepl)
	 ("sdl2" ,sbcl-sdl2)))
      (propagated-inputs
       (list sbcl-quickproject))
      (home-page "https://github.com/cbaggers/cepl")
      (synopsis "Development playground to work with OpenGL")
      (description
       "CEPL (Code Evaluate Play Loop ) is a lispy and REPL-friendly Common Lisp
library for working with OpenGL.

Its definition of success is making the user feel that GPU programming has
always been part of the languages standard.

The usual approach to using CEPL is to start it at the beginning of your Lisp
session and leave it open for the duration of your work.  You can then treat the
window it creates as just another output for your graphics, analogous to how
@code{*standard-output*} is treated for text.")
      (license license:bsd-2))))

(define-public sbcl-lisp-stat
  (package
    (name "sbcl-lisp-stat")
    (version "master")
    (source (origin
	      (method git-fetch)
	      (uri (git-reference
		    (url "https://github.com/Lisp-Stat/lisp-stat.git")
		    (commit version)))
	      (file-name (git-file-name name version))
	      (sha256
	       (base32
		"16iv1cvlky2gij1ndx2d6q8l35axm72bx52n6v5y3h21aibj197n"))))
    (build-system asdf-build-system/sbcl)
    (propagated-inputs
     `(("sbcl-data-frame" ,sbcl-data-frame)
       ("sbcl-dfio" ,sbcl-dfio)
       ("sbcl-special-functions" ,sbcl-special-functions)
       ("sbcl-numerical-utilities" ,sbcl-numerical-utilities)
       ("sbcl-documentation" ,sbcl-documentation)
       ("sbcl-plot" ,sbcl-plot)
       ("sbcl-select" ,sbcl-select)))
    (home-page "https://github.com/Lisp-Stat/lisp-stat.git")
    (synopsis "Common Lisp statistics package.")
    (description "Common Lisp statistics package.")
    (license license:ms-pl)))

(define-public sbcl-data-frame 
  (package
   (name "sbcl-data-frame")
   (version "master")
   (source (origin
	    (method git-fetch)
	    (uri (git-reference
		  (url "https://github.com/Lisp-Stat/data-frame.git")
		  (commit version)))
	    (file-name (git-file-name name version))
	    (sha256
	     (base32
	      "1dnryjwq8853skpw0r9wgf0sxsi3v4fmqis5kh6fazdcxd53bd2m"))))
   (build-system asdf-build-system/sbcl)
   (home-page "https://github.com/Lisp-Stat/lisp-stat.git")
   (synopsis "Common Lisp statistics package.")
   (description "Common Lisp statistics package.")
   (license license:ms-pl)))

(define-public sbcl-dfio 
  (package
   (name "sbcl-dfio")
   (version "master")
   (source (origin
	    (method git-fetch)
	    (uri (git-reference
		  (url "https://github.com/Lisp-Stat/dfio.git")
		  (commit version)))
	    (file-name (git-file-name name version))
	    (sha256
	     (base32
	      "1jr19ihqyd0k2rqppi8gs3f3gfzrklldzma36g5qsif68gfdfba8"))))
   (build-system asdf-build-system/sbcl)
   (home-page "https://github.com/Lisp-Stat/lisp-stat.git")
   (synopsis "Common Lisp statistics package.")
   (description "Common Lisp statistics package.")
   (license license:ms-pl)))

(define-public sbcl-special-functions 
  (package
   (name "sbcl-special-functions")
   (version "master")
   (source (origin
	    (method git-fetch)
	    (uri (git-reference
		  (url "https://github.com/Lisp-Stat/special-functions.git")
		  (commit version)))
	    (file-name (git-file-name name version))
	    (sha256
	     (base32
	      "16iv1cvlky2gij1ndx2d6q8l35axm72bx52n6v5y3h21aibj197n"))))
   (build-system asdf-build-system/sbcl)
   (home-page "https://github.com/Lisp-Stat/lisp-stat.git")
   (synopsis "Common Lisp statistics package.")
   (description "Common Lisp statistics package.")
   (license license:ms-pl)))

(define-public sbcl-numerical-utilities 
  (package
   (name "sbcl-numerical-utilities")
   (version "master")
   (source (origin
	    (method git-fetch)
	    (uri (git-reference
		  (url "https://github.com/Lisp-Stat/numerical-utilities.git")
		  (commit version)))
	    (file-name (git-file-name name version))
	    (sha256
	     (base32
	      "16iv1cvlky2gij1ndx2d6q8l35axm72bx52n6v5y3h21aibj197n"))))
   (build-system asdf-build-system/sbcl)
   (home-page "https://github.com/Lisp-Stat/lisp-stat.git")
   (synopsis "Common Lisp statistics package.")
   (description "Common Lisp statistics package.")
   (license license:ms-pl)))

(define-public sbcl-documentation 
  (package
   (name "sbcl-documentation")
   (version "master")
   (source (origin
	    (method git-fetch)
	    (uri (git-reference
		  (url "https://github.com/Lisp-Stat/documentation.git")
		  (commit version)))
	    (file-name (git-file-name name version))
	    (sha256
	     (base32
	      "16iv1cvlky2gij1ndx2d6q8l35axm72bx52n6v5y3h21aibj197n"))))
   (build-system asdf-build-system/sbcl)
   (home-page "https://github.com/Lisp-Stat/lisp-stat.git")
   (synopsis "Common Lisp statistics package.")
   (description "Common Lisp statistics package.")
   (license license:ms-pl)))

(define-public sbcl-plot 
  (package
   (name "sbcl-plot")
   (version "master")
   (source (origin
	    (method git-fetch)
	    (uri (git-reference
		  (url "https://github.com/Lisp-Stat/plot.git")
		  (commit version)))
	    (file-name (git-file-name name version))
	    (sha256
	     (base32
	      "16iv1cvlky2gij1ndx2d6q8l35axm72bx52n6v5y3h21aibj197n"))))
   (build-system asdf-build-system/sbcl)
   (home-page "https://github.com/Lisp-Stat/lisp-stat.git")
   (synopsis "Common Lisp statistics package.")
   (description "Common Lisp statistics package.")
   (license license:ms-pl)))

(define-public sbcl-select
  (package
   (name "sbcl-select")
   (version "master")
   (source (origin
	    (method git-fetch)
	    (uri (git-reference
		  (url "https://github.com/Lisp-Stat/select.git")
		  (commit version)))
	    (sha256
	     (base32
	      "1kplgpn79av1p2icyk7h93cy00gshyan96vxlinvwxibhsrhxsj2"))))
   (build-system asdf-build-system/sbcl)
   ;; (native-inputs `(("fiveam" ,sbcl-fiveam)))
   (inputs
    `(("alexandria" ,sbcl-alexandria)
      ("anaphora" ,sbcl-anaphora)
      ("let-plus" ,sbcl-let-plus)))
   (arguments
    `(#:asd-systems '("select"
		      "select/tests")
      #:asd-files '("select.asd")))
   (home-page "https://github.com/Lisp-Stat/lisp-stat.git")
   (synopsis "Common Lisp statistics package.")
   (description "Common Lisp statistics package.")
   (license license:ms-pl)))
