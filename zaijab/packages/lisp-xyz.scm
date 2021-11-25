(define-module (zaijab packages lisp-xyz)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system asdf)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages lisp-xyz))

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
   (native-inputs `(("fiveam" ,sbcl-fiveam)))
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
