(use-modules
 (guix packages)
 (guix git-download)
 (guix build-system asdf)
 (gnu packages lisp)
 (ice-9 match)
 ((guix licenses) #:prefix license:)
 (gnu packages lisp-xyz))

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
 (inputs
  `(("alexandria" ,sbcl-alexandria)
    ("anaphora" ,sbcl-anaphora)
    ("let-plus" ,sbcl-let-plus)
    ("fiveam" ,sbcl-fiveam)))
 (arguments
  `(#:asd-systems '("select"
		    "select/tests")
    #:asd-files '("select.asd")))
 (home-page "https://github.com/Lisp-Stat/lisp-stat.git")
 (synopsis "Common Lisp statistics package.")
 (description "Common Lisp statistics package.")
 (license license:ms-pl))
