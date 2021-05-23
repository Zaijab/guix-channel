(use-modules
 (guix packages)
 (guix git-download)
 (guix build-system asdf)
 (ice-9 match)
 ((guix licenses) #:prefix license:))

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
 (license license:ms-pl))

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
	    "16iv1cvlky2gij1ndx2d6q8l35axm72bx52n6v5y3h21aibj197n"))))
 (build-system asdf-build-system/sbcl)
 (home-page "https://github.com/Lisp-Stat/lisp-stat.git")
 (synopsis "Common Lisp statistics package.")
 (description "Common Lisp statistics package.")
 (license license:ms-pl))

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
	    "16iv1cvlky2gij1ndx2d6q8l35axm72bx52n6v5y3h21aibj197n"))))
 (build-system asdf-build-system/sbcl)
 (home-page "https://github.com/Lisp-Stat/lisp-stat.git")
 (synopsis "Common Lisp statistics package.")
 (description "Common Lisp statistics package.")
 (license license:ms-pl))

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
 (license license:ms-pl))

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
 (license license:ms-pl))

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
 (license license:ms-pl))

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
 (license license:ms-pl))

(package
 (name "sbcl-select")
 (version "master")
 (source (origin
	  (method git-fetch)
	  (uri (git-reference
		(url "https://github.com/Lisp-Stat/select.git")
		(commit version)))
	  (file-name (git-file-name "select" version))
	  (sha256
	   (base32
	    "1kplgpn79av1p2icyk7h93cy00gshyan96vxlinvwxibhsrhxsj2"))))
 (build-system asdf-build-system/sbcl)
 (home-page "https://github.com/Lisp-Stat/lisp-stat.git")
 (synopsis "Common Lisp statistics package.")
 (description "Common Lisp statistics package.")
 (license license:ms-pl))

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
	    "1zqix2gdv956q2z7ay890yq7jlgqqhdwxl96x6v3cj2m2gnkx96m"))))
 (build-system asdf-build-system/sbcl)
 ;; (propagated-inputs
 ;;  `(("sbcl-data-frame" ,sbcl-data-frame)
 ;;    ("sbcl-dfio" ,sbcl-dfio)
 ;;    ("sbcl-special-functions" ,sbcl-special-functions)
 ;;    ("sbcl-numerical-utilities" ,sbcl-numerical-utilities)
 ;;    ("sbcl-documentation" ,sbcl-documentation)
 ;;    ("sbcl-plot" ,sbcl-plot)
 ;;    ("sbcl-select" ,sbcl-select)))
 (home-page "https://github.com/Lisp-Stat/lisp-stat.git")
 (synopsis "Common Lisp statistics package.")
 (description "Common Lisp statistics package.")
 (license license:ms-pl))
