(define-module (zaijab packages emacs-xyz)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages emacs-xyz))

(define-public emacs-org-fc
  (package
    (name "emacs-org-fc")
    (version "main")
    (source (origin
	      (method git-fetch)
	      (uri (git-reference
		    (url "https://git.sr.ht/~l3kn/org-fc")
		    (commit version)))
	      (file-name (git-file-name name version))
	      (sha256
	       (base32
		"00fnkk6hl9l64dgmkhsqibhna7gdpazs4j28f7833n1dmg626ki6"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-org" ,emacs-org)))
    (home-page "https://github.com/conao3/leaf-keywords.el")
    (synopsis "Extra keywords for leaf.")
    (description "Extra keywords for leaf.")
    (license license:gpl3+)))

(define-public emacs-leaf-keywords
  (package
    (name "emacs-leaf-keywords")
    (version "master")
    (source (origin
	      (method git-fetch)
	      (uri (git-reference
		    (url "https://github.com/conao3/leaf-keywords.el.git")
		    (commit version)))
	      (file-name (git-file-name name version))
	      (sha256
	       (base32
		"00fnkk6hl9l64dgmkhsqibhna7gdpazs4j28f7833n1dmg626ki6"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-leaf" ,emacs-leaf)))
    (home-page "https://github.com/conao3/leaf-keywords.el")
    (synopsis "Extra keywords for leaf.")
    (description "Extra keywords for leaf.")
    (license license:gpl3+)))

(define-public emacs-system-packages
  (package
   (name "emacs-system-packages")
   (version "1.0.11")
   (source (origin
	    (method git-fetch)
	    (uri (git-reference
		  (url "https://gitlab.com/jabranham/system-packages.git")
		(commit version)))
	    (file-name (git-file-name name version))
	    (sha256
	     (base32
	      "0pxkyys2lgn16rhf4mzqlh27vs9aw6g083z2vr2agr7bmbavd2fp"))))
   (build-system emacs-build-system)
 (home-page "https://gitlab.com/jabranham/system-packages")
 (synopsis "Emacs interface to system package manager.")
 (description "Use Emacs to install packages using system package manager.")
 (license license:gpl3+)))

(define-public emacs-exlybar
  (package
   (name "emacs-exlybar")
   (version "focal")
   (source (origin
	    (method git-fetch)
	    (uri (git-reference
		  (url "https://github.com/jollm/exlybar.git")
		  (commit version)))
	    (file-name (git-file-name name version))
	    (sha256
	     (base32
	      "0a4c2f8pbzrzda1zdn9g7hmicqbp3ss3wf6b523kspr8wvhzghy1"))))
   (build-system emacs-build-system)
   ;; (arguments
   ;;  (substitute-keyword-arguments (package-arguments emacs-next)
   ;; 				  ((#:configure-flags flags ''())
   ;; 				   `(cons* "--with-pgtk" "--with-xwidgets" ,flags))))
   (home-page "https://gitlab.com/jollm/exlybar.git")
   (synopsis "An Emacs Polybar-like window manager status bar.")
   (description "An Emacs Polybar-like window manager status bar.")
   (license license:gpl3+)))


