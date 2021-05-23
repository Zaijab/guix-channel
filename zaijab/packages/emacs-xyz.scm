(define-module (zaijab packages emacs-xyz)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages emacs-xyz))

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
	      "16iv1cvlky2gij1ndx2d6q8l35axm72bx52n6v5y3h21aibj197n"))))
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


