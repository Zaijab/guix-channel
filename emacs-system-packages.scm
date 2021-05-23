(use-modules
 (guix packages)
 (guix git-download)
 (guix build-system emacs)
 ((guix licenses) #:prefix license:)
 (gnu packages emacs-xyz))

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
 (license license:gpl3+))
