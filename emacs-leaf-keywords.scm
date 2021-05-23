(define-module (zaijab packages emacs-leaf-keywords)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages emacs)
  ;#:use-module (zaijab packages)
  #:use-module (gnu packages emacs-xyz))


(define-public emacs-leaf
  (package
    (name "emacs-leaf")
    (version "3.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/conao3/leaf.el.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1rgd59146wad92yc64las0qgx67k2ifgsw1vwhp40xvkd7kb0r6d"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/conao3/leaf.el")
    (arguments
     `(#:tests? #t
       #:test-command '("emacs" "--batch"
                        "-l" "leaf-tests.el"
                        "-f" "cort-test-run")))
    (synopsis
     "Simplify your init.el configuration, extended use-package")
    (description
     "This package provides macros that allows you to declaratively configure
settings typical of an Elisp package with various keywords.  The syntax is
similar, but not identical to use-package -- overall, leaf aims at a cleaner
and more predictable implementation.")
    (license license:agpl3+)))

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
