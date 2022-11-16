(define-module (zaijab packages emacs-xyz)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages video)
  #:use-module (gnu packages curl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages emacs-xyz)
  )

(define-public emacs-dynaring
(package
  (name "emacs-dynaring")
  (version "20210924.2026")
  (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/countvajhula/dynaring.git")
                  (commit "dc9013117bdcdc1b12feebcc58eaf129a6ad3a73")))
            (sha256
             (base32
              "0z5r0wybpm74hlcbisavn90i31vh3jsalhk0frihfclfgbqd24d9"))))
  (build-system emacs-build-system)
  (home-page "https://github.com/countvajhula/dynaring")
  (synopsis "A dynamically sized ring structure")
  (description "This package provides a dynamically sized ring structure.")
  (license #f)))

(define-public emacs-buffer-ring
(package
  (name "emacs-buffer-ring")
  (version "20220120.124")
  (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/countvajhula/buffer-ring.git")
                  (commit "177d67238c4d126a0270585e21c0f03ae750ca2a")))
            (sha256
             (base32
              "1li3fq5797hcd2wy5w2vp6hmgf779mrm0pw2nj4a19snwl9ak02j"))))
  (build-system emacs-build-system)
  (propagated-inputs (list emacs-dynaring emacs-s emacs-ht))
  (home-page "https://github.com/countvajhula/buffer-ring")
  (synopsis "Rings and tori for buffer navigation")
  (description "Rings of buffers and tori of buffer rings.")
  (license #f))
  )

(define-public emacs-centaur-tabs
  (package
    (name "emacs-centaur-tabs")
    (version "20220926.1247")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ema2159/centaur-tabs.git")
                    (commit "7d9fad0daa44ffb2acecf6525759e46e08e35f2c")))
              (sha256
               (base32
		"0la8fmwirspg7m453qhfb64sqryl59dxc1lfmjkh6mzf85nqbl1i"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-powerline))
    (home-page "https://github.com/ema2159/centaur-tabs")
    (synopsis "Aesthetic, modern looking customizable tabs plugin")
    (description
     "Emacs plugin aiming to become an aesthetic, modern looking tabs plugin.  This
package offers tabs with a wide range of customization options, both aesthetical
and functional, implementing them trying to follow the Emacs philosophy packing
them with useful keybindings and a nice integration with the Emacs environment,
without sacrificing customizability.  Some of the features Centaur tabs offers
are: - Tab styles - Tab icons - Graying out icons - Selected tab bar (over,
under and left bar) - Close button - Modified marker - Buffer grouping -
Projectile integration - Ivy and Helm integration for group switching")
    (license #f)))

(define-public emacs-elfeed-web
  (package
    (name "emacs-elfeed-web")
    (version "20210226.258")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/skeeto/elfeed.git")
                    (commit "162d7d545ed41c27967d108c04aa31f5a61c8e16")))
              (sha256
               (base32
		"0v49l289wiral01pvgm30wyv79h5d3ly3i05dmcw1q93g4z4l56d"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-simple-httpd emacs-elfeed))
    (arguments
     '(#:include '("^web/[^/]+$")
       #:exclude '()))
    (home-page "https://github.com/skeeto/elfeed")
    (synopsis "web interface to Elfeed")
    (description
     "This is a very early work in progress.  The long-term goal is to provide a web
interface view of the database with optional remote tag updating.  An AngularJS
client accesses the database over a few RESTful endpoints with JSON for
serialization.  The IDs provided by RSS and Atom are completely arbitrary.  To
avoid ugly encoding issues they're normalized into short, unique, alphanumeric
codes called webids.  Both feeds and entries fall into the same webid namespace
so they share a single endpoint.  Endpoints: /elfeed/<path> Serves the static
HTML, JS, and CSS content. /elfeed/content/<ref-id> Serves content from the
content database (`elfeed-deref'). /elfeed/things/<webid> Serve up an
elfeed-feed or elfeed-entry in JSON format. /elfeed/search Accepts a q parameter
which is an filter string to be parsed and handled by
`elfeed-search-parse-filter'. /elfeed/tags Accepts a PUT request to modify the
tags of zero or more entries based on a JSON entry passed as the content.
/elfeed/update Accepts a time parameter.  If time < `elfeed-db-last-update',
respond with time.  Otherwise don't respond until database updates (long poll).")
    (license #f)))

(define-public emacs-symex
  (package
    (name "emacs-symex")
    (version "20221008.1605")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/countvajhula/symex.el.git")
                    (commit "b999c02284f6e72ff2061a98cbaa084954c44879")))
              (sha256
               (base32
		"1m7m0zmzib8kz765ny1miy9ydp512jgxix7bhdbxg1gfidqndp32"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-lispy
                             emacs-paredit
                             emacs-evil-cleverparens
                             emacs-evil
                             emacs-evil-surround
                             emacs-hydra
                             emacs-seq
                             emacs-undo-tree))
    (home-page "https://github.com/countvajhula/symex.el")
    (synopsis "An evil way to edit Lisp symbolic expressions as trees")
    (description
     "Symex mode (pronounced sym-ex, as in symbolic expression) is a vim- inspired way
of editing Lisp code as trees.  Entering symex mode allows you to reason about
your code in terms of its structure, similar to other tools like paredit and
lispy.  But while in those packages the tree representation is implicit, symex
mode models the tree structure explicitly so that tree navigations and
operations can be described using an expressive DSL, and invoked in a vim- style
modal interface implemented with a Hydra.  At the moment, symex mode uses
paredit, lispy, and evil-cleverparens to provide much of its low level
functionality.  In the future, this layer of primitives may be replaced with a
layer that explicitly uses the abstract syntax tree, for greater precision.")
    (license #f)))

(define-public emacs-rigpa
 (package
    (name "emacs-rigpa")
    (version "0.5")
    (source (origin
	      (method git-fetch)
	      (uri (git-reference
		    (url "https://github.com/countvajhula/rigpa.git")
		    (commit version)))
	      (file-name (git-file-name name version))
	      (sha256
	       (base32
		"14nx15chy36l6gdd6m926v98j1g4znx5pw7x7k3bgrnaw1dvyi9a"))))
    (build-system emacs-build-system)
    (propagated-inputs
     (list 
      `("emacs-next" ,(specification->package "emacs-next"))
      `("emacs-evil" ,(specification->package "emacs-evil"))
      `("emacs-hydra" ,(specification->package "emacs-hydra"))
      `("emacs-ht" ,(specification->package "emacs-ht"))
      `("emacs-symex" ,(specification->package "emacs-symex"))
      `("emacs-ace-window" ,(specification->package "emacs-ace-window"))
      `("emacs-dash" ,(specification->package "emacs-dash"))
      `("emacs-git-timemachine" ,(specification->package "emacs-git-timemachine"))
      `("emacs-beacon" ,(specification->package "emacs-beacon"))
      `("emacs-s" ,(specification->package "emacs-s"))
      `("emacs-centaur-tabs" ,(specification->package "emacs-centaur-tabs"))
      `("emacs-counsel" ,(specification->package "emacs-counsel"))
      `("emacs-dynaring" ,(specification->package "emacs-dynaring"))
      `("emacs-buffer-ring" ,(specification->package "emacs-buffer-ring"))
      `("emacs-parsec" ,(specification->package "emacs-parsec"))
      `("emacs-transpose-frame" ,(specification->package "emacs-transpose-frame"))
      ))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:gpl3+)))

(define-public emacs-mpv-el
  (package
    (name "emacs-mpv-el")
    (version "v0.2.0")
    (source (origin
	      (method git-fetch)
	      (uri (git-reference
		    (url "https://github.com/kljohann/mpv.el.git")
		    (commit version)))
	      (file-name (git-file-name name version))
	      (sha256
	       (base32
		"03zziy1lcvpf1wq15bsxwy0dhdb2z7rrdcj6srgrmgykz2wf33q7"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("mpv" ,mpv)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:gpl3+)))

(define-public emacs-elfeed-tube
  (package
    (name "emacs-elfeed-tube")
    (version "master")
    (source (origin
	      (method git-fetch)
	      (uri (git-reference
		    (url "https://github.com/karthink/elfeed-tube.git")
		    (commit version)))
	      (file-name (git-file-name name version))
	      (sha256
	       (base32
		"0h32bpq8w1j3rbwfqlfj10vvlw21j9rcpqjpp45pw8z6xxb2q0lp"))))
    (build-system emacs-build-system)
    (inputs
     `(("mpv" ,mpv)))
    (propagated-inputs
     `(("emacs-elfeed" ,emacs-elfeed)
       ("emacs-aio" ,emacs-aio)
       ("emacs-mpv-el" ,emacs-mpv-el)
       ("mpv" ,mpv)
       ("curl" ,curl)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:gpl3+)))
 
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
		"1d0a3vr09zkplclypcgpfbfd6r0h0i3g3zsqb4pcz6x239d59gd5"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("emacs-org" ,emacs-org)
       ("emacs-hydra" ,emacs-hydra)))
    (home-page "https://www.leonrische.me/fc/index.html")
    (synopsis "Flashcards in Org Mode.")
    (description "Flashcards in Org Mode.")
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

(define-public emacs-lean-mode 
  (package
    (name "emacs-lean-mode")
    (version "20220501.1007")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/leanprover/lean-mode.git")
                    (commit "362bc6fa3efb1874c525ed6b4b6f24f76af22596")))
              (sha256
               (base32
		"1lr4h555fa1kdi3q7hkhsnznv7nh9rgjqjkbj2bqp9zwh06245w3"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-dash emacs-s emacs-f emacs-flycheck))
    (arguments
     '(#:include '("^lean-[^/]+.el$")
       #:exclude '()))
    (home-page "https://github.com/leanprover/lean-mode")
    (synopsis "A major mode for the Lean 3 language")
    (description
     "This package provides a major mode for the Lean 3 programming language.

Provides highlighting, diagnostics, goal visualization, and many other useful
features for Lean users.

See the README.md for more advanced features and the associated keybindings.")
    (license #f)))

(define-public emacs-lsp-pyright
  (package
    (name "emacs-lsp-pyright")
    (version "20220614.1545")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/emacs-lsp/lsp-pyright.git")
                    (commit "2fa2c897659909ba9804baba72a108578d007677")))
              (sha256
               (base32
		"1d951q5dnb4zffgwbhzbg37wi3qcssxsp5q853zzyd7j6jp0iaws"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-lsp-mode emacs-dash emacs-ht))
    (home-page "https://github.com/emacs-lsp/lsp-pyright")
    (synopsis "Python LSP client using Pyright")
    (description "  Pyright language server.")
    (license #f)))
