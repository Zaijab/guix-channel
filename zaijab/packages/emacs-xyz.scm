(define-module (zaijab packages emacs-xyz)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages video)
  #:use-module (gnu packages curl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:hide (zip))
  #:use-module (srfi srfi-26)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module (guix hg-download)
  #:use-module (guix build-system)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system waf)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages dejagnu)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages file)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnunet)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages man)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages music)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ocr)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages perl-web)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages rdesktop)
  #:use-module (gnu packages re2c)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages text-editors)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages time)
  #:use-module (gnu packages upnp)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages web)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix channels)
  #:use-module (guix inferior)
  #:use-module (guix profiles)
  #:use-module (guix channels)
  #:use-module (guix transformations)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (srfi srfi-1))

(define-public emacs-xwwp
  (package
    (name "emacs-xwwp")
    (version "20200917.643")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/canatella/xwwp.git")
                    (commit "f67e070a6e1b233e60274deb717274b000923231")))
              (sha256 (base32
		       "1ikhgi3gc86w7y3cjmw875c8ccsmj22yn1zm3abprdzbjqlyzhhg"))))
    (build-system emacs-build-system)
    (arguments '(#:include '("^xwwp.el$" "^xwwp-follow-link.el$"
                             "^xwwp-follow-link-ido.el$")
                 #:exclude '()))
    (home-page "https://github.com/canatella/xwwp")
    (synopsis "Enhance xwidget webkit browser")
    (description
     "This package provides the common functionnality for other xwidget webkit plus
packages.  It provides the customize group and a framework to inject css and
javascript functions into an `xwidget-webkit session.")
    (license #f)))

(define-public emacs-tabspaces
  (package
    (name "emacs-tabspaces")
    (version "20230212.531")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mclear-tools/tabspaces.git")
                    (commit "6975c51a2154604db70fd38eba27cf784cc3c4a6")))
              (sha256
               (base32
		"0i01q1fv6kdpf3fmhdagh7wlhycy3c6lmqkjkhiah55h7xymr8rk"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-project))
    (home-page "https://github.com/mclear-tools/tabspaces")
    (synopsis "Leverage tab-bar and project for buffer-isolated workspaces")
    (description
     "This package provides several functions to facilitate a single frame-based
workflow with one workspace per tab, integration with project.el (for
project-based workspaces) and buffer isolation per tab (i.e.  a \"tabspace\"
workspace).  The package assumes project.el and tab-bar.el are both present
(they are built-in to Emacs 27.1+).  This file is not part of GNU Emacs. ;
Acknowledgements Much of the package code is inspired by: -
https://github.com/kaz-yos/emacs -
https://github.com/wamei/elscreen-separate-buffer-list/issues/8 -
https://www.rousette.org.uk/archives/using-the-tab-bar-in-emacs/ -
https://github.com/minad/consult#multiple-sources -
https://github.com/florommel/bufferlo")
    (license #f)))

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
    (license #f)))

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
      `("emacs-evil" ,(specification->package "emacs-evil"))
      `("emacs-hydra" ,(specification->package "emacs-hydra"))
      `("emacs-ht" ,(specification->package "emacs-ht"))
      `("emacs-symex" ,(specification->package "emacs-symex"))
      `("emacs-ace-window" ,(specification->package "emacs-ace-window"))
      `("emacs-dash" ,(specification->package "emacs-dash"))
      `("emacs-git-timemachine" ,(specification->package "emacs-git-timemachine"))
      `("emacs-transient" ,(specification->package "emacs-transient"))
      `("emacs-beacon" ,(specification->package "emacs-beacon"))
      `("emacs-s" ,(specification->package "emacs-s"))
      `("emacs-centaur-tabs" ,(specification->package "emacs-centaur-tabs"))
      `("emacs-counsel" ,(specification->package "emacs-counsel"))
      `("emacs-dynaring" ,(specification->package "emacs-dynaring"))
      `("emacs-buffer-ring" ,(specification->package "emacs-buffer-ring"))
      `("emacs-parsec" ,(specification->package "emacs-parsec"))
      `("emacs-transpose-frame" ,(specification->package "emacs-transpose-frame"))))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:gpl3+)))

(define-public emacs-elfeed-tube
  (package
    (name "emacs-elfeed-tube")
    (version "20220823.238")
    (source (origin
	      (method git-fetch)
	      (uri (git-reference
		    (url "https://github.com/karthink/elfeed-tube.git")
		    (commit "0c3fbc21259e1fa794f3179a53b410ba610231f2")))
	      (sha256
	       (base32
		"0hg2s5yzpd1fsl0fyrfv2cc2m61a67drfg86msfqpqdmkv30pbca"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-elfeed emacs-aio emacs-mpv mpv)) 
    (arguments
     '(#:include '("^elfeed-tube.el$" "^elfeed-tube-utils.el$"
		   "^elfeed-tube-fill.el$")
       #:exclude '()))
    (home-page "https://github.com/karthink/elfeed-tube")
    (synopsis "YouTube integration for Elfeed")
    (description
     "Elfeed Tube is an extension for Elfeed, the feed reader for Emacs, that en
hances
your Youtube RSS feed subscriptions.  Typically Youtube RSS feeds contain only
the title and author of each video.  Elfeed Tube adds video descriptions,
thumbnails, durations, chapters and \"live\" transcrips to video entries.  See
https://github.com/karthink/elfeed-tube for demos.  This information can
optionally be added to your entry in your Elfeed database.  The displayed
transcripts and chapter headings are time-aware, so you can click on any
transcript segment to visit the video at that time (in a browser or your video
player if you also have youtube-dl).  A companion package, `elfeed-tube-mpv',
provides complete mpv (video player) integration with the transcript, including
video seeking through the transcript and following along with the video in
Emacs.  To use this package, (i) Subscribe to Youtube channel or playlist feeds
in Elfeed.  You can use the helper function `elfeed-tube-add-feeds provided by
this package to search for Youtube channels by URLs or search queries. (ii)
Place in your init file the following: (require elfeed-tube) (elfeed-tube-setup)
(iii) Use Elfeed as normal, typically with `elfeed'.  Your Youtube feed entries
should be fully populated.  You can also call `elfeed-tube-fetch in an Elfeed
buffer to manually populate an entry, or obtain an Elfeed entry-like summary for
ANY youtube video (no subscription needed) by manually calling
`elfeed-tube-fetch from outside Elfeed.  User options: There are three options
of note: `elfeed-tube-fields': Customize this to set the kinds of metadata you
want added to Elfeed's Youtube entries.  You can selectively turn on/off
thumbnails, transcripts etc. `elfeed-tube-auto-save-p': Set this boolean to save
fetched Youtube metadata to your Elfeed database, i.e.  to persist the data on
disk for all entries. `elfeed-tube-auto-fetch-p': Unset this boolean to turn off
fetching metadata.  You can then call `elfeed-tube-fetch to manually fetch data
for specific feed entries.  See the customization group `elfeed-tube for more
options.  See the README for more information.")
    (license #f)))

(define-public emacs-nano-emacs
  (package
    (name "emacs-nano-emacs")
    (version "b8631088220dbbcd885ad1353bdc52b569655f85")
    (source (origin
	      (method git-fetch)
	      (uri (git-reference
		    (url "https://github.com/rougier/nano-emacs.git")
		    (commit "b8631088220dbbcd885ad1353bdc52b569655f85")))
	      (sha256
	       (base32
		"10sphksfy574kgplxq1lpz8dm5r8wnf8kg4kdcd8x9ldr7iy6qrf"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-aio emacs-mpv)) 
    (home-page "")
    (synopsis "")
    (description "")
    (license #f)))

(define-public emacs-py-autopep8
  (package
    (name "emacs-py-autopep8")
    (version "20230115.633")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://melpa.org/packages/py-autopep8-"
                                  version ".el"))
              (sha256
               (base32
		"14vhk4km6ga24nfniwrzpax1xclq7k198q7xng37iir2hmbp6i5v"))))
    (build-system emacs-build-system)
    (home-page "https://codeberg.org/ideasman42/emacs-py-autopep8")
    (synopsis "Use autopep8 to beautify a Python buffer")
    (propagated-inputs (list python-autopep8))
    (description
     "This package provides the `py-autopep8-buffer command, which uses the external
\"autopep8\" tool to tidy up the current buffer according to Python's PEP8. ;
Usage To automatically apply when saving a python file, use the following code:
(add-hook python-mode-hook py-autopep8-mode) To customize the behavior of
\"autopep8\" you can set the `py-autopep8-options e.g. (setq py-autopep8-options
(\"--max-line-length=100\" \"--aggressive\"))")
    (license #f)))

(define-public emacs-calfw-blocks
  (package
    (name "emacs-calfw-blocks")
    (version "0fe829035ffa491c3f2610f05a7f1ec936a4497e")
    (source (origin
	      (method git-fetch)
	      (uri (git-reference
		    (url "https://github.com/ml729/calfw-blocks.git")
		    (commit version)))
	      (file-name (git-file-name name version))
	      (sha256
	       (base32
		"1v8m1i7jci56c8k2zcd0gghn7qk9k2nn0rr311y0pzwsgwbqcif3"))))
    (build-system emacs-build-system)
    (propagated-inputs (list (specification->package "emacs-calfw")))
    (home-page "https://github.com/ml729/calfw-blocks")
    (synopsis "Visual time blocks for the Emacs Calendar Framework (calfw).")
    (description "Visual time blocks for the Emacs Calendar Framework (calfw).")
    (license license:gpl3+)))

(define-public emacs-phscroll
  (package
    (name "emacs-phscroll")
    (version "582abedb4cf6aba216cdb5fe7217d612a1d68d5a")
    (source (origin
	      (method git-fetch)
	      (uri (git-reference
		    (url "https://github.com/misohena/phscroll.git")
		    (commit version)))
	      (file-name (git-file-name name version))
	      (sha256
	       (base32
		"0cp0ffgdg6livjyb36m54mai0apwr2qf80v32x92v76q00542j9f"))))
    (build-system emacs-build-system)
    (home-page "")
    (synopsis "")
    (description "")
    (license license:gpl3+)))

(define-public emacs-org-preview
  (package
    (name "emacs-org-preview")
    (version "4a4e455a62f96fa5d1ba90ceaa025579a9dd3be6")
    (source (origin
	      (method git-fetch)
	      (uri (git-reference
		    (url "https://github.com/jsilve24/org-preview.git")
		    (commit version)))
	      (file-name (git-file-name name version))
	      (sha256
	       (base32
		"1bbpz4bzypk8wv68m5krnnb92c2xwvz87cv4jhxd8njpxngcjawh"))))
    (build-system emacs-build-system)
    (home-page "")
    (synopsis "")
    (description "")
    (license license:gpl3+)))

(define-public emacs-embark-consult
  (package
    (name "emacs-embark-consult")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://elpa.gnu.org/packages/embark-consult-"
                           version ".tar"))
       (sha256
        (base32 "06yh6w4zgvvkfllmcr0szsgjrfhh9rpjwgmcrf6h2gai2ps9xdqr"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-compat emacs-embark emacs-consult))
    (home-page "https://github.com/oantolin/embark")
    (synopsis "Consult integration for Embark")
    (description
     "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ EMBARK: EMACS MINI-BUFFER
ACTIONS ROOTED IN KEYMAPS ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ 1
Overview ══════════ Embark makes it easy to choose a command to run based on
what is near point, both during a minibuffer completion session (in a way
familiar to Helm or Counsel users) and in normal buffers.  Bind the command
`embark-act to a key and it acts like prefix-key for a keymap of /actions/
(commands) relevant to the /target/ around point.  With point on an URL in a
buffer you can open the URL in a browser or eww or download the file it points
to.  If while switching buffers you spot an old one, you can kill it right there
and continue to select another.  Embark comes preconfigured with over a hundred
actions for common types of targets such as files, buffers, identifiers,
s-expressions, sentences; and it is easy to add more actions and more target
types.  Embark can also collect all the candidates in a minibuffer to an
occur-like buffer or export them to a buffer in a major-mode specific to the
type of candidates, such as dired for a set of files, ibuffer for a set of
buffers, or customize for a set of variables.  1.1 Acting on targets
───────────────────── You can think of `embark-act as a keyboard-based version
of a right-click contextual menu.  The `embark-act command (which you should
bind to a convenient key), acts as a prefix for a keymap offering you relevant
/actions/ to use on a /target/ determined by the context: • In the minibuffer,
the target is the current top completion candidate. • In the `*Completions*
buffer the target is the completion at point. • In a regular buffer, the target
is the region if active, or else the file, symbol, URL, s-expression or defun at
point.  Multiple targets can be present at the same location and you can cycle
between them by repeating the `embark-act key binding.  The type of actions
offered depend on the type of the target.  Here is a sample of a few of the
actions offered in the default configuration: • For files you get offered
actions like deleting, copying, renaming, visiting in another window, running a
shell command on the file, etc. • For buffers the actions include switching to
or killing the buffer. • For package names the actions include installing,
removing or visiting the homepage. • For Emacs Lisp symbols the actions include
finding the definition, looking up documentation, evaluating (which for a
variable immediately shows the value, but for a function lets you pass it some
arguments first).  There are some actions specific to variables, such as setting
the value directly or though the customize system, and some actions specific to
commands, such as binding it to a key.  By default when you use `embark-act if
you don't immediately select an action, after a short delay Embark will pop up a
buffer showing a list of actions and their corresponding key bindings.  If you
are using `embark-act outside the minibuffer, Embark will also highlight the
current target.  These behaviors are configurable via the variable
`embark-indicators'.  Instead of selecting an action via its key binding, you
can select it by name with completion by typing `C-h after `embark-act'.
Everything is easily configurable: determining the current target, classifying
it, and deciding which actions are offered for each type in the classification.
The above introduction just mentions part of the default configuration.
Configuring which actions are offered for a type is particularly easy and
requires no programming: the variable `embark-keymap-alist associates target
types with variables containing keymaps, and those keymaps containing bindings
for the actions. (To examine the available categories and their associated
keymaps, you can use `C-h v embark-keymap-alist or customize that variable.) For
example, in the default configuration the type `file is associated with the
symbol `embark-file-map'.  That symbol names a keymap with single-letter key
bindings for common Emacs file commands, for instance `c is bound to
`copy-file'.  This means that if you are in the minibuffer after running a
command that prompts for a file, such as `find-file or `rename-file', you can
copy a file by running `embark-act and then pressing `c'.  These action keymaps
are very convenient but not strictly necessary when using `embark-act': you can
use any command that reads from the minibuffer as an action and the target of
the action will be inserted at the first minibuffer prompt.  After running
`embark-act all of your key bindings and even `execute-extended-command can be
used to run a command.  For example, if you want to replace all occurrences of
the symbol at point, just use `M-% as the action, there is no need to bind
`query-replace in one of Embark's keymaps.  Also, those action keymaps are
normal Emacs keymaps and you should feel free to bind in them whatever commands
you find useful as actions and want to be available through convenient bindings.
 The actions in `embark-general-map are available no matter what type of
completion you are in the middle of.  By default this includes bindings to save
the current candidate in the kill ring and to insert the current candidate in
the previously selected buffer (the buffer that was current when you executed a
command that opened up the minibuffer).  Emacs's minibuffer completion system
includes metadata indicating the /category/ of what is being completed.  For
example, `find-file''s metadata indicates a category of `file and
`switch-to-buffer''s metadata indicates a category of `buffer'.  Embark has the
related notion of the /type/ of a target for actions, and by default when
category metadata is present it is taken to be the type of minibuffer completion
candidates when used as targets.  Emacs commands often do not set useful
category metadata so the [Marginalia] package, which supplies this missing
metadata, is highly recommended for use with Embark.  Embark's default
configuration has actions for the following target types: files, buffers,
symbols, packages, URLs, bookmarks, and as a somewhat special case, actions for
when the region is active.  You can read about the [default actions and their
key bindings] on the @code{GitHub} project wiki. [Marginalia]
<https://github.com/minad/marginalia> [default actions and their key bindings]
<https://github.com/oantolin/embark/wiki/Default-Actions> 1.2 The default action
on a target ────────────────────────────────── Embark has a notion of default
action for a target: • If the target is a minibuffer completion candidate, then
the default action is whatever command opened the minibuffer in the first place.
 For example if you run `kill-buffer', then the default action will be to kill
buffers. • If the target comes from a regular buffer (i.e., not a minibuffer),
then the default action is whatever is bound to `RET in the keymap of actions
for that type of target.  For example, in Embark's default configuration for a
URL found at point the default action is `browse-url', because `RET is bound to
`browse-url in the `embark-url-map keymap.  To run the default action you can
press `RET after running `embark-act'.  Note that if there are several different
targets at a given location, each has its own default action, so first cycle to
the target you want and then press `RET to run the corresponding default action.
 There is also `embark-dwim which runs the default action for the first target
found.  It's pretty handy in non-minibuffer buffers: with Embark's default
configuration it will: • Open the file at point. • Open the URL at point in a
web browser (using the `browse-url command). • Compose a new email to the email
address at point. • In an Emacs Lisp buffer, if point is on an opening
parenthesis or right after a closing one, it will evaluate the corresponding
expression. • Go to the definition of an Emacs Lisp function, variable or macro
at point. • Find the file corresponding to an Emacs Lisp library at point.  1.3
Working with sets of possible targets ─────────────────────────────────────────
Besides acting individually on targets, Embark lets you work collectively on a
set of target /candidates/.  For example, while you are in the minibuffer the
candidates are simply the possible completions of your input.  Embark provides
three main commands to work on candidate sets: • The `embark-act-all command
runs the same action on each of the current candidates.  It is just like using
`embark-act on each candidate in turn. (Because you can easily act on many more
candidates than you meant to, by default Embark asks you to confirm uses of
`embark-act-all'; you can turn this off by setting the user option
`embark-confirm-act-all to `nil'.) • The `embark-collect command produces a
buffer listing all the current candidates, for you to peruse and run actions on
at your leisure.  The candidates are displayed as a list showing additional
annotations.  If any of the candidates contain newlines, then horizontal lines
are used to separate candidates.  The Embark Collect buffer is somewhat
\"dired-like\": you can select and deselect candidates through `embark-select
(available as an action in `embark-act', bound to `SPC'; but you could also give
it a global key binding).  In an Embark Collect buffer `embark-act is bound to
`a and `embark-act-all is bound to `A'; `embark-act-all will act on all
currently marked candidates if there any, and will act on all candidates if none
are marked.  In particular, this means that `a SPC will toggle whether the
candidate at point is selected, and `A SPC will select all candidates if none
are selected, or deselect all selected candidates if there are some. • The
`embark-export command tries to open a buffer in an appropriate major mode for
the set of candidates.  If the candidates are files export produces a Dired
buffer; if they are buffers, you get an Ibuffer buffer; and if they are packages
you get a buffer in package menu mode.  If you use the grepping commands from
the [Consult] package, `consult-grep', `consult-git-grep or `consult-ripgrep',
then you should install the `embark-consult package, which adds support for
exporting a list of grep results to an honest grep-mode buffer, on which you can
even use [wgrep] if you wish.  When in doubt choosing between exporting and
collecting, a good rule of thumb is to always prefer `embark-export since when
an exporter to a special major mode is available for a given type of target, it
will be more featureful than an Embark collect buffer, and if no such exporter
is configured the `embark-export command falls back to the generic
`embark-collect'.  These commands are always available as \"actions\" (although
they do not act on just the current target but on all candidates) for
`embark-act and are bound to `A', `S (for \"snapshot\"), and `E', respectively, in
`embark-general-map'.  This means that you do not have to bind your own key
bindings for these (although you can, of course!), just a key binding for
`embark-act'.  In Embark Collect or Embark Export buffers that were obtained by
running `embark-collect or `embark-export from within a minibuffer completion
session, `g is bound to a command that restarts the completion session, that is,
the command that opened the minibuffer is run again and the minibuffer contents
restored.  You can then interact normally with the command, perhaps editing the
minibuffer contents, and, if you wish, you can rerun `embark-collect or
`embark-export to get an updated buffer. [Consult]
<https://github.com/minad/consult/> [wgrep]
<https://github.com/mhayashi1120/Emacs-wgrep> 1.3.1 Selecting some targets to
make an ad hoc candidate set
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌ The commands for
working with sets of candidates just described, namely `embark-act-all',
`embark-export and `embark-collect by default work with all candidates defined
in the current context.  For example, in the minibuffer they operate on all
currently completion candidates, or in a dired buffer they work on all marked
files (or all files if none are marked).  Embark also has a notion of
/selection/, where you can accumulate an ad hoc list of targets for these
commands to work on.  The selection is controlled by using the `embark-select
action, bound to `SPC in `embark-general-map so that it is always available (you
can also give `embark-select a global key binding if you wish; when called
directly, not as an action for `embark-act', it will select the first target at
point).  Calling this action on a target toggles its membership in the current
buffer's Embark selection; that is, it adds it to selection if not selected and
removes it from the selection if it was selected.  Whenever the selection for a
buffer is non-empty, the commands `embark-act-all', `embark-export and
`embark-collect will act on the selection.  To deselect all selected targets,
you can use the `embark-select action through `embark-act-all', since this will
run `embark-select on each member of the current selection.  Similarly if no
targets are selected and you are in a minibuffer completion session, running
`embark-select from `embark-act-all will select all the current completion
candidates.  By default, whenever some targets are selected in the current
buffer, a count of selected targets appears in the mode line.  This can be
turned off or customized through the `embark-selection-indicator user option.
The selection functionality is supported in every buffer: • In the minibuffer
this gives a convenient way to act on several completion candidates that don't
follow any simple pattern: just go through the completions selecting the ones
you want, then use `embark-act-all'.  For example, you could attach several
files at once to an email. • For Embark Collect buffers this functionality
enables a dired-like workflow, in which you mark various candidates and apply an
action to all at once. (It supersedes a previous ad hoc dired-like interface
that was implemented only in Embark Collect buffers, with a slightly different
interface.) • In a eww buffer you could use this to select various links you
wish to follow up on, and then collect them into a buffer.  Similarly, while
reading Emacs's info manual you could select some symbols you want to read more
about and export them to an `apropos-mode buffer. • You can use selections in
regular text or programming buffers to do complex editing operations.  For
example, if you have three paragraphs scattered over a file and you want to
bring them together, you can select each one, insert them all somewhere and
finally delete all of them (from their original locations).  1.3.2 `embark-live
a live-updating variant of `embark-collect
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌ Finally, there
is also an `embark-live variant of the `embark-collect command which
automatically updates the collection after each change in the source buffer.
Users of a completion UI that automatically updates and displays the candidate
list (such as Vertico, Icomplete, Fido-mode, or MCT) will probably not want to
use `embark-live from the minibuffer as they will then have two live updating
displays of the completion candidates! A more likely use of `embark-live is to
be called from a regular buffer to display a sort of live updating \"table of
contents\" for the buffer.  This depends on having appropriate candidate
collectors configured in `embark-candidate-collectors'.  There are not many in
Embark's default configuration, but you can try this experiment: open a dired
buffer in a directory that has very many files, mark a few, and run
`embark-live'.  You'll get an Embark Collect buffer containing only the marked
files, which updates as you mark or unmark files in dired.  To make `embark-live
genuinely useful other candidate collectors are required.  The `embark-consult
package (documented near the end of this manual) contains a few: one for imenu
items and one for outline headings as used by `outline-minor-mode'.  Those
collectors really do give `embark-live a table-of-contents feel.  1.4 Switching
to a different command without losing what you've typed
───────────────────────────────────────────────────────────────────── Embark
also has the `embark-become command which is useful for when you run a command,
start typing at the minibuffer and realize you meant a different command.  The
most common case for me is that I run `switch-to-buffer', start typing a buffer
name and realize I haven't opened the file I had in mind yet! I'll use this
situation as a running example to illustrate `embark-become'.  When this happens
I can, of course, press `C-g and then run `find-file and open the file, but this
requires retyping the portion of the file name you already typed.  This process
can be streamlined with `embark-become': while still in the `switch-to-buffer
you can run `embark-become and effectively make the `switch-to-buffer command
become `find-file for this run.  You can bind `embark-become to a key in
`minibuffer-local-map', but it is also available as an action under the letter
`B (uppercase), so you don't need a binding if you already have one for
`embark-act'.  So, assuming I have `embark-act bound to, say, `C-.', once I
realize I haven't open the file I can type `C-.  B C-x C-f to have
`switch-to-buffer become `find-file without losing what I have already typed in
the minibuffer.  But for even more convenience, `embark-become offers shorter
key bindings for commands you are likely to want the current command to become.
When you use `embark-become it looks for the current command in all keymaps
named in the list `embark-become-keymaps and then activates all keymaps that
contain it.  For example, the default value of `embark-become-keymaps contains a
keymap `embark-become-file+buffer-map with bindings for several commands related
to files and buffers, in particular, it binds `switch-to-buffer to `b and
`find-file to `f'.  So when I accidentally try to switch to a buffer for a file
I haven't opened yet, `embark-become finds that the command I ran,
`switch-to-buffer', is in the keymap `embark-become-file+buffer-map', so it
activates that keymap (and any others that also contain a binding for
`switch-to-buffer').  The end result is that I can type `C-.  B f to switch to
`find-file'.  2 Quick start ═════════════ The easiest way to install Embark is
from GNU ELPA, just run `M-x package-install RET embark RET'. (It is also
available on MELPA.) It is highly recommended to also install [Marginalia] (also
available on GNU ELPA), so that Embark can offer you preconfigured actions in
more contexts.  For `use-package users, the following is a very reasonable
starting configuration: ┌──── │ (use-package marginalia │ :ensure t │ :config │
(marginalia-mode)) │ │ (use-package embark │ :ensure t │ │ :bind │ ((\"C-.\" .
embark-act) ;; pick some comfortable binding │ (\"C-;\" .  embark-dwim) ;; good
alternative: M-. │ (\"C-h B\" .  embark-bindings)) ;; alternative for
`describe-bindings │ │ :init │ │ ;; Optionally replace the key help with a
completing-read interface │ (setq prefix-help-command
#'embark-prefix-help-command) │ │ ;; Show the Embark target at point via Eldoc.
You may adjust the │ ;; Eldoc strategy, if you want to see the documentation
from │ ;; multiple providers.  Beware that using this can be a little │ ;;
jarring since the message shown in the minibuffer can be more │ ;; than one
line, causing the modeline to move up and down: │ │ ;; (add-hook
eldoc-documentation-functions #'embark-eldoc-first-target) │ ;; (setq
eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly) │ │ :config
│ │ ;; Hide the mode line of the Embark live/completions buffers │ (add-to-list
display-buffer-alist │ 	 (\"\\\\`\\\\*Embark Collect \\\\(Live\\\\|Completions\\\\)\\\\*\" │
		 nil │ 		 (window-parameters (mode-line-format .  none))))) │ │ ;; Consult
users will also want the embark-consult package. │ (use-package embark-consult │
:ensure t ; only need to install it, embark loads it after consult if found │
:hook │ (embark-collect-mode .  consult-preview-at-point-mode)) └──── About the
suggested key bindings for `embark-act and `embark-dwim': • Those key bindings
are unlikely to work in the terminal, but terminal users are probably well aware
of this and will know to select different bindings. • The suggested `C-.
binding is used by default in (at least some installations of) GNOME to input
emojis, and Emacs doesn't even get a chance to respond to the binding.  You can
select a different key binding for `embark-act or use `ibus-setup to change the
shortcut for emoji insertion (Emacs 29 will likely use `C-x 8 e e', in case you
want to set the same one system-wide). • The suggested alternative of `M-.  for
`embark-dwim is bound by default to `xref-find-definitions'.  That is a very
useful command but overwriting it with `embark-dwim is sensible since in
Embark's default configuration, `embark-dwim will also find the definition of
the identifier at point. (Note that `xref-find-definitions with a prefix
argument prompts you for an identifier, `embark-dwim does not cover this case).
Other Embark commands such as `embark-act-all', `embark-become',
`embark-collect', and `embark-export can be run through `embark-act as actions
bound to `A', `B', `S (for \"snapshot\"), and `E respectively, and thus don't
really need a dedicated key binding, but feel free to bind them directly if you
so wish.  If you do choose to bind them directly, you'll probably want to bind
them in `minibuffer-local-map', since they are most useful in the minibuffer (in
fact, `embark-become only works in the minibuffer).  The command `embark-dwim
executes the default action at point.  Another good keybinding for `embark-dwim
is `M-.  since `embark-dwim acts like `xref-find-definitions on the symbol at
point. `C-.  can be seen as a right-click context menu at point and `M-.  acts
like left-click.  The keybindings are mnemonic, both act at the point (`.').
Embark needs to know what your minibuffer completion system considers to be the
list of candidates and which one is the current candidate.  Embark works out of
the box if you use Emacs's default tab completion, the built-in `icomplete-mode
or `fido-mode', or the third-party packages [Vertico] or [Ivy].  If you are a
[Helm] or [Ivy] user you are unlikely to want Embark since those packages
include comprehensive functionality for acting on minibuffer completion
candidates. (Embark does come with Ivy integration despite this.) [Marginalia]
<https://github.com/minad/marginalia> [Vertico]
<https://github.com/minad/vertico> [Ivy] <https://github.com/abo-abo/swiper>
[Helm] <https://emacs-helm.github.io/helm/> 3 Advanced configuration
════════════════════════ 3.1 Showing information about available targets and
actions ─────────────────────────────────────────────────────────── By default,
if you run `embark-act and do not immediately select an action, after a short
delay Embark will pop up a buffer called `*Embark Actions* containing a list of
available actions with their key bindings.  You can scroll that buffer with the
mouse of with the usual commands `scroll-other-window and
`scroll-other-window-down (bound by default to `C-M-v and `C-M-S-v').  That
functionality is provided by the `embark-mixed-indicator', but Embark has other
indicators that can provide information about the target and its type, what
other targets you can cycle to, and which actions have key bindings in the
action map for the current type of target.  Any number of indicators can be
active at once and the user option `embark-indicators should be set to a list of
the desired indicators.  Embark comes with the following indicators: •
`embark-minimal-indicator': shows a messages in the echo area or minibuffer
prompt showing the current target and the types of all targets starting with the
current one. • `embark-highlight-indicator': highlights the target at point; on
by default. • `embark-verbose-indicator': displays a table of actions and their
key bindings in a buffer; this is not on by default, in favor of the mixed
indicator described next. • `embark-mixed-indicator': starts out by behaving as
the minimal indicator but after a short delay acts as the verbose indicator;
this is on by default. • `embark-isearch-highlight-indicator': this only does
something when the current target is the symbol at point, in which case it
lazily highlights all occurrences of that symbol in the current buffer, like
isearch; also on by default.  Users of the popular [which-key] package may
prefer to use the `embark-which-key-indicator from the [Embark wiki].  Just copy
its definition from the wiki into your configuration and customize the
`embark-indicators user option to exclude the mixed and verbose indicators and
to include `embark-which-key-indicator'.  If you use [Vertico], there is an even
easier way to get a `which-key'-like display that also lets you use completion
to narrow down the list of alternatives, described at the end of the next
section. [which-key] <https://github.com/justbur/emacs-which-key> [Embark wiki]
<https://github.com/oantolin/embark/wiki/Additional-Configuration#use-which-key-like-a-key-menu-prompt>
[Vertico] <https://github.com/minad/vertico> 3.2 Selecting commands via
completions instead of key bindings
────────────────────────────────────────────────────────────── As an alternative
to reading the list of actions in the verbose or mixed indicators (see the
previous section for a description of these), you can press the
`embark-help-key', which is `C-h by default (but you may prefer `? to free up
`C-h for use as a prefix) after running `embark-act'.  Pressing the help key
will prompt you for the name of an action with completion (but feel free to
enter a command that is not among the offered candidates!), and will also remind
you of the key bindings.  You can press `embark-keymap-prompter-key', which is
`@@ by default, at the prompt and then one of the key bindings to enter the name
of the corresponding action.  You may think that with the `*Embark Actions*
buffer popping up to remind you of the key bindings you'd never want to use
completion to select an action by name, but personally I find that typing a
small portion of the action name to narrow down the list of candidates feels
significantly faster than visually scanning the entire list of actions.  If you
find you prefer selecting actions that way, you can configure embark to always
prompt you for actions by setting the variable `embark-prompter to
`embark-completing-read-prompter'.  On the other hand, you may wish to continue
using key bindings for the actions you perform most often, and to use completion
only to explore what further actions are available or when you've forgotten a
key binding.  In that case, you may prefer to use the minimal indicator, which
does not pop-up an `*Embark Actions* buffer at all, and to use the
`embark-help-key whenever you need help.  This unobtrusive setup is achieved
with the following configuration: ┌──── │ (setq embark-indicators │
(embark-minimal-indicator ; default is embark-mixed-indicator │
	embark-highlight-indicator │ 	embark-isearch-highlight-indicator)) └────
[Vertico] users may wish to configure a grid display for the actions and
key-bindings, reminiscent of the popular package [which-key], but, of course,
enhanced by the use of completion to narrow the list of commands.  In order to
get the grid display, put the following in your Vertico configuration: ┌──── │
(add-to-list vertico-multiform-categories (embark-keybinding grid)) │
(vertico-multiform-mode) └──── This will make the available keys be shown in a
compact grid like in `which-key'.  The `vertico-multiform-mode also enables keys
such as `M-V', `M-G', `M-B', and `M-U for manually switching between layouts in
Vertico buffers. [Vertico] <https://github.com/minad/vertico> [which-key]
<https://github.com/justbur/emacs-which-key> 3.2.1 Selecting commands via
completion outside of Embark
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌ If you like this
completion interface for exploring key bindings for Embark actions, you may want
to use it elsewhere in Emacs.  You can use Embark's completion-based command
prompter to list: • key bindings under a prefix, • local key bindings, or • all
key bindings.  To use it for key bindings under a prefix (you can use this to
replace the `which-key package, for example), use this configuration: ┌──── │
(setq prefix-help-command #'embark-prefix-help-command) └──── Now, when you have
started on a prefix sequence such as `C-x or `C-c', pressing `C-h will bring up
the Embark version of the built-in `prefix-help-command', which will list the
keys under that prefix and their bindings, and lets you select the one you
wanted with completion, or by key binding if you press
`embark-keymap-prompter-key'.  To list local or global key bindings, use the
command `embark-bindings'.  You can bind that to `C-h b', which is the default
key binding for the built-in `describe-bindings command, which this command can
replace.  By default, `embark-bindings lists local key bindings, typically those
bound in the major mode keymap; to get global bindings as well, call it with a
`C-u prefix argument.  3.3 Quitting the minibuffer after an action
─────────────────────────────────────────── By default, if you call `embark-act
from the minibuffer it quits the minibuffer after performing the action.  You
can change this by setting the user option `embark-quit-after-action to `nil'.
Having `embark-act /not/ quit the minibuffer can be useful to turn commands into
little \"thing managers\".  For example, you can use `find-file as a little file
manager or `describe-package as a little package manager: you can run those
commands, perform a series of actions, and then quit the command.  If you want
to control the quitting behavior in a fine-grained manner depending on the
action, you can set `embark-quit-after-action to an alist, associating commands
to either `t for quitting or `nil for not quitting.  When using an alist, you
can use the special key `t to specify the default behavior.  For example, to
specify that by default actions should not quit the minibuffer but that using
`kill-buffer as an action should quit, you can use the following configuration:
┌──── │ (setq embark-quit-after-action ((kill-buffer .  t) (t .  nil))) └────
The variable `embark-quit-after-action only specifies a default, that is, it
only controls whether or not `embark-act quits the minibuffer when you call it
without a prefix argument, and you can select the opposite behavior to what the
variable says by calling `embark-act with `C-u'.  Also note that both the
variable `embark-quit-after-action and `C-u have no effect when you call
`embark-act outside the minibuffer.  If you find yourself using the quitting and
non-quitting variants of `embark-act about equally often, independently of the
action, you may prefer to simply have separate commands for them instead of a
single command that you call with `C-u half the time.  You could, for example,
keep the default exiting behavior of `embark-act and define a non-quitting
version as follows: ┌──── │ (defun embark-act-noquit () │ \"Run action but don't
quit the minibuffer afterwards.\" │ (interactive) │ (let
((embark-quit-after-action nil)) │ (embark-act))) └──── 3.4 Running some setup
after injecting the target ───────────────────────────────────────────────── You
can customize what happens after the target is inserted at the minibuffer prompt
of an action.  There are `embark-target-injection-hooks', that are run by
default after injecting the target into the minibuffer.  The variable
`embark-target-injection-hooks is an alist associating commands to their setup
hooks.  There are two special keys: if no setup hook is specified for a given
action, the hook associated to `t is run; and the hook associated to `:always is
run regardless of the action. (This variable used to have the less explicit name
of `embark-setup-action-hooks', so please update your configuration.) For
example, consider using `shell-command as an action during file completion.  It
would be useful to insert a space before the target file name and to leave the
point at the beginning, so you can immediately type the shell command to run on
that file.  That's why in Embark's default configuration there is an entry in
`embark-target-injection-hooks associating `shell-command to a hook that
includes `embark--shell-prep', a simple helper function that quotes all the
spaces in the file name, inserts an extra space at the beginning of the line and
leaves point to the left of it.  Now, the preparation that `embark--shell-prep
does would be useless if Embark did what it normally does after it inserts the
target of the action at the minibuffer prompt, which is to \"press `RET'\" for
you, accepting the target as is; if Embark did that for `shell-command you
wouldn't get a chance to type in the command to execute! That is why in Embark's
default configuration the entry for `shell-command in
`embark-target-injection-hooks also contains the function `embark--allow-edit'.
Embark used to have a dedicated variable `embark-allow-edit-actions to which you
could add commands for which Embark should forgo pressing `RET for you after
inserting the target.  Since its effect can also be achieved via the general
`embark-target-injection-hooks mechanism, that variable has been removed to
simplify Embark.  Be sure to update your configuration; if you had something
like: ┌──── │ (add-to-list embark-allow-edit-actions my-command) └──── you
should replace it with: ┌──── │ (push embark--allow-edit │ (alist-get my-command
embark-target-injection-hooks)) └──── Also note that while you could abuse
`embark--allow-edit so that you have to confirm \"dangerous\" actions such as
`delete-file', it is better to implement confirmation by adding the
`embark--confirm function to the appropriate entry of a different hook alist,
namely, `embark-pre-action-hooks'.  Besides `embark--allow-edit', Embark comes
with another function that is of general utility in action setup hooks:
`embark--ignore-target'.  Use it for commands that do prompt you in the
minibuffer but for which inserting the target would be inappropriate.  This is
not a common situation but does occasionally arise.  For example it is used by
default for `shell-command-on-region': that command is used as an action for
region targets, and it prompts you for a shell command; you typically do /not/
want the target, that is the contents of the region, to be entered at that
prompt! 3.5 Running hooks before, after or around an action
─────────────────────────────────────────────────── Embark has three variables,
`embark-pre-action-hooks', `embark-post-action-hooks and
`embark-around-action-hooks', which are alists associating commands to hooks
that should run before or after or as around advice for the command when used as
an action.  As with `embark-target-injection-hooks', there are two special keys
for the alists: `t designates the default hook to run when no specific hook is
specified for a command; and the hook associated to `:always runs regardless.
The default values of those variables are fairly extensive, adding creature
comforts to make running actions a smooth experience.  Embark comes with several
functions intended to be added to these hooks, and used in the default values of
`embark-pre-action-hooks', `embark-post-action-hooks and
`embark-around-action-hooks'.  For pre-action hooks: `embark--confirm Prompt the
user for confirmation before executing the action.  This is used be default for
commands deemed \"dangerous\", or, more accurately, hard to undo, such as
`delete-file and `kill-buffer'. `embark--unmark-target Unmark the active region.
 Use this for commands you want to act on the region contents but without the
region being active.  The default configuration uses this function as a
pre-action hook for `occur and `query-replace', for example, so that you can use
them as actions with region targets to search the whole buffer for the text
contained in the region.  Without this pre-action hook using `occur as an action
for a region target would be pointless: it would search for the the region
contents /in the region/, (typically, due to the details of regexps) finding
only one match! `embark--beginning-of-target Move to the beginning of the target
(for targets that report bounds).  This is used by default for backward motion
commands such as `backward-sexp', so that they don't accidentally leave you on
the current target. `embark--end-of-target Move to the end of the target.  This
is used similarly to the previous function, but also for commands that act on
the last s-expression like `eval-last-sexp'.  This allow you to act on an
s-expression from anywhere inside it and still use `eval-last-sexp as an action.
`embark--xref-push-markers Push the current location on the xref marker stack.
Use this for commands that take you somewhere and for which you'd like to be
able to come back to where you were using `xref-pop-marker-stack'.  This is used
by default for `find-library'.  For post-action hooks: `embark--restart Restart
the command currently prompting in the minibuffer, so that the list of
completion candidates is updated.  This is useful as a post action hook for
commands that delete or rename a completion candidate; for example the default
value of `embark-post-action-hooks uses it for `delete-file', `kill-buffer',
`rename-file', `rename-buffer', etc.  For around-action hooks:
`embark--mark-target Save existing mark and point location, mark the target and
run the action.  Most targets at point outside the minibuffer report which
region of the buffer they correspond to (this is the information used by
`embark-highlight-indicator to know what portion of the buffer to highlight);
this function marks that region.  It is useful as an around action hook for
commands that expect a region to be marked, for example, it is used by default
for `indent-region so that it works on s-expression targets, or for `fill-region
so that it works on paragraph targets. `embark--cd Run the action with
`default-directory set to the directory associated to the current target.  The
target should be of type `file', `buffer', `bookmark or `library', and the
associated directory is what you'd expect in each case.
`embark--narrow-to-target Run the action with buffer narrowed to current target.
 Use this as an around hook to localize the effect of actions that don't already
work on just the region.  In the default configuration it is used for
`repunctuate-sentences'. `embark--save-excursion Run the action restoring point
at the end.  The current default configuration doesn't use this but it is
available for users.  3.6 Creating your own keymaps
───────────────────────────── All internal keymaps are defined with the standard
helper macro `defvar-keymap'.  For example a simple version of the file action
keymap could be defined as follows: ┌──── │ (defvar-keymap embark-file-map │
:doc \"Example keymap with a few file actions\" │ :parent embark-general-map │ \"d\"
#'delete-file │ \"r\" #'rename-file │ \"c\" #'copy-file) └──── These action keymaps
are perfectly normal Emacs keymaps.  You may want to inherit from the
`embark-general-map if you want to access the default Embark actions.  Note that
`embark-collect and `embark-export are also made available via
`embark-general-map'.  3.7 Defining actions for new categories of targets
────────────────────────────────────────────────── It is easy to configure
Embark to provide actions for new types of targets, either in the minibuffer or
outside it.  I present below two very detailed examples of how to do this.  At
several points I'll explain more than one way to proceed, typically with the
easiest option first.  I include the alternative options since there will be
similar situations where the easiest option is not available.  3.7.1 New
minibuffer target example - tab-bar tabs
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌ As an example, take the new
[tab bars] from Emacs 27.  I'll explain how to configure Embark to offer
tab-specific actions when you use the tab-bar-mode commands that mention tabs by
name.  The configuration explained here is now built-in to Embark (and
Marginalia), but it's still a good self-contained example.  In order to setup up
tab actions you would need to: (1) make sure Embark knows those commands deal
with tabs, (2) define a keymap for tab actions and configure Embark so it knows
that's the keymap you want. [tab bars]
<https://www.gnu.org/software/emacs/manual/html_node/emacs/Tab-Bars.html> ◊
3.7.1.1 Telling Embark about commands that prompt for tabs by name For step (1),
it would be great if the `tab-bar-mode commands reported the completion category
`tab when asking you for a tab with completion. (All built-in Emacs commands
that prompt for file names, for example, do have metadata indicating that they
want a `file'.) They do not, unfortunately, and I will describe a couple of ways
to deal with this.  Maybe the easiest thing is to configure [Marginalia] to
enhance those commands.  All of the `tab-bar-*-tab-by-name commands have the
words \"tab by name\" in the minibuffer prompt, so you can use: ┌──── │
(add-to-list marginalia-prompt-categories (\"tab by name\" .  tab)) └──── That's
it! But in case you are ever in a situation where you don't already have
commands that prompt for the targets you want, I'll describe how writing your
own command with appropriate `category metadata looks: ┌──── │ (defun
my-select-tab-by-name (tab) │ (interactive │ (list │ (let ((tab-list (or (mapcar
(lambda (tab) (cdr (assq name tab))) │ 				(tab-bar-tabs)) │ 			(user-error \"No
tabs found\")))) │ (completing-read │ \"Tabs: \" │ (lambda (string predicate
action) │ 	 (if (eq action metadata) │ 	 (metadata (category .  tab)) │ 	
(complete-with-action │ 	 action tab-list string predicate))))))) │
(tab-bar-select-tab-by-name tab)) └──── As you can see, the built-in support for
setting the category meta-datum is not very easy to use or pretty to look at.
To help with this I recommend the `consult--read function from the excellent
[Consult] package.  With that function we can rewrite the command as follows:
┌──── │ (defun my-select-tab-by-name (tab) │ (interactive │ (list │ (let
((tab-list (or (mapcar (lambda (tab) (cdr (assq name tab))) │
				(tab-bar-tabs)) │ 			(user-error \"No tabs found\")))) │ (consult--read
tab-list │ 		 :prompt \"Tabs: \" │ 		 :category tab)))) │
(tab-bar-select-tab-by-name tab)) └──── Much nicer! No matter how you define the
`my-select-tab-by-name command, the first approach with Marginalia and prompt
detection has the following advantages: you get the `tab category for all the
`tab-bar-*-bar-by-name commands at once, also, you enhance built-in commands,
instead of defining new ones. [Marginalia] <https://github.com/minad/marginalia>
[Consult] <https://github.com/minad/consult/> ◊ 3.7.1.2 Defining and configuring
a keymap for tab actions Let's say we want to offer select, rename and close
actions for tabs (in addition to Embark general actions, such as saving the tab
name to the kill-ring, which you get for free).  Then this will do: ┌──── │
(defvar-keymap embark-tab-actions │ :doc \"Keymap for actions for tab-bar tabs
(when mentioned by name).\" │ :parent embark-general-map │ \"s\"
#'tab-bar-select-tab-by-name │ \"r\" #'tab-bar-rename-tab-by-name │ \"k\"
#'tab-bar-close-tab-by-name) │ │ (add-to-list embark-keymap-alist (tab .
embark-tab-actions)) └──── What if after using this for a while you feel closing
the tab without confirmation is dangerous? You have a couple of options: 1.  You
can keep using the `tab-bar-close-tab-by-name command, but have Embark ask you
for confirmation: ┌──── │ (push #'embark--confirm │ (alist-get
tab-bar-close-tab-by-name │ 		 embark-pre-action-hooks)) └──── 2.  You can write
your own command that prompts for confirmation and use that instead of
`tab-bar-close-tab-by-name in the above keymap: ┌──── │ (defun
my-confirm-close-tab-by-name (tab) │ (interactive \"@code{sTab} to close: \") │
(when (y-or-n-p (format \"Close tab %s'? \" tab)) │ (tab-bar-close-tab-by-name
tab))) └──── Notice that this is a command you can also use directly from `M-x
independently of Embark.  Using it from `M-x leaves something to be desired,
though, since you don't get completion for the tab names.  You can fix this if
you wish as described in the previous section.  3.7.2 New target example in
regular buffers - short Wikipedia links
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌ Say you want
to teach Embark to treat text of the form `wikipedia:Garry_Kasparov in any
regular buffer as a link to Wikipedia, with actions to open the Wikipedia page
in eww or an external browser or to save the URL of the page in the kill-ring.
We can take advantage of the actions that Embark has preconfigured for URLs, so
all we need to do is teach Embark that `wikipedia:Garry_Kasparov stands for the
URL `https://en.wikipedia.org/wiki/Garry_Kasparov'.  You can be as fancy as you
want with the recognized syntax.  Here, to keep the example simple, I'll assume
the link matches the regexp `wikipedia:[[:alnum:]_]+'.  We will write a function
that looks for a match surrounding point, and returns a dotted list of the form
`'(url URL-OF-THE-PAGE START .  END) where `START and `END are the buffer
positions bounding the target, and are used by Embark to highlight it if you
have `embark-highlight-indicator included in the list `embark-indicators'.
(There are a couple of other options for the return value of a target finder:
the bounding positions are optional and a single target finder is allowed to
return multiple targets; see the documentation for `embark-target-finders for
details.) ┌──── │ (defun my-short-wikipedia-link () │ \"Target a link at point of
the form wikipedia:Page_Name.\" │ (save-excursion │ (let* ((start (progn
(skip-chars-backward \"[:alnum:]_:\") (point))) │ 	 (end (progn
(skip-chars-forward \"[:alnum:]_:\") (point))) │ 	 (str
(buffer-substring-no-properties start end))) │ (save-match-data │ 	(when
(string-match \"wikipedia:\\\\([[:alnum:]_]+\\\\)\" str) │ 	 `(url │ 	 ,(format
\"https://en.wikipedia.org/wiki/%s\" │ 		 (match-string 1 str)) │ 	 ,start .
,end)))))) │ │ (add-to-list embark-target-finders my-short-wikipedia-link) └────
4 How does Embark call the actions? ═══════════════════════════════════ Embark
actions are normal Emacs commands, that is, functions with an interactive
specification.  In order to execute an action, Embark calls the command with
`call-interactively', so the command reads user input exactly as if run directly
by the user.  For example the command may open a minibuffer and read a string
(`read-from-minibuffer') or open a completion interface (`completing-read').  If
this happens, Embark takes the target string and inserts it automatically into
the minibuffer, simulating user input this way.  After inserting the string,
Embark exits the minibuffer, submitting the input. (The immediate minibuffer
exit can be disabled for specific actions in order to allow editing the input;
this is done by adding the `embark--allow-edit function to the appropriate entry
of `embark-target-injection-hooks').  Embark inserts the target string at the
first minibuffer opened by the action command, and if the command happens to
prompt the user for input more than once, the user still interacts with the
second and further prompts in the normal fashion.  Note that if a command does
not prompt the user for input in the minibuffer, Embark still allows you to use
it as an action, but of course, never inserts the target anywhere. (There are
plenty of examples in the default configuration of commands that do not prompt
the user bound to keys in the action maps, most of the region actions, for
instance.) This is how Embark manages to reuse normal commands as actions.  The
mechanism allows you to use as Embark actions commands that were not written
with Embark in mind (and indeed almost all actions that are bound by default in
Embark's action keymaps are standard Emacs commands).  It also allows you to
write new custom actions in such a way that they are useful even without Embark.
 Staring from version 28.1, Emacs has a variable `y-or-n-p-use-read-key', which
when set to `t causes `y-or-n-p to use `read-key instead of
`read-from-minibuffer'.  Setting `y-or-n-p-use-read-key to `t is recommended for
Embark users because it keeps Embark from attempting to insert the target at a
`y-or-n-p prompt, which would almost never be sensible.  Also consider this as a
warning to structure your own action commands so that if they use `y-or-n-p',
they do so only after the prompting for the target.  Here is a simple example
illustrating the various ways of reading input from the user mentioned above.
Bind the following commands to the `embark-symbol-map to be used as actions,
then put the point on some symbol and run them with `embark-act': ┌──── │ (defun
example-action-command1 () │ (interactive) │ (message \"The input was `%s'.\"
(read-from-minibuffer \"Input: \"))) │ │ (defun example-action-command2 (arg
input1 input2) │ (interactive \"P\\@code{nsInput} 1: \\@code{nsInput} 2: \") │
(message \"The first input %swas `%s', and the second was `%s'.\" │ 	 (if arg
\"truly \" \"\") │ 	 input1 │ 	 input2)) │ │ (defun example-action-command3 () │
(interactive) │ (message \"Your selection was `%s'.\" │ 	 (completing-read
\"Select: \" (\"E\" \"M\" \"B\" \"A\" \"R\" \"K\")))) │ │ (defun example-action-command4 () │
(interactive) │ (message \"I don't prompt you for input and thus ignore the
target!\")) │ │ (keymap-set embark-symbol-map \"X 1\" #'example-action-command1) │
(keymap-set embark-symbol-map \"X 2\" #'example-action-command2) │ (keymap-set
embark-symbol-map \"X 3\" #'example-action-command3) │ (keymap-set
embark-symbol-map \"X 4\" #'example-action-command4) └──── Also note that if you
are using the key bindings to call actions, you can pass prefix arguments to
actions in the normal way.  For example, you can use `C-u X2 with the above
demonstration actions to make the message printed by `example-action-command2
more emphatic.  This ability to pass prefix arguments to actions is useful for
some actions in the default configuration, such as
`embark-shell-command-on-buffer'.  4.1 Non-interactive functions as actions
──────────────────────────────────────── Alternatively, Embark does support one
other type of action: a non-interactive function of a single argument.  The
target is passed as argument to the function.  For example: ┌──── │ (defun
example-action-function (target) │ (message \"The target was `%s'.\" target)) │ │
(keymap-set embark-symbol-map \"X 4\" #'example-action-function) └──── Note that
normally binding non-interactive functions in a keymap is useless, since when
attempting to run them using the key binding you get an error message similar to
\"Wrong type argument: commandp, example-action-function\".  In general it is more
flexible to write any new Embark actions as commands, that is, as interactive
functions, because that way you can also run them directly, without Embark.  But
there are a couple of reasons to use non-interactive functions as actions: 1.
You may already have the function lying around, and it is convenient to simply
reuse it.  2.  For command actions the targets can only be simple string, with
no text properties.  For certain advanced uses you may want the action to
receive a string /with/ some text properties, or even a non-string target.  5
Embark, Marginalia and Consult ════════════════════════════════ Embark
cooperates well with the [Marginalia] and [Consult] packages.  Neither of those
packages is a dependency of Embark, but both are highly recommended companions
to Embark, for opposite reasons: Marginalia greatly enhances Embark's
usefulness, while Embark can help enhance Consult.  In the remainder of this
section I'll explain what exactly Marginalia does for Embark, and what Embark
can do for Consult. [Marginalia] <https://github.com/minad/marginalia> [Consult]
<https://github.com/minad/consult> 5.1 Marginalia ────────────── Embark comes
with actions for symbols (commands, functions, variables with actions such as
finding the definition, looking up the documentation, evaluating, etc.) in the
`embark-symbol-map keymap, and for packages (actions like install, delete,
browse url, etc.) in the `embark-package-keymap'.  Unfortunately Embark does not
automatically offers you these keymaps when relevant, because many built-in
Emacs commands don't report accurate category metadata.  For example, a command
like `describe-package', which reads a package name from the minibuffer, does
not have metadata indicating this fact.  In an earlier Embark version, there
were functions to supply this missing metadata, but they have been moved to
Marginalia, which augments many Emacs command to report accurate category
metadata.  Simply activating `marginalia-mode allows Embark to offer you the
package and symbol actions when appropriate again.  Candidate annotations in the
Embark collect buffer are also provided by the Marginalia package: • If you
install Marginalia and activate `marginalia-mode', Embark Collect buffers will
use the Marginalia annotations automatically. • If you don't install Marginalia,
you will see only the annotations that come with Emacs (such as key bindings in
`M-x', or the unicode characters in `C-x 8 RET').  5.2 Consult ─────────── The
excellent Consult package provides many commands that use minibuffer completion,
via the `completing-read function; plenty of its commands can be considered
enhanced versions of built-in Emacs commands, and some are completely new
functionality.  One common enhancement provided in all commands for which it
makes sense is preview functionality, for example `consult-buffer will show you
a quick preview of a buffer before you actually switch to it.  If you use both
Consult and Embark you should install the `embark-consult package which provides
integration between the two.  It provides exporters for several Consult commands
and also tweaks the behavior of many Consult commands when used as actions with
`embark-act in subtle ways that you may not even notice, but make for a smoother
experience.  You need only install it to get these benefits: Embark will
automatically load it after Consult if found.  The `embark-consult package
provides the following exporters: • You can use `embark-export from
`consult-line', `consult-outline', or `consult-mark to obtain an `occur-mode
buffer.  As with the built-in `occur command you use that buffer to jump to a
match and after that, you can then use `next-error and `previous-error to
navigate to other matches.  You can also press `e to activate `occur-edit-mode
and edit the matches in place! • You can export from any of the Consult
asynchronous search commands, `consult-grep', `consult-git-grep', or
`consult-ripgrep to get a `grep-mode buffer.  Here too you can use `next-error
and `previous-error to navigate among matches, and, if you install the [wgrep]
package, you can use it to edit the matches in place.  In both cases, pressing
`g will rerun the Consult command you had exported from and re-enter the input
you had typed (which is similar to reverting but a little more flexible).  You
can then proceed to re-export if that's what you want, but you can also edit the
input changing the search terms or simply cancel if you see you are done with
that search.  The `embark-consult also contains some candidates collectors that
allow you to run `embark-live to get a live-updating table of contents for your
buffer: • `embark-consult-outline-candidates produces the outline headings of
the current buffer, using `consult-outline'. • `embark-consult-imenu-candidates
produces the imenu items of the current buffer, using `consult-imenu'. •
`embark-consult-imenu-or-outline-candidates is a simple combination of the two
previous functions: it produces imenu items in buffers deriving from `prog-mode
and otherwise outline headings.  The way to configure `embark-live (or
`embark-collect and `embark-export for that matter) to use one of these function
is to add it at the end of the `embark-candidate-collectors list.  The
`embark-consult package by default adds the last one, which seems to be the most
sensible default.  Besides those exporters and candidate collectors, the
`embark-consult package provides many subtle tweaks and small integrations
between Embark and Consult.  Some examples are: • When used as actions, the
asynchronous search commands will search only the files associated to the
targets: if the targets /are/ files, it searches those files; for buffers it
will search either the associated file if there is one, else all files in the
buffer's `default-directory'; for bookmarks it will search the file they point
to, same for Emacs Lisp libraries.  This is particularly powerful when using
`embark-act-all to act on multiple files at once, for example you can use
`consult-find to search among file /names/ and then `embark-act-all and
`consult-grep to search within the matching files. • For all other target types,
those that do not have a sensible notion of associated file, a Consult search
command (asynchronous or not) will search for the text of the target but leave
the minibuffer open so you can interact with the Consult command. •
`consult-imenu will search for the target and take you directly to the location
if it matches a unique imenu entry, otherwise it will leave the minibuffer open
so you can navigate among the matches. [wgrep]
<http://github.com/mhayashi1120/Emacs-wgrep/raw/master/wgrep.el > 6 Related
Packages ══════════════════ There are several packages that offer functionality
similar to Embark's.  Acting on minibuffer completion candidates The popular Ivy
and Helm packages have support for acting on the completion candidates of
commands written using their APIs, and there is an extensive ecosystem of
packages meant for Helm and for Ivy (the Ivy ones usually have \"counsel\" in the
name) providing commands and appropriate actions.  Acting on things at point The
built-in `context-menu-mode provides a mouse-driven context-sensitive
configurable menu.  The `do-at-point package by Philip Kaludercic (available on
GNU ELPA), on the other hand is keyboard-driven.  Collecting completion
candidates into a buffer The Ivy package has the command `ivy-occur which is
similar to `embark-collect'.  As with Ivy actions, `ivy-occur only works for
commands written using the Ivy API. 7 Resources ═══════════ If you want to learn
more about how others have used Embark here are some links to read: • [Fifteen
ways to use Embark], a blog post by Karthik Chikmagalur. • [Protesilaos
Stavrou's dotemacs], look for the section called \"Extended minibuffer actions
and more (embark.el and prot-embark.el)\" And some videos to watch: • [Embark and
my extras] by Protesilaos Stavrou. • [Embark – Key features and tweaks] by Raoul
Comninos on the Emacs-Elements @code{YouTube} channel. • [Livestreamed: Adding
an Embark context action to send a stream message] by Sacha Chua. • [System
Crafters Live! - The Many Uses of Embark] by David Wilson. • [Using Emacs
Episode 80 - Vertico, Marginalia, Consult and Embark] by Mike Zamansky. [Fifteen
ways to use Embark] <https://karthinks.com/software/fifteen-ways-to-use-embark/>
[Protesilaos Stavrou's dotemacs] <https://protesilaos.com/dotemacs/> [Embark and
my extras] <https://protesilaos.com/codelog/2021-01-09-emacs-embark-extras/>
[Embark – Key features and tweaks] <https://youtu.be/@code{qpoQiiinCtY>}
[Livestreamed: Adding an Embark context action to send a stream message]
<https://youtu.be/@code{WsxXr1ncukY>} [System Crafters Live! - The Many Uses of
Embark] <https://youtu.be/qk2Is_@code{sC8Lk>} [Using Emacs Episode 80 - Vertico,
Marginalia, Consult and Embark] <https://youtu.be/5ffb2at2d7w> 8 Contributions
═══════════════ Contributions to Embark are very welcome.  There is a [wish
list] for actions, target finders, candidate collectors and exporters.  For
other ideas you have for Embark, feel free to open an issue on the [issue
tracker].  Any neat configuration tricks you find might be a good fit for the
[wiki].  Code contributions are very welcome too, but since Embark is now on GNU
ELPA, copyright assignment to the FSF is required before you can contribute
code. [wish list] <https://github.com/oantolin/embark/issues/95> [issue tracker]
<https://github.com/oantolin/embark/issues> [wiki]
<https://github.com/oantolin/embark/wiki> 9 Acknowledgments ═════════════════
While I, Omar Antolín Camarena, have written most of the Embark code and remain
very stubborn about some of the design decisions, Embark has received
substantial help from a number of other people which this document has neglected
to mention for far too long.  In particular, Daniel Mendler has been absolutely
invaluable, implementing several important features, and providing a lot of
useful advice.  Code contributions: • [Daniel Mendler] • [Clemens Radermacher] •
[José Antonio Ortega Ruiz] • [Itai Y. Efrat] • [a13] • [jakanakaevangeli] •
[mihakam] • [Brian Leung] • [Karthik Chikmagalur] • [Roshan Shariff] •
[condy0919] • [Damien Cassou] • [@code{JimDBh}] Advice and useful discussions: •
[Daniel Mendler] • [Protesilaos Stavrou] • [Clemens Radermacher] • [Howard
Melman] • [Augusto Stoffel] • [Bruce d'Arcus] • [JD Smith] • [Karthik
Chikmagalur] • [jakanakaevangeli] • [Itai Y. Efrat] • [Mohsin Kaleem] [Daniel
Mendler] <https://github.com/minad> [Clemens Radermacher]
<https://github.com/clemera/> [José Antonio Ortega Ruiz]
<https://codeberg.org/jao/> [Itai Y. Efrat] <https://github.com/iyefrat> [a13]
<https://github.com/a13> [jakanakaevangeli]
<https://github.com/jakanakaevangeli> [mihakam] <https://github.com/mihakam>
[Brian Leung] <https://github.com/leungbk> [Karthik Chikmagalur]
<https://github.com/karthink> [Roshan Shariff]
<https://github.com/roshanshariff> [condy0919] <https://github.com/condy0919>
[Damien Cassou] <https://github.com/@code{DamienCassou>} [@code{JimDBh}]
<https://github.com/@code{JimDBh>} [Protesilaos Stavrou]
<https://gitlab.com/protesilaos/> [Howard Melman] <https://github.com/hmelman/>
[Augusto Stoffel] <https://github.com/astoff> [Bruce d'Arcus]
<https://github.com/bdarcus> [JD Smith] <https://github.com/jdtsmith> [Mohsin
Kaleem] <https://github.com/mohkale>.")
    (license license:gpl3+)))

;emacs-elfeed-tube
;emacs-embark-consult
emacs-nano-emacs
