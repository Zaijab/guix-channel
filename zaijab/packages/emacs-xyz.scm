(define-module (zaijab packages emacs-xyz)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
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
  #:use-module (gnu packages emacs-xyz))

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
		    (commit "18d89f19203423b9e2df59a556c1240746903d8f")))
	      (sha256
	       (base32
		"0h32bpq8w1j3rbwfqlfj10vvlw21j9rcpqjpp45pw8z6xxb2q0lp"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-elfeed emacs-aio))
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



