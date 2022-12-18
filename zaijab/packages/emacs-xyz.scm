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

(define-public emacs-without-native-comp
  (package
    (name "emacs")
    (version "28.2")
    (source (origin
	      (method url-fetch)
	      (uri (string-append "mirror://gnu/emacs/emacs-"
				  version ".tar.xz"))
	      (sha256
	       (base32
		"12144dcaihv2ymfm7g2vnvdl4h71hqnsz1mljzf34cpg6ci1h8gf"))
	      (patches (search-patches "emacs-exec-path.patch"
				       "emacs-fix-scheme-indent-function.patch"
				       "emacs-source-date-epoch.patch"))
	      (modules '((guix build utils)))
	      (snippet
	       '(with-directory-excursion "lisp"
		  ;; Delete the bundled byte-compiled elisp files and generated
		  ;; autoloads.
		  (for-each delete-file
			    (append (find-files "." "\\.elc$")
				    (find-files "." "loaddefs\\.el$")
				    (find-files "eshell" "^esh-groups\\.el$")))

		  ;; Make sure Tramp looks for binaries in the right places on
		  ;; remote Guix System machines, where 'getconf PATH' returns
		  ;; something bogus.
		  (substitute* "net/tramp.el"
		    ;; Patch the line after "(defcustom tramp-remote-path".
		    (("\\(tramp-default-remote-path")
		     (format #f "(tramp-default-remote-path ~s ~s ~s ~s "
			     "~/.guix-profile/bin" "~/.guix-profile/sbin"
			     "/run/current-system/profile/bin"
			     "/run/current-system/profile/sbin")))

		  ;; Make sure Man looks for C header files in the right
		  ;; places.
		  (substitute* "man.el"
		    (("\"/usr/local/include\"" line)
		     (string-join
		      (list line
			    "\"~/.guix-profile/include\""
			    "\"/var/guix/profiles/system/profile/include\"")
		      " ")))))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list
      #:tests? #f                      ; no check target
      #:modules (%emacs-modules build-system)
      #:configure-flags #~(list "--with-modules"
				"--with-cairo"
				
				"--disable-build-details")
      #:make-flags #~(list "NATIVE_FULL_AOT=1")
      #:phases
      #~(modify-phases %standard-phases
	  (add-after 'set-paths 'set-libgccjit-path
	    (lambda* (#:key inputs #:allow-other-keys)
	      (define (first-subdirectory/absolute directory)
		(let ((files (scandir
			      directory
			      (lambda (file)
				(and (not (member file '("." "..")))
				     (file-is-directory? (string-append
							  directory "/"
							  file)))))))
		  (and (not (null? files))
		       (string-append directory "/" (car files)))))
	      (let* ((libgccjit-libdir
		      (first-subdirectory/absolute ;; version
		       (first-subdirectory/absolute ;; host type
			(search-input-directory inputs "lib/gcc")))))
		(setenv "LIBRARY_PATH"
			(string-append (getenv "LIBRARY_PATH")
				       ":" libgccjit-libdir)))))
	  (add-after 'unpack 'enable-elogind
	    (lambda _
	      (substitute* "configure.ac"
		(("libsystemd") "libelogind"))
	      (when (file-exists? "configure")
		(delete-file "configure"))))
	  (add-after 'unpack 'patch-program-file-names
	    (lambda* (#:key inputs #:allow-other-keys)
	      (substitute* '("src/callproc.c"
			     "lisp/term.el"
			     "lisp/htmlfontify.el"
			     "lisp/textmodes/artist.el"
			     "lisp/progmodes/sh-script.el")
		(("\"/bin/sh\"")
		 (format #f "~s" (search-input-file inputs "/bin/sh"))))
	      (substitute* "lisp/doc-view.el"
		(("\"(gs|dvipdf|ps2pdf|pdftotext)\"" all what)
		 (let ((replacement (false-if-exception
				     (search-input-file
				      inputs
				      (string-append "/bin/" what)))))
		   (if replacement
		       (string-append "\"" replacement "\"")
		       all))))
	      ;; match ".gvfs-fuse-daemon-real" and ".gvfsd-fuse-real"
	      ;; respectively when looking for GVFS processes.
	      (substitute* "lisp/net/tramp-gvfs.el"
		(("\\(tramp-compat-process-running-p \"(.*)\"\\)" all process)
		 (format #f "(or ~a (tramp-compat-process-running-p ~s))"
			 all (string-append "." process "-real"))))))
	  (add-after 'unpack 'patch-compilation-driver
	    (lambda _
	      (substitute* "lisp/emacs-lisp/comp.el"
		(("\\(defcustom native-comp-driver-options nil")
		 (format
		  #f "(defcustom native-comp-driver-options '(~@{~s~^ ~})"
		  (string-append
		   "-B" #$(this-package-input "binutils") "/bin/")
		  (string-append
		   "-B" #$(this-package-input "glibc") "/lib/")
		  (string-append
		   "-B" #$(this-package-input "libgccjit") "/lib/")
		  (string-append
		   "-B" #$(this-package-input "libgccjit") "/lib/gcc/"))))))
	  (add-before 'configure 'fix-/bin/pwd
	    (lambda _
	      ;; Use `pwd', not `/bin/pwd'.
	      (substitute* (find-files "." "^Makefile\\.in$")
		(("/bin/pwd")
		 "pwd"))))
	  (add-after 'install 'install-site-start
	    ;; Use 'guix-emacs' in "site-start.el", which is used autoload the
	    ;; Elisp packages found in EMACSLOADPATH.
	    (lambda* (#:key inputs outputs #:allow-other-keys)
	      (let* ((out      (assoc-ref outputs "out"))
		     (lisp-dir (string-append out "/share/emacs/site-lisp"))
		     (emacs    (string-append out "/bin/emacs")))

		;; This is duplicated from emacs-utils to prevent coupling.
		(define* (emacs-byte-compile-directory dir)
		  (let ((expr `(progn
				(setq byte-compile-debug t)
				(byte-recompile-directory
				 (file-name-as-directory ,dir) 0 1))))
		    (invoke emacs "--quick" "--batch"
			    (format #f "--eval=~s" expr))))

		(copy-file #$(local-file
			      (search-auxiliary-file "emacs/guix-emacs.el"))
			   (string-append lisp-dir "/guix-emacs.el"))
		(with-output-to-file (string-append lisp-dir "/site-start.el")
		  (lambda ()
		    (display
		     (string-append
		      "(when (require 'guix-emacs nil t)\n"
		      "  (guix-emacs-autoload-packages)\n"
		      "  (advice-add 'package-load-all-descriptors"
		      " :after #'guix-emacs-load-package-descriptors))"))))
		;; Remove the extraneous subdirs.el file, as it causes Emacs to
		;; add recursively all the the sub-directories of a profile's
		;; share/emacs/site-lisp union when added to EMACSLOADPATH,
		;; which leads to conflicts.
		(delete-file (string-append lisp-dir "/subdirs.el"))
		;; Byte compile the site-start files.
		(emacs-byte-compile-directory lisp-dir))))
	  (add-after 'glib-or-gtk-wrap 'restore-emacs-pdmp
	    ;; restore the dump file that Emacs installs somewhere in
	    ;; libexec/ to its original state
	    (lambda* (#:key outputs target #:allow-other-keys)
	      (let* ((libexec (string-append (assoc-ref outputs "out")
					     "/libexec"))
		     ;; each of these ought to only match a single file,
		     ;; but even if not (find-files) sorts by string<,
		     ;; so the Nth element in one maps to the Nth element of
		     ;; the other
		     (pdmp (find-files libexec "\\.pdmp$"))
		     (pdmp-real (find-files libexec "\\.pdmp-real$")))
		(for-each rename-file pdmp-real pdmp))))
	  (add-after 'glib-or-gtk-wrap 'strip-double-wrap
	    (lambda* (#:key outputs #:allow-other-keys)
	      ;; Directly copy emacs-X.Y to emacs, so that it is not wrapped
	      ;; twice.  This also fixes a minor issue, where WMs would not be
	      ;; able to track emacs back to emacs.desktop.
	      (with-directory-excursion (assoc-ref outputs "out")
		(copy-file
		 (car (find-files "bin" "^emacs-([0-9]+\\.)+[0-9]+$"))
		 "bin/emacs"))))
	  (add-after 'strip-double-wrap 'wrap-emacs-paths
	    (lambda* (#:key inputs outputs #:allow-other-keys)
	      (let* ((out (assoc-ref outputs "out"))
		     (lisp-dirs (find-files (string-append out "/share/emacs")
					    "^lisp$"
					    #:directories? #t)))
		(for-each
		 (lambda (prog)
		   (wrap-program prog
		     ;; emacs-next and variants rely on uname being in PATH for
		     ;; Tramp.  Tramp paths can't be hardcoded, because they
		     ;; need to be portable.
		     `("PATH" suffix
		       ,(map dirname
			     (list (search-input-file inputs "/bin/gzip")
				   ;; for coreutils
				   (search-input-file inputs "/bin/yes"))))
		     `("EMACSLOADPATH" suffix ,lisp-dirs)))
		 (find-files (string-append out "/bin")
			     ;; Matches versioned and unversioned emacs binaries.
			     ;; We don't patch emacsclient, because it takes its
			     ;; environment variables from emacs.
			     ;; Likewise, we don't need to patch helper binaries
			     ;; like etags, ctags or ebrowse.
			     "^emacs(-[0-9]+(\\.[0-9]+)*)?$"))))))))
    (inputs
     (list gnutls
	   ncurses

	   ;; To "unshadow" ld-wrapper in native builds
	   (make-ld-wrapper "ld-wrapper" #:binutils binutils)

	   ;; For native compilation
	   binutils
	   glibc
	   libgccjit

	   ;; Required for "core" functionality, such as dired and compression.
	   coreutils
	   gzip

	   ;; Avoid Emacs's limited movemail substitute that retrieves POP3
	   ;; email only via insecure channels.
	   ;; This is not needed for (modern) IMAP.
	   mailutils

	   gpm
	   libx11
	   gtk+
	   cairo
	   pango
	   harfbuzz
	   libxft
	   libtiff
	   giflib
	   lcms
	   libjpeg-turbo
	   libselinux
	   acl
	   jansson
	   gmp
	   ghostscript
	   poppler
	   elogind

	   ;; When looking for libpng `configure' links with `-lpng -lz', so we
	   ;; must also provide zlib as an input.
	   libpng
	   zlib
	   (if (target-x86-64?)
	       librsvg-bootstrap
	       librsvg-2.40)
	   libxpm
	   libxml2
	   libice
	   libsm
	   alsa-lib
	   dbus

	   ;; multilingualization support
	   libotf
	   m17n-lib))
    (native-inputs
     (list autoconf pkg-config texinfo))
    (native-search-paths
     (list (search-path-specification
	    (variable "EMACSLOADPATH")
	    (files '("share/emacs/site-lisp")))
	   (search-path-specification
	    (variable "EMACSNATIVELOADPATH")
	    (files '("lib/emacs/native-site-lisp")))
	   (search-path-specification
	    (variable "INFOPATH")
	    (files '("share/info")))))

    (home-page "https://www.gnu.org/software/emacs/")
    (synopsis "The extensible, customizable, self-documenting text editor")
    (description
     "GNU Emacs is an extensible and highly customizable text editor.  It is
based on an Emacs Lisp interpreter with extensions for text editing.  Emacs
has been extended in essentially all areas of computing, giving rise to a
vast array of packages supporting, e.g., email, IRC and XMPP messaging,
spreadsheets, remote server editing, and much more.  Emacs includes extensive
documentation on all aspects of the system, from basic editing to writing
large Lisp programs.  It has full Unicode support for nearly all human
languages.")
    (license license:gpl3+)))




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








