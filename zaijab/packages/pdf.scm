(define-module (zaijab packages pdf)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages djvu)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages javascript)
  #:use-module (gnu packages lesstif)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages boost)
  #:use-module (srfi srfi-1)
  #:use-module (gnu packages pdf))

(define-public poppler
  (package
   (name "poppler")
   (version "21.10.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://poppler.freedesktop.org/poppler-"
                                version ".tar.xz"))
            (sha256
             (base32
              "1563xz4rayf424vkl33mplv0gfs9xb6mnakwwpxc7ghg54b5njwn"))))
   (build-system cmake-build-system)
   ;; FIXME:
   ;;  use libcurl:        no
   (inputs `(("fontconfig" ,fontconfig)
             ("freetype" ,freetype)
             ("libjpeg" ,libjpeg-turbo)
             ("libpng" ,libpng)
             ("libtiff" ,libtiff)
             ("lcms" ,lcms)
             ("openjpeg" ,openjpeg)
             ("zlib" ,zlib)
             ("nss" ,nss)
             ("qtbase" ,qtbase)
             ("boost" ,boost)
	     ("poppler-data" ,poppler-data)
             ;; To build poppler-glib (as needed by Evince), we need Cairo and
             ;; GLib.  But of course, that Cairo must not depend on Poppler.
             ("cairo" ,cairo-sans-poppler)))
   (propagated-inputs
    ;; As per poppler-cairo and poppler-glib.pc.
    ;; XXX: Ideally we'd propagate Cairo too, but that would require a
    ;; different solution to the circular dependency mentioned above.
    `(("glib" ,glib)))
   (native-inputs
      `(("pkg-config" ,pkg-config)
        ("glib" ,glib "bin")                      ; glib-mkenums, etc.
        ("gobject-introspection" ,gobject-introspection)))
   (arguments
    `(#:tests? #f ; no test data provided with the tarball
      #:configure-flags
      (let* ((out (assoc-ref %outputs "out"))
             (lib (string-append out "/lib")))
        (list "-DENABLE_UNSTABLE_API_ABI_HEADERS=ON" ;to install header files
              "-DENABLE_ZLIB=ON"
              (string-append "-DCMAKE_INSTALL_LIBDIR=" lib)
              (string-append "-DCMAKE_INSTALL_RPATH=" lib)))))
   (synopsis "PDF rendering library")
   (description
    "Poppler is a PDF rendering library based on the xpdf-3.0 code base.")
   (license license:gpl2+)
   (home-page "https://poppler.freedesktop.org/")))
