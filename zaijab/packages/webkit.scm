(define-module (zaijab packages webkit)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

(define-public webkitgtk
  (package
    (name "webkitgtk")
    (version "2.34.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.webkitgtk.org/releases/"
                                  "webkitgtk-" version ".tar.xz"))
              (sha256
               (base32
                "1xn1hhd0qaxmjf6vy6664i4mmmjsw9zgrr4w8ni3415d981zvj3b"))
              (patches (search-patches "webkitgtk-bind-all-fonts.patch"
                                       "webkitgtk-adjust-bubblewrap-paths.patch"
                                       "webkitgtk-canonicalize-paths.patch"))))
    (build-system cmake-build-system)
    (outputs '("out" "doc" "debug"))
    (arguments
     `(#:tests? #f ; no tests
       #:build-type "Release" ; turn off debugging symbols to save space
       #:configure-flags (list
                          "-DPORT=GTK"
                          "-DENABLE_GTKDOC=ON" ; No doc by default
                          ;; Requires libmanette, new dependency added in 2.32.0.
                          ;; TODO Decide if we should enable this
                          "-DENABLE_GAMEPAD=OFF"
                          "-DUSE_SYSTEMD=OFF"
                          (string-append ; uses lib64 by default
                           "-DLIB_INSTALL_DIR="
                           (assoc-ref %outputs "out") "/lib"))
       #:make-flags
       ;; Never build with unsupported -j1: https://issues.guix.gnu.org/47964#5
       (list "-j" (number->string (max 2 (parallel-job-count))))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'configure-bubblewrap-store-directory
           (lambda _
             ;; This phase is a corollary to 'webkitgtk-share-store.patch' to
             ;; avoid hard coding /gnu/store, for users with other prefixes.
             (let ((store-directory (%store-directory)))
               (substitute*
                   "Source/WebKit/UIProcess/Launcher/glib/BubblewrapLauncher.cpp"
                 (("@storedir@") store-directory)))))
         (add-after 'unpack 'patch-gtk-doc-scan
           (lambda* (#:key inputs #:allow-other-keys)
             (for-each (lambda (file)
                         (substitute* file
                           (("http://www.oasis-open.org/docbook/xml/4.1.2/docbookx.dtd")
                            (string-append (assoc-ref inputs "docbook-xml")
                                           "/xml/dtd/docbook/docbookx.dtd"))))
                       (find-files "Source" "\\.sgml$"))))
         (add-after 'unpack 'embed-absolute-wpebackend-reference
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((wpebackend-fdo (assoc-ref inputs "wpebackend-fdo")))
               (substitute* "Source/WebKit/UIProcess/glib/WebProcessPoolGLib.cpp"
                 (("libWPEBackend-fdo-([\\.0-9]+)\\.so" all version)
                  (string-append wpebackend-fdo "/lib/" all))))))
         ,@(if (target-x86-64?)
               '()
               '((add-after 'unpack 'disable-sse2
                   (lambda _
                     (substitute* "Source/cmake/WebKitCompilerFlags.cmake"
                       (("WTF_CPU_X86 AND NOT CMAKE_CROSSCOMPILING")
                        "FALSE"))))))
         (add-after 'install 'move-doc-files
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (doc (assoc-ref outputs "doc")))
               (mkdir-p (string-append doc "/share"))
               (rename-file (string-append out "/share/gtk-doc")
                            (string-append doc "/share/gtk-doc"))))))))
    (native-inputs
     `(("bison" ,bison)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin") ; for glib-mkenums, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("gperf" ,gperf)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("gtk-doc" ,gtk-doc/stable) ; For documentation generation
       ("docbook-xml" ,docbook-xml) ; For documentation generation
       ("ruby" ,ruby)))
    (propagated-inputs
     (list gtk+ libsoup))
    (inputs
     `(("at-spi2-core" ,at-spi2-core)
       ("bubblewrap" ,bubblewrap)
       ("enchant" ,enchant)
       ("geoclue" ,geoclue)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gtk+-2" ,gtk+-2)
       ("harfbuzz" ,harfbuzz)
       ("hyphen" ,hyphen)
       ("icu4c" ,icu4c)
       ("lcms" ,lcms)
       ("libgcrypt" ,libgcrypt)
       ("libjpeg" ,libjpeg-turbo)
       ("libnotify" ,libnotify)
       ("libpng" ,libpng)
       ("libseccomp" ,libseccomp)
       ("libsecret" ,libsecret)
       ("libtasn1" ,libtasn1)
       ("libwebp" ,libwebp)
       ("libwpe" ,libwpe)
       ("libxcomposite" ,libxcomposite)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("libxt" ,libxt)
       ("mesa" ,mesa)
       ("openjpeg" ,openjpeg)
       ("sqlite" ,sqlite)
       ("woff2" ,woff2)
       ("wpebackend-fdo" ,wpebackend-fdo)
       ("xdg-dbus-proxy" ,xdg-dbus-proxy)))
    (home-page "https://www.webkitgtk.org/")
    (synopsis "Web content engine for GTK+")
    (description
     "WebKitGTK+ is a full-featured port of the WebKit rendering engine,
suitable for projects requiring any kind of web integration, from hybrid
HTML/CSS applications to full-fledged web browsers.  WebKitGTK+ video playing
capabilities can be extended through the use of GStreamer plugins (not
propagated by default) such as @code{gst-plugins-good} and
@code{gst-plugins-bad}.")
    ;; WebKit's JavaScriptCore and WebCore components are available under
    ;; the GNU LGPL, while the rest is available under a BSD-style license.
    (license (list license:lgpl2.0
                   license:lgpl2.1+
                   license:bsd-2
                   license:bsd-3))))

;;; Required by gnome-online-accounts; as webkitgtk 2.34 propagates libsoup 3,
;;; which causes the build to fail.
;;; Also required by e.g. emacs-next-pgtk,  emacs-xwidgets, and some other
;;; Gnome packages for webkit2gtk-4.0. See also the upstream tracker for
;;; libsoup 3: https://gitlab.gnome.org/GNOME/libsoup/-/issues/218
(define-public webkitgtk-with-libsoup2-old
  (package/inherit webkitgtk
		   (name "webkitgtk-with-libsoup2")
		   (arguments (substitute-keyword-arguments (package-arguments webkitgtk)
				((#:configure-flags flags)
				 `(cons "-DUSE_SOUP2=ON" ,flags))))
		   (propagated-inputs
		    (alist-replace "libsoup" (list libsoup-minimal-2)
				   (package-propagated-inputs webkitgtk)))))
