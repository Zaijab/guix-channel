(define-module (zaijab packages discord)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages video)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages node)
  #:use-module (gnu packages gcc))

(define-public discord
  (package
   (name "discord")
   (version "0.0.15")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://cdn.discordapp.com/apps/linux/"
                                version
                                "/"
                                name
                                "-"
                                version
                                ".tar.gz"))
            (sha256
             (base32
              "0pn2qczim79hqk2limgh88fsn93sa8wvana74mpdk5n6x5afkvdd"))))
   (build-system trivial-build-system)
   (inputs `(("alsa-lib"      ,alsa-lib)
             ("atk"           ,atk)
             ("at-spi2-atk"   ,at-spi2-atk)
             ("at-spi2-core"   ,at-spi2-core)
             ("cairo"         ,cairo)
             ("cups"          ,cups)
             ("dbus"          ,dbus)
             ("expat"         ,expat)
             ("fontconfig"    ,fontconfig)
             ("freetype"      ,freetype)
             ("gdk-pixbuf"    ,gdk-pixbuf)
             ("glib"          ,glib)
             ("gtk3"          ,gtk+)
             ("libnotify"     ,libnotify)
             ("libx11"        ,libx11)
             ("libxcomposite" ,libxcomposite)
             ("libxcursor"    ,libxcursor)
             ("libxdamage"    ,libxdamage)
             ("libuuid"       ,util-linux)
             ("libxext"       ,libxext)
             ("libxfixes"     ,libxfixes)
             ("libxi"         ,libxi)
             ("libxrandr"     ,libxrandr)
             ("libxrender"    ,libxrender)
             ("libxtst"       ,libxtst)
             ("nspr"          ,nspr)
             ("nss"           ,nss)
             ("libxcb"        ,libxcb)
             ("pango"         ,pango)
             ("libxscrnsaver" ,libxscrnsaver)
             ("libcxx"        ,clang-runtime)
             ("libpulseaudio" ,pulseaudio)
             ("libffmpeg"     ,ffmpeg)
             ("node"          ,node)
             ("wget"          ,wget)
             ("libgcc"        ,gcc "lib")
             ("libcxx"        ,libcxx)))
   (native-inputs `(("patchelf" ,patchelf)
                    ("glibc"    ,glibc)
                    ("tar"      ,tar)
                    ("gzip"     ,gzip)))
   (propagated-inputs `(("wget" ,wget)
			("libsm" ,libsm)
			("libappindicator" ,libappindicator)
			("libdbusmenu" ,libdbusmenu)
			("libdrm" ,libdrm)
			("mesa" ,mesa)
			("unzip" ,unzip)
                        ("node" ,node)))
   (arguments
    `(#:modules ((guix build utils))
      #:builder (begin
                  (use-modules (guix build utils)
                               (srfi srfi-26))
                  (let* ((patchelf (assoc-ref %build-inputs "patchelf"))
                         (glibc    (assoc-ref %build-inputs "glibc"))
                         (source   (assoc-ref %build-inputs "source"))
                         (tar      (assoc-ref %build-inputs "tar"))
                         (gzip     (assoc-ref %build-inputs "gzip"))
                         (output   (assoc-ref %outputs "out")))
                    (define libpath "")
                    (for-each
                     (lambda (i)
                       (set! libpath
                         (string-append libpath (string-append (cdr i) "/lib") ":")))
                     %build-inputs)
                    (set! libpath
                      (string-append libpath (assoc-ref %build-inputs "nss") "/lib/nss"))
                    (setenv "PATH" (string-append gzip "/bin"))
                    (mkdir-p (string-append output "/opt/discord"))
                    (invoke (string-append tar "/bin/tar") "xpvf" source)
                    (copy-recursively "./Discord" (string-append output "/opt/discord"))
                    (mkdir-p (string-append output "/bin")) 
                    (mkdir-p (string-append output "/share/pixmaps"))
                    (chmod (string-append output "/opt/discord/Discord") #o755)
                    (let ((output-port (open-file (string-append output "/bin/discord") "a")))
                      (display "#!/bin/sh\n" output-port)
                      (display (string-append "cd " output "/opt/discord\n") output-port)
                      (display (string-append "LD_LIBRARY_PATH=" libpath " " "./Discord\n") output-port)
                      (close output-port))
                    (chmod (string-append output "/bin/discord") #o755) 
                    (invoke (string-append patchelf "/bin/patchelf")
                            "--set-interpreter"
                            (string-append glibc "/lib/ld-linux-x86-64.so.2")
                            (string-append output "/opt/discord/Discord"))
                    ;(link (string-append %output "/opt/discord/Discord")
                    ;      (string-append %output "/bin/discord"))
                    (link (string-append %output "/opt/discord/discord.png")
                          (string-append %output "/share/pixmaps/discord.png"))
                    #t))))
   (synopsis "Discord chat client.")
   (description "Discord chat client.")
   (license #f)
   (home-page "https://discordapp.com")))
