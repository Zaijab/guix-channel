;; (define-module (zaijab packages discord)
;;   #:use-module (guix packages)
;;   #:use-module (guix download)
;;   #:use-module (guix build-system trivial)
;;   #:use-module (gnu packages base)
;;   #:use-module (gnu packages bootstrap)
;;   #:use-module (gnu packages compression)
;;   #:use-module (gnu packages cups)
;;   #:use-module (gnu packages elf)
;;   #:use-module (gnu packages fonts)
;;   #:use-module (gnu packages fontutils)
;;   #:use-module (gnu packages freedesktop)
;;   #:use-module (gnu packages gcc)
;;   #:use-module (gnu packages gl)
;;   #:use-module (gnu packages glib)
;;   #:use-module (gnu packages gnome)
;;   #:use-module (gnu packages gtk)
;;   #:use-module (gnu packages gnuzilla)
;;   #:use-module (gnu packages linux)
;;   #:use-module (gnu packages llvm)
;;   #:use-module (gnu packages node)
;;   #:use-module (gnu packages nss)
;;   #:use-module (gnu packages pulseaudio)
;;   #:use-module (gnu packages video)
;;   #:use-module (gnu packages wget)
;;   #:use-module (gnu packages xdisorg)
;;   #:use-module (gnu packages xml)
;;   #:use-module (gnu packages xorg)
;;   #:use-module (rc packages font-twitter-emoji)
;;   #:use-module (rc packages systemd))

;; (define-public discord
;;   (package
;;    (name "discord")
;;    (version "0.0.15")
;;    (source (origin
;;             (method url-fetch)
;;             (uri (string-append "https://cdn.discordapp.com/apps/linux/"
;;                                 version
;;                                 "/"
;;                                 name
;;                                 "-"
;;                                 version
;;                                 ".tar.gz"))
;;             (sha256
;;              (base32
;;               "0pn2qczim79hqk2limgh88fsn93sa8wvana74mpdk5n6x5afkvdd"))))
;;    (build-system trivial-build-system)
;;    (inputs `(("alsa-lib"                   ,alsa-lib)
;;              ("atk"                        ,atk)
;;              ("at-spi2-atk"                ,at-spi2-atk)
;;              ("at-spi2-core"               ,at-spi2-core)
;;              ("cairo"                      ,cairo)
;;              ("cups"                       ,cups)
;;              ("dbus"                       ,dbus)
;;              ("expat"                      ,expat)
;;              ("fontconfig"                 ,fontconfig)
;;              ("font-awesome"               ,font-awesome)
;;              ("font-dejavu"                ,font-dejavu)
;;              ("font-google-noto"           ,font-google-noto)
;;              ("font-twitter-emoji"         ,font-twitter-emoji)
;;              ("freetype"                   ,freetype)
;;              ("gdk-pixbuf"                 ,gdk-pixbuf)
;;              ("glib"                       ,glib)
;;              ("glibc"                      ,glibc)
;;              ("gtk3"                       ,gtk+)
;;              ("libappindicator"            ,libappindicator)
;;              ("libdbusmenu"                ,libdbusmenu)
;;              ("libdrm"                     ,libdrm)
;;              ("libgcc"                     ,gcc "lib")
;;              ("libcxx"                     ,clang-runtime)
;;              ("libcxx"                     ,libcxx)
;;              ("libffmpeg"                  ,ffmpeg)
;;              ("libnotify"                  ,libnotify)
;;              ("libpulseaudio"              ,pulseaudio)
;;              ("libsm"                      ,libsm)
;;              ("libx11"                     ,libx11)
;;              ("libxcomposite"              ,libxcomposite)
;;              ("libxcursor"                 ,libxcursor)
;;              ("libxdamage"                 ,libxdamage)
;;              ("libuuid"                    ,util-linux)
;;              ("libxcb"                     ,libxcb)
;;              ("libxext"                    ,libxext)
;;              ("libxfixes"                  ,libxfixes)
;;              ("libxi"                      ,libxi)
;;              ("libxrandr"                  ,libxrandr)
;;              ("libxrender"                 ,libxrender)
;;              ("libxscrnsaver"              ,libxscrnsaver)
;;              ("libxtst"                    ,libxtst)
;;              ("mesa"                       ,mesa)
;;              ("node"                       ,node)
;;              ("nspr"                       ,nspr)
;;              ("nss"                        ,nss)
;;              ("pango"                      ,pango)
;;              ("systemd"                    ,systemd)
;;              ("unzip"                      ,unzip)
;;              ("wget"                       ,wget)))
;;    (native-inputs `(("gzip"                ,gzip)
;;                     ("patchelf"            ,patchelf)
;;                     ("tar"                 ,tar)))
;;    (arguments
;;     `(#:modules ((guix build utils))
;;       #:builder (begin
;;                   (use-modules (guix build utils)
;;                                (srfi srfi-26)
;; 			       (sxml simple))
;;                   (let* ((patchelf (assoc-ref %build-inputs "patchelf"))
;;                          (glibc    (assoc-ref %build-inputs "glibc"))
;;                          (source   (assoc-ref %build-inputs "source"))
;;                          (tar      (assoc-ref %build-inputs "tar"))
;;                          (gzip     (assoc-ref %build-inputs "gzip"))
;;                          (output   (assoc-ref %outputs "out"))
;;                          (fonts    (list
;;                                      (assoc-ref %build-inputs "font-twitter-emoji")
;;                                      (assoc-ref %build-inputs "font-google-noto")
;;                                      (assoc-ref %build-inputs "font-dejavu")
;;                                      (assoc-ref %build-inputs "font-awesome")))
;;                          (libs     (cons
;;                                      (string-append (assoc-ref %build-inputs "nss") "/lib/nss")
;;                                      (map (lambda (i) (string-append (cdr i) "/lib"))
;;                                           %build-inputs)))
;;                          (bins     (map (lambda (i) (string-append (cdr i) "/bin"))
;;                                         %build-inputs))
;;                          (libpath  (string-join (filter file-exists? libs) ":"))
;;                          (binpath  (string-join (filter file-exists? bins) ":")))
;;                     (setenv "PATH" (string-append gzip "/bin:" (getenv "PATH")))
;;                     (mkdir-p (string-append output "/opt/discord"))
;;                     (invoke (string-append tar "/bin/tar") "xpvf" source)
;;                     (copy-recursively "./Discord" (string-append output "/opt/discord"))
;;                     (mkdir-p (string-append output "/bin"))
;;                     (mkdir-p (string-append output "/share/pixmaps"))
;;                     (mkdir-p (string-append output "/etc"))
;;                     (with-output-to-file
;; 		      (string-append output "/etc/fonts.conf")
;; 		      (lambda _
;; 			(display "<?xml version='1.0'?>\n")
;; 			(display "<!DOCTYPE fontconfig SYSTEM 'fonts.dtd'>\n")
;; 			(sxml->xml `(fontconfig
;; 				      ,@(map (lambda (f) `(dir ,f)) fonts)))))
;;                     (chmod (string-append output "/opt/discord/Discord") #o755)
;;                     (with-output-to-file
;; 		      (string-append output "/bin/discord")
;; 		      (lambda _
;; 			(display "#!/bin/sh\n")
;; 			(display (string-append "export LD_LIBRARY_PATH=" libpath "\n"))
;; 			(display (string-append "export FONTCONFIG_FILE=" output "/etc/fonts.conf\n"))
;; 			(display (string-append "export PATH=" binpath ":$PATH\n"))
;; 			(display (string-append "exec -a discord " output "/opt/discord/Discord\n"))))
;;                     (chmod (string-append output "/bin/discord") #o755)
;;                     (invoke (string-append patchelf "/bin/patchelf")
;;                             "--set-interpreter"
;;                             (string-append glibc "/lib/ld-linux-x86-64.so.2")
;;                             (string-append output "/opt/discord/Discord"))
;;                     (link (string-append %output "/opt/discord/discord.png")
;;                           (string-append %output "/share/pixmaps/discord.png"))
;;                     #t))))
;;    (synopsis "Discord chat client.")
;;    (description "Discord chat client.")
;;    (license #f)
;;    (home-page "https://discordapp.com")))
