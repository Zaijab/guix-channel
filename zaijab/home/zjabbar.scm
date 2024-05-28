(define-module (zaijab home zjabbar)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu services)
  #:use-module (gnu packages)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services desktop)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-9)
  #:use-module (nongnu packages chrome)
  #:use-module (nongnu packages messaging)
  #:use-module (zaijab services emacs)
  #:use-module (zaijab services searx)
  #:use-module (gnu home services sound)
  )

(define-public zains-home
  (home-environment
   (packages (list (specification->package "xmodmap")
		   (specification->package "unzip")
                   (specification->package "xset")
                   (specification->package "xinit")
                   (specification->package "xorg-server")
                   (specification->package "xf86-input-libinput")
                   (specification->package "xf86-video-fbdev")
                   (specification->package "xf86-video-nouveau")
		   zoom
		   google-chrome-unstable))
   (services
    (list
     (service home-bash-service-type
              (home-bash-configuration
               (aliases
                '(("ll"   . "ls -l")
                  ("mbsync" . "mbsync -c ~/.config/mbsyncrc")))
               (environment-variables
                '(("GNUPGHOME" . "~/.config/gnupg")
                  ("PASSWORD_STORE_SYSTEM_EXTENSION_DIR" . "/home/zjabbar/.guix-profile/lib/password-store/extensions/")
                  ("PASSWORD_STORE_ENABLE_EXTENSIONS" . "true")
                  ("GIO_MODULE_DIR" . "/home/zjabbar/.guix-home/profile/lib/gio/modules/")
                  ("EDITOR" . "emacsclient")
                  ("PATH" . "$PATH:/home/zjabbar/.local/bin")
                  ("GUIX_PROFILE" . "/home/zjabbar/.guix-profile")
                  ("TF_CPP_MIN_LOG_LEVEL" . "3")
                  ("DISABLE_RTKIT" . "1")
                  ("GUILE_LOAD_PATH" . "$GUILE_LOAD_PATH:/home/zjabbar/.config/guix/current/share/guile/site/3.0/:/home/zjabbar/.guix-profile/share/guile/site/3.0/:/home/zjabbar/.guix-home/profile/share/guile/site/3.0/:/home/zjabbar/code/guix-channel/")))
               (bash-profile
                (list
                 (mixed-text-file "profile.sh"
                                  "export HOSTNAME\n"
                                  "eval \"$(direnv hook bash)\"\n")))
	       (bashrc
		(list
		 (mixed-text-file "login.sh"
                                  "eval \"$(direnv hook bash)\"\n"
				  "if [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then \n"
				  "exec /home/zjabbar/code/guix-channel/zaijab/files/xinitrc.sh\n"
				  "fi\n")))))
     (service home-emacs-service-type home-emacs-total-configuration)
     ;(service home-searx-service-type)
     (service home-pipewire-service-type)
     (service home-dbus-service-type)
     (service home-gpg-agent-service-type
              (home-gpg-agent-configuration
               (pinentry-program
                (file-append pinentry-emacs "/bin/pinentry-emacs"))
               (ssh-support? #t)))
     (simple-service 'dotfiles
                     home-files-service-type
                     `((".msmtprc" ,(local-file "/home/zjabbar/code/guix-channel/zaijab/files/msmtprc"))
                       (".config/mbsyncrc" ,(local-file "/home/zjabbar/code/guix-channel/zaijab/files/mbsyncrc"))
                       (".config/pycodestyle" ,(local-file "/home/zjabbar/code/guix-channel/zaijab/files/pycodestyle"))
                       (".config/mpv/mpv.conf" ,(local-file "/home/zjabbar/code/guix-channel/zaijab/files/mpv.conf"))
                       (".config/emacs/templates" ,(local-file "/home/zjabbar/code/guix-channel/zaijab/files/templates"))))))))

zains-home
