(define-module (zaijab home zjabbar)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu services)
  #:use-module (gnu packages)
  #:use-module (gnu packages vpn)
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
  #:use-module (gnu home services sound)
  #:use-module (gnu services shepherd))

(define-public zains-home
  (home-environment
   (packages (list
	      ;; strongswan
	      (specification->package "xmodmap")
	      (specification->package "unzip")
              (specification->package "xset")
              (specification->package "xinit")
              (specification->package "xorg-server")
              (specification->package "xf86-input-libinput")
              (specification->package "xf86-video-fbdev")
              (specification->package "xf86-video-nouveau")))
   (services
    (list
     (service home-bash-service-type
              (home-bash-configuration
               (aliases
                '(("ll"   . "ls -l")
                  ("mbsync" . "mbsync -c ~/.config/mbsyncrc")))
               (environment-variables
                '(("GNUPGHOME" . "~/.config/gnupg")
                  ("PASSWORD_STORE_SYSTEM_EXTENSION_DIR" . "/home/zjabbar/.guix-home/profile/lib/password-store/extensions")
                  ("PASSWORD_STORE_ENABLE_EXTENSIONS" . "true")
                  ("GIO_MODULE_DIR" . "/home/zjabbar/.guix-home/profile/lib/gio/modules/")
                  ("EDITOR" . "emacsclient -n")
		  ("DICPATH" . "~/.guix-home/profile/share/hunspell/")
                  ("PATH" . "$PATH:/home/zjabbar/.local/bin:/home/zjabbar/.nix-profile/bin")
                  ("GUIX_PROFILE" . "/home/zjabbar/.guix-profile")
                  ("TF_CPP_MIN_LOG_LEVEL" . "1")
		  ("SWANCTL_DIR" . "/home/zjabbar/.config/swanctl/")
                  ("DISABLE_RTKIT" . "1")
                  ("LD_LIBRARY_PATH" . "/run/current-system/profile/lib/")
                  ("GUILE_LOAD_PATH" . "$GUILE_LOAD_PATH:/home/zjabbar/.config/guix/current/share/guile/site/3.0/")))
               (bash-profile
                (list
                 (mixed-text-file "profile.sh"
                                  "export HOSTNAME\n"
                                  "eval \"$(direnv hook bash)\"\n"
				  "if [ -f ~/.nix-profile/etc/profile.d/nix.sh ]; then\n"
				  "source ~/.nix-profile/etc/profile.d/nix.sh\n"
				  "fi"
				  )))
	       (bashrc
		(list
		 (mixed-text-file "login.sh"
                                  "eval \"$(direnv hook bash)\"\n"
				  "if [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then \n"
				  "exec /home/zjabbar/code/guix-channel/zaijab/files/xinitrc.sh\n"
				  "fi\n"
				  "[ -n \"$EAT_SHELL_INTEGRATION_DIR\" ] && \\\n"
				  "source \"$EAT_SHELL_INTEGRATION_DIR/bash\"")))))
     (service home-emacs-service-type home-emacs-total-configuration)
     (service home-dbus-service-type)
     (service home-pipewire-service-type)
     (service home-gpg-agent-service-type
              (home-gpg-agent-configuration
               (pinentry-program
                (file-append pinentry-emacs "/bin/pinentry-emacs"))
               (ssh-support? #t)))

     (simple-service 'dotfiles
                     home-files-service-type
                     `(#;(".xsession" ,(program-file "xsession" (local-file "/home/zjabbar/code/guix-channel/zaijab/files/xsession")))
		       (".msmtprc" ,(local-file "/home/zjabbar/code/guix-channel/zaijab/files/msmtprc"))
                       (".config/mbsyncrc" ,(local-file "/home/zjabbar/code/guix-channel/zaijab/files/mbsyncrc"))
                       (".config/pycodestyle" ,(local-file "/home/zjabbar/code/guix-channel/zaijab/files/pycodestyle"))
                       (".config/mpv/mpv.conf" ,(local-file "/home/zjabbar/code/guix-channel/zaijab/files/mpv.conf"))
                       (".config/emacs/templates" ,(local-file "/home/zjabbar/code/guix-channel/zaijab/files/templates"))))))))

zains-home
