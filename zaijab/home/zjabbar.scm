;; -*- compile-command: "make -k zjabbar" -*-
(define-module (zaijab home zjabbar)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu services)
  #:use-module (gnu packages)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-9)
  #:use-module (zaijab services emacs))

(define-public zains-home
  (home-environment
   (packages (list (specification->package "xmodmap")
                   (specification->package "xset")
                   (specification->package "font-iosevka")))
   (services
    (list
     (service home-bash-service-type
              (home-bash-configuration
               (aliases
                '(("grep" . "--color auto")
                  ("ll"   . "ls -l")
                  ("ls"   . "-p --color auto")
                  ("mbsync" . "mbsync -c ~/.config/mbsyncrc")))
               (environment-variables
                '(("GNUPGHOME" . "~/.config/gnupg")
                  ("PASSWORD_STORE_ENABLE_EXTENSIONS" . "true")
                  ("GIO_MODULE_DIR" . "/home/zjabbar/.guix-profile/lib/gio/modules/")
                  ("PATH" . "$PATH:/home/zjabbar/.local/bin")
                  ("GUIX_PROFILE" . "/home/zjabbar/.guix-profile")
                  ("PASSWORD_STORE_SYSTEM_EXTENSION_DIR" . "/home/zjabbar/.local/lib/python3.9/site-packages/")
                  ("GUILE_LOAD_PATH" . "$GUILE_LOAD_PATH:/home/zjabbar/.config/guix/current/share/guile/site/3.0/:/home/zjabbar/.guix-profile/share/guile/site/3.0/:/home/zjabbar/.guix-home/profile/share/guile/site/3.0/:/home/zjabbar/code/guix-channel/")))
               (bash-profile
                (list
                 (mixed-text-file "profile.sh"
                                  "xmodmap -e 'clear Lock' -e 'keycode 0x42 = Escape'\n"
                                  "unset SSH_AGENT_PID\nif [ \"${gnupg_SSH_AUTH_SOCK_by:-0}\" -ne $$ ];"
                                  " then\nexport SSH_AUTH_SOCK=\"$(gpgconf --list-dirs agent-ssh-socket)\"\nfi\n"
                                  "export GPG_TTY=$(tty)\ngpg-connect-agent updatestartuptty /bye >/dev/null\n"
                                  "export HOSTNAME\n"
                                  "eval \"$(direnv hook bash)\"\n"
				  "source /run/current-system/profile/etc/profile.d/nix.sh")))
	       (bashrc
		(list
		 (mixed-text-file "login.sh"
                                  "eval \"$(direnv hook bash)\"\n"
				  )))))
     (service home-emacs-service-type home-emacs-total-configuration)
     (simple-service 'dotfiles
                   home-files-service-type
                   `((".msmtprc" ,(local-file "/home/zjabbar/code/guix-channel/zaijab/files/msmtprc"))
                     (".config/mbsyncrc" ,(local-file "/home/zjabbar/code/guix-channel/zaijab/files/mbsyncrc"))
                     ;; (".xsession" ,(local-file "files/xsession")) Cannot use this currently. .xsession needs to be executable.
                     (".config/mpv/mpv.conf" ,(local-file "/home/zjabbar/code/guix-channel/zaijab/files/mpv.conf"))
                     (".config/emacs/templates" ,(local-file "/home/zjabbar/code/guix-channel/zaijab/files/templates"))))))))

zains-home
