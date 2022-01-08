;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules
 (gnu home)
 (gnu packages)
 (gnu packages emacs)
 (gnu services)
 (gnu home-services emacs)
 (gnu home-services-utils)
 (guix gexp)
 (gnu home services shells))

(home-environment
  (packages
    (map specification->package
         (list "emacs-origami-el"
               "mpv"
               "emacs-org-roam"
	       "emacs-doom-modeline"
               "emacs-pdf-tools"
               "emacs-no-littering"
               "curl"
               "emacs-sly-quicklisp"
               "emacs-guix"
               "gst-plugins-bad"
               "jami-qt"
               "gst-plugins-good"
               "gst-libav"
               "youtube-dl"
               "emacs-doom-modeline"
               "emacs-ytel"
               "nyxt"
               "emacs-org-drill"
               "xhost"
               "xauth"
               "compton"
               "emacs-next"
               "emacs-engine-mode"
               "s6-rc"
               "s6"
               "lshw"
               "polybar"
               "emacs-lispyville"
               "emacs-lispy"
               "emacs-doom-themes"
               "emacs-exwm"
               "emacs-next"
               "sbcl-slynk"
               "gst-plugins-ugly"
               "gst-plugins-base"
               "git"
               "font-lohit"
               "sbcl"
               "emacs-sly"
               "font-ipa-mj-mincho"
               "emacs-ddskk"
               "emacs-valign"
               "emacs-which-key"
               "emacs-general"
               "emacs-popup"
               "emacs-undo-tree"
               "emacs-helm"
               "emacs-yasnippet"
               "emacs-rainbow-delimiters"
               "emacs-vterm"
               "emacs-polymode-org"
               "emacs-google-translate"
               "emacs-company"
               "emacs-evil"
               "emacs-projectile"
               "emacs-perspective"
               "font-fira-code"
               "fontconfig"
               "emacs-system-packages"
               "emacs-leaf-keywords"
               "emacs-helm-system-packages"
               "emacs-leaf")))
  (services
    (list (service
           home-bash-service-type
           (home-bash-configuration
            (aliases
             '(("grep" . "--color auto")
               ("gu"   . "git add *;git commit -m bash;git push -u origin master")
               ("ll"   . "ls -l")
               ("ls"   . "-p --color auto")
               ("u"    . "guix pull;guix upgrade;sudo guix system reconfigure ~/.config/guix/config.scm")))
            (bashrc
             (list (local-file
                    "/home/zjabbar/.config/guix/.bashrc"
                    "bashrc")))
            (bash-profile
             (list (local-file
                    "/home/zjabbar/.config/guix/.bash_profile"
                    "bash_profile")))))
	  (service
	   home-emacs-service-type
	   (home-emacs-configuration
	    (package emacs-next)
	    (rebuild-elisp-packages? #t)
	    (init-el
	     `(,(slurp-file-gexp (local-file "files/init.el")))))))))
