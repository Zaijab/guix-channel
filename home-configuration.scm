(use-modules
 (gnu home)
 (gnu packages)
 (gnu packages emacs)
 (gnu services)
 (gnu home-services emacs)
 (gnu home-services-utils)
 (guix gexp)
 (guix transformations)
 (gnu home services shells))

(define transform1
  (options->transformation
    '((with-branch . "emacs-next=master"))))

(home-environment
 (packages
  (list (transform1
              (specification->package "emacs-next"))
             (specification->package "emacs-org-roam")
             (specification->package "emacs-guix")
             (specification->package "youtube-dl")
             (specification->package "nyxt")
             (specification->package "emacs-lispyville")
             (specification->package "emacs-lispy")
             (specification->package "font-fira-code")
             (specification->package "emacs-doom-modeline")
             (specification->package "emacs-origami-el")
             (specification->package "emacs-doom-themes")
             (specification->package "emacs-exwm")
             (specification->package "emacs-vterm")
             (specification->package "sbcl-iterate")
             (specification->package "pavucontrol")
             (specification->package "emacs-pdf-tools")
             (specification->package "emacs-no-littering")
             (specification->package "curl")
             (specification->package "emacs-ytel")
             (specification->package "emacs-org-drill")
             (specification->package "xhost")
             (specification->package "xauth")
             (specification->package "emacs-engine-mode")
             (specification->package "s6-rc")
             (specification->package "s6")
             (specification->package "lshw")
             (specification->package "sbcl-slynk")
             (specification->package "git")
             (specification->package "font-lohit")
             (specification->package "sbcl")
             (specification->package "emacs-sly")
             (specification->package "font-ipa-mj-mincho")
             (specification->package "emacs-ddskk")
             (specification->package "emacs-valign")
             (specification->package "emacs-which-key")
             (specification->package "emacs-general")
             (specification->package "emacs-popup")
             (specification->package "emacs-undo-tree")
             (specification->package "emacs-helm")
             (specification->package "emacs-yasnippet")
             (specification->package
              "emacs-rainbow-delimiters")
             (specification->package "emacs-polymode-org")
             (specification->package "emacs-google-translate")
             (specification->package "emacs-company")
             (specification->package "emacs-evil")
             (specification->package "emacs-projectile")
             (specification->package "emacs-perspective")
             (specification->package "fontconfig")
             (specification->package "emacs-system-packages")
             (specification->package "emacs-leaf-keywords")
             (specification->package
              "emacs-helm-system-packages")
             (specification->package "emacs-leaf")
             (specification->package "libvirt")))
 
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
	  (package (transform1 (specification->package "emacs-next")))
	  (rebuild-elisp-packages? #t)
	  (init-el
	   `(,(slurp-file-gexp (local-file "files/init.el")))))))))

(home-environment
 (packages
  (list 
   (specification->package "emacs-next")
             (specification->package "emacs-org-roam")
             (specification->package "emacs-guix")
             (specification->package "youtube-dl")
             (specification->package "nyxt")
             (specification->package "emacs-lispyville")
             (specification->package "emacs-lispy")
             (specification->package "font-fira-code")
             (specification->package "emacs-doom-modeline")
             (specification->package "emacs-origami-el")
             (specification->package "emacs-doom-themes")
             (specification->package "emacs-exwm")
             (specification->package "emacs-vterm")
             (specification->package "sbcl-iterate")
             (specification->package "pavucontrol")
             (specification->package "emacs-pdf-tools")
             (specification->package "emacs-no-littering")
             (specification->package "curl")
             (specification->package "emacs-ytel")
             (specification->package "emacs-org-drill")
             (specification->package "xhost")
             (specification->package "xauth")
             (specification->package "emacs-engine-mode")
             (specification->package "s6-rc")
             (specification->package "s6")
             (specification->package "lshw")
             (specification->package "sbcl-slynk")
             (specification->package "git")
             (specification->package "font-lohit")
             (specification->package "sbcl")
             (specification->package "emacs-sly")
             (specification->package "font-ipa-mj-mincho")
             (specification->package "emacs-ddskk")
             (specification->package "emacs-valign")
             (specification->package "emacs-which-key")
             (specification->package "emacs-general")
             (specification->package "emacs-popup")
             (specification->package "emacs-undo-tree")
             (specification->package "emacs-helm")
             (specification->package "emacs-yasnippet")
             (specification->package
              "emacs-rainbow-delimiters")
             (specification->package "emacs-polymode-org")
             (specification->package "emacs-google-translate")
             (specification->package "emacs-company")
             (specification->package "emacs-evil")
             (specification->package "emacs-projectile")
             (specification->package "emacs-perspective")
             (specification->package "fontconfig")
             (specification->package "emacs-system-packages")
             (specification->package "emacs-leaf-keywords")
             (specification->package
              "emacs-helm-system-packages")
             (specification->package "emacs-leaf")
             (specification->package "libvirt")))
 
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
	  (package (transform1 (specification->package "emacs-next")))
	  (rebuild-elisp-packages? #t)
	  (init-el
	   `(,(slurp-file-gexp (local-file "files/init.el")))))))))
