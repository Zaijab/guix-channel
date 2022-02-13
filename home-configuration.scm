(use-modules
 (gnu)
 (gnu home)
 (gnu packages linux)
 (gnu packages firmware)
 (gnu packages certs)
 (gnu packages gnome)
 (gnu packages emacs)
 (gnu packages wm)
 (gnu packages virtualization)
 (gnu services)
 (gnu services desktop)
 (gnu services sddm)
 (gnu services xorg)
 (gnu services ssh)
 (gnu services virtualization)
 (gnu services linux)
 (gnu services networking)
 (gnu system nss)
 (guix channels)
 (guix gexp)
 (guix transformations)
 (guix profiles)
 (gnu home services shells)
 (gnu home-services emacs)
 (gnu home-services-utils)
 (rnrs lists))

(home-environment
 (services
  (list (service
         home-bash-service-type
         (home-bash-configuration
          (aliases
           '(("grep" . "--color auto")
             ("gu"   . "git add *;git commit -m bash;git push -u origin master")
             ("ll"   . "ls -l")
             ("ls"   . "-p --color auto")
             ("u"    . "guix pull;guix upgrade;sudo guix system reconfigure ~/.config/guix/operating-system.scm;guix home reconfigure ~/.config/guix/home-configuration.scm")
             ("ghome"    . "guix home reconfigure ~/.config/guix/home-configuration.scm")
             ("gsystem"    . "sudo guix system reconfigure ~/.config/guix/operating-system.scm")
             ("gpull"    . "guix pull")
             ("gpackage"    . "guix package -m ~/.config/guix/manifest.scm")
             ("gupgrade"    . "guix upgrade")))
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
	  (rebuild-elisp-packages? #t)
	  (early-init-el `(,(slurp-file-gexp (local-file "files/early-init.el"))))
	  (init-el `(,(slurp-file-gexp (local-file "files/init.el")))))))))
