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
 (gnu home services shells)
 (nongnu packages linux)
 (nongnu system linux-initrd)
 (rnrs lists))

(operating-system
 (kernel linux)
 (firmware (list linux-firmware))
 (initrd-modules (cons*	"vfio_pci"
			"vfio"
			"vfio_iommu_type1"
			"vfio_virqfd"
			"kvm"
			%base-initrd-modules))
 (kernel-arguments (cons* "intel_iommu=on"
			  ;; "iommu=pt"
			  "vfio_pci.ids=10de:1e89,10de:10f8,10de:1ad8,10de:1ad9"
			  "rd.driver.pre=vfio-pci"
			  "kvm.ignore_msrs=1"
			  ;; "modprobe.blacklist=nouveau,snd_hda_intel,xhci_hcd,nvidia-gpu"
			  %default-kernel-arguments))
 (host-name "tao")
 (timezone "Pacific/Honolulu")
 (locale "en_US.utf8")
 (keyboard-layout (keyboard-layout "us"))
 (sudoers-file
  (plain-file "sudoers"
	      (string-append (plain-file-content %sudoers-specification)
			     (format #f "~a ALL = NOPASSWD: ALL~%"
				     "zjabbar"))))
 (bootloader (bootloader-configuration
	      (bootloader grub-efi-bootloader)
	      (targets '("/boot/efi"))
	      (keyboard-layout keyboard-layout)))
 (file-systems (append
		(list (file-system
		       (device (file-system-label "root"))
		       (mount-point "/")
		       (type "btrfs"))
		      (file-system
		       (device (uuid "1C35-540A" 'fat))
		       (mount-point "/boot/efi")
		       (type "vfat")))
		%base-file-systems))
 (users (cons (user-account
	       (name "zjabbar")
	       (comment "Zain Jabbar")
	       (group "users")
	       (supplementary-groups '("wheel"
				       "netdev"
				       "audio"
				       "libvirt"
				       "kvm"
				       "video")))
	      %base-user-accounts))
 (packages (append (list
		    (specification->package "emacs")
		    (specification->package "emacs-exwm")
		    (specification->package "emacs-desktop-environment")
		    (specification->package "pulseaudio")
		    (specification->package "gvfs")
		    (specification->package "at-spi2-core")
		    (specification->package "font-fira-code")
		    (specification->package "font-adobe-source-han-sans")
		    (specification->package "qemu")
		    (specification->package "ovmf")
		    (specification->package "libvirt")
		    (specification->package "bridge-utils")
		    (specification->package "nss-certs"))
		   %base-packages))
 (services (cons*
	    ;; (simple-service 'etc-modprobe-service etc-service-type
	    ;; 		    `(("modprobe.d/vfio.conf"
	    ;; 		       ,(plain-file "vfio.conf"
	    ;; 				    "softdep nouveau pre: vfio-pci\nsoftdep snd_hda_intel pre: vfio-pci\nsoftdep xhci_hcd pre: vfio-pci\nsoftdep nvidia-gpu pre: vfio-pci\noptions vfio-pci ids=10de:1e89,10de:10f8,10de:1ad8,10de:1ad9"))))
	    ;; (simple-service 'etc-modprobe-service etc-service-type
	    ;; 		    `(("modprobe.d/blacklist.conf"
	    ;; 		       ,(plain-file "blacklist.conf"
	    ;; 				    "blacklist nouveau\nblacklist snd_hda_intel\nblacklist xhci_hcd\nblacklist nvidia-gpu\n"))))
	    ;; (simple-service 'etc-initramfs-tools-service etc-service-type
	    ;; 		    `(("initramfs-tools/modules"
	    ;; 		       ,(plain-file "modules"
	    ;; 				    "vfio\nvfio_iommu_type1\nvfio_pci ids=10de:1e89,10de:10f8,10de:1ad8,10de:1ad9\nvfio_virqfd\nvhost-net"))))
	    ;; (simple-service 'etc-modules=load-service etc-service-type
	    ;; 		    `(("modules-load.d/mhwd-gpu.conf"
	    ;; 		       ,(plain-file "mhwd-gpu.conf"
	    ;; 				    "vfio-pci"))))
	    (service kernel-module-loader-service-type
		     '("vfio-pci" "kvm"))
	    (service sddm-service-type
		     (sddm-configuration
		      (auto-login-user "zjabbar")
		      (auto-login-session "exwm.desktop")
		      (minimum-uid 1000)
		      (theme "darkine")))
	    (service libvirt-service-type)
	    (service virtlog-service-type)
	    (service elogind-service-type)
	    (service network-manager-service-type)
	    (service modem-manager-service-type)
	    (service wpa-supplicant-service-type)
	    (service ntp-service-type)
	    %base-services))
 (name-service-switch %mdns-host-lookup-nss))
(cons*
 (channel
  (name 'zaijab)
  (url "https://github.com/zaijab/guix-channel.git"))
 ;; (channel
 ;;  (name 'rc)
 ;;  (url "https://github.com/bqv/rc.git")
 ;;  (branch "live")
 ;;  (commit "ce711fcc3bcebbaa108ae8816c262d53008d44fb"))
 (channel
  (name 'rde)
  (url "https://git.sr.ht/~abcdw/rde")
  (introduction
   (make-channel-introduction
    "257cebd587b66e4d865b3537a9a88cccd7107c95"
    (openpgp-fingerprint
     "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
 (channel
  (name 'nonguix)
  (url "https://gitlab.com/nonguix/nonguix"))
 %default-channels)


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
