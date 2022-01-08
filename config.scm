(use-modules (gnu) (gnu system nss))
(use-service-modules desktop xorg)
(use-package-modules certs gnome)

(use-modules (gnu)
	     (gnu services)
	     (gnu services desktop)
	     (gnu services sddm)
	     (gnu services xorg)
	     (gnu services ssh)
	     (gnu services virtualization)
	     (gnu services linux)
	     (gnu packages linux)
	     (gnu packages firmware)
	     (nongnu packages linux)
	     (nongnu system linux-initrd)
	     (rnrs lists))

(use-package-modules emacs wm virtualization)
(use-service-modules desktop networking)

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
		     '("vfio-pci"))
	    (service sddm-service-type
		     (sddm-configuration
		      (auto-login-user "zjabbar")
		      (auto-login-session "exwm.desktop")
		      (minimum-uid 1000)
		      (theme "darkine")))
	    (service libvirt-service-type)
	    (service elogind-service-type)
	    (service network-manager-service-type)
	    (service modem-manager-service-type)
	    (service wpa-supplicant-service-type)
	    (service ntp-service-type)
	    %base-services))
 (name-service-switch %mdns-host-lookup-nss))
