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
 (gnu services nix)
 (gnu system nss)
 (guix channels)
 (guix gexp)
 (guix transformations)
 (guix profiles)
 (gnu home services shells)
 (nongnu packages linux)
 (nongnu system linux-initrd)
 (gnu home-services emacs)
 (gnu home-services-utils)
 (rnrs lists))

(define %my-services
  (modify-services %base-services
    (mingetty-service-type config =>
                           (mingetty-configuration
                            (inherit config)
                            (auto-login "zjabbar")))))

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
				       "video")))
	      %base-user-accounts))
 (packages (append (list
		    (specification->package "emacs-desktop-environment")
		    (specification->package "emacs-exwm")
		    (specification->package "pulseaudio")
		    (specification->package "gvfs")
		    (specification->package "at-spi2-core")
		    (specification->package "font-fira-code")
		    (specification->package "font-adobe-source-han-sans")
		    
		    (specification->package "qemu")
		    (specification->package "ovmf")
		    (specification->package "libvirt")
		    (specification->package "bridge-utils")
		    (specification->package "virt-manager")

		    (specification->package "nix")
		    
		    (specification->package "nss-certs"))
		   %base-packages))
 (services (cons*
	    (service elogind-service-type)
	    (service network-manager-service-type)
	    (service modem-manager-service-type)
	    (service wpa-supplicant-service-type)
	    (service ntp-service-type)
	    (service nix-service-type)
	    %my-services))
 (name-service-switch %mdns-host-lookup-nss))
