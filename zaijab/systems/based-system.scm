(define-module (zaijab systems based-system)
  #:use-module (gnu)
  #:use-module (srfi srfi-1)
  #:use-module (guix gexp)
  #:use-module (gnu)
  #:use-module (guix)
  #:use-module (gnu services base)
  #:use-module (gnu services admin)
  #:use-module (gnu services desktop)
  #:use-module (gnu services xorg)
  #:use-module (gnu services audio)
  #:use-module (gnu services sound)
  #:use-module (gnu services sddm)
  #:use-module (gnu services dbus)
  #:use-module (gnu packages databases) 
  #:use-module (gnu services databases) 
  #:use-module (gnu services virtualization)
  #:use-module (gnu services vpn)
  #:use-module (gnu services mail)
  #:use-module (gnu services nix)
  #:use-module (gnu services networking)
  #:use-module (gnu services syncthing)
  #:use-module (gnu services ssh)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages vpn)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages linux)
  #:use-module (nongnu packages linux)
  #:use-module (ice-9 textual-ports)
  #:use-module (gnu)
  #:use-module (srfi srfi-1)
  #:use-module (guix gexp)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services xorg)
  #:use-module (gnu services audio)
  #:use-module (gnu services sddm)
  #:use-module (gnu services dbus) 
  #:use-module (gnu services vpn) 
  #:use-module (gnu services virtualization))

(define-public based-operating-system
  (operating-system
   (kernel linux)
   (firmware (list linux-firmware))
    (locale "en_US.utf8")
    (timezone "Pacific/Honolulu")
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
    
    (packages (cons*
	       emacs-exwm
	       nss-certs
	       pulseaudio
	       network-manager-applet
	       gsettings-desktop-schemas
	       %base-packages))
    
    (host-name "tao")
    (file-systems (cons*
		   (file-system
		     (device (file-system-label "root"))
		     (mount-point "/")
		     (type "btrfs"))
		   (file-system
		     (device (uuid "1C35-540A" 'fat))
		     (mount-point "/boot/efi")
		     (type "vfat"))
		   (file-system
		     (mount-point "/tmp")
		     (device "none")
		     (type "tmpfs")
		     (check? #f))
		   %base-file-systems))
    (users (cons (user-account
		  (name "zjabbar")
		  (comment "Zain Jabbar")
		  (group "users")
		  (supplementary-groups '("wheel"
					  "netdev"
					  "audio"
					  "mpd"
					  "lp"
					  "video")))
		 %base-user-accounts))
    
    (services (cons*
	       (service sddm-service-type
			(sddm-configuration
			 (minimum-uid 1000)
			 (auto-login-session "exwm.desktop")
			 (theme "darkine")
			 (auto-login-user "zjabbar")))
	       (service openssh-service-type)
	       (service mpd-service-type
			(mpd-configuration
			 (user "zjabbar")
			 (music-dir "~/music")
			 (playlist-dir "~/.config/mpd/playlists")
			 (db-file "~/.config/mpd/database")
			 (state-file "~/.config/mpd/state")
			 (sticker-file "~/.config/mpd/sticker.sql")
			 (outputs
			  (list (mpd-output
				 (type "pulse"))))))
	       (service syncthing-service-type
			(syncthing-configuration (user "zjabbar")))
	       (service unattended-upgrade-service-type)
	       (bluetooth-service)
	       
	       (remove (lambda (service) (eq? (service-kind service) gdm-service-type))
		       (modify-services %desktop-services
			 (network-manager-service-type
			  config => (network-manager-configuration
				     (inherit config)
				     (vpn-plugins
				      (list network-manager-openvpn
					    network-manager-openconnect))))))))))

(define-public tao-operating-system
  based-operating-system)

(define-public euler-operating-system
  (operating-system
    (inherit based-operating-system)
    (host-name "euler")
    (file-systems (cons*
		   (file-system
		     (mount-point "/boot/efi")
		     (device (uuid "A2B7-72DF" 'fat32))
		     (type "vfat"))
		   (file-system
		     (mount-point "/")
		     (device
		      (uuid "08173b91-2416-4b98-a5e1-59281ae236a2"
			    'btrfs))
		     (type "btrfs"))
		   %base-file-systems))))


tao-operating-system
