(define-module (zaijab systems based-system)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 popen)
  #:use-module (guix)
  #:use-module (guix gexp)
  #:use-module (guix transformations)
  #:use-module (nongnu packages linux)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages search)
  #:use-module (gnu services base)
  #:use-module (gnu services file-sharing)
  #:use-module (gnu services admin)
  #:use-module (gnu services desktop)
  #:use-module (gnu services xorg)
  #:use-module (gnu services audio)
  #:use-module (gnu services sound)
  #:use-module (gnu services sddm)
  #:use-module (gnu services dbus)
  #:use-module (gnu services databases) 
  #:use-module (gnu services virtualization)
  #:use-module (gnu services vpn)
  #:use-module (gnu services mail)
  #:use-module (gnu services nix)
  #:use-module (gnu services networking)
  #:use-module (gnu services syncthing)
  #:use-module (gnu services ssh)
  #:use-module (gnu packages networking)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services xorg)
  #:use-module (gnu services audio)
  #:use-module (gnu services sddm)
  #:use-module (gnu services dbus) 
  #:use-module (gnu services vpn) 
  #:use-module (gnu services mcron) 
  #:use-module (gnu services virtualization))

(define-public tao-operating-system
  (operating-system
    (kernel linux@6.1.9)
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
	       (specification->package "nss-certs")
	       (specification->package "pulseaudio")
	       (specification->package "network-manager-applet")
	       (specification->package "nix")
	       (specification->package "searx") 
	       (specification->package "gsettings-desktop-schemas")
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
		  (supplementary-groups '("wheel" "netdev" "audio" "mpd" "lp" "video")))
		 %base-user-accounts))
    
    (services (cons*
	       (service openssh-service-type)
	       (service mpd-service-type
			(mpd-configuration
			 (user "mpd")
			 (music-directory "~/music")
			 (playlist-directory "~/.config/mpd/playlists")
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
	       (service nix-service-type)
	       (extra-special-file "/etc/searx/settings.yml"
				   (plain-file "settings.yml" (string-append "use_default_settings: True\n"
									     "general:\n"
									     "  instance_name : \"searx\" # displayed name\n"
									     "server:\n"
									     "  bind_address : \"0.0.0.0\"\n"
									     "  secret_key : \""
									     (transmission-random-salt)
									     (transmission-random-salt)
									     (transmission-random-salt)
									     (transmission-random-salt)
									     "\"")))
	       (pam-limits-service
		(list
		 (pam-limits-entry "@wheel" 'hard 'nofile 524288)))
	       (modify-services %desktop-services
		 (gdm-service-type
		  config => (gdm-configuration
			     (inherit config)
			     (auto-login? #t)
			     (default-user "zjabbar")))
		 (network-manager-service-type
		  config => (network-manager-configuration
			     (inherit config)
			     (vpn-plugins
			      (list
			       (specification->package "network-manager-openvpn")
			       (specification->package "network-manager-openconnect")
			       (specification->package "openconnect-sso")))))
		 (guix-service-type
		  config => (guix-configuration
			     (inherit config)
			     (substitute-urls
			      (append (list "https://substitutes.nonguix.org")
				      %default-substitute-urls))
			     (authorized-keys
			      (append (list (local-file "./signing-key.pub"))
				      %default-authorized-guix-keys)))))))))


(define-public euler-operating-system
  (operating-system
    (inherit tao-operating-system)
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


(let ((hostname (read-delimited "\n" (open-input-pipe "echo $HOSTNAME"))))
  (cond ((string= hostname "tao") tao-operating-system)
	((string= hostname "euler") euler-operating-system)))
