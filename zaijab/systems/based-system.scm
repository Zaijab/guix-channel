(define-module (zaijab systems based-system)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 textual-ports)
  #:use-module (guix)
  #:use-module (guix gexp)
  #:use-module (guix transformations)
  #:use-module (nongnu packages linux)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages search)
  #:use-module (gnu services base)
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
	       ;; (specification->package "emacs-next")
	       ;; ((options->transformation
	       ;; 	 '((with-input . "emacs=emacs-next")
	       ;; 	   (with-git-url . "emacs-exwm=https://github.com/ch11ng/exwm.git")))
	       ;; 	(specification->package "emacs-exwm"))
	       (specification->package "nss-certs")
	       (specification->package "pulseaudio")
	       (specification->package "network-manager-applet")
	       (specification->package "texmacs")
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
	       (service nix-service-type)
	       (simple-service 'searx-start-job mcron-service-type
			       (list
				#~(job '(next-minute (range 0 60 1))
				       (string-append "pgrep searx || " #$(file-append searx "/bin/searx-run & disown")))))
	       (extra-special-file "/etc/searx/settings.yml"
				   (plain-file "settings.yml" (string-append "
                                                           use_default_settings: True
                                                           general:
                                                               instance_name : \"searx\" # displayed name
                                                           server:
                                                               bind_address : \"0.0.0.0\"      # address to listen on
                                                               secret_key : \""
									     (transmission-random-salt)
									     (transmission-random-salt)
									     (transmission-random-salt)
									     (transmission-random-salt) "\"")))
	       (pam-limits-service
		(list
		 (pam-limits-entry "@wheel" 'hard 'nofile 524288)))
	       (modify-services %desktop-services
		 (network-manager-service-type
		  config => (network-manager-configuration
			     (inherit config)
			     (vpn-plugins
			      (list
			       (specification->package "network-manager-openvpn")
			       (specification->package "network-manager-openconnect")
			       #;(specification->package "openconnect-sso"))))))))))

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
