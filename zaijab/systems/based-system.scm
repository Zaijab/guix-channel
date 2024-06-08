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
  #:use-module (gnu home)
  #:use-module (gnu services guix)  
  #:use-module (gnu services pm)  
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu services linux)
  #:use-module (gnu packages search)
  #:use-module (gnu services base)
  #:use-module (gnu services file-sharing)
  #:use-module (gnu services admin)
  #:use-module (gnu services desktop)
  #:use-module (gnu services xorg)
  #:use-module (gnu services audio)
  #:use-module (gnu services sound)
  #:use-module (gnu services sddm)
  #:use-module (gnu services docker)
  #:use-module (gnu services dbus)
  #:use-module (gnu services databases) 
  #:use-module (gnu services virtualization)
  #:use-module (gnu services vpn)
  #:use-module (gnu services mail)
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
  #:use-module (gnu services virtualization)
  #:use-module (zaijab home zjabbar))

(define-public tao-operating-system
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
	       (specification->package "connman")
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
		  (supplementary-groups '("wheel" "netdev" "audio" "lp" "video" "docker")))
		 %base-user-accounts))
    
    (services (cons*
	       (service unattended-upgrade-service-type)
	       (service openssh-service-type)
	       (service syncthing-service-type (syncthing-configuration (user "zjabbar")))
	       (service guix-home-service-type `(("zjabbar" ,zains-home)))
	       (service connman-service-type)
	       (service tlp-service-type)
	       (service docker-service-type)
	       (service oci-container-service-type
			(list
			 (oci-container-configuration
			  (image "docker.io/library/caddy:2-alpine")
			  (network "host")
			  (volumes '("/var/run/Caddyfile:/etc/caddy/Caddyfile:ro"
				     "/var/run/caddy-data:/data:rw"
				     "/var/run/caddy-config:/config:rw"))
			  (environment '("SEARXNG_HOSTNAME=${SEARXNG_HOSTNAME:-http://localhost:80}"
					 "SEARXNG_TLS=${LETSENCRYPT_EMAIL:-internal}")))
			 (oci-container-configuration
			  (image "docker.io/valkey/valkey:7-alpine")
			  (network "host")
			  (volumes '("valkey-data2:/data"))
			  )
			 
			 (oci-container-configuration
			  (image "docker.io/searxng/searxng")
			  (network "host")
			  (ports '(("8888" . "8888")))
			  (volumes '("/tmp/searxng:/etc/searxng:rw"))
			  (environment '(("SEARXNG_BASE_URL" . "https://${SEARXNG_HOSTNAME:-localhost}/"))))
			 ))

	       (modify-services %desktop-services
		 (delete pulseaudio-service-type)
		 (delete gdm-service-type)
		 (delete network-manager-service-type)
		 (mingetty-service-type
		  config => (mingetty-configuration
			     (inherit config)
			     (auto-login "zjabbar")
			     (login-pause? #t)))
		 (guix-service-type
		  config => (guix-configuration
			     (inherit config)
			     (substitute-urls
			      (cons* "https://substitutes.nonguix.org"
				     "https://guix.bordeaux.inria.fr"
				     %default-substitute-urls))
			     (authorized-keys
			      (cons*
			       (plain-file "nonguix.pub" "(public-key (ecc (curve Ed25519) (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))")
			       (plain-file "bordeaux.pub" "(public-key (ecc (curve Ed25519) (q #89FBA276A976A8DE2A69774771A92C8C879E0F24614AAAAE23119608707B3F06#)))")
			       %default-authorized-guix-keys)))))
	       ))))


(define-public euler-operating-system
  (operating-system
    (inherit tao-operating-system)    
    (host-name "euler")
    (file-systems (cons*
		   (file-system
		     (mount-point "/boot/efi")
		     (device (uuid "00E0-82FE" 'fat32))
		     (type "vfat"))
		   (file-system
		     (mount-point "/")
		     (device
		      (uuid "c9e4a837-e67e-4a5e-8e53-54421e676f4b"
			    'btrfs))
		     (type "btrfs"))
		   %base-file-systems))))


(let ((hostname (read-delimited "\n" (open-input-pipe "echo $HOSTNAME"))))
  (cond ((string= hostname "tao") tao-operating-system)
	((string= hostname "euler") euler-operating-system)))
