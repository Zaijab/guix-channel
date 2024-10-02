(define-module (zaijab systems based-system)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 popen)
  #:use-module (guix)
  #:use-module (guix gexp)
  #:use-module (guix transformations)
  #:use-module (guix channels)
  #:use-module (nongnu packages linux)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu home)
  #:use-module (gnu services guix)
  #:use-module (gnu packages cups)
  #:use-module (gnu services cups)
  #:use-module (gnu services pm)  
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu services linux)
  #:use-module (gnu packages search)
  #:use-module (gnu services base)
  #:use-module (gnu packages base)
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

(define-public main-services
  (cons*
   (service unattended-upgrade-service-type)
   (service openssh-service-type)
   (service syncthing-service-type (syncthing-configuration (user "zjabbar")))
   (service guix-home-service-type `(("zjabbar" ,zains-home)))
   (service tlp-service-type)
   (service opendht-service-type)
   (service docker-service-type)
   (service containerd-service-type)
   (service oci-container-service-type
	    (list
	     ;; (oci-container-configuration
	     ;;  (image "jami-daemon")
	     ;;  (volumes '("/var/run/client/src:/web-client/client/src"
	     ;; 		 "/var/run/server/src:/web-client/server/src"
	     ;; 		 "/var/run/client/.env.development:/web-client/client/.env.development"
	     ;; 		 "/var/run/server/.env:/web-client/server/.env"))
	     ;;  (network "host")
	     ;;  (ports '(("3000" . "3000")
	     ;; 	       ("5000" . "5000"))))
	     (oci-container-configuration
	      (image "docker.io/library/caddy:2-alpine")
	      (network "host")
	      (volumes '("/home/zjabbar/code/guix-channel/zaijab/files/Caddyfile:/etc/caddy/Caddyfile:ro"
			 "/var/run/caddy-data:/data:rw"
			 "/var/run/caddy-config:/config:rw"))
	      (environment '("LETSENCRYPT_EMAIL=zaijab2000@gmail.com"
			     "SEARXNG_HOSTNAME=http://localhost:8080"
			     "SEARXNG_TLS=zaijab2000@gmail.com"))
	      (respawn? #t))
	     (oci-container-configuration
	      (image "docker.io/valkey/valkey:7-alpine")
	      (network "host")
	      (volumes '("/var/run/valkey-data2:/data"))
	      (respawn? #t))
	     (oci-container-configuration
	      (image "docker.io/searxng/searxng")
	      (network "host")
	      (ports '(("8080" . "8080")))
	      (volumes '("/var/run/searxng:/etc/searxng:rw"
			 "/home/zjabbar/code/guix-channel/zaijab/files/limiter.toml:/etc/searxng/limiter.toml"
			 "/home/zjabbar/code/guix-channel/zaijab/files/settings.yml:/etc/searxng/settings.yml"))
	      (environment '(("SEARXNG_BASE_URL" . "http://localhost:8080")))
	      (respawn? #t))))
(service cups-service-type
         (cups-configuration
           (web-interface? #t)
           (extensions
             (list cups-filters epson-inkjet-printer-escpr hplip-minimal hplip))))

   (modify-services %desktop-services
     (delete pulseaudio-service-type)
     (delete gdm-service-type)
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
		  (cons* (plain-file "nonguix.pub" "(public-key (ecc (curve Ed25519) (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))")
			 (plain-file "bordeaux.pub" "(public-key (ecc (curve Ed25519) (q #89FBA276A976A8DE2A69774771A92C8C879E0F24614AAAAE23119608707B3F06#)))")
			 %default-authorized-guix-keys)))))))

(define-public tao-operating-system
  (operating-system
    (kernel linux)
    (kernel-arguments (cons* "module_blacklist=pcspkr,snd_pcsp" %default-kernel-arguments))
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
	       (specification->package "git")
	       (specification->package "scrot")
	       (specification->package "xauth")
	       gnu-make
	       cups
	       (specification->package "gsettings-desktop-schemas")
	       %base-packages))
    
    (host-name "tao")
    (file-systems (cons*
		   (file-system
		     (device (file-system-label "root"))
		     (mount-point "/")
		     (type "btrfs"))
		   (file-system
		     (device (file-system-label "boot"))
		     (mount-point "/boot/efi")
		     (type "vfat"))
		   %base-file-systems))
    
    (users (cons (user-account
		  (name "zjabbar")
		  (comment "Zain Jabbar")
		  (group "users")
		  (supplementary-groups '("wheel" "netdev" "audio" "lp" "lpadmin" "video" "docker")))
		 %base-user-accounts))
    
    (services main-services)))


(define-public euler-operating-system
  (operating-system
    (inherit tao-operating-system)
    (host-name "euler")))

(define-public my-operating-system
  (let ((hostname (read-delimited "\n" (open-input-pipe "echo $HOSTNAME"))))
    (cond ((string= hostname "tao") tao-operating-system)
	  ((string= hostname "euler") euler-operating-system))))

euler-operating-system
