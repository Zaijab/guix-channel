(define-module (zaijab systems euler)
  #:use-module (zaijab systems based-system))

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

euler-operating-system
