(define-module (zaijab channel)
  #:use-module (guix ci)
  #:use-module (guix channels))

(define-public master-zaijab
  (list
   (channel
    (name 'zaijab)
    (url "/home/zjabbar/code/guix-channel/")
    (branch "main"))))

master-zaijab
