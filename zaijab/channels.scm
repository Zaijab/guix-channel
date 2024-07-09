;; -*- mode: scheme -*-
(define-module (zaijab channels)
  #:use-module (guix ci)
  #:use-module (ice-9 popen)
  #:use-module (guix channels))

(define filename "./zaijab/files/channel_lock.tmpl")
(define in (open-input-file filename))
(define raw-data (read in))
(close-input-port in)

(define master-zaijab-channel-lock
  (cons*
   '(channel (name 'zaijab)
	     (url "/home/zjabbar/code/guix-channel/"))
   (cddr raw-data)))

(define-public master-zaijab
  (list
   (channel
    (name 'zaijab)
    (url "/home/zjabbar/code/guix-channel/")
    (branch "main"))))
