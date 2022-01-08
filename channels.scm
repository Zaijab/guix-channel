(cons*
 (channel
  (name 'zaijab)
  (url "https://github.com/zaijab/guix-channel.git"))
 ;; (channel
 ;;  (name 'rc)
 ;;  (url "https://github.com/bqv/rc.git")
 ;;  (branch "live")
 ;;  (commit "ce711fcc3bcebbaa108ae8816c262d53008d44fb"))
 (channel
  (name 'rde)
  (url "https://git.sr.ht/~abcdw/rde")
  (introduction
   (make-channel-introduction
    "257cebd587b66e4d865b3537a9a88cccd7107c95"
    (openpgp-fingerprint
     "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
 (channel
  (name 'nonguix)
  (url "https://gitlab.com/nonguix/nonguix"))
 %default-channels)
