(cons*
 (channel
  (name 'zaijab)
  (url "https://github.com/zaijab/guix-channel.git"))
 (channel
  (name 'flat)
  (url "https://github.com/flatwhatson/guix-channel.git")
  (introduction
   (make-channel-introduction
    "33f86a4b48205c0dc19d7c036c85393f0766f806"
    (openpgp-fingerprint
     "736A C00E 1254 378B A982  7AF6 9DBE 8265 81B6 4490"))))
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
(channel
  (name 'home-service-dwl-guile)
  (url "https://github.com/engstrand-config/home-service-dwl-guile")
  (branch "main")
  (introduction
    (make-channel-introduction
      "314453a87634d67e914cfdf51d357638902dd9fe"
      (openpgp-fingerprint
        "C9BE B8A0 4458 FDDF 1268 1B39 029D 8EB7 7E18 D68C"))))
 %default-channels)
