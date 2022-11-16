(define-module (zaijab packages tensorflow)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system maven)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages java)
  #:use-module (gnu packages python)
  #:use-module ((guix licenses) #:prefix license:))

;; (define-public bazel
;;   (package
;;     (name "bazel")
;;     (version "7.0.0-pre.20221026.2")
;;     (source (origin
;;               (method git-fetch)
;;               (uri (git-reference
;; 		    (url "https://github.com/bazelbuild/bazel.git")
;; 		    (commit "7.0.0-pre.20221026.2")))
;;               (sha256 (base32 "1xd53idxx52s0hv50y7zdlmm2f3vr244i9n02x1bhx75v60chgx1"))))
;;     (inputs (list bash zip unzip python openjdk:jdk gcc glibc))
;;     (build-system gnu-build-system)
    
;;     (synopsis "")
;;     (description "")
;;     (home-page "")
;;     (license #f)))
