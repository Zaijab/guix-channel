(define-module (zaijab packages inferior-guix)
  #:use-module (guix inferior)
  #:use-module (guix profiles)
  #:use-module (guix channels)
  #:use-module (guix transformations)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (srfi srfi-1)
  )

;; (first (lookup-inferior-packages
;; 	(inferior-for-channels
;; 	 (list (channel
;; 		(name 'guix)
;; 		(url "https://git.savannah.gnu.org/git/guix.git")
;; 		(commit "8e2f32cee982d42a79e53fc1e9aa7b8ff0514714"))))
;; 	"webkitgtk-with-libsoup2"))
