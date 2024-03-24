(define-module (zaijab packages inferior-guix)
  #:use-module (guix inferior)
  #:use-module (guix channels))

(define inferior-guix-with-old-webkit
  (inferior-for-channels
   (list (channel
	  (name 'guix)
	  (url "https://git.savannah.gnu.org/git/guix.git")
	  (commit "8e2f32cee982d42a79e53fc1e9aa7b8ff0514714")))))
