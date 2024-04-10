(define-module (zaijab packages searxng)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xml)
  )

(define-public searxng
  (package
    (name "searxng")
    (version "64584902714c0aee61857c7681ad91e2a93d2409")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/searxng/searxng")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xr5wddxdv8awn0svavfyjjx06vj436a0nzp2wr98yh313hhm7ya"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ;what tests do is make online requests to each engine
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'relax-requirements
           (lambda _
             ;; Tests can run after build with 'searx-checker' tool in /bin.
             ;; allow using a higher dependency version
             (substitute* "requirements.txt"
               (("==") ">="))))
         (add-before 'sanity-check 'set-debug
           (lambda _
             ;; the user will need to change the secret key
             ;; https://github.com/searx/searx/issues/2278
             (setenv "SEARX_DEBUG" "1"))))))
    (propagated-inputs
     (list python-babel
           python-certifi
           python-dateutil
           python-flask
           python-flask-babel
           python-idna
           python-jinja2
           python-langdetect
           python-lxml
           python-pygments
           python-pyyaml
           python-requests
	   python-pytoml))
    (inputs (list python-pytoml))
    (home-page "https://searx.github.io/searx/")
    (synopsis "Privacy-respecting metasearch engine")
    (description "Searx is a privacy-respecting, hackable metasearch engine.")
    (license license:agpl3+)))

searxng
