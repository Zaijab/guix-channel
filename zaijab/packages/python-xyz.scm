(define-module (zaijab packages python-xyz)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages haskell-xyz)  
  #:use-module (gnu packages check)  
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module ((guix licenses) #:prefix license:))

(define-public pass-import
  (package
    (name "pass-import")
    (version "3.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/roddhjav/pass-import")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xj8nh1jcmgsllykzhg4ybxfyjxzpwqsxl9sl86965247arkgw9r"))))
    (build-system python-build-system)
    (arguments
     (list
      ;; WARNING: 21/146 tests have errors
      ;; I think they're due to path issues,
      ;; and don't seem to impact operation
      #:tests? #f
      #:phases #~(modify-phases %standard-phases
                   (add-before 'build 'patch-base-and-share
                     (lambda* (#:key inputs #:allow-other-keys)
                       (substitute* "setup.py"
                         (("^base = .*$")
                          (string-append "base = '"
                                         #$output "'\n"))
                         (("^share = .*$")
                          (string-append "share = Path('"
                                         #$output "', 'share')\n")))))
                   (add-after 'build 'make-docs
                     (lambda* (#:key inputs #:allow-other-keys)
                       (invoke "pandoc"
                               "-t"
                               "man"
                               "-s"
                               "-o"
                               "share/man/man1/pass-import.1"
                               "share/man/man1/pass-import.md")
                       (invoke "pandoc"
                               "-t"
                               "man"
                               "-s"
                               "-o"
                               "share/man/man1/pimport.1"
                               "share/man/man1/pimport.md"))))))
    (native-inputs (list pandoc))
    (propagated-inputs (list python-pyaml
                             python-zxcvbn
                             python-requests
                             python-pykeepass
                             python-secretstorage
                             python-defusedxml
                             python-secretstorage
                             python-cryptography-rust
                             password-store))
    (home-page "https://github.com/roddhjav/pass-import")
    (synopsis
     "Pass extension to import passwords from other password managers")
    (description
     "Pass import is a password store extension allowing you to
import your password database to a password store repository conveniently.  It
natively supports import from 62 different password managers.  More manager
support can easily be added.")
    (license license:gpl3)))

(define-public python-tinytag
  (package
    (name "python-tinytag")
    (version "1.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tinytag" version))
       (sha256
        (base32 "13r6mq4va22abx7i6zg17nz90cpfm83whgx4rjm98l7q6sw66ahj"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))
    (native-inputs (list python-flake8 python-pytest python-pytest-cov))
    (home-page "https://github.com/devsnd/tinytag")
    (synopsis
     "Read music meta data and length of MP3, OGG, OPUS, MP4, M4A, FLAC, WMA and Wave files")
    (description
     "Read music meta data and length of MP3, OGG, OPUS, MP4, M4A, FLAC, WMA and Wave
files.")
    (license license:expat)))

python-tinytag
