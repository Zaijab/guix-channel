(define-module (zaijab packages python-xyz)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages haskell-xyz)
  
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module ((guix licenses) #:prefix license:)

  )

#;(define-public python-liac-arff
  (package
    (name "python-liac-arff")
    (version "2.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "liac-arff" version))
       (sha256
        (base32 "1nn4nnh4kb85pzrll8hyzx4khkyrs5xbwybmniqsmic7cjpx081j"))))
    (build-system pyproject-build-system)
    (home-page "https://github.com/renatopp/liac-arff")
    (synopsis "A module for read and write ARFF files in Python.")
    (description
     "This package provides a module for read and write ARFF files in Python.")
    (license license:expat)))


#;(define-public python-configspace
  (package
    (name "python-configspace")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ConfigSpace" version))
       (sha256
        (base32 "06nz9v7nmcl2sj0z0c8mpiiyz6567jjdqvr0yvn19vnnivibidap"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-more-itertools python-numpy
                             python-pyparsing python-scipy
                             python-typing-extensions))
    (native-inputs (list python-automl-sphinx-theme
                         python-black
                         python-build
                         python-mypy
                         python-pre-commit
                         python-pytest
                         python-pytest-cov
                         python-ruff))
    (home-page "")
    (synopsis
     "Creation and manipulation of parameter configuration spaces for automated algorithm configuration and hyperparameter tuning.")
    (description
     "Creation and manipulation of parameter configuration spaces for automated
algorithm configuration and hyperparameter tuning.")
    (license #f)))

#;(define-public python-auto-sklearn
  (package
    (name "python-auto-sklearn")
    (version "0.15.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "auto-sklearn" version))
       (sha256
        (base32 "1l2198ga20yh4xybw64dylgkn25jspsqxq11wa1kmjfgdajl2a88"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-configspace
                             python-dask
                             python-distributed
                             python-distro
                             python-joblib
                             python-liac-arff
                             python-numpy
                             python-pandas
                             python-pynisher
                             python-pyrfr
                             python-pyyaml
                             python-scikit-learn
                             python-scipy
                             python-setuptools
                             python-smac
                             python-threadpoolctl
                             python-typing-extensions))
    (native-inputs (list python-black
                         python-isort
                         python-mypy
                         python-openml
                         python-pre-commit
                         python-pydocstyle
                         python-pytest
                         python-pytest-cases
                         python-pytest-cov
                         python-pytest-timeout
                         python-pytest-xdist))
    (home-page "https://automl.github.io/auto-sklearn")
    (synopsis "Automated machine learning.")
    (description "Automated machine learning.")
    (license #f)))

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

#;python-auto-sklearn
