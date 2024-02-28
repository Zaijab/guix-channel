(define-module (zaijab packages python-xyz)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages python)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages check)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz))

(define-public python-ruff
  (package
    (name "python-ruff")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ruff" version))
       (sha256
        (base32 "07dkrixjy0jqchkj3mgshv26nw6dfi1a34wilc5s6s1hdgrxfbp6"))))
    (build-system pyproject-build-system)
    (home-page "https://docs.astral.sh/ruff")
    (synopsis
     "An extremely fast Python linter and code formatter, written in Rust.")
    (description
     "An extremely fast Python linter and code formatter, written in Rust.")
    (license license:expat)))

(define-public python-scikit-lego
  (package
    (name "python-scikit-lego")
    (version "0.7.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "scikit-lego" version))
       (sha256
        (base32 "0havfmbm7b10bsramciw5lqkpdzz04s902pxaz607252f85rvin5"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-deprecated python-importlib-metadata
                             python-pandas python-scikit-learn))
    (native-inputs (list pre-commit
                         python-pytest
                         python-pytest-cov
                         python-pytest-mock
                         python-pytest-xdist
                         python-ruff
                         python-scikit-lego))
    (home-page "")
    (synopsis "A collection of lego bricks for scikit-learn pipelines")
    (description
     "This package provides a collection of lego bricks for scikit-learn pipelines")
    (license #f)))
