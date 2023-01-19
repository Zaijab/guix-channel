(define-module (zaijab packages python-xyz)
  #:use-module (guix packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz))

(define-public python-proc
  (package
    (name "python-proc")
    (version "1.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "proc" version))
              (sha256
               (base32
                "0y3jdpj06pxf0cw47hhapim7nrnhr5jsyzmas7wbcbg2plmi8ybc"))))
    (build-system python-build-system)
    (propagated-inputs (list python-coloredlogs python-executor
                             python-humanfriendly python-property-manager
                             python-verboselogs))
    (home-page "https://proc.readthedocs.io")
    (synopsis "Simple interface to Linux process information")
    (description "Simple interface to Linux process information")
    (license license:expat)))

(define-public python-naturalsort
  (package
    (name "python-naturalsort")
    (version "1.5.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "naturalsort" version))
              (sha256
               (base32
                "0mkk27km6sibxjfmvp39szsphj775z2x3pm2ig020d5swzb5waqp"))))
    (build-system python-build-system)
    (home-page "https://github.com/xolox/python-naturalsort")
    (synopsis "Simple natural order sorting API for Python")
    (description "Simple natural order sorting API for Python")
    (license #f)))

(define-public python-qpass
  (package
    (name "python-qpass")
    (version "2.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "qpass" version))
              (sha256
               (base32
                "0krprgbxgzcqq311s3mmhhk4lyn7psmib6zfrjmm5lm9fm559pfz"))))
    (build-system python-build-system)
    (propagated-inputs (list python-coloredlogs
                             python-executor
                             python-humanfriendly
                             python-naturalsort
                             python-proc
                             python-property-manager
                             python-verboselogs))
    (home-page "https://github.com/xolox/python-qpass")
    (synopsis "Frontend for pass (the standard unix password manager)")
    (description "Frontend for pass (the standard unix password manager)")
    (license license:expat)))

(define-public python-pyamg
  (package
    (name "python-pyamg")
    (version "4.2.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pyamg" version))
              (sha256
               (base32
		"0bhqyxrp9c4fs6y5jl6kcg2jc1xg89lw73kwmg13a97zr8fkrb9p"))))
    (build-system python-build-system)
    (inputs (list python-numpy python-scipy))
    (home-page "https://github.com/pyamg/pyamg")
    (synopsis "PyAMG: Algebraic Multigrid Solvers in Python")
    (description "PyAMG: Algebraic Multigrid Solvers in Python")
    (license license:expat)))

(define-public python-py-pde
  (package
    (name "python-py-pde")
    (version "0.27.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "py-pde" version))
              (sha256
               (base32
                "00hlrrhjvs542qmgx3l6y4mbf7pcyzxbza0csy7bv3pxcdqrvqy4"))))
    (build-system python-build-system)
    (propagated-inputs (list python-matplotlib
                             python-numba
                             python-numpy
                             python-scipy
                             python-sympy
                             python-tqdm))
    (home-page "")
    (synopsis "Python package for solving partial differential equations")
    (description "Python package for solving partial differential equations")
    (license license:expat)))

(define-public python-tree-sitter
  (package
    (name "python-tree-sitter")
    (version "0.20.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "tree_sitter" version))
              (sha256
               (base32
		"03f6xqpnjh4g28d9g219w9n4l07bap91ws2xzfy4jrjxahn0hgz9"))))
    (build-system python-build-system)
    (home-page "https://github.com/tree-sitter/py-tree-sitter")
    (synopsis "Python bindings to the Tree-sitter parsing library")
    (description "Python bindings to the Tree-sitter parsing library")
    (license license:expat)))

(define-public python-pyright
  (package
    (name "python-pyright")
    (version "1.1.290")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pyright" version))
              (sha256
               (base32
		"0fhyjq8y55nikiv6924wf60kmzdg8bwfwby8akd58npbf3y8wvcm"))))
    (build-system python-build-system)
    (propagated-inputs (list python-nodeenv python-typing-extensions))
    (native-inputs (list python-twine))
    (home-page "https://github.com/RobertCraigie/pyright-python")
    (synopsis "Command line wrapper for pyright")
    (description "Command line wrapper for pyright")
    (license license:expat)))
