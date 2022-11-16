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
