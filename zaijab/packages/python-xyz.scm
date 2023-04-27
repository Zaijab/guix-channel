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

(define-public python-animdl  
  (package
    (name "python-animdl")
    (version "1.7.12")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "animdl" version))
              (sha256
               (base32
		"1ww071grnrl5cz2x8kx6ayk8fsmbfsqmwchw0sa2ph87ayx36nr7"))))
    (build-system python-build-system)
    (propagated-inputs (list python-anchor-kr
                             python-anitopy
                             python-click
                             python-comtypes
                             python-cssselect
                             python-httpx
                             python-lxml
                             python-packaging
                             python-pkginfo
                             python-pycryptodomex
                             python-pyyaml
                             python-regex
                             python-rich
                             python-tqdm
                             python-yarl))
    (home-page "")
    (synopsis
     "A highly efficient, fast, powerful and light-weight anime downloader and streamer for your favorite anime.")
    (description
     "This package provides a highly efficient, fast, powerful and light-weight anime
downloader and streamer for your favorite anime.")
    (license #f)))

(define-public python-anchor-kr
  (package
    (name "python-anchor-kr")
    (version "0.1.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "anchor-kr" version))
              (sha256
               (base32
		"1gsbxjahyan617dj91q6bcjg6njk47vkjk6w4l1d56rmznv5bh0g"))))
    (build-system python-build-system)
    (home-page "https://github.com/justfoolingaround/anchor")
    (synopsis "Me, you and anchor make your scraper complete.")
    (description "Me, you and anchor make your scraper complete.")
    (license #f)))
