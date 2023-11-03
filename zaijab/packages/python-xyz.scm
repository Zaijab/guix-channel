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
  #:use-module (gnu packages python-crypto)
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

(define-public python-pytweening
  (package
    (name "python-pytweening")
    (version "1.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytweening" version))
       (sha256
        (base32 "1nyygvyhhic50j6lwwp6klkrlvbpzhfdv4pnx4f6rdsppzqk8wbn"))))
    (build-system python-build-system)
    (home-page "https://github.com/asweigart/pytweening")
    (synopsis "A collection of tweening / easing functions.")
    (description
     "This package provides a collection of tweening / easing functions.")
    (license license:expat)))

(define-public python-mss
  (package
    (name "python-mss")
    (version "9.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mss" version))
       (sha256
        (base32 "04lznfa5lvkwxi4xvhkw7wgbhk9kycsynfm33y0jhx7jih0bkdvf"))))
    (build-system pyproject-build-system)
    (home-page "https://github.com/BoboTiG/python-mss")
    (synopsis
     "An ultra fast cross-platform multiple screenshots module in pure python using ctypes.")
    (description
     "An ultra fast cross-platform multiple screenshots module in pure python using
ctypes.")
    (license license:expat)))

(define-public python-pyscreenshot
  (package
    (name "python-pyscreenshot")
    (version "3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyscreenshot" version))
       (sha256
        (base32 "0kar4lkasa2ay0v1xk7nnkj6lfdxsv7bysm8apz6nspnmvq963lc"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-easyprocess python-entrypoint2
                             python-jeepney python-mss))
    (home-page "https://github.com/ponty/pyscreenshot")
    (synopsis "python screenshot")
    (description "python screenshot")
    (license license:bsd-3)))

(define-public python-pyscreeze
  (package
    (name "python-pyscreeze")
    (version "0.1.29")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PyScreeze" version))
       (sha256
        (base32 "0di8c71q8ycah3lj5kq5kga2br000m3x99lrclbxla71jmvm8x6h"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-pyscreenshot))
    (home-page "https://github.com/asweigart/pyscreeze")
    (synopsis "A simple, cross-platform screenshot module for Python 2 and 3.")
    (description
     "This package provides a simple, cross-platform screenshot module for Python 2
and 3.")
    (license license:expat)))

(define-public python-pyrect
  (package
    (name "python-pyrect")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PyRect" version))
       (sha256
        (base32 "0y4vl13zpgvlh0bhsn1vkm2fankwjk05gggzr9krp4lvvzv5algn"))))
    (build-system pyproject-build-system)
    (home-page "https://github.com/asweigart/pyrect")
    (synopsis
     "PyRect is a simple module with a Rect class for Pygame-like rectangular areas.")
    (description
     "@code{PyRect} is a simple module with a Rect class for Pygame-like rectangular
areas.")
    (license license:bsd-3)))

(define-public python-pygetwindow
  (package
    (name "python-pygetwindow")
    (version "0.0.9")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PyGetWindow" version))
       (sha256
        (base32 "1256s0nj9w7vyzv29klhi9lw25s07047fw9dhg6hbcyjwxal728p"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-pyrect))
    (home-page "https://github.com/asweigart/pygetwindow")
    (synopsis
     "A simple, cross-platform module for obtaining GUI information on application's windows.")
    (description
     "This package provides a simple, cross-platform module for obtaining GUI
information on application's windows.")
    (license license:bsd-3)))

(define-public python-mouseinfo
  (package
    (name "python-mouseinfo")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "MouseInfo" version))
       (sha256
        (base32 "1rznyzv6w9f8bfq2x5b0ik0dqyk5ghlhmkiw1998waq6hn4gnqic"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-pyperclip))
    (home-page "https://github.com/asweigart/mouseinfo")
    (synopsis
     "An application to display XY position and RGB color information for the pixel currently under the mouse. Works on Python 2 and 3.")
    (description
     "An application to display XY position and RGB color information for the pixel
currently under the mouse.  Works on Python 2 and 3.")
    (license #f)))

(define-public python-pyautogui
  (package
    (name "python-pyautogui")
    (version "0.9.54")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PyAutoGUI" version))
       (sha256
        (base32 "1ch4mvrnkz4wyc27p75raf98xzy6wmbxyx1z375l328izpl2j7fx"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-mouseinfo python-pygetwindow
                             python-pymsgbox python-pyscreeze
                             python-pytweening))
    (home-page "https://github.com/asweigart/pyautogui")
    (synopsis
     "PyAutoGUI lets Python control the mouse and keyboard, and other GUI automation tasks. For Windows, macOS, and Linux, on Python 3 and 2.")
    (description
     "@code{PyAutoGUI} lets Python control the mouse and keyboard, and other GUI
automation tasks.  For Windows, @code{macOS}, and Linux, on Python 3 and 2.")
    (license license:bsd-3)))
