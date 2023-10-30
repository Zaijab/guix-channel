(define-module (zaijab packages python-xyz)
  #:use-module (guix packages)
  #:use-module (guix gexp)
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

(define-public python-documentation
  (package
    (inherit python)  ; Ensure that the 'python' package is defined elsewhere in your Guix environment
    (name "python-documentation")
    (arguments
     (list
      #:tests? #f  ; we're only generating the documentation
      #:phases
      (modify-phases %standard-phases
        (add-before 'build 'add-gnu-freefont-to-texmf
          (lambda _
            ;; The same workaround for GNU FreeFont as in the Numpy documentation package
            (mkdir-p "texmf-dist/fonts/opentype/public")
            (symlink (string-append
                      #$(this-package-native-input "font-gnu-freefont")
                      "/share/fonts/opentype")
                     (string-append
                      (getcwd) "/"
                      "texmf-dist/fonts/opentype/public/gnu-freefont"))
            (setenv "GUIX_TEXMF" (string-append (getenv "GUIX_TEXMF") ":"
                                                (getcwd) "/texmf-dist"))))
        (delete 'build)
        (delete 'install)  ; Delete the existing 'install' phase
        (add-after 'build 'install-docs  ; Add a custom 'install-docs' phase after 'build'
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((data (string-append (assoc-ref outputs "out") "/share"))
                   (doc (string-append data "/doc/python"))
                   (html (string-append doc "/html"))
                   (info (string-append data "/info"))
                   (sphinxopts (string-append
                                "SPHINXOPTS=-j"
                                (number->string (parallel-job-count)))))
              (with-directory-excursion "Doc"  ; Assuming 'Doc' is the directory containing the documentation
                ;; Do not treat warnings as errors.
                (substitute* "Makefile"
                  ((" -WT ") " -T "))
                (setenv "HOME" "/tmp")
                ;; Build the HTML documentation.
                (invoke "make" "html" sphinxopts)
                ;; Build the Texinfo documentation.
                (invoke "make" "info" sphinxopts)
                ;; Install the HTML documentation.
                (mkdir-p html)
                (copy-recursively "build/html" html)
                ;; Install the Info documentation.
                (install-file "build/texinfo/python.info" info)
                (symlink (string-append html "/_images")
                         (string-append info "/python-figures")))))))))
    (native-inputs
     (list font-gnu-freefont
           perl
           python-sphinx
           texinfo
           (texlive-updmap.cfg
            (list texlive-cbfonts
                  texlive-cm-super
                  texlive-expdlist
                  texlive-greek-fontenc
                  texlive-latexmk
                  texlive-polyglossia
                  texlive-xetex
                  texlive-xindy))))
    (inputs '())
    (propagated-inputs '())
    (synopsis "Documentation for the @code{python} package")
    (description "This package provides the complete Python documentation in
the Texinfo, HTML, and PDF formats.")))

