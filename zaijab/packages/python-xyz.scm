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

(define-public python-pandas-documentation
  (package
    (inherit python-pandas)
    (name "python-pandas-documentation")
    (arguments
     (list
      #:tests? #f                     ;we're only generating the documentation
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'add-gnu-freefont-to-texmf
            (lambda _
              ;; XXX: The Sphinx-generated tex output specifies the GNU
              ;; FreeFont font to be searched via its extension, which uses
              ;; kpathsea instead of fontconfig and fail (see:
              ;; https://github.com/sphinx-doc/sphinx/issues/10347).  Create a
              ;; symlink to GNU FreeFont and add it to the TEXMF tree via
              ;; GUIX_TEXMF.
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
          (replace 'install
            (lambda _
              (let* ((data (string-append #$output "/share"))
                     (doc (string-append data "/doc/pandas"))
                     (html (string-append doc "/html"))
                     (info (string-append data "/info"))
                     (sphinxopts (string-append
                                  "SPHINXOPTS=-j"
                                  (number->string (parallel-job-count)))))
                (with-directory-excursion "doc"
                  ;; Do not treat warnings as errors.
                  (substitute* "Makefile"
                    ((" -WT ") " -T "))
                  (setenv "HOME" "/tmp")
                  ;; Build the PDF documentation.
                  (invoke "make" "latex-build" sphinxopts)
                  (invoke "make" "-C" "build/latex" "all-pdf" sphinxopts)
                  ;; Build the HTML documentation
                  (invoke "make" "html" sphinxopts)
                  ;; Build the Info documentation.  The issues worked around
                  ;; below can be tracked at
                  ;; https://github.com/numpy/numpy/issues/12278.
                  (substitute* "source/conf.py"
                    ;; The root document should be "index", not "contents".
                    (("\"contents\"") "'index'")
                    ;; Disable Sphinx extensions that produce broken Texinfo.
                    ((".*'numpydoc'.*") "")
                    ((".*'sphinx.ext.autosummary'.*") ""))
                  (invoke "make" "info" sphinxopts)
                  ;; Install the HTML documentation.
                  (mkdir-p html)
                  (copy-recursively "build/html" html)
                  ;; Install the PDF reference and user manuals.
                  (install-file "build/latex/numpy-ref.pdf" doc)
                  (install-file "build/latex/numpy-user.pdf" doc)
                  ;; Install the info manual.
                  (install-file "build/texinfo/numpy.info" info)
                  (symlink (string-append html "/_images")
                           (string-append info "/numpy-figures")))))))))
    (native-inputs
     (list font-gnu-freefont
           perl
           python-breathe
           python-ipython
           python-matplotlib
           python-numpy
           python-numpydoc
           python-pandas
           python-pydata-sphinx-theme
           python-scipy                 ;used by matplotlib
           python-sphinx-4
           python-sphinx-panels
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
    (synopsis "Documentation for the @code{python-numpy} package")
    (description "This package provides the complete NumPy documentation in
the Texinfo, HTML, and PDF formats.")))

#;python-auto-sklearn
