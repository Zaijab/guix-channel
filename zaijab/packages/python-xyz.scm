(define-module (zaijab packages python-xyz)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages python)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages check)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages rust-apps)
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
    (arguments
     `(#:cargo-inputs
       (("cargo" ,(specification->package "rust-cargo")) ; Ensure cargo is available
        ("maturin" ,maturin)) ; Include maturin for the build process
       #:phases
       (modify-phases %standard-phases
         ;; Add or modify phases here as needed
         )))
    (native-inputs
     `(("rust" ,rust) ; Rust programming language compiler
       ("python" ,python-3) ; Python interpreter, adjust version as necessary
       ;; Add other native inputs here
       ))
    (propagated-inputs
     (list maturin)
     ;; Add any inputs that need to be available to users of this package
     )
    (home-page "https://docs.astral.sh/ruff")
    (synopsis
     "An extremely fast Python linter and code formatter, written in Rust.")
    (description
     "An extremely fast Python linter and code formatter, written in Rust.")
    (license license:expat)))

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
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'setup-rust-environment
           (lambda* (#:key inputs native-inputs #:allow-other-keys)
             ;; Here, customize the environment and possibly run `cargo build`
             ;; or `maturin build` as needed for your package.
             ;; This is a placeholder; you'll need to fill in the specific commands.
             (invoke "cargo" "build")
             ;; Make sure to return #t to indicate success.
             #t)))))
    (native-inputs
     `(("rust" ,rust) ; Rust programming language compiler
       ("cargo" ,(specification->package "rust-cargo"))
       ("maturin" ,(specification->package "maturin"))
       ("python" ,python-3) ; Python interpreter, adjust version as necessary
       ;; Add other native inputs here
       ))
    (propagated-inputs
     '()
     ;; Add any inputs that need to be available to users of this package
     )
    (home-page "https://docs.astral.sh/ruff")
    (synopsis
     "An extremely fast Python linter and code formatter, written in Rust.")
    (description
     "An extremely fast Python linter and code formatter, written in Rust.")
    (license license:expat)))


python-ruff
