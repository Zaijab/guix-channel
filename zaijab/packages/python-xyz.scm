(define-module (zaijab packages python-xyz)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages security-token)
  ;; #:use-module (guix-science packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages rust-apps)
  ;; #:use-module (gnu packages crates-io)
  #:use-module (gnu packages rust)  
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages haskell-xyz)  
  #:use-module (gnu packages check)  
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pkg-config)
  #:use-module ((guix licenses) #:prefix license:))

;; (define-public pass-import
;;   (package
;;     (name "pass-import")
;;     (version "3.5")
;;     (source
;;      (origin
;;        (method git-fetch)
;;        (uri (git-reference
;;              (url "https://github.com/roddhjav/pass-import")
;;              (commit (string-append "v" version))))
;;        (file-name (git-file-name name version))
;;        (sha256
;;         (base32 "0xj8nh1jcmgsllykzhg4ybxfyjxzpwqsxl9sl86965247arkgw9r"))))
;;     (build-system python-build-system)
;;     (arguments
;;      (list
;;       ;; WARNING: 21/146 tests have errors
;;       ;; I think they're due to path issues,
;;       ;; and don't seem to impact operation
;;       #:tests? #f
;;       #:phases #~(modify-phases %standard-phases
;;                    (add-before 'build 'patch-base-and-share
;;                      (lambda* (#:key inputs #:allow-other-keys)
;;                        (substitute* "setup.py"
;;                          (("^base = .*$")
;;                           (string-append "base = '"
;;                                          #$output "'\n"))
;;                          (("^share = .*$")
;;                           (string-append "share = Path('"
;;                                          #$output "', 'share')\n")))))
;;                    (add-after 'build 'make-docs
;;                      (lambda* (#:key inputs #:allow-other-keys)
;;                        (invoke "pandoc"
;;                                "-t"
;;                                "man"
;;                                "-s"
;;                                "-o"
;;                                "share/man/man1/pass-import.1"
;;                                "share/man/man1/pass-import.md")
;;                        (invoke "pandoc"
;;                                "-t"
;;                                "man"
;;                                "-s"
;;                                "-o"
;;                                "share/man/man1/pimport.1"
;;                                "share/man/man1/pimport.md"))))))
;;     (native-inputs (list pandoc))
;;     (propagated-inputs (list python-pyaml
;;                              python-zxcvbn
;;                              python-requests
;;                              python-pykeepass
;;                              python-secretstorage
;;                              python-defusedxml
;;                              python-secretstorage
;;                              python-cryptography
;;                              password-store))
;;     (home-page "https://github.com/roddhjav/pass-import")
;;     (synopsis
;;      "Pass extension to import passwords from other password managers")
;;     (description
;;      "Pass import is a password store extension allowing you to
;; import your password database to a password store repository conveniently.  It
;; natively supports import from 62 different password managers.  More manager
;; support can easily be added.")
;;     (license license:gpl3)))

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

;; (define-public python-equinox
;;   (package
;;     (name "python-equinox")
;;     (version "0.11.7")
;;     (source
;;      (origin
;;        (method url-fetch)
;;        (uri (pypi-uri "equinox" version))
;;        (sha256
;;         (base32 "1gjilam3mnw72995zib2gmzgndjan6xby32v8sqw8bl2kmm23q4n"))))
;;     (build-system pyproject-build-system)
;;     (propagated-inputs (list python-jax python-jaxtyping
;;                              python-typing-extensions python-hatchling))
;;     (home-page "")
;;     (synopsis "Elegant easy-to-use neural networks in JAX.")
;;     (description "Elegant easy-to-use neural networks in JAX.")
;;     (license #f)))

;; Built from ty's ruff submodule; crates live in (zaijab packages rust-crates).
(define-public ty
  (package
    (name "ty")
    (version "0.0.109")
    (source
     (origin
       (method git-fetch)
       (uri
	(git-reference
	 (url "https://github.com/astral-sh/ty")
	 (commit version)
	 (recursive? #t)
	 ) ; git-reference
	) ; uri
       (file-name (git-file-name name version))
       (sha256
	(base32 "0dkld73qdlx7n2gphs1kqgfq3lga8qzf71dvqsxgqvp92vl80w67")
	) ; sha256
       ) ; origin
     ) ; source
    (build-system cargo-build-system)
    (arguments
     (list
      #:tests? #f
      #:install-source? #f
      #:cargo-build-flags ''("--release" "--package" "ty")
      #:cargo-install-paths ''("crates/ty")
      #:phases
      #~(modify-phases %standard-phases
	  ;; the Cargo workspace is the ruff submodule; build.rs reads
	  ;; the version from ../dist-workspace.toml
	  (add-after 'unpack 'enter-ruff-workspace
	    (lambda _
	      (chdir "ruff")
	      ) ; lambda
	    ) ; add-after
	  (add-before 'build 'override-jemalloc
	    (lambda* (#:key inputs #:allow-other-keys)
	      (setenv "CARGO_FEATURE_UNPREFIXED_MALLOC_ON_SUPPORTED_PLATFORMS" "1")
	      (setenv "JEMALLOC_OVERRIDE"
		      (search-input-file inputs "lib/libjemalloc.so")
		      ) ; setenv
	      ) ; lambda*
	    ) ; add-before
	  ) ; modify-phases
      ) ; list
     ) ; arguments
    (native-inputs (list pkg-config))
    (inputs
     (cons* jemalloc
	    (list zstd "lib")
	    (cargo-inputs 'ty #:module '(zaijab packages rust-crates))
	    ) ; cons*
     ) ; inputs
    (home-page "https://docs.astral.sh/ty/")
    (synopsis "Python type checker and language server")
    (description "ty is a type checker and language server for Python from
Astral, the makers of ruff and uv.  @code{ty server} speaks LSP.")
    (license license:expat)
    ) ; package
  ) ; define-public ty

;; ruff from the same ruff submodule and crate list as ty.
(define-public ruff-next
  (package
    (inherit ty)
    (name "ruff")
    (version "0.17.6")
    (arguments
     (substitute-keyword-arguments (package-arguments ty)
       ((#:cargo-build-flags _) ''("--release" "--package" "ruff"))
       ((#:cargo-install-paths _) ''("crates/ruff"))
       ) ; substitute-keyword-arguments
     ) ; arguments
    (home-page "https://docs.astral.sh/ruff")
    (synopsis "Python linter, formatter and language server")
    (description "Ruff is a Python linter and code formatter.  @code{ruff
server} speaks LSP.")
    ) ; package
  ) ; define-public ruff-next

;; LSP multiplexer the Eglot manual suggests for ty + ruff.
(define-public rassumfrassum
  (package
    (name "rassumfrassum")
    (version "0.3.5")
    (source
     (origin
       (method git-fetch)
       (uri
	(git-reference
	 (url "https://github.com/joaotavora/rassumfrassum")
	 (commit (string-append "v" version))
	 ) ; git-reference
	) ; uri
       (file-name (git-file-name name version))
       (sha256
	(base32 "05h6c4ywzrza3zmzgd82sdzj01rk7jsf5n1yd4sxx3x5m419mn9v")
	) ; sha256
       ) ; origin
     ) ; source
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f)
     ) ; arguments
    (native-inputs (list python-setuptools))
    (home-page "https://github.com/joaotavora/rassumfrassum")
    (synopsis "LSP multiplexer for one client and several servers")
    (description "Rassumfrassum (@command{rass}) presents several language
servers to an LSP client as a single server, merging their capabilities and
responses.")
    (license license:gpl3+)
    ) ; package
  ) ; define-public rassumfrassum
