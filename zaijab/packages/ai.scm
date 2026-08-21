(define-module (zaijab packages ai)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (nonguix build-system binary))

;; Upstream split the V8 code-mode runtime out of the main binary in 0.147.0
;; (PR #36217, "Run code mode exclusively through the standalone host").  The
;; pantherx package still installs only codex-<target>.tar.gz, so code-mode-only
;; models (gpt-5.6-*) cannot execute anything.  Keep both halves of the release
;; in one output; codex resolves the helper next to the realpath of its own
;; executable, so the profile's bin/ is not a valid home for it.

(define %codex-version "0.148.0")

(define (codex-release-uri component)
  (string-append "https://github.com/openai/codex/releases/download/rust-v"
                 %codex-version "/" component
                 "-x86_64-unknown-linux-musl.tar.gz"))

(define codex-code-mode-host-source
  (origin
    (method url-fetch)
    (uri (codex-release-uri "codex-code-mode-host"))
    (sha256
     (base32 "0acqsncr9iv3rw03zkmw4l87as10q0gc6a1czcc1p9lg4admavlf"))))

(define-public codex-with-code-mode
  (package
    (name "codex")
    (version %codex-version)
    (source
     (origin
       (method url-fetch)
       (uri (codex-release-uri "codex"))
       (sha256
        (base32 "0npfrz98kjpj1hjnm6ak9pcc4qbna7cmld46pcrzbgmkyrigfdhs"))))
    (build-system binary-build-system)
    (arguments
     (list
      #:validate-runpath? #f
      #:install-plan
      #~'(("codex-x86_64-unknown-linux-musl" "bin/codex"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-code-mode-host
            (lambda _
              (let ((host (string-append #$output
                                         "/bin/codex-code-mode-host")))
                (invoke "tar" "xf" #$codex-code-mode-host-source)
                (copy-file "codex-code-mode-host-x86_64-unknown-linux-musl"
                           host)
                (chmod host #o555)))))))
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/openai/codex")
    (synopsis "AI coding agent from OpenAI")
    (description
     "Codex CLI is an AI-powered coding agent from OpenAI.  This variant also
installs @command{codex-code-mode-host}, the helper process that code-mode-only
models such as gpt-5.6-sol use to execute every tool call.")
    (license license:asl2.0)))
