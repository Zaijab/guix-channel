(define-module (zaijab packages ai)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (nonguix build-system binary)
  #:use-module ((px packages ai) #:prefix px:))

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

;; Since 0.31, Ollama runs inference in a separate `llama-server' process
;; (lib/ollama/llama-server) that dlopens ~15 sibling .so files (ggml/llama
;; backends) from its own directory via a "$ORIGIN" RUNPATH baked in at
;; release build time.  The pantherx package's install-plan only copies
;; bin/ollama, so every generate/chat request -- CPU included -- fails with
;; "llama-server binary not found".  Reinstall the missing CPU-backend
;; subset (CUDA/ROCm/Vulkan libs are dropped: ~1 GiB, and useless without
;; matching drivers this profile doesn't provide) and rpath each file to
;; keep "$ORIGIN" for sibling-library resolution while adding glibc/gcc for
;; libc/libstdc++ -- the pantherx patchelf-plan machinery can only replace
;; an ELF's RPATH outright, which would otherwise wipe out "$ORIGIN".
(define-public ollama
  (package
    (inherit px:ollama)
    (arguments
      (substitute-keyword-arguments (package-arguments px:ollama)
        ((#:install-plan _)
          #~'(("bin/ollama" "bin/")
              ("lib/ollama/llama-server" "lib/ollama/")
              ("lib/ollama/llama-quantize" "lib/ollama/")
              ("lib/ollama/libggml-base.so" "lib/ollama/")
              ("lib/ollama/libggml-base.so.0" "lib/ollama/")
              ("lib/ollama/libggml-base.so.0.15.3" "lib/ollama/")
              ("lib/ollama/libggml.so" "lib/ollama/")
              ("lib/ollama/libggml.so.0" "lib/ollama/")
              ("lib/ollama/libggml.so.0.15.3" "lib/ollama/")
              ("lib/ollama/libggml-cpu-alderlake.so" "lib/ollama/")
              ("lib/ollama/libggml-cpu-cannonlake.so" "lib/ollama/")
              ("lib/ollama/libggml-cpu-cascadelake.so" "lib/ollama/")
              ("lib/ollama/libggml-cpu-cooperlake.so" "lib/ollama/")
              ("lib/ollama/libggml-cpu-haswell.so" "lib/ollama/")
              ("lib/ollama/libggml-cpu-icelake.so" "lib/ollama/")
              ("lib/ollama/libggml-cpu-ivybridge.so" "lib/ollama/")
              ("lib/ollama/libggml-cpu-piledriver.so" "lib/ollama/")
              ("lib/ollama/libggml-cpu-sandybridge.so" "lib/ollama/")
              ("lib/ollama/libggml-cpu-sapphirerapids.so" "lib/ollama/")
              ("lib/ollama/libggml-cpu-skylakex.so" "lib/ollama/")
              ("lib/ollama/libggml-cpu-sse42.so" "lib/ollama/")
              ("lib/ollama/libggml-cpu-x64.so" "lib/ollama/")
              ("lib/ollama/libggml-cpu-zen4.so" "lib/ollama/")
              ("lib/ollama/libllama.so" "lib/ollama/")
              ("lib/ollama/libllama.so.0" "lib/ollama/")
              ("lib/ollama/libllama.so.0.0.1" "lib/ollama/")
              ("lib/ollama/libllama-common.so" "lib/ollama/")
              ("lib/ollama/libllama-common.so.0" "lib/ollama/")
              ("lib/ollama/libllama-common.so.0.0.1" "lib/ollama/")
              ("lib/ollama/libllama-server-impl.so" "lib/ollama/")
              ("lib/ollama/libllama-quantize-impl.so" "lib/ollama/")
              ("lib/ollama/libmtmd.so" "lib/ollama/")
              ("lib/ollama/libmtmd.so.0" "lib/ollama/")
              ("lib/ollama/libmtmd.so.0.0.1" "lib/ollama/")
              ) ; quoted install-plan list
          ) ; #:install-plan clause
        ((#:phases phases)
          #~(modify-phases #$phases
              (add-after 'install 'patch-llama-server-rpath
                (lambda* (#:key inputs outputs #:allow-other-keys)
                  (let* ((out (assoc-ref outputs "out"))
                         (dest (string-append out "/lib/ollama"))
                         (glibc-lib
                           (string-append (assoc-ref inputs "glibc") "/lib")
                           ) ; string-append glibc-lib
                         (gcc-lib
                           (string-append (assoc-ref inputs "gcc") "/lib")
                           ) ; string-append gcc-lib
                         (interpreter
                           (car (find-files glibc-lib "^ld-linux.*\\.so"))
                           ) ; car find-files interpreter
                         (rpath
                           (string-append "$ORIGIN:" glibc-lib ":" gcc-lib)
                           ) ; string-append rpath
                         ) ; let* bindings
                    (for-each
                      (lambda (file)
                        (when (eq? 'regular (stat:type (lstat file)))
                          (unless (string-contains file ".so")
                            (invoke "patchelf" "--set-interpreter" interpreter file)
                            ) ; unless
                          (invoke "patchelf" "--set-rpath" rpath file)
                          ) ; when
                        ) ; lambda (file)
                      (find-files dest)
                      ) ; for-each
                    ) ; let*
                  ) ; lambda*
                ) ; add-after 'install 'patch-llama-server-rpath
              ) ; modify-phases
          ) ; #:phases clause
        ) ; substitute-keyword-arguments
      ) ; arguments
    ) ; package
  ) ; define-public ollama
