;;; Native Lean 4 library packaging for Guix (Mathlib and its dependency DAG).
;;;
;;; The hard part on Guix is that Lake (Lean's build tool) wants to fetch
;;; dependencies over git at build time, which the build sandbox forbids.  We
;;; sidestep that exactly as Nixpkgs PR #497946 does: every dependency is a
;;; separately-built package whose whole `.lake/build' tree lives in the store,
;;; and before calling `lake build' we write a `.lake/package-overrides.json'
;;; that redirects each Lake `require' to the dependency's store path.  No git
;;; fetch happens.  Each library also drops a `.lake-package-name' marker so a
;;; dependent can recover the *Lake* package name (which must match the name in
;;; the upstream lakefile, e.g. "batteries", "Qq").
;;;
;;; Pinned to Lean 4.28.0 to match the `lean4' package in this Guix revision.
;;; Dependency revisions come from mathlib4's lake-manifest.json at tag v4.28.0.

(define-module (zaijab packages lean)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix search-paths)
  #:use-module (gnu packages)
  #:use-module (gnu packages lean)              ;lean4
  #:use-module ((guix licenses) #:prefix license:))

;;; ---------------------------------------------------------------------------
;;; Patched lean4.
;;;
;;; When a Lean-DSL `lakefile.lean' dependency is consumed from the read-only
;;; store, Lake's `importConfigFile' tries to take a write lock on the cached
;;; config olean (`.lake/config/<pkg>/lakefile.olean.lock'), because its
;;; up-to-date check keys on the package index/name which differ between a
;;; standalone elaboration and a dependency context.  Following Nixpkgs PR
;;; #497946, we force the config to be considered up-to-date when the package
;;; directory lives in the store, so Lake reads the prebuilt config olean
;;; without locking or re-elaborating.  Only `.lean' lakefiles hit this path;
;;; `.toml' ones do not.
;;; ---------------------------------------------------------------------------

;; Internal: the patched lean4 *without* a LEAN_PATH search path.  The Lean
;; libraries below build against this, so adding the search path to the
;; user-facing package (below) does not rebuild them — a search-path spec is
;; otherwise baked into every dependent's build environment.
(define lean4-patched-base
  (package
    (inherit lean4)
    (name "lean4-patched")
    (arguments
     (substitute-keyword-arguments (package-arguments lean4)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'patch-lake-store-uptodate
              (lambda _
                (let ((file "src/lake/Lake/Load/Lean/Elab.lean"))
                  (substitute* file
                    (("let upToDate := \\(← olean\\.pathExists\\) ∧")
                     (string-append
                      "let upToDate := "
                      "cfg.pkgDir.toString.startsWith \"/gnu/store/\" ∨ "
                      "(← olean.pathExists) ∧")))
                  ;; Fail loudly if the upstream line moved (substitute* is a
                  ;; no-op on no match).
                  (invoke "grep" "-q" "startsWith \"/gnu/store/\"" file))))))))))

;; User-facing toolchain.  Identical build to the base (so it shares the same
;; store output — no rebuild), but carries a LEAN_PATH search path: Guix scans
;; every package in the profile for this subdirectory and unions them into
;; LEAN_PATH.  Thus `guix install lean4-patched lean-mathlib' makes
;; `import Mathlib' work with no manual setup.
(define-public lean4-patched
  (package
    (inherit lean4-patched-base)
    (native-search-paths
     (list (search-path-specification
            (variable "LEAN_PATH")
            (files (list ".lake/build/lib/lean")))))))

(define %lean-phases
  ;; Shared build/install phases for every Lean library.  LAKE-NAME and
  ;; LAKE-TARGETS are spliced in per-package via `lean-arguments'.
  #~%standard-phases)

(define* (lean-arguments #:key lake-name (lake-targets '()))
  "Return the gnu-build-system arguments for a Lean library whose Lake package
name is LAKE-NAME, building LAKE-TARGETS (default: the package's default)."
  (list
   #:tests? #f
   #:modules '((guix build gnu-build-system)
               (guix build utils)
               (ice-9 rdelim)
               (srfi srfi-1))
   #:phases
   #~(modify-phases %standard-phases
       (delete 'bootstrap)
       (delete 'configure)
       (delete 'patch-source-shebangs)
       (delete 'check)
       (replace 'build
         (lambda* (#:key inputs #:allow-other-keys)
           ;; Lake refuses to run as if it had no project root unless HOME is
           ;; writable; keep it out of the network entirely.
           (setenv "HOME" (getcwd))
           (setenv "LAKE_NO_CACHE" "1")
           (setenv "RESERVOIR_API_URL" "")
           ;; leanc shells out to `cc'; Guix only provides `gcc'.
           (setenv "LEAN_CC" "gcc")
           ;; Discover Lean-library inputs by their marker file, and build the
           ;; override file + LEAN_PATH from them.
           (let* ((deps
                   (filter-map
                    (lambda (input)
                      (let* ((dir (cdr input))
                             (marker (string-append dir "/.lake-package-name")))
                        (and (file-exists? marker)
                             (cons (call-with-input-file marker read-line)
                                   dir))))
                    inputs))
                  (lean-paths
                   (filter-map
                    (lambda (dep)
                      (let ((p (string-append (cdr dep) "/.lake/build/lib/lean")))
                        (and (file-exists? p) p)))
                    deps)))
             (unless (null? lean-paths)
               (let ((old (getenv "LEAN_PATH")))
                 (setenv "LEAN_PATH"
                         (string-join (append lean-paths (if old (list old) '()))
                                      ":"))))
             (mkdir-p ".lake")
             (call-with-output-file ".lake/package-overrides.json"
               (lambda (port)
                 (format port
                         "{\"schemaVersion\":\"1.2.0\",\"packages\":[~a]}"
                         (string-join
                          (map (lambda (dep)
                                 (format #f
                                         "{\"type\":\"path\",\"name\":~s,\"inherited\":false,\"dir\":~s}"
                                         (car dep) (cdr dep)))
                               deps)
                          ","))))
             (format #t "lean: overriding deps ~s~%" (map car deps))
             (format #t "lean: LEAN_PATH=~s~%" (getenv "LEAN_PATH"))
             (apply invoke (append (list "lake" "build" "--no-ansi")
                                   (list #$@lake-targets))))))
       (replace 'install
         (lambda* (#:key outputs #:allow-other-keys)
           (let ((out (assoc-ref outputs "out")))
             ;; Drop the ephemeral dep copies / override file before publishing.
             (when (file-exists? ".lake/packages")
               (delete-file-recursively ".lake/packages"))
             (when (file-exists? ".lake/package-overrides.json")
               (delete-file ".lake/package-overrides.json"))
             (mkdir-p out)
             (copy-recursively "." out)
             (call-with-output-file (string-append out "/.lake-package-name")
               (lambda (port) (display #$lake-name port)))))))))

(define* (make-lean-lib name version url commit hash lake-name
                        #:key (propagated-inputs '()) (lake-targets '())
                        (synopsis "Lean 4 library") (home-page url))
  (package
    (name name)
    (version version)
    (source (origin
              (method git-fetch)
              (uri (git-reference (url url) (commit commit)))
              (file-name (git-file-name name version))
              (sha256 (base32 hash))))
    (build-system gnu-build-system)
    (arguments (lean-arguments #:lake-name lake-name #:lake-targets lake-targets))
    (inputs (list lean4-patched-base))
    (propagated-inputs propagated-inputs)
    (home-page home-page)
    (synopsis synopsis)
    (description synopsis)
    (license license:asl2.0)))

;;; ---------------------------------------------------------------------------
;;; Leaf libraries (no Lean dependencies)
;;; ---------------------------------------------------------------------------

(define-public lean-batteries
  (make-lean-lib
   "lean-batteries" "4.28.0"
   "https://github.com/leanprover-community/batteries"
   "495c008c3e3f4fb4256ff5582ddb3abf3198026f"
   "19qqxm1x29hbh12kk1k7qg02h0jvpcr880130jq4ir90bc44rpfw"
   "batteries"
   #:synopsis "The Lean 4 standard library extension (Batteries)"))

(define-public lean-qq
  (make-lean-lib
   "lean-qq" "4.28.0"
   "https://github.com/leanprover-community/quote4"
   "b8f98e9087e02c8553945a2c5abf07cec8e798c3"
   "1m0c99ls04v58p1zhzf4rkzv69qc9rhjz4npzh6ch0jh69sd46h5"
   "Qq"
   #:synopsis "Lean 4 type-safe expression quotations (Qq)"))

(define-public lean-cli
  (make-lean-lib
   "lean-cli" "4.28.0"
   "https://github.com/leanprover/lean4-cli"
   "4f10f47646cb7d5748d6f423f4a07f98f7bbcc9e"
   "03lbimfkgp139ymmfvdansphpcndg2ymi2lvdyahc376iivgwxgn"
   "Cli"
   #:synopsis "Command-line-interface library for Lean 4"))

(define-public lean-plausible
  (make-lean-lib
   "lean-plausible" "4.28.0"
   "https://github.com/leanprover-community/plausible"
   "55c8532eb21ec9f6d565d51d96b8ca50bd1fbef3"
   "0lkcg667hwhyn9gjv428hdm38l4br29i07sf8vs95dsghix9zqy6"
   "plausible"
   #:synopsis "Property-based testing (plausible) for Lean 4"))

(define-public lean-search-client
  (make-lean-lib
   "lean-search-client" "4.28.0"
   "https://github.com/leanprover-community/LeanSearchClient"
   "c5d5b8fe6e5158def25cd28eb94e4141ad97c843"
   "05v9l41s83yyrwkhqa08cam20jjb86vp0p7mnzai4yffgp180rig"
   "LeanSearchClient"
   #:synopsis "Search client for Lean 4 (leansearch/Moogle)"))

;;; ---------------------------------------------------------------------------
;;; Libraries with dependencies (exercise the override mechanism)
;;; ---------------------------------------------------------------------------

(define-public lean-aesop
  (make-lean-lib
   "lean-aesop" "4.28.0"
   "https://github.com/leanprover-community/aesop"
   "f642a64c76df8ba9cb53dba3b919425a0c2aeaf1"
   "1blh96pqjbmv9mp8jiykwz7dl1i1k1dhj8ifhbjqyzs4mgmgiqr9"
   "aesop"
   #:propagated-inputs (list lean-batteries)
   #:synopsis "Aesop: a proof-search tactic for Lean 4"))

(define-public lean-import-graph
  (make-lean-lib
   "lean-import-graph" "4.28.0"
   "https://github.com/leanprover-community/import-graph"
   "85b59af46828c029a9168f2f9c35119bd0721e6e"
   "12rr017z3jyb7f7mzbk15dq84fa7jpj0yw12cj5vfmr3ainbr53x"
   "importGraph"
   ;; Default targets include the `graph' executable, which statically links
   ;; Cli and would try to compile Cli's objects in the read-only store.  We
   ;; only need the library (which is all Mathlib consumes).
   #:lake-targets '("ImportGraph")
   #:propagated-inputs (list lean-cli)
   #:synopsis "Import-graph tooling for Lean 4 projects"))

;;; ---------------------------------------------------------------------------
;;; ProofWidgets: built from its upstream *release* tarball.
;;;
;;; Building from source would require a Node/npm toolchain to compile the
;;; TypeScript widgets (the `widgetJsAll' Lake target).  Upstream itself ships
;;; a prebuilt release (`preferReleaseBuild := true'), and its oleans load fine
;;; under our lean4 (Lean checks only the version string, not the githash).  We
;;; therefore take the real git source (so Lake sees a valid package: lakefile,
;;; widget sources, lean-toolchain) and overlay the prebuilt `.lake/build' from
;;; the release on top.  Because the release was built from this exact source,
;;; the build trace files match, so Lake treats everything as up-to-date and
;;; never re-runs npm.  A future improvement would build the widgets from
;;; source with a packaged Node toolchain.
;;; ---------------------------------------------------------------------------

(define proofwidgets-release
  ;; Prebuilt `.lake/build' contents (compiled `js/' widgets + oleans) for the
  ;; v0.0.87 tag, used to avoid a Node/npm build.
  (origin
    (method url-fetch)
    (uri (string-append
          "https://github.com/leanprover-community/ProofWidgets4"
          "/releases/download/v0.0.87/ProofWidgets4.tar.gz"))
    (file-name "proofwidgets-0.0.87-release.tar.gz")
    (sha256
     (base32 "09f718k3wh8rc7fwkgnnn26rhf9sprvvlanbfzhvdc297k9nqgyf"))))

(define-public lean-proofwidgets
  (package
    (name "lean-proofwidgets")
    (version "0.0.87")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/leanprover-community/ProofWidgets4")
             (commit "be3b2e63b1bbf496c478cef98b86972a37c1417d")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fhvrshk7kwg5vh8v5sqh8rnnn04acb91lfj0bqzj10lzhsjlwd9"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'bootstrap)
          (delete 'configure)
          (delete 'patch-source-shebangs)
          (delete 'check)
          ;; No `lake build': overlay the prebuilt artifacts instead of
          ;; compiling (which would need npm).  Keep the upstream
          ;; `lakefile.lean' unchanged so the release oleans' build traces
          ;; still match (changing it would make Lake think they are stale and
          ;; try to recompile into our read-only store).  But that Lean-DSL
          ;; lakefile must be *elaborated*, and Lake caches the result under
          ;; `.lake/config'; a consumer reading us from the read-only store
          ;; would fail trying to write it.  So pre-elaborate it here, while
          ;; the tree is still writable, via `lake env'.
          (replace 'build
            (lambda _
              (setenv "HOME" (getcwd))
              (setenv "LAKE_NO_CACHE" "1")
              (setenv "RESERVOIR_API_URL" "")
              (setenv "LEAN_CC" "gcc")
              (mkdir-p ".lake/build")
              (invoke "tar" "xzf" #$proofwidgets-release "-C" ".lake/build")
              ;; Run a full `lake build' here, while the tree is writable.  The
              ;; overlaid prebuilt `js/' widgets satisfy the `widgetJsAll'
              ;; target offline (no npm), and this materialises *every* trace
              ;; file (module setups, widget/package-lock.json.hash, the
              ;; elaborated config, ...).  Once present, a read-only consumer
              ;; replays without writing anything.
              (invoke "lake" "build" "ProofWidgets" "--no-ansi")
              ;; Lake caches the elaborated config under `[anonymous]' when the
              ;; package is built standalone, but a consumer looks it up by
              ;; package name.  Rename so the cache is found (matching Nixpkgs).
              (when (file-exists? ".lake/config/[anonymous]")
                (rename-file ".lake/config/[anonymous]"
                             ".lake/config/proofwidgets"))))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (mkdir-p out)
                (copy-recursively "." out)
                (call-with-output-file
                    (string-append out "/.lake-package-name")
                  (lambda (port) (display "proofwidgets" port)))))))))
    (inputs (list lean4-patched-base))
    (home-page "https://github.com/leanprover-community/ProofWidgets4")
    (synopsis "User-interface components for Lean 4 (ProofWidgets)")
    (description "ProofWidgets4 provides a library of user-interface
components and infrastructure for Lean 4 proofs.  This package combines the
upstream source with its prebuilt release artifacts to avoid a Node toolchain.")
    (license license:asl2.0)))

;;; ---------------------------------------------------------------------------
;;; Mathlib: the top of the DAG.  Builds the `Mathlib' library only (the
;;; executables are not default targets and would need native linking).  This
;;; is a very large build (thousands of modules, hours of CPU).
;;; ---------------------------------------------------------------------------

(define-public lean-mathlib
  (make-lean-lib
   "lean-mathlib" "4.28.0"
   "https://github.com/leanprover-community/mathlib4"
   "v4.28.0"
   "1y748ig23vq3ryzac1vjamvi167w1r8jk2ns25j2wyq3y5d78i7f"
   "mathlib"
   #:lake-targets '("Mathlib")
   #:propagated-inputs (list lean-batteries
                             lean-aesop
                             lean-qq
                             lean-proofwidgets
                             lean-plausible
                             lean-search-client
                             lean-import-graph)
   #:synopsis "Mathlib: the Lean 4 mathematical library"))

;;; ---------------------------------------------------------------------------
;;; Mathlib for *downstream* (editable) Lake projects.
;;;
;;; `lean-mathlib' builds standalone, so Lake caches its elaborated config under
;;; `.lake/config/[anonymous]'.  A project that `require's mathlib instead looks
;;; for `.lake/config/mathlib'.  Rather than recompile 9 GB of oleans just to
;;; rename a 624 KB directory, this package reuses lean-mathlib's whole
;;; `.lake/build' by *symlink* and only copies+renames the config dir.  It lives
;;; in the store, so the lean4 Lake patch treats it as up-to-date and never
;;; writes.  Point your project's package-overrides at this for `mathlib'.
;;; ---------------------------------------------------------------------------

(define-public lean-mathlib-dev
  (package
    (name "lean-mathlib-dev")
    (version (package-version lean-mathlib))
    (source #f)
    (build-system trivial-build-system)
    (inputs (list lean-mathlib))
    (arguments
     (list
      #:modules '((guix build utils) (ice-9 ftw) (srfi srfi-1))
      #:builder
      #~(begin
          (use-modules (guix build utils) (ice-9 ftw) (srfi srfi-1))
          (let ((m #$lean-mathlib)
                (out #$output))
            (mkdir-p out)
            ;; Symlink every top-level entry except `.lake'.
            (for-each (lambda (e)
                        (symlink (string-append m "/" e)
                                 (string-append out "/" e)))
                      (scandir m (lambda (e)
                                   (not (member e '("." ".." ".lake"))))))
            ;; Rebuild `.lake': symlink everything except `config', which we
            ;; copy with the directory renamed [anonymous] -> mathlib.
            (mkdir-p (string-append out "/.lake"))
            (for-each (lambda (e)
                        (symlink (string-append m "/.lake/" e)
                                 (string-append out "/.lake/" e)))
                      (scandir (string-append m "/.lake")
                               (lambda (e)
                                 (not (member e '("." ".." "config"))))))
            (mkdir-p (string-append out "/.lake/config"))
            (copy-recursively (string-append m "/.lake/config/[anonymous]")
                              (string-append out "/.lake/config/mathlib"))))))
    (home-page (package-home-page lean-mathlib))
    (synopsis "Mathlib with its Lake config renamed for downstream projects")
    (description "Same artifacts as @code{lean-mathlib} (shared by symlink), but
with the cached Lake configuration moved from the standalone @code{[anonymous]}
name to @code{mathlib}, so another Lake project can @code{require} it from the
read-only store.")
    (license license:asl2.0)))
