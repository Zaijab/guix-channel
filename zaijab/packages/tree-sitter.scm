;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Luis Henrique Gomes Higino <luishenriquegh2701@gmail.com>
;;; Copyright © 2022 Pierre Langlois <pierre.langlois@gmx.com>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (zaijab packages tree-sitter)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-io)
  #:use-module ((guix build node-build-system) #:prefix node:)
  #:use-module (guix build json)
  #:use-module (guix build utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (gnu packages icu4c))



;; Commentary:
;;
;; Build procedures for tree-sitter grammar packages.  This is the
;; builder-side code, which builds on top fo the node build-system.
;;
;; Tree-sitter grammars are written in JavaScript and compiled to a native
;; shared object.  The `tree-sitter generate' command invokes `node' in order
;; to evaluate the grammar.js into a grammar.json file, which is then
;; translated into C code.  We then compile the C code ourselve.  Packages
;; also sometimes add extra manually written C/C++ code.
;;
;; In order to support grammars depending on each other, such as C and C++,
;; JavaScript and TypeScript, this build-system installs the source of the
;; node module in a dedicated "js" output.
;;
;; Code:

(define* (patch-dependencies #:key inputs #:allow-other-keys)
  "Rewrite dependencies in 'package.json'.  We remove all runtime dependencies
and replace development dependencies with tree-sitter grammar node modules."

  (define (rewrite package.json)
    (map (match-lambda
           (("dependencies" @ . _)
            '("dependencies" @))
           (("devDependencies" @ . _)
            `("devDependencies" @
              ,@(filter-map (match-lambda
                              ((key . directory)
                               (let ((node-module
                                      (string-append directory
                                                     "/lib/node_modules/"
                                                     key)))
                                 (and (directory-exists? node-module)
                                      `(,key . ,node-module)))))
                            (alist-delete "node" inputs))))
           (other other))
         package.json))

  (node:with-atomic-json-file-replacement "package.json"
    (match-lambda
      (('@ . package.json)
       (cons '@ (rewrite package.json))))))

;; FIXME: The node build-system's configure phase does not support
;; cross-compiling so we re-define it.
(define* (configure #:key native-inputs inputs #:allow-other-keys)
  (invoke (search-input-file (or native-inputs inputs) "/bin/npm")
          "--offline" "--ignore-scripts" "install"))

(define* (build #:key grammar-directories #:allow-other-keys)
  (for-each (lambda (dir)
              (with-directory-excursion dir
                ;; Avoid generating binding code for other languages, we do
                ;; not support this use-case yet and it relies on running
                ;; `node-gyp' to build native addons.
                (invoke "tree-sitter" "generate" "--no-bindings")))
            grammar-directories))

(define* (check #:key grammar-directories tests? #:allow-other-keys)
  (when tests?
    (for-each (lambda (dir)
                (with-directory-excursion dir
                  (invoke "tree-sitter" "test")))
              grammar-directories)))

(define* (install #:key target grammar-directories outputs #:allow-other-keys)
  (let ((lib (string-append (assoc-ref outputs "out")
                            "/lib/tree-sitter")))
    (mkdir-p lib)
    (define (compile-language dir)
      (with-directory-excursion dir
        (let ((lang (assoc-ref (call-with-input-file "src/grammar.json"
                                 read-json)
                               "name"))
              (source-file (lambda (path)
                             (if (file-exists? path)
                                 path
                                 #f))))
          (apply invoke
                 `(,(if target
                        (string-append target "-g++")
                        "g++")
                   "-shared"
                   "-fPIC"
                   "-fno-exceptions"
                   "-O2"
                   "-g"
                   "-o" ,(string-append lib "/" lang ".so")
                   ;; An additional `scanner.{c,cc}' file is sometimes
                   ;; provided.
                   ,@(cond
                      ((source-file "src/scanner.c")
                       => (lambda (file) (list "-xc" "-std=c99" file)))
                      ((source-file "src/scanner.cc")
                       => (lambda (file) (list file)))
                      (else '()))
                   "-xc" "src/parser.c")))))
    (for-each compile-language grammar-directories)))

(define* (install-js #:key native-inputs inputs outputs #:allow-other-keys)
  (invoke (search-input-file (or native-inputs inputs) "/bin/npm")
          "--prefix" (assoc-ref outputs "js")
          "--global"
          "--offline"
          "--loglevel" "info"
          "--production"
          ;; Skip scripts to prevent building bindings via GYP.
          "--ignore-scripts"
          "install" "../package.tgz"))

(define %standard-phases
  (modify-phases node:%standard-phases
    (replace 'patch-dependencies patch-dependencies)
    (replace 'configure configure)
    (replace 'build build)
    (replace 'check check)
    (replace 'install install)
    (add-after 'install 'install-js install-js)))

(define* (tree-sitter-build #:key inputs (phases %standard-phases)
                            #:allow-other-keys #:rest args)
  (apply node:node-build #:inputs inputs #:phases phases args))

;;; tree-sitter-build-system.scm ends here


(define-public tree-sitter
  (package
    (name "tree-sitter")
    (version "0.20.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tree-sitter/tree-sitter")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1z20518snyg0zp75qgs5bxmzjqws4dd19vnp6sya494za3qp5b6d"))
              (modules '((guix build utils)))
              (snippet '(begin
                          ;; Remove bundled ICU parts
                          (delete-file-recursively "lib/src/unicode")
                          #t))))
    (build-system gnu-build-system)
    (inputs (list icu4c))
    (arguments
     (list #:phases
           '(modify-phases %standard-phases
              (delete 'configure))
           #:tests? #f ; there are no tests for the runtime library
           #:make-flags
           #~(list (string-append "PREFIX="
                                  #$output)
                   (string-append "CC="
                                  #$(cc-for-target)))))
    (home-page "https://tree-sitter.github.io/tree-sitter/")
    (synopsis "Incremental parsing system for programming tools")
    (description
     "Tree-sitter is a parser generator tool and an incremental parsing
library.  It can build a concrete syntax tree for a source file and efficiently
update the syntax tree as the source file is edited.

Tree-sitter aims to be:

@itemize
@item General enough to parse any programming language
@item Fast enough to parse on every keystroke in a text editor
@item Robust enough to provide useful results even in the presence of syntax errors
@item Dependency-free so that the runtime library (which is written in pure C)
can be embedded in any application
@end itemize

This package includes the @code{libtree-sitter} runtime library.
")
    (license license:expat)))

(define-public tree-sitter-cli
  (package (inherit tree-sitter)
    (name "tree-sitter-cli")
    (source (origin
              (inherit (package-source tree-sitter))
              (snippet
               '(begin
                  ;; Remove the runtime library code and dynamically link to
                  ;; it instead.
                  (delete-file-recursively "lib/src")
                  (delete-file "lib/binding_rust/build.rs")
                  (with-output-to-file "lib/binding_rust/build.rs"
                    (lambda _
                      (format #t "fn main() {~@
                              println!(\"cargo:rustc-link-lib=tree-sitter\");~@
                              }~%")))
                  #t))))
    (build-system cargo-build-system)
    (inputs (list tree-sitter))
    (arguments
     `(;; Running test requires downloading fixtures, see the
       ;; script/fetch-fixtures script.
       #:tests? #f
       ;; We're only packaging the CLI program so we do not need to install
       ;; sources.
       #:install-source? #f
       #:cargo-inputs
       (("rust-ansi-term" ,rust-ansi-term-0.12)
        ("rust-anyhow" ,rust-anyhow-1)
        ("rust-atty" ,rust-atty-0.2)
        ("rust-clap" ,rust-clap-2)
        ("rust-difference" ,rust-difference-2)
        ("rust-html-escape" ,rust-html-escape-0.2)
        ("rust-libloading" ,rust-libloading-0.7)
        ("rust-rand" ,rust-rand-0.8)
        ("rust-rustc-hash" ,rust-rustc-hash-1)
        ("rust-semver" ,rust-semver-1)
        ("rust-smallbitvec" ,rust-smallbitvec-2)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-tiny-http" ,rust-tiny-http-0.8)
        ("rust-toml" ,rust-toml-0.5)
        ("rust-walkdir" ,rust-walkdir-2)
        ("rust-webbrowser" ,rust-webbrowser-0.5)
        ("rust-which" ,rust-which-4))
       #:cargo-development-inputs
       (("rust-pretty-assertions" ,rust-pretty-assertions-0.7))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'delete-cargo.lock
           (lambda _ (delete-file "Cargo.lock")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib")))
               (mkdir-p bin)
               (install-file "target/release/tree-sitter" bin)))))))
    (synopsis "Incremental parsing system for programming tools")
    (description "Tree-sitter is a parser generator tool and an incremental
parsing library.  It can build a concrete syntax tree for a source file and
efficiently update the syntax tree as the source file is edited.

Tree-sitter aims to be:

@enumerate
@item General enough to parse any programming language.
@item Fast enough to parse on every keystroke in a text editor.
@item Robust enough to provide useful results even in the presence of syntax
errors.
@item Dependency-free so that the runtime library (which is written in pure C)
can be embedded in any application.
@end enumerate

This package includes the @command{tree-sitter} command-line tool.")
    (license license:expat)))

(define-public rust-tree-sitter
  (package
    (name "rust-tree-sitter")
    (version "0.20.6")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "tree-sitter" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0wcvxgnvj7ga1y7xa7wm0pmabkfj8936ifg8jacd4201cj0vgcq9"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Remove the runtime library code and dynamically link to
                  ;; it instead.
                  (delete-file-recursively "src")
                  (delete-file "binding_rust/build.rs")
                  (with-output-to-file "binding_rust/build.rs"
                    (lambda _
                      (format #t "fn main() {~@
                              println!(\"cargo:rustc-link-lib=tree-sitter\");~@
                              }~%")))
                  #t))))
    (build-system cargo-build-system)
    (inputs (list tree-sitter))
    (arguments
     `(#:cargo-inputs
       (("rust-cc" ,rust-cc-1)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-regex" ,rust-regex-1)
        ("rust-spin" ,rust-spin-0.7))))
    (home-page "https://tree-sitter.github.io/tree-sitter/")
    (synopsis "Rust bindings to the Tree-sitter parsing library")
    (description "This package provides Rust bindings to the Tree-sitter
parsing library.")
    (license license:expat)))

;; We need to apply a patch in order to compile the rust bindings against the
;; emacs tree-sitter module.
;; See https://github.com/tree-sitter/tree-sitter/pull/1294
(define-public rust-tree-sitter-for-emacs
  (package (inherit rust-tree-sitter)
    (source (origin
              (inherit (package-source rust-tree-sitter))
              (patches (search-patches
                        "rust-tree-sitter-text-provider-fix.patch"))))
    ;; Do not show this package in the UI as it's only meant to be used for
    ;; emacs's tree-sitter module.
    (properties '((hidden? . #t)))))

(define tree-sitter-delete-generated-files
  '(begin
     (delete-file "binding.gyp")
     (delete-file-recursively "bindings")
     (delete-file "src/grammar.json")
     (delete-file "src/node-types.json")
     (delete-file "src/parser.c")
     (delete-file-recursively "src/tree_sitter")
     #t))

(define-public tree-sitter-c
  (package
    (name "tree-sitter-c")
    (version "0.20.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tree-sitter/tree-sitter-c")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0454jziys33i4kbwnvi9xcck0fzya792ghy32ahgk1hhv96xga9w"))
              (modules '((guix build utils)))
              (snippet tree-sitter-delete-generated-files)))
    (build-system tree-sitter-build-system)
    (home-page "https://github.com/tree-sitter/tree-sitter-c")
    (synopsis "Tree-sitter C grammar")
    (description
     "This package provides a C grammar for the Tree-sitter library.")
    (license license:expat)))

(define-public tree-sitter-cpp
  (package
    (name "tree-sitter-cpp")
    (version "0.20.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tree-sitter/tree-sitter-cpp")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0hxcpdvyyig8njga1mxp4qcnbbnr1d0aiy27vahijwbh98b081nr"))
              (modules '((guix build utils)))
              (snippet tree-sitter-delete-generated-files)))
    (build-system tree-sitter-build-system)
    (inputs (list tree-sitter-c))
    (home-page "https://github.com/tree-sitter/tree-sitter-cpp")
    (synopsis "Tree-sitter C++ grammar")
    (description
     "This package provides a C++ grammar for the Tree-sitter library.")
    (license license:expat)))

(define-public tree-sitter-c-sharp
  (package
    (name "tree-sitter-c-sharp")
    (version "0.19.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tree-sitter/tree-sitter-c-sharp")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "054fmpf47cwh59gbg00sc0nl237ba4rnxi73miz39yqzcs87055r"))
              (modules '((guix build utils)))
              (snippet tree-sitter-delete-generated-files)))
    (build-system tree-sitter-build-system)
    (home-page "https://github.com/tree-sitter/tree-sitter-c-sharp")
    (synopsis "Tree-sitter C# grammar")
    (description
     "This package provides a C# grammar for the Tree-sitter library.")
    (license license:expat)))

(define-public tree-sitter-bash
  (package
    (name "tree-sitter-bash")
    (version "0.19.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tree-sitter/tree-sitter-bash")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "18c030bb65r50i6z37iy7jb9z9i8i36y7b08dbc9bchdifqsijs5"))
              (modules '((guix build utils)))
              (snippet tree-sitter-delete-generated-files)))
    (build-system tree-sitter-build-system)
    (home-page "https://github.com/tree-sitter/tree-sitter-bash")
    (synopsis "Tree-sitter Bash grammar")
    (description
     "This package provides a Bash grammar for the Tree-sitter library.")
    (license license:expat)))

(define-public tree-sitter-css
  (package
    (name "tree-sitter-css")
    (version "0.19.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tree-sitter/tree-sitter-css")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "014jrlgi7zfza9g38hsr4vlbi8964i5p7iglaih6qmzaiml7bja2"))
              (modules '((guix build utils)))
              (snippet tree-sitter-delete-generated-files)))
    (build-system tree-sitter-build-system)
    (home-page "https://github.com/tree-sitter/tree-sitter-css")
    (synopsis "Tree-sitter CSS grammar")
    (description
     "This package provides a CSS grammar for the Tree-sitter library.")
    (license license:expat)))

(define-public tree-sitter-elixir
  (let ((commit "de20391afe5cb03ef1e8a8e43167e7b58cc52869")
        (revision "1"))
    (package
      (name "tree-sitter-elixir")
      (version (git-version "0.19.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/elixir-lang/tree-sitter-elixir")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0zrkrwhw3g1vazsxcwrfd1fk4wvs9hdwmwp6073mfh370bz4140h"))
                (modules '((guix build utils)))
                (snippet tree-sitter-delete-generated-files)))
      (build-system tree-sitter-build-system)
      (home-page "https://elixir-lang.org/tree-sitter-elixir/")
      (synopsis "Tree-sitter Elixir grammar")
      (description
       "This package provides a Elixir grammar for the Tree-sitter library.")
      (license (list license:asl2.0
                     ;; Files in tests/corpus are under MIT license.
                     license:expat)))))

(define-public tree-sitter-elm
  (package
    (name "tree-sitter-elm")
    (version "5.5.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/elm-tooling/tree-sitter-elm")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "10hbi4vyj4hjixqswdcbvzl60prldczz29mlp02if61wvwiwvqrw"))
              (modules '((guix build utils)))
              (snippet tree-sitter-delete-generated-files)))
    (build-system tree-sitter-build-system)
    (home-page "https://elm-tooling.github.io/tree-sitter-elm/")
    (synopsis "Tree-sitter Elm grammar")
    (description
     "This package provides an Elm grammar for the Tree-sitter library.")
    (license license:expat)))

(define-public tree-sitter-go
  (package
    (name "tree-sitter-go")
    (version "0.19.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tree-sitter/tree-sitter-go")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0nxs47vd2fc2fr0qlxq496y852rwg39flhg334s7dlyq7d3lcx4x"))
              (modules '((guix build utils)))
              (snippet tree-sitter-delete-generated-files)))
    (build-system tree-sitter-build-system)
    (home-page "https://github.com/tree-sitter/tree-sitter-go")
    (synopsis "Tree-sitter Go grammar")
    (description
     "This package provides a Golang grammar for the Tree-sitter library.")
    (license license:expat)))

(define-public tree-sitter-html
  (package
    (name "tree-sitter-html")
    (version "0.19.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tree-sitter/tree-sitter-html")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1hg7vbcy7bir6b8x11v0a4x0glvqnsqc3i2ixiarbxmycbgl3axy"))
              (modules '((guix build utils)))
              (snippet tree-sitter-delete-generated-files)))
    (build-system tree-sitter-build-system)
    (home-page "https://github.com/tree-sitter/tree-sitter-html")
    (synopsis "Tree-sitter HTML grammar")
    (description
     "This package provides a HTML grammar for the Tree-sitter library.")
    (license license:expat)))

(define-public tree-sitter-java
  (package
    (name "tree-sitter-java")
    (version "0.19.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tree-sitter/tree-sitter-java")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "07zw9ygb45hnvlx9qlz7rlz8hc3byjy03d24v72i5iyhpiiwlhvl"))
              (modules '((guix build utils)))
              (snippet tree-sitter-delete-generated-files)))
    (build-system tree-sitter-build-system)
    (home-page "https://github.com/tree-sitter/tree-sitter-java")
    (synopsis "Tree-sitter Java grammar")
    (description
     "This package provides a Java grammar for the Tree-sitter library.")
    (license license:expat)))

(define-public tree-sitter-javascript
  (package
    (name "tree-sitter-javascript")
    (version "0.20.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tree-sitter/tree-sitter-javascript")
                    (commit (string-append "rust-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "175yrk382n2di0c2xn4gpv8y4n83x1lg4hqn04vabf0yqynlkq67"))
              (modules '((guix build utils)))
              (snippet tree-sitter-delete-generated-files)))
    (build-system tree-sitter-build-system)
    (home-page "https://github.com/tree-sitter/tree-sitter-javascript")
    (synopsis "Tree-sitter Javascript grammar")
    (description
     "This package provides Javascript and JSX grammars for the Tree-sitter
library.")
    (license license:expat)))

(define-public tree-sitter-json
  (package
    (name "tree-sitter-json")
    (version "0.19.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tree-sitter/tree-sitter-json")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "06pjh31bv9ja9hlnykk257a6zh8bsxg2fqa54al7qk1r4n9ksnff"))
              (modules '((guix build utils)))
              (snippet tree-sitter-delete-generated-files)))
    (build-system tree-sitter-build-system)
    (home-page "https://github.com/tree-sitter/tree-sitter-json")
    (synopsis "Tree-sitter JSON grammar")
    (description
     "This package provides a JSON grammar for the Tree-sitter library.")
    (license license:expat)))

(define-public tree-sitter-julia
  (package
    (name "tree-sitter-julia")
    (version "0.19.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tree-sitter/tree-sitter-julia")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1pbnmvhy2gq4vg1b0sjzmjm4s2gsgdjh7h01yj8qrrqbcl29c463"))
              (modules '((guix build utils)))
              (snippet tree-sitter-delete-generated-files)))
    (build-system tree-sitter-build-system)
    (home-page "https://github.com/tree-sitter/tree-sitter-julia")
    (synopsis "Tree-sitter Julia grammar")
    (description
     "This package provides a Julia grammar for the Tree-sitter library.")
    (license license:expat)))

(define-public tree-sitter-ocaml
  (let ((commit "0348562f385bc2bd67ecf181425e1afd6d454192")
        (revision "1"))
    (package
      (name "tree-sitter-ocaml")
      (version (git-version "0.19.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/tree-sitter/tree-sitter-ocaml")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0iqmwcz3c2ai4gyx4xli1rhn6hi6a0f60dn20f8jas9ham9dc2df"))
                (modules '((guix build utils)))
                (snippet
                 '(begin
                    (delete-file "binding.gyp")
                    (delete-file-recursively "bindings")
                    (for-each
                     (lambda (lang)
                       (with-directory-excursion lang
                         (delete-file "src/grammar.json")
                         (delete-file "src/node-types.json")
                         (delete-file "src/parser.c")
                         (delete-file-recursively "src/tree_sitter")))
                     '("ocaml" "interface"))
                    #t))))
      (build-system tree-sitter-build-system)
      (arguments
       (list
        #:grammar-directories '("ocaml" "interface")))
      (home-page "https://github.com/tree-sitter/tree-sitter-ocaml")
      (synopsis "Tree-sitter OCaml grammar")
      (description
       "This package provides a OCaml grammar for the Tree-sitter library.")
      (license license:expat))))

(define-public tree-sitter-php
  (let ((commit "435fa00006c0d1515c37fbb4dd6a9de284af75ab")
        (revision "1"))
    (package
      (name "tree-sitter-php")
      (version (git-version "0.19.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/tree-sitter/tree-sitter-php")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "05k4h58gi616gv41r0qqdb1x4rs8y94vghn2r10yczisgzq4vbad"))
                (modules '((guix build utils)))
                (snippet
                 '(begin
                    (delete-file "src/grammar.json")
                    (delete-file "src/node-types.json")
                    (delete-file "src/parser.c")
                    (delete-file-recursively "src/tree_sitter")
                    #t))))
    (build-system tree-sitter-build-system)
    (home-page "https://github.com/tree-sitter/tree-sitter-php")
    (synopsis "Tree-sitter PHP grammar")
    (description
     "This package provides a PHP grammar for the Tree-sitter library.")
    (license license:expat))))

(define-public tree-sitter-python
  (let ((commit "ed0fe62e55dc617ed9dec8817ebf771aa7cf3c42")
        (revision "1"))
    (package
      (name "tree-sitter-python")
      (version (git-version "0.19.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/tree-sitter/tree-sitter-python")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0wrfpg84mc3pzcrdi6n5fqwijkqr1nj5sqfnayb502krvqpjilal"))
                (modules '((guix build utils)))
                (snippet tree-sitter-delete-generated-files)))
    (build-system tree-sitter-build-system)
    (home-page "https://github.com/tree-sitter/tree-sitter-python")
    (synopsis "Tree-sitter Python grammar")
    (description
     "This package provides a Python grammar for the Tree-sitter library.")
    (license license:expat))))

(define-public tree-sitter-ruby
  (package
    (name "tree-sitter-ruby")
    (version "0.19.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tree-sitter/tree-sitter-ruby")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0m3h4928rbs300wcb6776h9r88hi32rybbhcaf6rdympl5nzi83v"))
              (modules '((guix build utils)))
              (snippet tree-sitter-delete-generated-files)))
    (build-system tree-sitter-build-system)
    (home-page "https://github.com/tree-sitter/tree-sitter-ruby")
    (synopsis "Tree-sitter Ruby grammar")
    (description
     "This package provides a Ruby grammar for the Tree-sitter library.")
    (license license:expat)))

(define-public tree-sitter-rust
  (package
    (name "tree-sitter-rust")
    (version "0.20.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tree-sitter/tree-sitter-rust")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "174j5pxwf80c4xniri39l3a6bb7nq96g2s8hh5sgv4i7xvbpfsmg"))
              (modules '((guix build utils)))
              (snippet tree-sitter-delete-generated-files)))
    (build-system tree-sitter-build-system)
    (home-page "https://github.com/tree-sitter/tree-sitter-rust")
    (synopsis "Tree-sitter Rust grammar")
    (description
     "This package provides a Rust grammar for the Tree-sitter library.")
    (license license:expat)))

(define-public tree-sitter-typescript
  (package
    (name "tree-sitter-typescript")
    (version "0.20.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tree-sitter/tree-sitter-typescript")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "07fl9d968lal0aqj4f0n16p3n94cjkgfp54wynfr8gbdkjss5v5x"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (delete-file "binding.gyp")
                  (delete-file-recursively "bindings")
                  (for-each
                   (lambda (lang)
                     (with-directory-excursion lang
                       (delete-file "src/grammar.json")
                       (delete-file "src/node-types.json")
                       (delete-file "src/parser.c")
                       (delete-file-recursively "src/tree_sitter")))
                   '("typescript" "tsx"))
                  #t))))
    (build-system tree-sitter-build-system)
    (inputs (list tree-sitter-javascript))
    (arguments
     (list
      #:grammar-directories '("typescript" "tsx")))
    (home-page "https://github.com/tree-sitter/tree-sitter-typescript")
    (synopsis "Tree-sitter Typescript grammar")
    (description
     "This package provides Typescript and TSX grammars for the Tree-sitter
library.")
    (license license:expat)))

;; Local package definition solely for building the native emacs module
;; written in Rust.
(define tree-sitter-emacs-module
  (package
    (name "tree-sitter-emacs-module")
    (version "0.18.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/emacs-tree-sitter/elisp-tree-sitter")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1sdvz827v436qijs6xafakkfw2d16bvp8frymd818rppjc7a9dif"))))
    (build-system cargo-build-system)
    (inputs
     (list tree-sitter))
    (arguments
     `(#:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-emacs" ,rust-emacs-0.18)
        ("rust-libloading" ,rust-libloading-0.7)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-tree-sitter" ,rust-tree-sitter-for-emacs))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "core")))
         (add-after 'chdir 'delete-cargo.lock
           (lambda _ (delete-file "Cargo.lock")))
         (add-after 'delete-cargo.lock 'do-not-fetch-from-github
           (lambda _
             (substitute* "Cargo.toml"
               (("\\[patch.*") "")
               (("git = .*") ""))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((lib (string-append (assoc-ref outputs "out") "/lib")))
               (mkdir-p lib)
               (copy-file "target/release/libtsc_dyn.so"
                          (string-append lib "/tsc-dyn.so"))))))))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license license:expat)))

(define-public emacs-tree-sitter-core
  (package
    (name "emacs-tree-sitter-core")
    (version (package-version tree-sitter-emacs-module))
    (source (package-source tree-sitter-emacs-module))
    (build-system emacs-build-system)
    (native-inputs
     (list tree-sitter-emacs-module))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "core")))
         (add-before 'install 'find-tsc-dyn
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((elpa (elpa-directory (assoc-ref outputs "out")))
                   (module (search-input-file inputs "/lib/tsc-dyn.so")))
               ;; Install the tsc-dyn module in site-lisp and the current
               ;; directory for test to pass.
               (install-file module elpa)
               (copy-file module "tsc-dyn.so")
               ;; We replace the tsc-dyn-get.el file with an empty stub to
               ;; prevent the code from downloading the module.
               (call-with-output-file "tsc-dyn-get.el"
                 (lambda (port)
                   (for-each
                    (lambda (sexp) (write sexp port))
                    '((defun tsc-dyn-get-ensure (requested)
                        nil)
                      (provide 'tsc-dyn-get)))))))))))
    (home-page "https://github.com/emacs-tree-sitter/elisp-tree-sitter")
    (synopsis "Tree-sitter bindings for Emacs Lisp, core library")
    (description "This package provides core APIs of the Emacs binding for
Tree-sitter, an incremental parsing system.")
    (license license:expat)))

(define-public emacs-tree-sitter
  (package
    (name "emacs-tree-sitter")
    (version (package-version emacs-tree-sitter-core))
    (source (package-source emacs-tree-sitter-core))
    (build-system emacs-build-system)
    (propagated-inputs
     (list emacs-tree-sitter-core))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "lisp"))))))
    (home-page "https://github.com/emacs-tree-sitter/elisp-tree-sitter")
    (synopsis "Tree-sitter bindings for Emacs Lisp")
    (description "This package provides Emacs bindings for Tree-sitter, an
incremental parsing library.  It aims to be the foundation for a new breed of
Emacs packages that understand code structurally.  For example:

@enumerate
@item Faster, fine-grained code highlighting.
@item More flexible code folding.
@item Structural editing (like Paredit, or even better) for non-Lisp code.
@item More informative indexing for imenu.
@end enumerate")
    (license license:expat)))

(define emacs-tree-sitter-langs-grammar-bundle
  (package
    (name "emacs-tree-sitter-langs-grammar-bundle")
    (source #f)
    (version (package-version tree-sitter))
    (build-system trivial-build-system)
    (inputs
     ;; FIXME: Support for some languages is still left to package.
     (list tree-sitter-bash
           tree-sitter-c
           tree-sitter-c-sharp
           tree-sitter-cpp
           tree-sitter-css
           tree-sitter-elixir
           tree-sitter-elm
           tree-sitter-go
           tree-sitter-html
           tree-sitter-java
           tree-sitter-javascript
           tree-sitter-json
           tree-sitter-julia
           tree-sitter-ocaml
           tree-sitter-php
           tree-sitter-python
           tree-sitter-rust
           tree-sitter-ruby
           tree-sitter-typescript))
    (arguments
     (list #:builder
           (with-imported-modules '((guix build union)
                                    (guix build utils))
             #~(begin
                 (use-modules (ice-9 match)
                              (guix build union)
                              (guix build utils))
                 (union-build
                  #$output
                  (filter directory-exists?
                          (map (match-lambda
                                 ((name directory)
                                  (string-append directory "/lib/tree-sitter")))
                               '#$(package-inputs this-package))))))))
    (synopsis #f)
    (description #f)
    (home-page #f)
    (license #f)))

(define-public emacs-tree-sitter-langs
  (package
    (name "emacs-tree-sitter-langs")
    (version "0.12.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/emacs-tree-sitter/tree-sitter-langs")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1p2zbb6ac7wi6x6zpbczcmpkb2p45md2csd2bj43d8s56ckzw5mp"))))
    (build-system emacs-build-system)
    (inputs
     (list emacs-tree-sitter-langs-grammar-bundle))
    (propagated-inputs
     (list emacs-tree-sitter))
    (arguments
     (list
      #:tests? #t
      #:test-command ''("script/test")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-downloader
            (lambda _
              (call-with-output-file "tree-sitter-langs-build.el"
                (lambda (port)
                  (let ((on-load-message
                         (string-append
                          "tree-sitter-langs: Grammar bundle already installed "
                          "via Guix.  Installing external grammars via this "
                          "function isn't supported, if a language you need is "
                          "missing please report a bug at bug-guix@gnu.org.")))
                    (format
                     port
                     ";;;###autoload
                      (defun tree-sitter-langs-install-grammars
                             (&optional skip-if-installed version os
                                        keep-bundle)
                        (interactive)
                        (message \"~a\"))
                      (defconst tree-sitter-langs--queries-dir
                        (file-name-as-directory
                          (concat (file-name-directory (locate-library \"tree-sitter-langs.el\"))
                                  \"queries\")))
                      (defun tree-sitter-langs--bin-dir () \"~a\")
                      (provide 'tree-sitter-langs-build)"
                     on-load-message
                     #$emacs-tree-sitter-langs-grammar-bundle))))))
          (add-after 'unpack 'remove-cask
            (lambda _
              (substitute* "script/test"
                (("cask") ""))))
          (add-before 'check 'patch-tree-sitter-require-test
            (lambda _
              (use-modules (ice-9 regex))
              ;; This test needs a git repositories with submodules for
              ;; each languages in order to map all repositories.  We patch
              ;; the mapping function with one that invokes the tests for each
              ;; packaged language.
              (let ((supported-languages
                     (map (lambda (lib)
                            (match:substring
                             (string-match "(.*)\\.so$" (basename lib))
                             1))
                          (find-files "bin" "\\.so$"))))
                (substitute* "tree-sitter-langs-tests.el"
                  (("tree-sitter-langs--map-repos")
                   (call-with-output-string
                     (lambda (port)
                       (write `(lambda (fn)
                                 (dolist (lang ',supported-languages)
                                         (funcall fn lang)))
                              port))))))))
          ;; Tests for queries will fail given those languages are not
          ;; packages yet.
          (add-before 'check 'remove-unused-highlight-queries
            (lambda _
              (delete-file-recursively "queries/hcl")
              (delete-file-recursively "queries/pgn")))
          (add-after 'install 'install-queries
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((elpa (elpa-directory (assoc-ref outputs "out"))))
                (copy-recursively "queries" (string-append elpa "/queries"))))))))
    (home-page "https://ubolonton.github.io/emacs-tree-sitter/languages/")
    (synopsis "Language support bundle for Tree-sitter")
    (description "This package is a convenient language bundle for
Tree-sitter.  For each supported language, this package provides:

@enumerate
@item Pre-compiled grammar binaries.
@item An optional highlights.scm file that provides highlighting patterns.
This is mainly intended for major modes that are not aware of tree-sitter.
@item Optional query patterns for other minor modes that provide high-level
functionalities on top of tree-sitter, such as code folding, evil text
objects, ...etc.
@end enumerate")
    (license license:expat)))
