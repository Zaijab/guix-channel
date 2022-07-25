(define-module (gnu packages firmware-extra)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (gnu packages)
  #:use-module (gnu packages firmware)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

(define-public ovmf-with-vars
  (package
    (inherit ovmf)
    (name "ovmf-with-vars")
    (arguments
     `(#:tests? #f ; No check target.
       #:phases
       (modify-phases %standard-phases
         ;; Hide the default GCC from CPLUS_INCLUDE_PATH to prevent it from
         ;; shadowing the version of GCC provided in native-inputs.
         (add-after 'set-paths 'hide-gcc7
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gcc (assoc-ref inputs "gcc")))
               (setenv "CPLUS_INCLUDE_PATH"
                       (string-join
                        (delete (string-append gcc "/include/c++")
                                (string-split (getenv "CPLUS_INCLUDE_PATH")
                                              #\:))
                        ":"))
               #t)))
         (replace 'configure
           (lambda _
             (let* ((cwd (getcwd))
                    (tools (string-append cwd "/BaseTools"))
                    (bin (string-append tools "/BinWrappers/PosixLike")))
               (setenv "WORKSPACE" cwd)
               (setenv "EDK_TOOLS_PATH" tools)
               (setenv "PATH" (string-append (getenv "PATH") ":" bin))
               ; FIXME: The below script errors out. When using 'invoke' instead
               ; of 'system*' this causes the build to fail.
               (system* "bash" "edksetup.sh")
               (substitute* "Conf/target.txt"
                 (("^TARGET[ ]*=.*$") "TARGET = RELEASE\n")
                 (("^MAX_CONCURRENT_THREAD_NUMBER[ ]*=.*$")
                  (format #f "MAX_CONCURRENT_THREAD_NUMBER = ~a~%"
                          (number->string (parallel-job-count)))))
               ;; Build build support.
               (setenv "BUILD_CC" "gcc")
               (invoke "make" "-C" tools)
               #t)))
         (replace 'build
           (lambda _
             (invoke "build" "-a" "IA32" "-t" "GCC49"
                     "-p" "OvmfPkg/OvmfPkgIa32.dsc")))
         ,@(if (string=? "x86_64-linux" (%current-system))
             '((add-after 'build 'build-x64
                (lambda _
                  (invoke "build" "-a" "X64" "-t" "GCC49"
                          "-p" "OvmfPkg/OvmfPkgX64.dsc"))))
             '())
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (fmw (string-append out "/share/firmware")))
               (mkdir-p fmw)
               (copy-file "Build/OvmfIa32/RELEASE_GCC49/FV/OVMF.fd"
                          (string-append fmw "/ovmf_ia32.bin"))
               (copy-file "Build/OvmfIa32/RELEASE_GCC49/FV/OVMF_VARS.fd"
                          (string-append fmw "/ovmf_vars_ia32.bin"))
               ,@(if (string=? "x86_64-linux" (%current-system))
                   '((copy-file "Build/OvmfX64/RELEASE_GCC49/FV/OVMF.fd"
                                (string-append fmw "/ovmf_x64.bin"))
                     (copy-file "Build/OvmfX64/RELEASE_GCC49/FV/OVMF_VARS.fd"
                          (string-append fmw "/ovmf_vars_x64.bin")))
                   '()))
             #t)))))))
