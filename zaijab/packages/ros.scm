;;; ROS 2 Kilted Kaiju packages for Guix.
;;;
;;; Build order follows the tier structure in ros-audit.md.
;;; All git commits pin the kilted branch HEAD as of 2026-05-10.

(define-module (zaijab packages ros)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix search-paths)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages check)          ; googletest
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages logging)        ; spdlog
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)   ; python-setuptools, python-wheel, python-pyparsing
  #:use-module (gnu packages python-web)     ; python-requests
  #:use-module (gnu packages python-xyz)     ; python-pyyaml, python-lark, python-packaging, python-docutils, pybind11
  #:use-module (gnu packages serialization)  ; libyaml
  #:use-module (gnu packages time)           ; python-dateutil
  #:use-module (gnu packages tls)            ; openssl
  #:use-module (gnu packages xml)            ; tinyxml2
  #:use-module ((guix licenses) #:prefix license:))


;;; ---------------------------------------------------------------------------
;;; Shared helpers
;;; ---------------------------------------------------------------------------

;; AMENT_PREFIX_PATH lets runtime tools discover installed ament packages.
(define %ament-search-paths
  (list (search-path-specification
         (variable "AMENT_PREFIX_PATH")
         (files '("share/ament_index/resource_index"))
         (separator ":"))))

;; Every ament cmake package needs these at configure time.
;; We bind this after ament-cmake is defined; packages below reference it.


;;; ---------------------------------------------------------------------------
;;; Tier 0 – Python tools (no ROS cmake deps)
;;; ---------------------------------------------------------------------------

(define-public python-empy
  (package
    (name "python-empy")
    (version "3.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "empy" version))
       (sha256
        (base32 "1cq1izl6l87i5i3vj0jcqfksh10kpiwpr2m19vgpj530bdw4kb3k"))))
    (build-system python-build-system)
    (arguments (list #:tests? #f))
    (home-page "https://github.com/dirk-thomas/empy")
    (synopsis "Python templating engine used by ROS 2 code generation")
    (description "empy is a Python templating system.  ROS 2 uses it to
generate C, C++, and Python source files from .em template files.")
    (license license:lgpl2.1+)))

(define-public python-ament-package
  (package
    (name "python-ament-package")
    (version "0.17.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ament/ament_package")
             (commit "9c11780af11dd87e85c066f02c712071b684fd66")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03msrjzncbhj9qa7zdc9ilspj36kqwkj02b782gn6b144l6ic2w1"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))
    (native-inputs (list python-setuptools python-wheel))
    (propagated-inputs (list python-packaging))
    (home-page "https://github.com/ament/ament_package")
    (synopsis "Parser for ament package.xml metadata")
    (description "ament_package parses package.xml manifests and provides
package metadata to ament cmake macros at build time.")
    (license license:asl2.0)))

(define-public python-catkin-pkg
  (package
    (name "python-catkin-pkg")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://files.pythonhosted.org/packages/1c/7a/dcd7ba56dc82d88b3059a6770828388fc2e136ca4c5d79003f9febf33087/catkin_pkg-1.1.0.tar.gz")
       (sha256
        (base32 "0g5lpsld9nkd2282fnsyhns0iicgrq9nc2hh19vjwxrska3vc76z"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))
    (native-inputs (list python-setuptools python-wheel))
    (propagated-inputs
     (list python-docutils python-pyyaml python-pyparsing python-dateutil))
    (home-page "https://github.com/ros-infrastructure/catkin_pkg")
    (synopsis "ROS package.xml parsing utilities")
    (description "catkin_pkg provides utilities for parsing the package.xml
manifest format used by both catkin and ament build systems.")
    (license license:bsd-3)))

(define-public python-vcstool
  (package
    (name "python-vcstool")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "vcstool" version))
       (sha256
        (base32 "0b7f68q25x9nxqa3xcg32js3qgp4jg99anwy2c7nd1jkw5iskcq4"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))
    (native-inputs (list python-setuptools python-wheel))
    (propagated-inputs (list python-pyyaml python-requests))
    (home-page "https://github.com/dirk-thomas/vcstool")
    (synopsis "Batch VCS operations across many repos via a .repos file")
    (description "vcstool provides the `vcs` command for cloning all ROS 2
source repositories at once from a ros2.repos descriptor file.")
    (license license:asl2.0)))

(define-public python-colcon-core
  (package
    (name "python-colcon-core")
    (version "0.20.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "colcon-core" version))
       (sha256
        (base32 "0vd1x2aa6czp62ny4mng9381yrgmbw97zhbg3w70mlvrbjrhhy30"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))
    (native-inputs (list python-setuptools python-wheel))
    (propagated-inputs (list python-packaging python-setuptools))
    (home-page "https://colcon.readthedocs.io")
    (synopsis "Core of the colcon ROS 2 workspace build tool")
    (description "colcon-core is the foundation of the colcon build
orchestrator used to build, test, and install ROS 2 packages from source.")
    (license license:asl2.0)))

(define-public python-colcon-cmake
  (package
    (name "python-colcon-cmake")
    (version "0.2.29")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "colcon-cmake" version))
       (sha256
        (base32 "0s8zpwz0idcv6rhiwq7gvw1hlalyslkrqrfhz0wbglrh4m7a3h7i"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))
    (native-inputs (list python-setuptools python-wheel))
    (propagated-inputs (list python-colcon-core))
    (home-page "https://colcon.readthedocs.io")
    (synopsis "CMake build type support for colcon")
    (description "colcon-cmake adds CMake and ament_cmake build type support
to colcon so it can build ROS 2 packages that use CMake.")
    (license license:asl2.0)))


;;; ---------------------------------------------------------------------------
;;; Tier 1 – Fast DDS / middleware C++ deps (no ament)
;;; ---------------------------------------------------------------------------

(define-public asio-headers
  (package
    (name "asio-headers")
    (version "1.30.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/chriskohlhoff/asio")
             (commit "42a93679dc4c8c5caf3d3082542f1bfa2438271d")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ygry6794pc7h85b94gnf2x3gps75qwvkg67pmh6j1112ll4zrl3"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'enter-asio-dir
            (lambda _ (chdir "asio")))
          (replace 'configure (lambda _ #t))
          (replace 'build     (lambda _ #t))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((inc (string-append (assoc-ref outputs "out") "/include")))
                (mkdir-p inc)
                (copy-file "include/asio.hpp"
                           (string-append inc "/asio.hpp"))
                (copy-recursively "include/asio"
                                  (string-append inc "/asio"))))))))
    (home-page "https://think-async.com/Asio/")
    (synopsis "Header-only standalone Asio async I/O library")
    (description "Asio is a cross-platform C++ async I/O library.  Fast DDS
uses the standalone (non-Boost) header-only version.")
    (license license:boost1.0)))

(define-public foonathan-memory
  (package
    (name "foonathan-memory")
    (version "1.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/eProsima/memory")
             (commit "fe698dbc956f8c728f90ce95cc18e02cb24633db")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16isi6qiml30p1a3vkxrkvzcdcxf8b31fq4p4wh11gnd7q4j1i6m"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:configure-flags
      #~(list "-DFOONATHAN_MEMORY_BUILD_TESTS=OFF"
              "-DFOONATHAN_MEMORY_BUILD_EXAMPLES=OFF"
              "-DFOONATHAN_MEMORY_BUILD_TOOLS=OFF")
      #:phases
      #~(modify-phases %standard-phases
          ;; Create a cmake shim so find_package(foonathan_memory_vendor) works.
          (add-after 'install 'install-vendor-shim
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (dir (string-append
                           out "/lib/cmake/foonathan_memory_vendor")))
                (mkdir-p dir)
                (call-with-output-file
                    (string-append
                     dir "/foonathan_memory_vendor-config.cmake")
                  (lambda (p)
                    (display
                     "find_package(foonathan_memory REQUIRED CONFIG)\n" p)
                    (display
                     "set(foonathan_memory_vendor_FOUND TRUE)\n" p)))))))))
    (native-inputs (list cmake ninja))
    (home-page "https://github.com/eProsima/memory")
    (synopsis "eProsima fork of foonathan/memory used by Fast DDS")
    (description "This is eProsima's fork of foonathan/memory, providing
custom C++ allocators used internally by Fast DDS.")
    (license license:zlib)))

(define-public console-bridge
  (package
    (name "console-bridge")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ros/console_bridge")
             (commit "81ec67f6daf3cd19ef506e00f02efb1645597b9c")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1npyh15xcav7qbrymbnpr4cnvwbd24p5yf1r4jhya4n8hpxk1vs9"))))
    (build-system cmake-build-system)
    (arguments (list #:tests? #f))
    (native-inputs (list cmake ninja))
    (home-page "https://github.com/ros/console_bridge")
    (synopsis "C++ logging bridge for ROS class_loader")
    (description "console_bridge provides a thin C++ logging facade used by
class_loader and other ROS packages.")
    (license license:bsd-3)))

(define-public console-bridge-vendor
  (package
    (name "console-bridge-vendor")
    (version "1.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ros2/console_bridge_vendor")
             (commit "96d11d3efddafabdc138756b09f4a1b1b4d60e39")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0cijc03lk6z8d7jh4xn4mkvfl60l4pcsk8nhdmajj2bx5dmc49qq"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'force-satisfied
                 (lambda _
                   (substitute* "CMakeLists.txt"
                     (("find_package\\(console_bridge[^)]*\\)" all)
                      (string-append
                       all "\nset(console_bridge_FOUND TRUE)\n"))))))))
    (native-inputs (%ament-ros-native))
    (propagated-inputs (list console-bridge))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/console_bridge_vendor")
    (synopsis "ROS 2 vendor wrapper for console_bridge")
    (description "console_bridge_vendor provides cmake config for console_bridge.
When the system console_bridge is detected, it exports it without downloading.")
    (license license:asl2.0)))

(define-public fast-cdr
  (package
    (name "fast-cdr")
    (version "2.3.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/eProsima/Fast-CDR")
             (commit "dab0618aa957ebcf7b62164cbd33ba74a50397f1")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g44vjhb0v0dnp666k2p4zzfw034jg1bs5h4bcxnpsbnmrbipclj"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")))
    (native-inputs (list cmake ninja))
    (home-page "https://github.com/eProsima/Fast-CDR")
    (synopsis "eProsima CDR serialization library")
    (description "Fast CDR provides CDR serialization as defined in the RTPS
spec.  Used by Fast DDS for wire encoding.")
    (license license:asl2.0)))

(define-public fast-dds
  (package
    (name "fast-dds")
    (version "3.2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/eProsima/Fast-DDS")
             (commit "cca41730529d9bf9b7003a7aadcc64205db3e1a8")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16wiaxsjh6cr96kazcywcjr4427dmn2x8v2wyk9hiak2cn09m3r9"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:configure-flags
      #~(list "-DBUILD_TESTING=OFF"
              "-DCOMPILE_EXAMPLES=OFF"
              (string-append "-DOPENSSL_ROOT_DIR="
                             #$(this-package-input "openssl")))))
    (native-inputs (list cmake ninja pkg-config))
    (inputs (list asio-headers fast-cdr foonathan-memory openssl tinyxml2))
    (home-page "https://fast-dds.docs.eprosima.com/")
    (synopsis "eProsima Fast DDS — default ROS 2 DDS middleware")
    (description "Fast DDS is a C++ DDS implementation and the default RMW
middleware for ROS 2.")
    (license license:asl2.0)))


;;; ---------------------------------------------------------------------------
;;; Tier 2 – ament_cmake build system
;;;
;;; ament-cmake-core bootstraps itself without needing ament_cmake installed.
;;; ament-cmake then builds ALL sub-packages in one sequential cmake loop.
;;; ---------------------------------------------------------------------------

(define %ament-cmake-version "2.7.5")
(define %ament-cmake-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/ament/ament_cmake")
          (commit "49fb40f2d3878304901c7712c3d904848907709b")))
    (file-name (git-file-name "ament-cmake" %ament-cmake-version))
    (sha256
     (base32 "1djl7rqnx1j44nlirl5hcr7xh9aaz8dw80vb89qlv7pb8b6b5av1"))))

(define-public ament-cmake-core
  (package
    (name "ament-cmake-core")
    (version %ament-cmake-version)
    (source %ament-cmake-source)
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'enter-subpackage
            (lambda _ (chdir "ament_cmake_core"))))))
    (native-inputs
     (list cmake ninja `(,python "out")
           python-ament-package python-catkin-pkg))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ament/ament_cmake")
    (synopsis "Bootstrap cmake macros for the ament build system")
    (description "ament_cmake_core provides the fundamental cmake macros used
by all ament packages.  It bootstraps without itself needing ament_cmake.")
    (license license:asl2.0)))

(define-public ament-cmake
  ;; Builds ALL ament_cmake sub-packages in the correct order.
  (package
    (name "ament-cmake")
    (version %ament-cmake-version)
    (source %ament-cmake-source)
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure (lambda _ #t))
          (replace 'build     (lambda _ #t))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out"))
                    (src (getcwd)))
                (setenv "PYTHONPATH"
                        (string-append
                         #$(this-package-native-input "python-ament-package")
                         "/lib/python3.11/site-packages:"
                         #$(this-package-native-input "python-catkin-pkg")
                         "/lib/python3.11/site-packages"))
                ;; Build order: core → export/utility pkgs → meta → auto
                (for-each
                 (lambda (subpkg)
                   (let ((bdir (string-append "/tmp/ament-build-" subpkg)))
                     (mkdir-p bdir)
                     (invoke "cmake"
                             (string-append src "/" subpkg)
                             (string-append "-DCMAKE_INSTALL_PREFIX=" out)
                             (string-append "-DCMAKE_PREFIX_PATH=" out ":"
                                            #$(this-package-native-input
                                               "ament-cmake-core") ":"
                                            #$(this-package-native-input
                                               "googletest"))
                             "-DBUILD_TESTING=OFF"
                             "-G" "Ninja"
                             "-B" bdir)
                     (invoke "cmake" "--build" bdir)
                     (invoke "cmake" "--install" bdir)))
                 (list "ament_cmake_core"
                       "ament_cmake_export_definitions"
                       "ament_cmake_export_dependencies"
                       "ament_cmake_export_include_directories"
                       "ament_cmake_export_interfaces"
                       "ament_cmake_export_libraries"
                       "ament_cmake_export_link_flags"
                       "ament_cmake_export_targets"
                       "ament_cmake_include_directories"
                       "ament_cmake_libraries"
                       "ament_cmake_python"
                       "ament_cmake_target_dependencies"
                       "ament_cmake_test"
                       "ament_cmake_version"
                       "ament_cmake_pytest"
                       "ament_cmake_gtest"
                       "ament_cmake_gmock"
                       "ament_cmake_gen_version_h"
                       "ament_cmake_vendor_package"
                       "ament_cmake"        ; meta-pkg installs the config
                       "ament_cmake_auto"))))))))
    (native-inputs
     (list ament-cmake-core cmake ninja googletest
           `(,python "out") python-ament-package python-catkin-pkg))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ament/ament_cmake")
    (synopsis "Complete ament cmake build system for ROS 2")
    (description "ament_cmake installs all cmake macro sub-packages needed
to build ROS 2 packages: dependency export, include directories, target
linking, Python module support, test integration, and vendor_package support.")
    (license license:asl2.0)))


;;; ---------------------------------------------------------------------------
;;; Helper: common native inputs for every ament cmake package
;;; ---------------------------------------------------------------------------

(define (%ament-native . rest)
  ;; python-empy: needed for em-template generation (e.g. logging_macros.h).
  ;; NOTE: ament-cmake-ros is NOT included here to avoid circular dependencies
  ;; (ament-cmake-ros uses %ament-native).  Add it explicitly to each package.
  (append (list ament-cmake cmake ninja `(,python "out")
                python-ament-package python-catkin-pkg python-empy)
          rest))

;; Convenience: %ament-ros-native adds ament-cmake-ros for packages that need it.
;; Do not use inside ament-cmake-ros or ament-cmake-ros-core definitions.
(define (%ament-ros-native . rest)
  (apply %ament-native ament-cmake-ros rest))

;; Packages transitively re-exported by rosidl-runtime-c / rmw cmake configs.
;; Any package depending on rosidl-runtime-c, rosidl-runtime-cpp, or rmw needs
;; all of these in CMAKE_PREFIX_PATH.
(define (%rosidl-transitive-deps)
  ;; rcutils: re-exported by rosidl-runtime-c.
  ;; rosidl-typesupport-c/cpp: provide cmake targets like pkg__rosidl_typesupport_cpp.
  (list rcutils rcpputils rosidl-typesupport-interface rosidl-runtime-c
        rosidl-runtime-cpp rosidl-dynamic-typesupport
        rosidl-typesupport-c rosidl-typesupport-cpp))

;; Packages re-exported by rcl-interfaces that any package using
;; rcl-interfaces must have in CMAKE_PREFIX_PATH.
(define (%rcl-interfaces-transitive-deps)
  (list builtin-interfaces service-msgs type-description-interfaces))

;; All cmake packages re-exported by rosidl-default-generators and
;; rosidl-core-generators.  Any message-generating package needs these.
(define (%rosidl-generator-deps)
  (list rosidl-cmake rosidl-adapter rosidl-core-generators rosidl-core-runtime
        rosidl-default-generators rosidl-default-runtime
        rosidl-generator-c rosidl-generator-cpp rosidl-generator-type-description))


;;; ---------------------------------------------------------------------------
;;; Tier 3 – ament_cmake_ros (extends ament_cmake for ROS 2)
;;; ---------------------------------------------------------------------------

(define %ament-cmake-ros-version "0.14.7")
(define %ament-cmake-ros-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/ros2/ament_cmake_ros")
          (commit "a2329f8b5c7d0c6160266adecab1703d2550dd58")))
    (file-name (git-file-name "ament-cmake-ros" %ament-cmake-ros-version))
    (sha256
     (base32 "176h2rw3m7kajy0g54mdb9f4iramfm8vrr34654cfj3f7as6g81s"))))

(define (ament-cmake-ros-pkg name subdir)
  (package
    (name name)
    (version %ament-cmake-ros-version)
    (source %ament-cmake-ros-source)
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'enter-subpackage
                 (lambda _ (chdir #$subdir))))))
    (native-inputs (%ament-native))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/ament_cmake_ros")
    (synopsis (string-append "ament cmake ROS extension – " subdir))
    (description "ament_cmake_ros extends ament_cmake with ROS 2 specific
cmake macros including DDS QoS policies and RMW abstractions.")
    (license license:asl2.0)))

(define-public ament-cmake-ros-core
  (ament-cmake-ros-pkg "ament-cmake-ros-core" "ament_cmake_ros_core"))

(define-public ament-cmake-ros
  (package
    (inherit (ament-cmake-ros-pkg "ament-cmake-ros" "ament_cmake_ros"))
    ;; propagated so that downstream cmake packages get ament-cmake-ros-core
    ;; added to CMAKE_PREFIX_PATH automatically.
    (propagated-inputs (list ament-cmake-ros-core))))


;;; ---------------------------------------------------------------------------
;;; Tier 4 – ament_index
;;; ---------------------------------------------------------------------------

(define %ament-index-version "1.11.3")
(define %ament-index-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/ament/ament_index")
          (commit "82b57d073e2b7cbc878360cf48a52dcedec96103")))
    (file-name (git-file-name "ament-index" %ament-index-version))
    (sha256
     (base32 "111j1xpgyx0px3979rv9397wcm6r0x71bmpdm8cd9cgh0vra8n7g"))))

;; ament_index_cpp uses cmake; ament_index_python uses ament_python.
(define-public ament-index-cpp
  (package
    (name "ament-index-cpp")
    (version %ament-index-version)
    (source %ament-index-source)
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'enter-subpackage
                 (lambda _ (chdir "ament_index_cpp"))))))
    (native-inputs (%ament-ros-native))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ament/ament_index")
    (synopsis "C++ API for the ament package resource index")
    (description "ament_index_cpp provides a C++ API for querying the ament
resource index, which packages use to register themselves and their resources.")
    (license license:asl2.0)))

(define-public ament-index-python
  ;; ament_python build type — pure Python package with setup.py.
  (package
    (name "ament-index-python")
    (version %ament-index-version)
    (source %ament-index-source)
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'enter-subpackage
                 (lambda _ (chdir "ament_index_python"))))))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/ament/ament_index")
    (synopsis "Python API for the ament package resource index")
    (description "ament_index_python provides a Python API for querying the
ament resource index, which packages use to register themselves.")
    (license license:asl2.0)))


;;; ---------------------------------------------------------------------------
;;; Tier 5 – rcpputils
;;; ---------------------------------------------------------------------------

(define-public rcpputils
  (package
    (name "rcpputils")
    (version "2.13.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ros2/rcpputils")
             (commit "bfd397e236ce2a3a86596c4819102da283da2c32")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wbfs9l6khzc3swn1qbd187bdgiqzm9ln94gm3qhdj87y3y4cpnc"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")))
    (native-inputs (%ament-native ament-cmake-ros-core))
    (inputs (list rcutils))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/rcpputils")
    (synopsis "C++ utilities for ROS 2 (file system, string, type traits)")
    (description "rcpputils provides C++ utilities commonly used by ROS 2
packages: filesystem abstraction, string utilities, and type-trait helpers.")
    (license license:asl2.0)))


;;; ---------------------------------------------------------------------------
;;; Tier 6 – tracetools (LTTng tracing disabled)
;;; ---------------------------------------------------------------------------

(define-public tracetools
  (package
    (name "tracetools")
    (version "8.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ros2/ros2_tracing")
             (commit "888e17f1f93241d942519cb1fa93224cb0ae4b24")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pdm2cgs1nzwkh90zbckavsfr0nziv0rf9rsf7mqkx9bg95q22nm"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags
           #~(list "-DBUILD_TESTING=OFF"
                   ;; Disable LTTng tracing — not available in Guix by default.
                   "-DTRACETOOLS_DISABLED=ON"
                   "-DTRACETOOLS_TRACEPOINTS_EXCLUDED=ON")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'enter-subpackage
                 (lambda _ (chdir "tracetools"))))))
    (native-inputs (%ament-native ament-cmake-ros-core pkg-config))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/ros2_tracing")
    (synopsis "ROS 2 tracing instrumentation (LTTng disabled)")
    (description "tracetools provides instrumentation macros for ROS 2.  This
build has LTTng tracing disabled; the API is still present as no-ops.")
    (license license:asl2.0)))


;;; ---------------------------------------------------------------------------
;;; Tier 7 – rcutils
;;; ---------------------------------------------------------------------------

(define-public rcutils
  (package
    (name "rcutils")
    (version "6.9.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ros2/rcutils")
             (commit "a9608e3dc26a1711845aabc4bcd6e1b1252c24bc")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16isxng4bm88impvnbw435js7cbywlcpsh82kfmq7cmxcndbii02"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags
           #~(list "-DBUILD_TESTING=OFF"
                   "-DRCUTILS_ENABLE_FAULT_INJECTION=OFF")))
    (native-inputs (%ament-native ament-cmake-ros-core))
    (inputs (list acl))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/rcutils")
    (synopsis "Common C utility functions for ROS 2")
    (description "rcutils provides C utility functions used across ROS 2:
string handling, error propagation, logging, file I/O, and time abstraction.")
    (license license:asl2.0)))


;;; ---------------------------------------------------------------------------
;;; Tier 8 – rosidl Python tools (pure Python)
;;; ---------------------------------------------------------------------------

(define %rosidl-version "4.9.6")
(define %rosidl-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/ros2/rosidl")
          (commit "609398ec5bf97fb183dc8bb403a8839dfcab9235")))
    (file-name (git-file-name "rosidl" %rosidl-version))
    (sha256
     (base32 "16czjcjig500npd3aw0qn8znxxkbf6bvzal2sk4zxf7v5a9r2k03"))))

(define (rosidl-cmake-pkg name subdir inputs)
  "Build a cmake sub-package from the rosidl monorepo.
rcutils is always included because ament's cmake export system makes it a
transitive dep for essentially every rosidl package."
  (package
    (name name)
    (version %rosidl-version)
    (source %rosidl-source)
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'enter-subpackage
                 (lambda _ (chdir #$subdir))))))
    (native-inputs (%ament-ros-native python-empy))
    (inputs (append (list rcutils) inputs))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/rosidl")
    (synopsis (string-append "ROS 2 interface definition – " subdir))
    (description "rosidl provides the tools and runtime libraries for
defining, generating, and using typed ROS 2 messages, services, and actions.")
    (license license:asl2.0)))


;;; ---------------------------------------------------------------------------
;;; Tier 9 – rosidl core generators/runtime [separate repos]
;;; ---------------------------------------------------------------------------

(define %rosidl-core-version "0.3.2")
(define %rosidl-core-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/ros2/rosidl_core")
          (commit "53bb29ee508fec0fddbc20bc793863b8c2f7063a")))
    (file-name (git-file-name "rosidl-core" %rosidl-core-version))
    (sha256
     (base32 "0b392z8475msagi5fw0fzzyfn5ch8r62wmniby5a0zjklvg2imar"))))

(define-public rosidl-core-generators
  (package
    (name "rosidl-core-generators")
    (version %rosidl-core-version)
    (source %rosidl-core-source)
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'enter-subpackage
                 (lambda _ (chdir "rosidl_core_generators"))))))
    (native-inputs (%ament-ros-native))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/rosidl_core")
    (synopsis "Meta-package grouping core rosidl generators")
    (description "rosidl_core_generators is a meta-package that groups the
core rosidl code generators required for basic message generation.")
    (license license:asl2.0)))

(define-public rosidl-core-runtime
  (package
    (name "rosidl-core-runtime")
    (version %rosidl-core-version)
    (source %rosidl-core-source)
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'enter-subpackage
                 (lambda _ (chdir "rosidl_core_runtime"))))))
    (native-inputs (%ament-ros-native))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/rosidl_core")
    (synopsis "Meta-package grouping core rosidl runtime libraries")
    (description "rosidl_core_runtime groups the runtime libraries needed by
packages that use rosidl-generated message types.")
    (license license:asl2.0)))


;;; ---------------------------------------------------------------------------
;;; Tier 10 – rosidl cmake + runtime C/C++ + generators
;;; ---------------------------------------------------------------------------

(define-public rosidl-typesupport-interface
  (rosidl-cmake-pkg "rosidl-typesupport-interface"
                    "rosidl_typesupport_interface" '()))

(define-public rosidl-cmake
  (rosidl-cmake-pkg "rosidl-cmake" "rosidl_cmake" '()))

(define-public rosidl-adapter
  (rosidl-cmake-pkg "rosidl-adapter" "rosidl_adapter" '()))

(define-public rosidl-runtime-c
  (rosidl-cmake-pkg "rosidl-runtime-c" "rosidl_runtime_c"
                    (list rcutils rosidl-typesupport-interface)))

(define-public rosidl-runtime-cpp
  ;; rosidl-runtime-c's cmake config re-exports rosidl-typesupport-interface,
  ;; so it must be in CMAKE_PREFIX_PATH here too.
  (rosidl-cmake-pkg "rosidl-runtime-cpp" "rosidl_runtime_cpp"
                    (list rosidl-runtime-c rosidl-typesupport-interface)))

(define-public rosidl-generator-type-description
  (rosidl-cmake-pkg "rosidl-generator-type-description"
                    "rosidl_generator_type_description"
                    (list rosidl-runtime-c)))

(define-public rosidl-generator-c
  (rosidl-cmake-pkg "rosidl-generator-c" "rosidl_generator_c"
                    (list rcutils rosidl-runtime-c
                          rosidl-typesupport-interface
                          rosidl-generator-type-description)))

(define-public rosidl-generator-cpp
  (rosidl-cmake-pkg "rosidl-generator-cpp" "rosidl_generator_cpp"
                    (list rosidl-runtime-cpp
                          rosidl-typesupport-interface
                          rosidl-generator-c)))


;;; ---------------------------------------------------------------------------
;;; Tier 11 – rosidl_defaults meta-packages
;;; ---------------------------------------------------------------------------

(define %rosidl-defaults-version "1.7.2")
(define %rosidl-defaults-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/ros2/rosidl_defaults")
          (commit "260080769b0954a131e120f9d60a82e9298c7342")))
    (file-name (git-file-name "rosidl-defaults" %rosidl-defaults-version))
    (sha256
     (base32 "0jnzgga1b4x97dccq329nfnhr5g2iyl16191l489i9azm888qcjp"))))

(define (rosidl-defaults-pkg name subdir)
  (package
    (name name)
    (version %rosidl-defaults-version)
    (source %rosidl-defaults-source)
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'enter-subpackage
                 (lambda _ (chdir #$subdir))))))
    (native-inputs (%ament-ros-native))
    (inputs (list rosidl-generator-c rosidl-generator-cpp
                  rosidl-generator-type-description
                  rosidl-runtime-c rosidl-runtime-cpp))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/rosidl_defaults")
    (synopsis (string-append "rosidl default package group – " subdir))
    (description "rosidl_defaults provides meta-packages that group the
default rosidl generators and runtime libraries for ROS 2.")
    (license license:asl2.0)))

(define-public rosidl-default-generators
  (rosidl-defaults-pkg "rosidl-default-generators" "rosidl_default_generators"))

(define-public rosidl-default-runtime
  (rosidl-defaults-pkg "rosidl-default-runtime" "rosidl_default_runtime"))


;;; ---------------------------------------------------------------------------
;;; Tier 12 – rosidl_dynamic_typesupport
;;; ---------------------------------------------------------------------------

(define-public rosidl-dynamic-typesupport
  (package
    (name "rosidl-dynamic-typesupport")
    (version "0.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ros2/rosidl_dynamic_typesupport")
             (commit "9de4742020d772c420ebda0c0cb33d561deae7d9")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0cqm2vzvigfkzwy4xb7dskv6ijaxb6lh6hfpbz247mlaz396lyfg"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")))
    (native-inputs (%ament-ros-native))
    (inputs (list rcutils rosidl-runtime-c rosidl-runtime-cpp
                  rosidl-typesupport-interface))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/rosidl_dynamic_typesupport")
    (synopsis "Dynamic type support for ROS 2 messages")
    (description "rosidl_dynamic_typesupport provides runtime APIs for
dynamic (not pre-generated) type introspection of ROS 2 messages.")
    (license license:asl2.0)))


;;; ---------------------------------------------------------------------------
;;; Tier 13 – base message interface packages
;;; ---------------------------------------------------------------------------

(define %rcl-interfaces-version "2.3.1")
(define %rcl-interfaces-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/ros2/rcl_interfaces")
          (commit "00e3b6a0272bf17acc540507d85af2aaf4120aff")))
    (file-name (git-file-name "rcl-interfaces" %rcl-interfaces-version))
    (sha256
     (base32 "1vkqyxhzvifyhv7v743w46axnfirb0zrp3n8barwspbmdg53wxb7"))))

(define (rcl-interfaces-pkg name subdir inputs)
  (package
    (name name)
    (version %rcl-interfaces-version)
    (source %rcl-interfaces-source)
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'enter-subpackage
                 (lambda _ (chdir #$subdir))
               ) ; add-after
               ;; AMENT_PREFIX_PATH must contain rosidl typesupport implementations
               ;; so that get_used_typesupports() finds them at cmake configure time.
               (add-before 'configure 'set-ament-prefix-path
                 (lambda _
                   (setenv "AMENT_PREFIX_PATH"
                     (string-append
                       #$(this-package-native-input "rosidl-typesupport-fastrtps-c")
                       ":"
                       #$(this-package-native-input "rosidl-typesupport-introspection-c")
                       ":"
                       #$(this-package-native-input "fast-cdr")
                     ) ; string-append
                   ) ; setenv
                 ) ; lambda
               ) ; add-before
             ) ; modify-phases gexp
        ) ; list
      ) ; arguments
    (native-inputs (append (%ament-ros-native python-empy)
                           (list fast-cdr fast-dds
                                 rosidl-typesupport-fastrtps-c
                                 rosidl-typesupport-introspection-c)))
    (inputs (append (%rosidl-generator-deps) (%rosidl-transitive-deps) inputs))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/rcl_interfaces")
    (synopsis (string-append "ROS 2 core interface messages – " subdir))
    (description "rcl_interfaces contains the message, service, and action
type definitions used internally by the ROS 2 client libraries.")
    (license license:asl2.0)))

(define-public builtin-interfaces
  (rcl-interfaces-pkg
   "builtin-interfaces" "builtin_interfaces"
   (list rosidl-runtime-c rosidl-runtime-cpp
         rosidl-default-generators rosidl-core-generators)))

(define-public service-msgs
  ;; Defined without rcl-interfaces-pkg to avoid the circular dep that arises
  ;; because the helper now includes service-msgs in all message pkg inputs.
  (package
    (name "service-msgs")
    (version %rcl-interfaces-version)
    (source %rcl-interfaces-source)
    (build-system cmake-build-system)
    (arguments
      (list
        #:tests? #f
        #:configure-flags #~(list "-DBUILD_TESTING=OFF")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'enter-subpackage
              (lambda _
                (chdir "service_msgs")
              ) ; lambda
            ) ; add-after
            (add-before 'configure 'set-ament-prefix-path
              (lambda _
                (setenv "AMENT_PREFIX_PATH"
                  (string-append
                    #$(this-package-native-input "rosidl-typesupport-fastrtps-c")
                    ":"
                    #$(this-package-native-input "rosidl-typesupport-introspection-c")
                    ":"
                    #$(this-package-native-input "fast-cdr")
                  ) ; string-append
                ) ; setenv
              ) ; lambda
            ) ; add-before
          ) ; modify-phases gexp
      ) ; list
    ) ; arguments
    (native-inputs (append (%ament-ros-native python-empy)
                           (list fast-cdr fast-dds
                                 rosidl-typesupport-fastrtps-c
                                 rosidl-typesupport-introspection-c)))
    (inputs (append (%rosidl-generator-deps) (%rosidl-transitive-deps)
                    (list builtin-interfaces)))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/rcl_interfaces")
    (synopsis "ROS 2 service metadata message types")
    (description "service_msgs provides message types describing ROS 2 service
call metadata, used internally by the client libraries.")
    (license license:asl2.0)))

(define-public type-description-interfaces
  (rcl-interfaces-pkg
   "type-description-interfaces" "type_description_interfaces"
   (list builtin-interfaces service-msgs
         rosidl-default-generators rosidl-core-generators)))

(define-public rcl-interfaces
  ;; service-msgs is explicitly needed for service interface generation.
  (rcl-interfaces-pkg
   "rcl-interfaces" "rcl_interfaces"
   (list builtin-interfaces service-msgs
         rosidl-default-generators rosidl-core-generators)))

(define-public lifecycle-msgs
  (rcl-interfaces-pkg
   "lifecycle-msgs" "lifecycle_msgs"
   (list builtin-interfaces service-msgs
         rosidl-default-generators rosidl-core-generators)))

(define-public unique-identifier-msgs
  (package
    (name "unique-identifier-msgs")
    (version "2.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ros2/unique_identifier_msgs")
             (commit "865e7d21ad21ef74c64c1a9d28130257d9b5b743")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06zl9m8clciwzck068v57mlca4w6pd5fywqw6irw9d69p00h9yfj"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")))
    (native-inputs (%ament-ros-native python-empy))
    (inputs (append (list builtin-interfaces) (%rosidl-generator-deps)))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/unique_identifier_msgs")
    (synopsis "ROS 2 unique identifier message types")
    (description "unique_identifier_msgs provides UUID message types used
by ROS 2 action servers and other components requiring unique identifiers.")
    (license license:bsd-3)))

(define-public action-msgs
  (rcl-interfaces-pkg
   "action-msgs" "action_msgs"
   (list builtin-interfaces service-msgs unique-identifier-msgs
         rosidl-default-generators rosidl-core-generators)))


;;; ---------------------------------------------------------------------------
;;; Tier 14 – rmw (middleware abstraction C API)
;;; ---------------------------------------------------------------------------

(define %rmw-version "7.8.2")
(define %rmw-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/ros2/rmw")
          (commit "f8b2cca23c744242faab4b546c972b3b28989ac0")))
    (file-name (git-file-name "rmw" %rmw-version))
    (sha256
     (base32 "1bn5pl62hzz2szi787fc1b9k0jj0lvhz2py5vwaf6vvz38vxqa9j"))))

(define-public rmw
  (package
    (name "rmw")
    (version %rmw-version)
    (source %rmw-source)
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'enter-subpackage
                 (lambda _ (chdir "rmw"))))))
    (native-inputs (%ament-native ament-cmake-ros-core))
    (inputs (list rcutils rosidl-runtime-c rosidl-typesupport-interface
                  rosidl-dynamic-typesupport))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/rmw")
    (synopsis "ROS 2 middleware abstraction C API")
    (description "rmw defines the abstract C API that decouples ROS 2 from
any specific DDS implementation, allowing the middleware to be swapped.")
    (license license:asl2.0)))

(define-public rmw-implementation-cmake
  (package
    (name "rmw-implementation-cmake")
    (version %rmw-version)
    (source %rmw-source)
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'enter-subpackage
                 (lambda _ (chdir "rmw_implementation_cmake"))))))
    (native-inputs (%ament-ros-native))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/rmw")
    (synopsis "cmake macros for selecting RMW implementations")
    (description "rmw_implementation_cmake provides cmake macros that let
packages declare which RMW implementations they support.")
    (license license:asl2.0)))


;;; ---------------------------------------------------------------------------
;;; Tier 15 – vendor packages (use Guix system libs via SATISFIED check)
;;; ---------------------------------------------------------------------------

(define-public libyaml-vendor
  (package
    (name "libyaml-vendor")
    (version "1.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ros2/libyaml_vendor")
             (commit "855754620fd05bdc7601d464958522b395435f3c")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jjdsavdzrwbn1h6cmi0h8bwq4wk28jxvs6y5zmkqlpazahf4ywc"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")
           #:phases
           #~(modify-phases %standard-phases
               ;; Guix libyaml has no cmake yaml-config.cmake so
               ;; find_package(yaml QUIET) returns FALSE and ament_vendor
               ;; tries to download via vcs/git.  Patch to force SATISFIED.
               (add-after 'unpack 'force-satisfied
                 (lambda _
                   (substitute* "CMakeLists.txt"
                     (("find_package\\(yaml[^)]*\\)" all)
                      (string-append
                       all "\nset(yaml_FOUND TRUE)\n"
                       "set(yaml_LIBRARIES yaml)\n"))))))))
    (native-inputs (%ament-ros-native))
    (propagated-inputs (list libyaml))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/libyaml_vendor")
    (synopsis "ROS 2 vendor wrapper for libyaml")
    (description "libyaml_vendor provides cmake config for libyaml.  When the
system libyaml is detected, it exports it without downloading.")
    (license license:asl2.0)))

(define-public spdlog-vendor
  (package
    (name "spdlog-vendor")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ros2/spdlog_vendor")
             (commit "b391a1d9919273f46a2f6be2496caa4ce19c3f95")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fv62vn1bmhxh01zff7fbkf20px0s3mzblyb11wqa6ammyq3vxpf"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'force-satisfied
                 (lambda _
                   (substitute* "CMakeLists.txt"
                     (("find_package\\(spdlog[^)]*\\)" all)
                      (string-append all "\nset(spdlog_FOUND TRUE)\n"))))))))
    (native-inputs (%ament-ros-native))
    ;; propagated so downstream packages get spdlog in CMAKE_PREFIX_PATH.
    (propagated-inputs (list spdlog))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/spdlog_vendor")
    (synopsis "ROS 2 vendor wrapper for spdlog")
    (description "spdlog_vendor provides cmake config for spdlog.  When the
system spdlog is detected, it exports it without downloading.")
    (license license:asl2.0)))

(define-public pybind11-vendor
  (package
    (name "pybind11-vendor")
    (version "3.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ros2/pybind11_vendor")
             (commit "ea251ffc8b07907e15b506b6a6d4caf94b1c887a")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0syd5ijmkgcbpgz7qpqv5pvqr2ji2xz56nzkl2cwhk8yahv8z63k"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'force-satisfied
                 (lambda _
                   (substitute* "CMakeLists.txt"
                     (("find_package\\(pybind11[^)]*\\)" all)
                      (string-append all "\nset(pybind11_FOUND TRUE)\n"))))))))
    (native-inputs (%ament-native `(,python "out")))
    (inputs (list pybind11 `(,python "out")))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/pybind11_vendor")
    (synopsis "ROS 2 vendor wrapper for pybind11")
    (description "pybind11_vendor provides cmake config for pybind11.  When
the system pybind11 is detected, it exports it without downloading.")
    (license license:asl2.0)))


;;; ---------------------------------------------------------------------------
;;; Tier 16 – rcl_logging
;;; ---------------------------------------------------------------------------

(define %rcl-logging-version "3.2.4")
(define %rcl-logging-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/ros2/rcl_logging")
          (commit "ff75b3715fdb5cd48a510bf690acb1ae944a680b")))
    (file-name (git-file-name "rcl-logging" %rcl-logging-version))
    (sha256
     (base32 "17zsyvqwms9jz1i112g3pfzfisbwpg78s08qqqc7h4cnq7406ywk"))))

(define-public rcl-logging-interface
  (package
    (name "rcl-logging-interface")
    (version %rcl-logging-version)
    (source %rcl-logging-source)
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'enter-subpackage
                 (lambda _ (chdir "rcl_logging_interface"))))))
    (native-inputs (%ament-native ament-cmake-ros))
    (inputs (list rcutils))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/rcl_logging")
    (synopsis "Abstract C logging interface for RCL")
    (description "rcl_logging_interface defines the C API that RCL uses to
dispatch log messages, allowing the logging backend to be swapped.")
    (license license:asl2.0)))

(define-public rcl-logging-spdlog
  (package
    (name "rcl-logging-spdlog")
    (version %rcl-logging-version)
    (source %rcl-logging-source)
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'enter-subpackage
                 (lambda _ (chdir "rcl_logging_spdlog"))))))
    (native-inputs (%ament-native ament-cmake-ros))
    (inputs (list rcl-logging-interface rcpputils rcutils spdlog-vendor))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/rcl_logging")
    (synopsis "spdlog-backed RCL logging implementation")
    (description "rcl_logging_spdlog is the default RCL logging backend,
routing ROS 2 log messages through spdlog for level-filtered output.")
    (license license:asl2.0)))

(define-public rcl-logging-noop
  (package
    (name "rcl-logging-noop")
    (version %rcl-logging-version)
    (source %rcl-logging-source)
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'enter-subpackage
                 (lambda _ (chdir "rcl_logging_noop"))))))
    (native-inputs (%ament-native ament-cmake-ros))
    (inputs (list rcl-logging-interface rcutils))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/rcl_logging")
    (synopsis "No-op RCL logging backend")
    (description "rcl_logging_noop is a drop-in RCL logging backend that
silently discards all log messages.")
    (license license:asl2.0)))


;;; ---------------------------------------------------------------------------
;;; Tier 17 – rcl_yaml_param_parser
;;; ---------------------------------------------------------------------------

(define-public rcl-yaml-param-parser
  (package
    (name "rcl-yaml-param-parser")
    (version "10.1.4")  ; same version as rcl repo
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ros2/rcl")
             (commit "7447e7f637910ad169cf3f602476707584b46576")))
       (file-name (git-file-name "rcl" "10.1.4"))
       (sha256
        (base32 "0gawq7rclk677rq8g6fy1hyax6raw65diq125c05z1iw0wbwa3mf"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'enter-subpackage
                 (lambda _ (chdir "rcl_yaml_param_parser"))))))
    (native-inputs (%ament-native ament-cmake-ros))
    (inputs (append (list libyaml-vendor rcutils rmw) (%rosidl-transitive-deps)))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/rcl")
    (synopsis "YAML parameter file parser for RCL")
    (description "rcl_yaml_param_parser parses ROS 2 parameter YAML files
into rcl parameter structures used by nodes at startup.")
    (license license:asl2.0)))


;;; ---------------------------------------------------------------------------
;;; Tier 18 – rosidl typesupport introspection
;;; ---------------------------------------------------------------------------

(define-public rosidl-typesupport-introspection-c
  (rosidl-cmake-pkg "rosidl-typesupport-introspection-c"
                    "rosidl_typesupport_introspection_c"
                    (list rcutils rosidl-runtime-c
                          rosidl-typesupport-interface
                          rosidl-generator-c)))

(define-public rosidl-typesupport-introspection-cpp
  (rosidl-cmake-pkg "rosidl-typesupport-introspection-cpp"
                    "rosidl_typesupport_introspection_cpp"
                    (list rosidl-runtime-c rosidl-runtime-cpp
                          rosidl-typesupport-interface
                          rosidl-typesupport-introspection-c)))


;;; ---------------------------------------------------------------------------
;;; Tier 19 – rosidl_typesupport_c/cpp [separate repo]
;;; ---------------------------------------------------------------------------

(define %rosidl-typesupport-version "3.3.3")
(define %rosidl-typesupport-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/ros2/rosidl_typesupport")
          (commit "45155d68397c38abd2f5ae0f6c0bd78ced782395")))
    (file-name (git-file-name "rosidl-typesupport" %rosidl-typesupport-version))
    (sha256
     (base32 "17aqk2adk1vd3pfa46rlq3jswl7mhd1f571zjqnspxadh3gk1gpy"))))

(define (rosidl-typesupport-pkg name subdir inputs)
  (package
    (name name)
    (version %rosidl-typesupport-version)
    (source %rosidl-typesupport-source)
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'enter-subpackage
                 (lambda _ (chdir #$subdir))))))
    (native-inputs (%ament-ros-native python-empy))
    (inputs inputs)
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/rosidl_typesupport")
    (synopsis (string-append "ROS 2 typesupport – " subdir))
    (description "rosidl_typesupport provides runtime type introspection
support for ROS 2 messages, enabling serialization and type discovery.")
    (license license:asl2.0)))

(define-public rosidl-typesupport-c
  (rosidl-typesupport-pkg
   "rosidl-typesupport-c" "rosidl_typesupport_c"
   (list rcpputils rcutils rosidl-runtime-c
         rosidl-typesupport-interface
         rosidl-typesupport-introspection-c)))

(define-public rosidl-typesupport-cpp
  ;; rosidl_typesupport_c cmake config calls get_used_typesupports() which
  ;; checks AMENT_PREFIX_PATH for registered typesupport implementations.
  (package
    (inherit
     (rosidl-typesupport-pkg
      "rosidl-typesupport-cpp" "rosidl_typesupport_cpp"
      (list rcpputils rcutils rosidl-typesupport-c
            rosidl-runtime-c rosidl-runtime-cpp rosidl-typesupport-interface
            rosidl-typesupport-introspection-cpp)))
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'enter-subpackage
                 (lambda _ (chdir "rosidl_typesupport_cpp")))
               (add-before 'configure 'set-ament-prefix-path
                 (lambda _
                   ;; get_used_typesupports() uses AMENT_PREFIX_PATH to find
                   ;; rosidl typesupport implementations registered in the
                   ;; ament index.  Supply the implementations we have.
                   (setenv "AMENT_PREFIX_PATH"
                           (string-append
                            #$(this-package-native-input
                               "rosidl-typesupport-fastrtps-c") ":"
                            #$(this-package-native-input
                               "rosidl-typesupport-introspection-c"))))))))
    (native-inputs
     (append (list ament-cmake ament-cmake-ros cmake ninja `(,python "out")
                   python-ament-package python-catkin-pkg python-empy
                   fast-cdr fast-dds
                   rosidl-typesupport-fastrtps-c
                   rosidl-typesupport-introspection-c)
             (%rosidl-generator-deps)))))


;;; ---------------------------------------------------------------------------
;;; Tier 20 – libstatistics_collector
;;; ---------------------------------------------------------------------------

(define-public libstatistics-collector
  (package
    (name "libstatistics-collector")
    (version "2.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ros-tooling/libstatistics_collector")
             (commit "ec4d7517e9f2098e0051d38452095163c24c3ef8")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "022w0pjk5jg4c7b521vwyajmr1a4y9kxfllmbg9y1w5d31xnlqzn"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")))
    (native-inputs (%ament-ros-native))
    (inputs (append (list builtin-interfaces service-msgs rcl rcl-interfaces
                          rcpputils rcutils rmw)
                    (%rosidl-transitive-deps)
                    (%rosidl-generator-deps)))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros-tooling/libstatistics_collector")
    (synopsis "ROS 2 statistics collection library")
    (description "libstatistics_collector provides utilities for collecting
and aggregating statistics measurements in ROS 2 nodes.")
    (license license:asl2.0)))


;;; ---------------------------------------------------------------------------
;;; Tier 21 – lifecycle_msgs + statistics_msgs + rosgraph_msgs
;;; ---------------------------------------------------------------------------

(define-public statistics-msgs
  (rcl-interfaces-pkg
   "statistics-msgs" "statistics_msgs"
   (list builtin-interfaces rosidl-default-generators rosidl-core-generators)))

(define-public rosgraph-msgs
  (rcl-interfaces-pkg
   "rosgraph-msgs" "rosgraph_msgs"
   (list builtin-interfaces rcl-interfaces
         rosidl-default-generators rosidl-core-generators)))

(define-public composition-interfaces
  (rcl-interfaces-pkg
   "composition-interfaces" "composition_interfaces"
   (list builtin-interfaces service-msgs rcl-interfaces
         rosidl-default-generators rosidl-core-generators)))


;;; ---------------------------------------------------------------------------
;;; Tier 22 – rcl (C client library)
;;; ---------------------------------------------------------------------------

(define %rcl-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/ros2/rcl")
          (commit "7447e7f637910ad169cf3f602476707584b46576")))
    (file-name (git-file-name "rcl" "10.1.4"))
    (sha256
     (base32 "0gawq7rclk677rq8g6fy1hyax6raw65diq125c05z1iw0wbwa3mf"))))

(define-public rcl
  (package
    (name "rcl")
    (version "10.1.4")
    (source %rcl-source)
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'enter-subpackage
                 (lambda _ (chdir "rcl"))))))
    (native-inputs (%ament-native ament-cmake-ros ament-cmake-ros-core))
    (inputs
     (append (list libyaml-vendor rmw rmw-implementation
                   rcl-interfaces rcl-logging-interface rcl-logging-spdlog
                   rcl-yaml-param-parser rcutils rmw-implementation-cmake
                   rosidl-typesupport-c tracetools)
             (%rcl-interfaces-transitive-deps)
             (%rosidl-transitive-deps)
             (%rosidl-generator-deps)))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/rcl")
    (synopsis "ROS 2 C client library")
    (description "rcl is the low-level C library implementing core ROS 2
concepts: nodes, topics, services, parameters, and timers.")
    (license license:asl2.0)))

(define-public rcl-action
  (package
    (name "rcl-action")
    (version "10.1.4")
    (source %rcl-source)
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'enter-subpackage
                 (lambda _ (chdir "rcl_action"))))))
    (native-inputs (%ament-native ament-cmake-ros))
    (inputs (append (list action-msgs rcl rcutils rmw)
                    (%rosidl-transitive-deps)))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/rcl")
    (synopsis "ROS 2 action client/server C library")
    (description "rcl_action provides the C API for ROS 2 action clients and
servers, built on top of rcl services and topics.")
    (license license:asl2.0)))

(define-public rcl-lifecycle
  (package
    (name "rcl-lifecycle")
    (version "10.1.4")
    (source %rcl-source)
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'enter-subpackage
                 (lambda _ (chdir "rcl_lifecycle"))))))
    (native-inputs (%ament-native ament-cmake-ros))
    (inputs (append (list lifecycle-msgs rcl rcutils rmw tracetools)
                    (%rosidl-transitive-deps)))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/rcl")
    (synopsis "ROS 2 lifecycle state machine C library")
    (description "rcl_lifecycle implements the ROS 2 managed node lifecycle
state machine at the C layer.")
    (license license:asl2.0)))


;;; ---------------------------------------------------------------------------
;;; Tier 23 – rmw_dds_common + rmw_security_common
;;; ---------------------------------------------------------------------------

(define %rmw-dds-common-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/ros2/rmw_dds_common")
          (commit "eab17912b4871ae65fd246f470372949a2e2c17c")))
    (file-name (git-file-name "rmw-dds-common" "5.0.0"))
    (sha256
     (base32 "13r5p0xrqi6myhnpsn811a2lx07p68by0nnrc2zf7af961p7zqn5"))))

(define-public rmw-security-common
  (package
    (name "rmw-security-common")
    (version %rmw-version)
    (source %rmw-source)
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'enter-subpackage
                 (lambda _ (chdir "rmw_security_common"))))))
    (native-inputs (%ament-ros-native))
    (inputs (append (list rcutils rmw) (%rosidl-transitive-deps)))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/rmw")
    (synopsis "Shared security utilities for DDS RMW implementations")
    (description "rmw_security_common provides shared security-related
utilities used by DDS-based RMW implementations.")
    (license license:asl2.0)))

(define-public rmw-dds-common
  (package
    (name "rmw-dds-common")
    (version "5.0.0")
    (source %rmw-dds-common-source)
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'enter-subpackage
                 (lambda _ (chdir "rmw_dds_common")))
               (add-before 'configure 'set-ament-prefix-path
                 (lambda _
                   (setenv "AMENT_PREFIX_PATH"
                           (string-append
                            #$(this-package-native-input
                               "rosidl-typesupport-fastrtps-c") ":"
                            #$(this-package-native-input
                               "rosidl-typesupport-introspection-c") ":"
                            #$(this-package-native-input "fast-cdr"))))))))
    (native-inputs (append (%ament-ros-native python-empy)
                           (list fast-cdr fast-dds
                                 rosidl-typesupport-fastrtps-c
                                 rosidl-typesupport-introspection-c)))
    (inputs (append (list rcpputils rcutils rmw rmw-security-common)
                    (%rosidl-transitive-deps)
                    (%rosidl-generator-deps)))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/rmw_dds_common")
    (synopsis "Shared utilities for DDS-based RMW implementations")
    (description "rmw_dds_common provides data structures and utilities
shared by all DDS-based RMW implementations: graph cache, QoS profiles.")
    (license license:asl2.0)))


;;; ---------------------------------------------------------------------------
;;; Tier 24 – rmw_implementation
;;; ---------------------------------------------------------------------------

(define-public rmw-implementation
  (package
    (name "rmw-implementation")
    (version "3.0.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ros2/rmw_implementation")
             (commit "cedcb031e4915f5b9328b8c55fab884a6b34c35d")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04q9jx1f5gv12bf6akww8v2zzqccidrcs9lkwbhb1hmr7kl0gz7a"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           ;; Tell rmw_implementation which RMW to use as default.
           #:configure-flags #~(list "-DBUILD_TESTING=OFF"
                                     "-DRMW_IMPLEMENTATION=rmw_fastrtps_cpp")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'enter-subpackage
                 (lambda _ (chdir "rmw_implementation"))))))
    ;; rmw-fastrtps-cpp must be a native-input so cmake can find it in the
    ;; ament index during configure (get_default_rmw_implementation checks it).
    (native-inputs (%ament-ros-native rmw-fastrtps-cpp))
    (inputs (append (list ament-index-cpp rcpputils rcutils rmw
                          rmw-implementation-cmake)
                    (%rosidl-transitive-deps)))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/rmw_implementation")
    (synopsis "Runtime RMW implementation loader for ROS 2")
    (description "rmw_implementation loads the active RMW implementation at
startup by inspecting RMW_IMPLEMENTATION or the compiled-in default.")
    (license license:asl2.0)))


;;; ---------------------------------------------------------------------------
;;; Tier 25 – rosidl_typesupport_fastrtps + rosidl_dynamic_typesupport_fastrtps
;;; ---------------------------------------------------------------------------

(define-public rosidl-dynamic-typesupport-fastrtps
  (package
    (name "rosidl-dynamic-typesupport-fastrtps")
    (version "0.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ros2/rosidl_dynamic_typesupport_fastrtps")
             (commit "bbbdc49d61e6743bf6a07e3ac05090ced341d625")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rvx2922dzz16mfgln7dwzvxvkg6yf5d8rw2fhl46s8n2csqmsc4"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")))
    (native-inputs (%ament-ros-native))
    (inputs (append (list fast-dds fast-cdr rcpputils rcutils)
                    (%rosidl-transitive-deps)))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/rosidl_dynamic_typesupport_fastrtps")
    (synopsis "Fast DDS dynamic typesupport for ROS 2")
    (description "rosidl_dynamic_typesupport_fastrtps provides a Fast DDS
backend for dynamic type introspection of ROS 2 messages.")
    (license license:asl2.0)))

(define %rosidl-typesupport-fastrtps-version "3.8.2")
(define %rosidl-typesupport-fastrtps-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/ros2/rosidl_typesupport_fastrtps")
          (commit "cda76159addf188be64417deb8e2b72ac1ac1942")))
    (file-name (git-file-name "rosidl-typesupport-fastrtps"
                              %rosidl-typesupport-fastrtps-version))
    (sha256
     (base32 "1p0422rjri17w4s6mrb7sr3w6m7hzsd6i8wiarvvslfnf5xn6inw"))))

(define (rosidl-typesupport-fastrtps-pkg name subdir inputs)
  (package
    (name name)
    (version %rosidl-typesupport-fastrtps-version)
    (source %rosidl-typesupport-fastrtps-source)
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'enter-subpackage
                 (lambda _ (chdir #$subdir))))))
    (native-inputs (%ament-ros-native python-empy))
    (inputs inputs)
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/rosidl_typesupport_fastrtps")
    (synopsis (string-append "Fast DDS typesupport – " subdir))
    (description "rosidl_typesupport_fastrtps provides Fast DDS-specific type
support for generated ROS 2 message types, enabling DDS serialization.")
    (license license:asl2.0)))

(define-public rosidl-typesupport-fastrtps-c
  (rosidl-typesupport-fastrtps-pkg
   "rosidl-typesupport-fastrtps-c"
   "rosidl_typesupport_fastrtps_c"
   (list fast-cdr rcutils rosidl-runtime-c
         rosidl-typesupport-interface
         rosidl-typesupport-introspection-c)))

(define-public rosidl-typesupport-fastrtps-cpp
  (rosidl-typesupport-fastrtps-pkg
   "rosidl-typesupport-fastrtps-cpp"
   "rosidl_typesupport_fastrtps_cpp"
   (append (list fast-cdr rcutils rmw)
           (%rosidl-transitive-deps)
           (list rosidl-typesupport-introspection-cpp
                 rosidl-typesupport-fastrtps-c))))


;;; ---------------------------------------------------------------------------
;;; Tier 26 – rmw_fastrtps
;;; ---------------------------------------------------------------------------

(define %rmw-fastrtps-version "9.3.4")
(define %rmw-fastrtps-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/ros2/rmw_fastrtps")
          (commit "423272653d522a62aa668f702250b99d4111e1e0")))
    (file-name (git-file-name "rmw-fastrtps" %rmw-fastrtps-version))
    (sha256
     (base32 "09447m5ik79gmmslm75wm8aqsszs57dy5bg6wk7nbpb1p3qiv0p4"))))

(define-public rmw-fastrtps-shared-cpp
  (package
    (name "rmw-fastrtps-shared-cpp")
    (version %rmw-fastrtps-version)
    (source %rmw-fastrtps-source)
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'enter-subpackage
                 (lambda _ (chdir "rmw_fastrtps_shared_cpp"))))))
    (native-inputs (%ament-native ament-cmake-ros-core))
    (inputs (list fast-dds rcpputils rcutils rmw rmw-dds-common
                  rmw-security-common rosidl-dynamic-typesupport
                  rosidl-runtime-c rosidl-typesupport-introspection-c
                  rosidl-typesupport-introspection-cpp tracetools))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/rmw_fastrtps")
    (synopsis "Shared C++ utilities for the Fast DDS RMW implementation")
    (description "rmw_fastrtps_shared_cpp provides common code shared between
the static and dynamic Fast DDS RMW typesupport variants.")
    (license license:asl2.0)))

(define-public rmw-fastrtps-cpp
  (package
    (name "rmw-fastrtps-cpp")
    (version %rmw-fastrtps-version)
    (source %rmw-fastrtps-source)
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'enter-subpackage
                 (lambda _ (chdir "rmw_fastrtps_cpp"))))))
    (native-inputs (%ament-native ament-cmake-ros-core))
    (inputs (list fast-dds rcutils rmw rmw-dds-common rmw-fastrtps-shared-cpp
                  rosidl-dynamic-typesupport-fastrtps
                  rosidl-typesupport-fastrtps-c
                  rosidl-typesupport-fastrtps-cpp tracetools))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/rmw_fastrtps")
    (synopsis "Fast DDS RMW implementation for ROS 2 (static typesupport)")
    (description "rmw_fastrtps_cpp bridges ROS 2 to Fast DDS using static
typesupport for maximum performance.")
    (license license:asl2.0)))


;;; ---------------------------------------------------------------------------
;;; Tier 27 – std_msgs + example_interfaces
;;; ---------------------------------------------------------------------------

(define %common-interfaces-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/ros2/common_interfaces")
          (commit "681a35c658818e149785e801d2b1b847e6a20daa")))
    (file-name (git-file-name "common-interfaces" "5.5.2"))
    (sha256
     (base32 "0fp1innnh7zbp66rai85ilpkzp28msdrzwsnwv840ig402ki5jvc"))))

(define (common-interfaces-pkg name subdir inputs)
  (package
    (name name)
    (version "5.5.2")
    (source %common-interfaces-source)
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'enter-subpackage
                 (lambda _ (chdir #$subdir)))
               (add-before 'configure 'set-ament-prefix-path
                 (lambda _
                   (setenv "AMENT_PREFIX_PATH"
                           (string-append
                            #$(this-package-native-input
                               "rosidl-typesupport-fastrtps-c") ":"
                            #$(this-package-native-input
                               "rosidl-typesupport-introspection-c") ":"
                            #$(this-package-native-input "fast-cdr"))))))))
    (native-inputs (append (%ament-ros-native python-empy)
                           (list fast-cdr fast-dds
                                 rosidl-typesupport-fastrtps-c
                                 rosidl-typesupport-introspection-c)))
    (inputs (append (%rosidl-generator-deps) (%rosidl-transitive-deps) inputs))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/common_interfaces")
    (synopsis (string-append "ROS 2 common interface messages – " subdir))
    (description "common_interfaces provides standard ROS 2 message types
including std_msgs, sensor_msgs, geometry_msgs, and others.")
    (license license:asl2.0)))

(define-public std-msgs
  (common-interfaces-pkg
   "std-msgs" "std_msgs"
   (list builtin-interfaces rosidl-default-generators rosidl-core-generators)))

(define-public example-interfaces
  (package
    (name "example-interfaces")
    (version "0.13.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ros2/example_interfaces")
             (commit "3de6c4d36e1df03436b160b79a06f8d7df426f24")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1p2ygfvmzk3km35718q4bzv2i0yk0p5y9flfmk2vgsq31kmjaga1"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")))
    (native-inputs (%ament-ros-native python-empy))
    (inputs (append (list service-msgs unique-identifier-msgs
                          action-msgs builtin-interfaces std-msgs)
                    (%rosidl-generator-deps)))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/example_interfaces")
    (synopsis "ROS 2 example message and service interfaces")
    (description "example_interfaces provides simple message and service types
used by ROS 2 demos such as AddTwoInts service and various talker/listener demos.")
    (license license:asl2.0)))


;;; ---------------------------------------------------------------------------
;;; Tier 28 – rclcpp
;;; ---------------------------------------------------------------------------

(define %rclcpp-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/ros2/rclcpp")
          (commit "a4a65c6502b0f845ce7fbeb671972e4d78dc9a25")))
    (file-name (git-file-name "rclcpp" "29.5.8"))
    (sha256
     (base32 "0m8rb0j3647q0hzhn6x9h17wqi02jajdxzxlzz4w5kzhkcviy96v"))))

(define-public rclcpp
  (package
    (name "rclcpp")
    (version "29.5.8")
    (source %rclcpp-source)
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'enter-subpackage
                 (lambda _ (chdir "rclcpp"))))))
    (native-inputs (%ament-native ament-cmake-ros ament-cmake-ros-core))
    (inputs
     (list ament-index-cpp builtin-interfaces libstatistics-collector
           rcl rcl-interfaces rcl-logging-interface rcl-yaml-param-parser
           rcpputils rcutils rmw rmw-implementation rosidl-dynamic-typesupport
           rosidl-runtime-c rosidl-runtime-cpp
           rosidl-typesupport-c rosidl-typesupport-cpp
           rosgraph-msgs statistics-msgs tracetools))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/rclcpp")
    (synopsis "ROS 2 C++ client library")
    (description "rclcpp provides the idiomatic C++ API for ROS 2 nodes:
RAII resource management, executor models, and type-safe message handles.")
    (license license:asl2.0)))

(define-public rclcpp-action
  (package
    (name "rclcpp-action")
    (version "29.5.8")
    (source %rclcpp-source)
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'enter-subpackage
                 (lambda _ (chdir "rclcpp_action"))))))
    (native-inputs (%ament-native ament-cmake-ros))
    (inputs (list action-msgs rcl rcl-action rclcpp rcpputils
                  rosidl-runtime-c))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/rclcpp")
    (synopsis "ROS 2 C++ action client/server")
    (description "rclcpp_action provides the C++ API for ROS 2 action clients
and servers, built on top of rclcpp and rcl_action.")
    (license license:asl2.0)))

(define-public rclcpp-lifecycle
  (package
    (name "rclcpp-lifecycle")
    (version "29.5.8")
    (source %rclcpp-source)
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'enter-subpackage
                 (lambda _ (chdir "rclcpp_lifecycle"))))))
    (native-inputs (%ament-native ament-cmake-ros))
    (inputs (list lifecycle-msgs rcl rcl-interfaces rcl-lifecycle rclcpp
                  rcutils rmw rosidl-typesupport-cpp))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/rclcpp")
    (synopsis "ROS 2 C++ lifecycle node")
    (description "rclcpp_lifecycle provides a C++ managed node implementation
following the ROS 2 lifecycle state machine specification.")
    (license license:asl2.0)))


;;; ---------------------------------------------------------------------------
;;; Tier 29 – class_loader + rclcpp_components
;;; ---------------------------------------------------------------------------

(define-public class-loader
  (package
    (name "class-loader")
    (version "2.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ros/class_loader")
             (commit "7eec645f8126824278c08b2aae8cd7fa61fb251a")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wm0p7s9sfazq2h6ipvmwkg73v5rrighrf6ykh9nmf41f40avxyr"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")))
    (native-inputs (%ament-native ament-cmake-ros ament-cmake-ros-core))
    (inputs (list console-bridge console-bridge-vendor rcpputils rcutils))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros/class_loader")
    (synopsis "ROS 2 runtime plugin loading library")
    (description "class_loader provides a mechanism for loading C++ plugins
from shared libraries at runtime without directly linking against them.")
    (license license:bsd-3)))

(define-public rclcpp-components
  (package
    (name "rclcpp-components")
    (version "29.5.8")
    (source %rclcpp-source)
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'enter-subpackage
                 (lambda _ (chdir "rclcpp_components"))))))
    (native-inputs (%ament-native ament-cmake-ros))
    (inputs (list ament-index-cpp class-loader composition-interfaces
                  rclcpp rcpputils))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/rclcpp")
    (synopsis "ROS 2 composable node component system")
    (description "rclcpp_components provides the composable node API that
allows loading multiple nodes into a single process.")
    (license license:asl2.0)))


;;; ---------------------------------------------------------------------------
;;; Tier 30 – rosidl_generator_py + rpyutils + rclpy
;;; ---------------------------------------------------------------------------

(define-public rpyutils
  (package
    (name "rpyutils")
    (version "0.6.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ros2/rpyutils")
             (commit "a57aaa49a598d312225e0200d8c7f1984eaf241b")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0w1pr5s92rv1gq5s1a0vgsvakc57040ific6s8q3mdky92mass82"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/ros2/rpyutils")
    (synopsis "Python utilities for ROS 2")
    (description "rpyutils provides Python utilities used by rclpy and other
Python ROS 2 packages.")
    (license license:asl2.0)))

(define-public rosidl-generator-py
  (package
    (name "rosidl-generator-py")
    (version "0.24.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ros2/rosidl_python")
             (commit "ac6028844af60691aadfcf0e9b9fa532c96dd333")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yhq8vd4dikzbwms106riwyajq2if9j2ff7r3xbwiwkjwlzqrkdh"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'enter-subpackage
                 (lambda _ (chdir "rosidl_generator_py"))))))
    (native-inputs (%ament-native `(,python "out") python-empy))
    (inputs (list pybind11-vendor `(,python "out")
                  rosidl-cmake rosidl-runtime-c rosidl-typesupport-c
                  rosidl-typesupport-cpp rosidl-typesupport-interface))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/rosidl_python")
    (synopsis "Python binding generator for ROS 2 interface types")
    (description "rosidl_generator_py generates Python bindings for ROS 2
message, service, and action types.")
    (license license:asl2.0)))

(define-public rclpy
  (package
    (name "rclpy")
    (version "9.1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ros2/rclpy")
             (commit "134b013ff1eba49f823871d3c6cf96d6097066c9")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10qzim4g72f2nn4zv7z5013syd1bsl2n2dgvd6578zdkg526fgy8"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'enter-subpackage
                 (lambda _ (chdir "rclpy"))))))
    (native-inputs (%ament-native `(,python "out") python-empy))
    (inputs
     (list lifecycle-msgs pybind11-vendor `(,python "out")
           rcl rcl-action rcl-interfaces rcl-lifecycle rcl-logging-interface
           rcl-yaml-param-parser rcpputils rcutils rmw rmw-implementation
           rosidl-runtime-c type-description-interfaces unique-identifier-msgs))
    (propagated-inputs (list python-pyyaml rpyutils))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/rclpy")
    (synopsis "ROS 2 Python client library")
    (description "rclpy is the Python client library for ROS 2, wrapping rcl
through a C extension module to expose the full ROS 2 API to Python.")
    (license license:asl2.0)))


;;; ---------------------------------------------------------------------------
;;; Tier 31 – demo_nodes_cpp + demo_nodes_py
;;; ---------------------------------------------------------------------------

(define %demos-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/ros2/demos")
          (commit "faa0e40545ff2e35b25ed18a4e9d2b7f4a1d36c4")))
    (file-name (git-file-name "demos" "0.36.5"))
    (sha256
     (base32 "0d9qnw8arfa43y3r61m4yrf9s7krnzd5ns28wf2hmpq2c71byj94"))))

(define-public demo-nodes-cpp
  (package
    (name "demo-nodes-cpp")
    (version "0.36.5")
    (source %demos-source)
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~(list "-DBUILD_TESTING=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'enter-subpackage
                 (lambda _ (chdir "demo_nodes_cpp"))))))
    (native-inputs (%ament-ros-native))
    (inputs
     (list example-interfaces rcl rclcpp rclcpp-components
           rcl-interfaces rcpputils rcutils rmw std-msgs
           rmw-fastrtps-cpp))
    (native-search-paths %ament-search-paths)
    (home-page "https://github.com/ros2/demos")
    (synopsis "ROS 2 C++ talker/listener demo nodes")
    (description "demo_nodes_cpp provides the classic ROS 2 talker and listener
demos written in C++ using rclcpp, demonstrating publish/subscribe messaging.")
    (license license:asl2.0)))

(define-public demo-nodes-py
  ;; demo_nodes_py uses ament_python build type — pure Python package.
  (package
    (name "demo-nodes-py")
    (version "0.36.5")
    (source %demos-source)
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'enter-subpackage
                 (lambda _ (chdir "demo_nodes_py"))))))
    (native-inputs (list python-setuptools python-wheel))
    (propagated-inputs
     (list ament-index-python example-interfaces rclpy rcl-interfaces std-msgs))
    (home-page "https://github.com/ros2/demos")
    (synopsis "ROS 2 Python talker/listener demo nodes")
    (description "demo_nodes_py provides the classic ROS 2 talker and listener
demos written in Python using rclpy.")
    (license license:asl2.0)))
