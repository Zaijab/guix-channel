# ROS 2 Kilted — Complete Dependency Audit

## Repos still to clone (17 new, all kilted branch)

| Repo | URL | Packages |
|---|---|---|
| ament_cmake_ros | github.com/ros2/ament_cmake_ros | ament_cmake_ros, ament_cmake_ros_core |
| ament_index | github.com/ament/ament_index | ament_index_cpp, ament_index_python |
| rcpputils | github.com/ros2/rcpputils | rcpputils |
| ros2_tracing | github.com/ros2/ros2_tracing | tracetools |
| libyaml_vendor | github.com/ros2/libyaml_vendor | libyaml_vendor |
| spdlog_vendor | github.com/ros2/spdlog_vendor | spdlog_vendor |
| libstatistics_collector | github.com/ros-tooling/libstatistics_collector | libstatistics_collector |
| pybind11_vendor | github.com/ros2/pybind11_vendor | pybind11_vendor |
| rpyutils | github.com/ros2/rpyutils | rpyutils |
| unique_identifier_msgs | github.com/ros2/unique_identifier_msgs | unique_identifier_msgs |
| example_interfaces | github.com/ros2/example_interfaces | example_interfaces |
| rosidl_defaults | github.com/ros2/rosidl_defaults | rosidl_default_generators, rosidl_default_runtime |
| rosidl_core | github.com/ros2/rosidl_core | rosidl_core_generators, rosidl_core_runtime |
| rosidl_dynamic_typesupport | github.com/ros2/rosidl_dynamic_typesupport | rosidl_dynamic_typesupport |
| rosidl_dynamic_typesupport_fastrtps | github.com/ros2/rosidl_dynamic_typesupport_fastrtps | rosidl_dynamic_typesupport_fastrtps |
| rosidl_typesupport_fastrtps | github.com/ros2/rosidl_typesupport_fastrtps | rosidl_typesupport_fastrtps_c/cpp |
| rosidl_python | github.com/ros2/rosidl_python | rosidl_generator_py |
| class_loader | github.com/ros/class_loader | class_loader |

## System packages from Guix (already available)

cmake, ninja, python, python-setuptools, python-empy, python-catkin-pkg,
python-argcomplete, python-lark, python-pyyaml, python-typing-extensions,
openssl, libyaml, tinyxml2, spdlog, acl, googletest, glibc (libatomic)

## Complete ordered build sequence (93 packages)

### Tier 0 — Python tools (no ROS deps)
1. ament_package
2. rosidl_cli
3. rosidl_parser

### Tier 1 — ament_cmake core (cmake + ament_package only)
4. ament_cmake_core
5. ament_cmake_export_definitions
6. ament_cmake_export_include_directories
7. ament_cmake_export_libraries
8. ament_cmake_export_link_flags
9. ament_cmake_export_targets
10. ament_cmake_export_dependencies
11. ament_cmake_export_interfaces
12. ament_cmake_include_directories
13. ament_cmake_libraries
14. ament_cmake_python
15. ament_cmake_test
16. ament_cmake_version
17. ament_cmake_gtest
18. ament_cmake_gmock
19. ament_cmake_gen_version_h
20. ament_cmake_target_dependencies
21. ament_cmake_vendor_package
22. ament_cmake  ← meta-package, must come AFTER all sub-pkgs
23. ament_cmake_auto  ← last, needs ament_cmake

### Tier 2 — ament_cmake_ros [NEW REPO]
24. ament_cmake_ros_core
25. ament_cmake_ros

### Tier 3 — ament_index [NEW REPO]
26. ament_index_cpp
27. ament_index_python

### Tier 4 — rcpputils [NEW REPO]
28. rcpputils

### Tier 5 — tracetools [NEW REPO]
29. tracetools  (header-only + minimal C)

### Tier 6 — rosidl adapter/pycommon
30. rosidl_adapter
31. rosidl_pycommon

### Tier 7 — rosidl_core [NEW REPO]
32. rosidl_core_generators
33. rosidl_core_runtime

### Tier 8 — rosidl typesupport interface + rcutils
34. rosidl_typesupport_interface
35. rcutils  (needs ament_cmake_ros_core, libatomic)

### Tier 9 — rosidl cmake/generator infrastructure
36. rosidl_cmake
37. rosidl_generator_type_description
38. rosidl_runtime_c
39. rosidl_runtime_cpp

### Tier 10 — rosidl generators
40. rosidl_generator_c
41. rosidl_generator_cpp

### Tier 11 — rosidl_defaults [NEW REPO]
42. rosidl_default_generators
43. rosidl_default_runtime

### Tier 12 — rosidl_dynamic_typesupport [NEW REPO]
44. rosidl_dynamic_typesupport

### Tier 13 — base message interfaces
45. builtin_interfaces
46. service_msgs
47. type_description_interfaces
48. unique_identifier_msgs  [NEW REPO]

### Tier 14 — rcl_interfaces messages
49. rcl_interfaces
50. action_msgs

### Tier 15 — rmw
51. rmw  (needs ament_cmake_ros_core, ament_cmake_version, rcutils,
         rosidl_runtime_c, rosidl_dynamic_typesupport)

### Tier 16 — libyaml_vendor, rcl_logging, rmw_implementation_cmake
52. libyaml_vendor  [NEW REPO]
53. rmw_implementation_cmake
54. rcl_logging_interface

### Tier 17 — spdlog_vendor, rcl_logging_spdlog
55. spdlog_vendor  [NEW REPO]
56. rcl_logging_spdlog  (needs rcpputils, spdlog_vendor)
57. rcl_logging_noop

### Tier 18 — rcl_yaml_param_parser
58. rcl_yaml_param_parser  (needs libyaml_vendor, rcutils, rmw)

### Tier 19 — rcl
59. rcl  (needs rcl_interfaces, rcl_logging_interface, rcl_yaml_param_parser,
          rcutils, rmw_implementation, service_msgs, tracetools,
          type_description_interfaces)

### Tier 20 — lifecycle + rcl_action + rcl_lifecycle
60. lifecycle_msgs
61. rcl_action
62. rcl_lifecycle

### Tier 21 — rosidl introspection
63. rosidl_typesupport_introspection_c
64. rosidl_typesupport_introspection_cpp

### Tier 22 — rosidl_typesupport_c/cpp [SEPARATE REPO: rosidl_typesupport]
65. rosidl_typesupport_c
66. rosidl_typesupport_cpp

### Tier 23 — libstatistics_collector [NEW REPO]
67. libstatistics_collector

### Tier 24 — statistics_msgs, rosgraph_msgs
68. statistics_msgs
69. rosgraph_msgs

### Tier 25 — Fast DDS stack
70. foonathan_memory  (eProsima fork, vendor-1.4.1)
71. fast-cdr
72. fast-dds

### Tier 26 — rmw_security_common, rmw_dds_common
73. rmw_security_common
74. rmw_dds_common

### Tier 27 — rosidl_typesupport_fastrtps [NEW REPO]
75. rosidl_dynamic_typesupport_fastrtps  [NEW REPO]
76. rosidl_typesupport_fastrtps_c
77. rosidl_typesupport_fastrtps_cpp

### Tier 28 — rmw_fastrtps
78. rmw_fastrtps_shared_cpp
79. rmw_fastrtps_cpp
80. rmw_fastrtps_dynamic_cpp

### Tier 29 — rmw_implementation
81. rmw_implementation

### Tier 30 — rclcpp
82. rclcpp

### Tier 31 — rclcpp_* + composition
83. composition_interfaces
84. rclcpp_lifecycle
85. class_loader  [NEW REPO]
86. rclcpp_action
87. rclcpp_components

### Tier 32 — std_msgs + example_interfaces
88. std_msgs
89. example_interfaces  [NEW REPO]

### Tier 33 — rclpy deps [NEW REPOS]
90. pybind11_vendor
91. rpyutils
92. rosidl_generator_py

### Tier 34 — rclpy
93. rclpy

### Tier 35 — demos
94. demo_nodes_cpp
95. demo_nodes_py
