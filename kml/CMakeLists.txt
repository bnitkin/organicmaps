project(kml)

set(
  SRC
  header_binary.hpp
  minzoom_quadtree.hpp
  serdes.cpp
  serdes.hpp
  serdes_binary.cpp
  serdes_binary.hpp
  type_utils.cpp
  type_utils.hpp
  types.cpp
  types.hpp
  types_v3.hpp
  types_v6.hpp
  types_v7.hpp
  types_v8.hpp
  types_v9.hpp
  visitors.hpp
)

omim_add_library(${PROJECT_NAME} ${SRC})

omim_add_pybindings_subdirectory(pykmlib)
omim_add_test_subdirectory(kml_tests)

if(NOT PLATFORM_ANDROID)
  add_subdirectory(kmb_to_kml)
endif()
