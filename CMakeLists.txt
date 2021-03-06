cmake_minimum_required(VERSION 2.8.2)
project(vindicat CXX C)

if(NOT CMAKE_BUILD_TYPE)
	set(CMAKE_BUILD_TYPE Debug)
endif()
SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -Wall -Wextra") 
SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -Wextra") 
if(CMAKE_BUILD_TYPE MATCHES Debug)
	SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -O0") 
	SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g -O0") 
endif()

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${PROJECT_SOURCE_DIR}/cmake/modules)

find_package(Sodium REQUIRED)
find_package(Protobuf REQUIRED)
find_package(LEMON REQUIRED)
find_package(PCAP REQUIRED)
if(HAVE_PCAP_GET_SELECTABLE_FD)
  add_definitions(-DHAVE_PCAP_GET_SELECTABLE_FD)
endif()

# Doxygen documentation
find_package(Doxygen)
if(DOXYGEN_FOUND)
	configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in
		${CMAKE_CURRENT_BINARY_DIR}/Doxyfile @ONLY)
	add_custom_target(doc
		${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
		WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
		COMMENT "Generating API documentation with Doxygen" VERBATIM
		)
endif(DOXYGEN_FOUND)

include_directories(${PROJECT_SOURCE_DIR}/src)
include_directories(SYSTEM ${PROJECT_SOURCE_DIR}/lib/libev)
add_definitions(-DEV_STANDALONE)
include_directories(${PROJECT_SOURCE_DIR}/lib/ed25519-donna)
include_directories(${PROJECT_SOURCE_DIR}/lib/keccak)
include_directories(${PROJECT_SOURCE_DIR}/lib/xtea)
include_directories(${PROJECT_SOURCE_DIR}/lib/libcrypto-sha512-wrapper)
include_directories(${PROJECT_SOURCE_DIR}/libvindicat)
include_directories(${PROTOBUF_INCLUDE_DIRS})
include_directories(${LEMON_INCLUDE_DIRS})
include_directories(${SODIUM_INCLUDE_DIRS})
include_directories(${PCAP_INCLUDE_DIRS})
include_directories(${CMAKE_BINARY_DIR})
add_definitions(-DANSI) # for coloured logs

FILE(GLOB_RECURSE srcs src/*.c src/*.cc src/*.cpp)
FILE(GLOB_RECURSE libs lib/keccak/*.c lib/libev/ev.c lib/libev/event.c)
FILE(GLOB_RECURSE clibs lib/xtea/*.c lib/*.cc lib/*.cpp)
PROTOBUF_GENERATE_CPP(PROTO_SRCS PROTO_HDRS src/vindicat.proto)

file(GLOB_RECURSE lib_srcs libvindicat/*.cpp)
PROTOBUF_GENERATE_CPP(lib_proto_srcs lib_proto_hdrs libvindicat/libvindicat.proto)

ADD_EXECUTABLE(vindicat-server ${clibs} ${libs} ${srcs} ${PROTO_SRCS} ${lib_proto_srcs})
set_target_properties(vindicat-server PROPERTIES OUTPUT_NAME vindicat)
ADD_LIBRARY(vindicat ${lib_srcs} ${lib_proto_srcs})
set_source_files_properties(${libs} PROPERTIES LANGUAGE CXX)

target_link_libraries(vindicat-server ${PROTOBUF_LITE_LIBRARIES} ${SODIUM_LIBRARIES} ${LEMON_LIBRARIES} ${PCAP_LIBRARIES})
target_link_libraries(vindicat ${PROTOBUF_LITE_LIBRARIES})

# Testing
set(DART_TESTING_TIMEOUT "60")
include(CTest)
enable_testing()
add_custom_target(check COMMAND ${CMAKE_CTEST_COMMAND} -V)
ADD_EXECUTABLE(Ping EXCLUDE_FROM_ALL tests/ping.cpp)
target_link_libraries(Ping vindicat)
add_test(NAME Ping-after COMMAND Ping $<TARGET_FILE:vindicat-server> 25)
add_test(NAME Ping-before COMMAND Ping $<TARGET_FILE:vindicat-server> 1)
add_dependencies(Ping vindicat-server)
add_dependencies(check Ping)
