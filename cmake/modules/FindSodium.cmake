cmake_minimum_required(VERSION 2.8.2)

find_path(SODIUM_INCLUDE_DIRS
	NAMES
		sodium.h
	PATHS
		${SODIUM_PREFIX}/include
		/usr/include
		/usr/local/include
		/opt/local/include
	NO_DEFAULT_PATH
)

find_library(SODIUM_LIBRARIES
	NAMES
		sodium
	PATHS
		${SODIUM_INCLUDE_DIRS}/..
		${SODIUM_INCLUDE_DIRS}/../lib
		${SODIUM_INCLUDE_DIRS}/../../lib
	NO_DEFAULT_PATH
)

include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(SODIUM
                                  DEFAULT_MSG
                                  SODIUM_LIBRARIES
                                  SODIUM_INCLUDE_DIRS)
