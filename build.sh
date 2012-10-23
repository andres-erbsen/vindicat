g++ --std=c++11 -O3 \
	UDPServerTransport.cpp \
	-DEV_STANDALONE        \
	lib/libev/ev.c         \
	lib/libsocket/*.c*     \
	-I lib/libsocket -I lib/libev
