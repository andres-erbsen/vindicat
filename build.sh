echo g++ --std=c++11 -O3 \
	-o "${1%.*}" \
	-DEV_STANDALONE          \
	lib/libev/ev.c           \
	lib/libsocket/*.c*       \
	-I lib/libsocket -I lib/libev $@
g++ --std=c++11 -O3 \
	-o "${1%.*}" \
	-DEV_STANDALONE          \
	lib/libev/ev.c           \
	lib/libsocket/*.c*       \
	-I lib/libsocket -I lib/libev $@
