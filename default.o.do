s2=${2#build\/}

: ${CC:="gcc"}
: ${CXX:="g++"}
: ${LIBS:="-Ilib/libev -Ilib/libsocket -Ilib/nacl"}

if false; then :;
elif [ -f "$s2.c" ]; then
	redo-ifchange "$s2.c"
	$CC  -MD -MF "$2.d" -c $CFLAGS $CPPFLAGS $LIBS -o "$3" "$s2.c"   --std=gnu99
elif [ -f "$s2.cpp" ]; then
	redo-ifchange "$s2.cpp"
	$CXX -MD -MF "$2.d" -c $CFLAGS $CPPFLAGS $LIBS -o "$3" "$s2.cpp" --std=c++11
fi

read DEPS < "$2.d"
redo-ifchange ${DEPS#*:}
