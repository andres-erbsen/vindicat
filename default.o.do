s2=${2#build\/}

: ${CXX:="g++"}
: ${LIBS:="-Ilib/libev -Ilib/libsocket -Ilib/nacl -Ilib/ed25519-donna"}

if [ "$1" == "build/randombytes.o" ]; then
	redo-ifchange nacl
	host=$(hostname | tr -d _ )
	abi=$( ./lib/nacl/build/$host/bin/okabi | sort | head -1 )
	cp "lib/nacl/build/$host/lib/$abi/randombytes.o" "$3"
	exit 0
fi


if false; then :;
elif [ -f "$s2.c" ];   then srcfile="$s2.c"
elif [ -f "$s2.cpp" ]; then srcfile="$s2.cpp"
elif [ -f "$s2.cc" ];  then srcfile="$s2.cc"
else echo "Source file $s2.{c,cc,cpp} not found!" >&2; exit 1
fi

redo-ifchange "$srcfile"
mkdir -p $(dirname "$1")
$CXX -MD -MF "$2.d" -c $CXXFLAGS $LIBS -o "$3" "$srcfile" --std=c++11 -DEV_STANDALONE

read DEPS < "$2.d"
redo-ifchange ${DEPS#*:}
