#!/bin/bash

if false; then :;
elif [ "$1" == "build/libnacl.a" ]; then
	redo-ifchange nacl

	# FIXME: pick an arbitrary arbitrary ABI, note that "amd64" < "x86"
	host=$(hostname | tr -d _ )
	abi=$( ./lib/nacl/build/$host/bin/okabi | sort | head -1 )
	echo abi=$abi >&2

	cp -t lib/nacl lib/nacl/build/$host/include/$abi/*.h
	cp "lib/nacl/build/$host/lib/$abi/libnacl.a" "$3"
	cp "lib/nacl/build/$host/lib/$abi/randombytes.o" build/
fi
