#!/bin/bash

if false; then :;
elif [ "$1" == "build/libnacl.a" ]; then
	redo-ifchange nacl
	host=$(hostname | tr -d _ )
	abi=$( ./lib/nacl/build/$host/bin/okabi | sort | head -1 )

	cp -t lib/nacl lib/nacl/build/$host/include/$abi/*.h
	cp "lib/nacl/build/$host/lib/$abi/libnacl.a" "$3"
fi
