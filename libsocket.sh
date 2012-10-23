#!/bin/sh

mkdir -p lib/libsocket || :
cd lib/libsocket
rm *.1 || :
for file in headers/socket.hpp headers/libinetsocket.h headers/exception.hpp headers/inetbase.hpp headers/inetdgram.hpp headers/inetclientdgram.hpp headers/dgramclient.hpp headers/inetserverdgram.hpp C++/socket.cpp C/libinetsocket.c C++/exception.cpp C++/inetbase.cpp C++/inetdgram.cpp C++/inetclientdgram.cpp C++/dgramclient.cpp C++/inetserverdgram.cpp;
do
	rm "$file" || :
	wget "https://raw.github.com/dermesser/libsocket/master/$file"
done

sed "s:../headers/::g" -i *.cpp
