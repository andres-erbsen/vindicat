#!/bin/sh

( cd libsocket && git pull ) || git clone git://github.com/dermesser/libsocket.git
rm -rf lib/libsocket/*
cp -t lib/libsocket libsocket/headers/socket.hpp libsocket/headers/libinetsocket.h libsocket/headers/exception.hpp libsocket/headers/inetbase.hpp libsocket/headers/inetdgram.hpp libsocket/headers/inetclientdgram.hpp libsocket/headers/dgramclient.hpp libsocket/headers/inetserverdgram.hpp
cp -t lib/libsocket libsocket/C++/socket.cpp libsocket/C/libinetsocket.c libsocket/C++/exception.cpp libsocket/C++/inetbase.cpp libsocket/C++/inetdgram.cpp libsocket/C++/inetclientdgram.cpp libsocket/C++/dgramclient.cpp libsocket/C++/inetserverdgram.cpp
sed "s:../headers/::g" -i lib/libsocket/*.cpp
