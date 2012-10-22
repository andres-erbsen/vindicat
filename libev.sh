#!/bin/sh

mkdir lib/libev || :
cd lib/libev
rm *.h
wget https://raw.github.com/brimworks/libev/master/ev.h
wget https://raw.github.com/brimworks/libev/master/ev++.h
