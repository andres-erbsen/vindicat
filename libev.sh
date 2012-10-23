#!/bin/sh

mkdir -p lib/libev || :
cd lib/libev

for file in ev.h ev.c ev++.h ev_vars.h ev_wrap.h ev_select.c ev_poll.c ev_epoll.c;
do
	rm "$file" | :
	wget "https://raw.github.com/brimworks/libev/master/$file"
done
