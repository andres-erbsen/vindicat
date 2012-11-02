LIBEV=libev-4.11

mkdir -p lib/libev || :
cd lib/libev
wget "http://dist.schmorp.de/libev/Attic/$LIBEV.tar.gz"

tar -xzf "./$LIBEV.tar.gz" 
cp -t . "./$LIBEV/ev.h" "./$LIBEV/ev.c" "./$LIBEV/ev++.h" "./$LIBEV/ev_vars.h" "./$LIBEV/ev_wrap.h" "./$LIBEV/ev_select.c" "./$LIBEV/ev_poll.c" "./$LIBEV/ev_epoll.c"

rm "$LIBEV.tar.gz"
rm -rf "$LIBEV"
