# download dependencies
mkdir -p build/lib

redo-ifchange libev
redo-ifchange libsocket
redo-ifchange nacl

