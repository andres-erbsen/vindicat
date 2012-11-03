# download dependencies
mkdir -p build/lib

redo-ifchange libev
redo-ifchange libsocket
redo-ifchange nacl

redo-ifchange build/UDPClientTransport.o
redo-ifchange build/UDPServerTransport.o
redo-ifchange build/CryptoIdentity.o
redo-ifchange build/Forwarding.o
redo-ifchange build/NetworkMap.o
redo-ifchange build/PacketHandler.o
