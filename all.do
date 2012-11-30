OBJECTS="build/vindicat.o build/lib/libev/ev.o build/lib/libsocket/libinetsocket.o build/lib/libsocket/inetdgram.o build/lib/libsocket/exception.o build/lib/libsocket/inetserverdgram.o  build/lib/libsocket/dgramclient.o build/lib/libsocket/inetbase.o build/lib/libsocket/inetclientdgram.o build/lib/libsocket/socket.o build/lib/ed25519-donna/ed25519.o build/randombytes.o build/CryptoIdentity.o build/Forwarding.o build/PacketHandler.o build/vindicat.pb.o build/NetworkMap.o build/UDPServerTransport.o build/UDPClientTransport.o build/Beacon.o build/LinkNegotiator.o build/libnacl.a"

redo-ifchange $OBJECTS
g++ $CXXFLAGS $LDFLAGS $OBJECTS -lcrypto -lssl -lprotobuf -o build/vindicat
