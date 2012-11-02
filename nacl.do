mkdir -p lib/nacl || :
cd lib/nacl
rm -rf *

wget http://hyperelliptic.org/nacl/nacl-20110221.tar.bz2
bunzip2 < nacl-20110221.tar.bz2 | tar -xf -
mv nacl-20110221/* .
rm -rf nacl-20110221
