mkdir -p lib/ed25519-donna || :
cd lib/ed25519-donna

rm -rf *

wget --no-check-certificate https://github.com/floodyberry/ed25519-donna/tarball/master
tar xpvf master

mv floodyberry-ed25519-donna-*/* .
rm -rf floodyberry-ed25519-donna-*
rm -rf master
