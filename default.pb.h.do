redo-ifchange "$2.proto"

tmp=".tmp/protoc/$2"
mkdir -p "$tmp"

protoc --cpp_out="$tmp" "$2.proto"

mv "$tmp/$1" "$3"
mv "$tmp/$2.pb.cc" .
rm -rf .tmp
