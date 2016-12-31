#!/bin/bash
CHALL=coercive
FILES="Dockerfile"
set -x
rm -rf $CHALL *$CHALL*.tar.xz
mkdir -p $CHALL
cp -r $FILES $CHALL
tar cJvf $CHALL.tar.xz $CHALL
sha1=($(sha1sum $CHALL.tar.xz))
mv $CHALL.tar.xz $CHALL-$sha1.tar.xz
