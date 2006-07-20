#!/bin/bash

VERSION=0.1
DIR=clickr-$VERSION
TARGZ=clickr-$VERSION.tar.gz

rm -rf $DIR $TARGZ
mkdir $DIR
cp README COPYING flickr.lisp clickr.lisp automatr.lisp let-match.lisp load-flickr.lisp utils.lisp $DIR/
cp dist-my-config.lisp $DIR/my-config.lisp
tar -zcvf $TARGZ $DIR
