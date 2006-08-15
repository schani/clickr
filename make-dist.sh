#!/bin/bash

VERSION=0.2
DIR=clickr-$VERSION
TARGZ=clickr-$VERSION.tar.gz

rm -rf $DIR $TARGZ
mkdir $DIR
cp README COPYING clickr.asd flickr.lisp clickr.lisp automatr.lisp let-match.lisp utils.lisp $DIR/
cp dist-load-flickr.lisp $DIR/load-flickr.lisp
tar -zcvf $TARGZ $DIR
