#!/bin/sh
SHAKEDIR=".shake"
mkdir -p $SHAKEDIR
ghc --make Build.hs -rtsopts -with-rtsopts=-I0 -outputdir=$SHAKEDIR -o $SHAKEDIR/build && $SHAKEDIR/build "$@"
