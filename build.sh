#!/bin/sh
mkdir -p _shake
ghc -package-db=/Users/sad/Documents/Programming/Haskell/Shake/.cabal-sandbox/x86_64-osx-ghc-8.4.3-packages.conf.d/ --make Build.hs -rtsopts -threaded -with-rtsopts=-I0 -outputdir=_shake -o _shake/build && _shake/build "$@"
