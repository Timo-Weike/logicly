#!/bin/bash

cabal v1-sandbox delete # kill it with fire!
cabal v1-sandbox init
cabal v2-clean
cabal v2-update
cabal v2-install --only-dependencies -j
cabal v2-build -j -fstatic