#!/bin/bash

stack build
echo ""
echo "*** build ended ***"
echo ""
stack exec --ghci sat-solver-hs-exe
