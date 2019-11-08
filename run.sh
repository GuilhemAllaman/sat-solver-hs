#!/bin/bash

stack build
echo ""
echo "*** build ended ***"
echo ""
stack exec sat-solver-hs-exe
