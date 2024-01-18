#!/usr/bin/env bash

# Set up a Risc OS build - run from root of sz81
mkdir -p c
mkdir -p h
rm c/*
rm h/*
cp *.c c
cp *.h h
cd c
for v in *.c ; do mv "$v"  "$(basename "$v" .c)"; done
cd ../h
for v in *.h ; do mv "$v"  "$(basename "$v" .h)"; done
cd ..
#Back-up existing Makefile
cp Makefile Makefile.linux
cp RiscOS/* .
