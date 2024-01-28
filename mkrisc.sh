#!/usr/bin/env bash

# Set up a Risc OS build - run from root of sz81
mkdir -p riscroot
cd riscroot
mkdir -p c
mkdir -p h
rm -f c/*
rm -f h/*
cp ../*.c c
cp ../*.h h
cd c
for v in *.c ; do mv "$v"  "$(basename "$v" .c)"; done
cd ../h
for v in *.h ; do mv "$v"  "$(basename "$v" .h)"; done
cd ..
cp ../RiscOS/* .
mkdir -p data
cp ../data/* data
mkdir -p games-etc
cp -p -r ../games-etc/* games-etc
