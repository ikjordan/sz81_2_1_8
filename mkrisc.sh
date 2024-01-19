#!/usr/bin/env bash

# Set up a Risc OS build - run from root of sz81
mkdir riscroot
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
mkdir p
cp ../games-etc/*.81 p
cp ../games-etc/*.p p
cd p
for v in *.p ; do mv "$v"  "$(basename "$v" .p)"; done
for v in *.81 ; do mv "$v"  "$(basename "$v" .81)"; done
