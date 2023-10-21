#!/bin/bash
gfortran $1
./a.out
rm -rf a.out
