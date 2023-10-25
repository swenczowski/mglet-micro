#!/bin/bash

rm -rf build

mkdir build
cd build

CMAKE_PREFIX_PATH=/home/swenczowski/Software/openmpi_gcc11_cuda FC=gfortran-11 CC=gcc-11 CXX=g++-11 cmake -DCMAKE_BUILD_TYPE=Debug ..
