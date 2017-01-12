#/bin/bash

gfortran -fno-automatic -std=f2008 -Wall -fcheck=all prec_def.f90 global.f90 MYFI.FOR ansti.for -o ansti
