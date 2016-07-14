#!/bin/bash

rm *.mod

#gfortran --std=legacy -fno-automatic -fcheck=all rang.for mov131.for myfi.for loc16.for hig2com.for cspak2.for coll16.for buu257.for -o buu

gfortran --std=legacy -fno-automatic -fcheck=all -Wall -Wno-conversion prec_def.f90 global.f90 LOC16.FOR MYFI.FOR SINT.FOR rang.for MOV132.FOR spaco.for HIG2COM.FOR CSPAK2.FOR COLL16.FOR arbstorage.f08 class_BVector.f90 bwbfunc.f08 buu258.FOR -o buu

