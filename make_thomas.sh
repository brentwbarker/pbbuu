#!/bin/bash
#

gfortran --std=legacy -fno-automatic -fcheck=all -Wall -Wno-conversion -O2 SOLPAG.FOR SINT.FOR spaco.for prec_def.f90 global.f90 thomas66b.for -o thomas
