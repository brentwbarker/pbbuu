#!/bin/bash

rm *.mod

gfortran -std=f2008 -Wall -fcheck=all arbstorage.f08 class_BVector.f90 prec_def.f90 global.f90 pbuuproc.f90 -o pbuuproc
