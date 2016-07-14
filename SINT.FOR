      FUNCTION SINT(X)                          !Jun functions
C  INTERACTION PART OF DENSITY-DEP SYMMETRY ENERGY
C
      PARAMETER(SI0=.020)
      PARAMETER(ZMU=1.)
C
      IF(X.GT.1E-6)THEN
        SINT=SI0*X**ZMU
**        SINT=50.1E-3*X-31.9E-3*X**1.47
      ELSE
        SINT=0.
      ENDIF
C
      END
