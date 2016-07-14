C#################################
        SUBROUTINE SIMDAT_CLUSTERS(IAP,IZP,EP,VP,THP,TAKE)
        use global
C  NOTE IAP MAY BE ALTERED..., IZP too
C  EP MAY BE ALTERED, AND VP

C  This routine simulates the Ball-Wall experimental thresholds.  It returns
C  Adapted by P. D. in July '90

        LOGICAL TAKE

        PARAMETER (RAD= 57.29578)
        PARAMETER (EUNIT= 931.5)        ! the mass of an amu
        INTEGER*4 ISEED
        LOGICAL   IMF_EXPT

        DIMENSION ECAF2(4), EPUNCH(5)

        DATA ECAF2 /38.,54.,62.,162./   ! Energy which stops in CaF2 for H
C                                       isotopes and 4He
        DATA EPUNCH /247.,438.,621.,900.,681./  !Energy which punches through
C                                               plastic for H and He isotopes
        DATA ISEED /1234567/            ! Random number seed
        DATA ETHRESH /20./              ! Energy threshold for protons
        DATA IMF_EXPT /.FALSE./         ! Should be set .TRUE. to simulate
C                                         the 200 MeV/A Au + Au and Au + Fe
C                                         experiments


C      Simulate the Fast Clear
C
      IF (IAP .EQ. 0 .AND. IZP .LE. 0) THEN
C              neutral or negative pions
        GO TO 180
      ELSE IF (IZP .LE. 0) THEN
C              neutral or negative baryons (neutrons)
        GO TO 180
      ELSE IF (THP .GT. 161.2/RAD) THEN
C              backward cone
        GO TO 180
      ELSE IF (THP .GT. 10./RAD) THEN
C              Ball
        CALL TARCUT_CLUSTERS(IAP,IZP,ECUT,THP)
        IF (ECUT .EQ. 0.) GO TO 180
        IF (IAP .EQ. 0) THEN
C                  Pi plus
          IF (EP .LT. ECUT + 15.) GO TO 180
          IF (EP .GT. ECUT + 140.) GO TO 180
C                  pion detection efficiency
          IF (EP .LE. ECUT + 75.) THEN
            EFFPI= 0.8
          ELSE
            EFFPI= 0.8 - 0.0036*(EP - ECUT - 75.)
          END IF
          IF (getRan() .GT. EFFPI) GO TO 180
          ELSE IF (IZP .GT. 0) THEN
C                  charged baryon
            IF (EP .LT. ECUT) GO TO 180
            IF (IZP .EQ. 1) THEN
              ECAF2STOP= ECAF2(IAP)
            ELSE
              ECAF2STOP= 10.*IZP*IZP + 63.*IZP
            END IF
            IT= IAP
            IF (IZP .GE. 2) IT= IT+1
            IF (IMF_EXPT .AND. THP .LT. 28.4/RAD .AND.
     1        IZP .GT. 2 .AND. IZP .LT. 10 .AND.
     1        EP .GT. ECUT + ECAF2STOP) THEN
C                           an intermediate mass fragment
            ELSE IF (IZP .GT. 2 .OR. EP .LT. ECUT + ECAF2STOP) THEN
C                      stops in CaF2
C                       assign to p, d, t, or 4He, depending on energy
              DO L= 1, 4
                IF (EP .LT. ECUT + ECAF2(L)) GO TO 60
              END DO
              L= 4
60            IAP= L
              IF (IAP .EQ. 4) THEN
                IZP= 2
              ELSE
                IZP= 1
              END IF
              IF (IAP .EQ. 1 .AND. EP .LT. ECUT + ETHRESH) GOTO 180

              AM= IAP*EUNIT
              VP= SQRT(1. - (AM/(EP + AM))**2)
            ELSE IF (IZP .LE. 2 .AND. EP .GT. ECUT + EPUNCH(IT)) THEN

C                      punches through
C                       assign to p or 3He at same velocity
              IF (IZP .EQ. 1) THEN
                EP= EP/IAP
                IAP= 1
                IF (EP .GT. 1000.) GO TO 180
              ELSE IF (IZP .EQ. 2) THEN
                EP= EP/IAP*3.
                IAP= 3
                IF (EP .GT. ECUT + 2500.) GO TO 180
              ELSE
                GO TO 180
              END IF

            ELSE
C                      stops in plastic
            END IF
          END IF
        ELSE IF (IAP .EQ. 0) THEN
C              pion in Wall
          GO TO 180
        ELSE IF (THP .GT. 2.82/RAD) THEN
C              Outer Wall
          IF (VP .LT. 0.42) GO TO 180
C              assign to p or 4He at same velocity
          IF (IZP .EQ. 1) THEN
            EP= EP/IAP
            IAP= 1
          ELSE IF (IZP .EQ. 2) THEN
            EP= EP/IAP*4.
            IAP= 4
          ELSE IF (IZP .LE. 3) THEN
            IF (EP/IAP .LT. 135.) GO TO 180
          ELSE IF (IMF_EXPT .AND. IZP .LE. 6) THEN
            IF (EP/IAP .LT. 135.) GO TO 180
          ELSE
            GO TO 180
          END IF
        ELSE IF (THP .GT. 2.21/RAD) THEN
C              gap
          GO TO 180
        ELSE IF (THP .GT. 0.7/RAD) THEN
C              Inner Wall
          IF (VP .LT. 0.37) GO TO 180
C              assign to p or 4He at same velocity
          IF (IZP .EQ. 1) THEN
            EP= EP/IAP
            IAP= 1
          ELSE IF (IZP .EQ. 2) THEN
            EP= EP/IAP*4.
            IAP= 4
          ELSE
            GO TO 180
          END IF
        ELSE
C              Bull'S Eye
        IF (VP .LT. 0.32) GO TO 180
C              assign to p at same velocity
        IF (IZP .EQ. 1) THEN
          EP= EP/IAP
          IAP= 1
        ELSE
          GO TO 180
        END IF
      END IF

C          Accept this particle
      TAKE=.TRUE.
      RETURN

180   CONTINUE
C          Reject this particle
      TAKE= .FALSE.
C
      END


C#########################################
        SUBROUTINE TARCUT_CLUSTERS(IAP,IZP,ECUT,THP)
C  Determines the minimum energy with which the particle can exit the
C  target at this angle.  CTGT is a character mnemonic to indicate the
C  target used in the data to be simulated.

      DIMENSION TARTHICK(5),TARCPI(5),TARBPI(5),TARCPR(5),TARBPR(5),
     1 TARCDE(5),TARBDE(5),TARCTR(5),TARBTR(5),TARC3H(5),TARB3H(5),
     1 TARC4H(5),TARB4H(5)

C           IF (CTGT .EQ. 'CA') NTGT= 1
C           IF (CTGT .EQ. 'NB') NTGT= 2
C           IF (CTGT .EQ. 'PB') NTGT= 3
C           IF (CTGT .EQ. 'AU') NTGT= 4
C           IF (CTGT .EQ. 'FE') NTGT= 5

*       COMMON /TGT/ NTGT
      parameter (ntgt=1)

C  the target thicknesses and dE/dX parameters:
        DATA TARTHICK /0.159, 0.218, 0.466, 0.250, 0.174/
        DATA TARCPI /40.349, 24.746, 19.41, 22.65, 33.75/
        DATA TARBPI /1.721, 1.624, 1.614, 1.663, 1.688/
        DATA TARCPR /174.16, 116.10, 78.08, 66.8, 113.0/
        DATA TARBPR /1.742, 1.706, 1.668, 1.631, 1.671/
        DATA TARCDE /291.62, 194.17, 126.6, 101.8, 178.1/
        DATA TARBDE /1.743, 1.706, 1.673, 1.628, 1.670/
        DATA TARCTR /392.61, 258.55, 167.1, 130.2, 232.4/
        DATA TARBTR /1.742, 1.706, 1.675, 1.627, 1.669/
        DATA TARC3H /1576.4, 1034.2, 666.7, 539.3, 945.7/
        DATA TARB3H /1.743, 1.706, 1.674, 1.632, 1.671/
        DATA TARC4H /1951.9, 1267.2, 806.9, 639.2, 1142.4/
        DATA TARB4H /1.743, 1.706, 1.673, 1.631, 1.671/

        ECUT= 0.

        IF (IAP .EQ. 0) THEN
            TARC= TARCPI(NTGT)
            TARB= TARBPI(NTGT)
        ELSE IF (IZP .EQ. 1) THEN
            IF (IAP .EQ. 1) THEN
                TARC= TARCPR(NTGT)
                TARB= TARBPR(NTGT)
            ELSE IF (IAP .EQ. 2) THEN
                TARC= TARCDE(NTGT)
                TARB= TARBDE(NTGT)
            ELSE
                TARC= TARCTR(NTGT)
                TARB= TARBTR(NTGT)
            END IF
        ELSE IF (IZP .EQ. 2) THEN
            IF (IAP .EQ. 3) THEN
                TARC= TARC3H(NTGT)
                TARB= TARB3H(NTGT)
            ELSE
                TARC= TARC4H(NTGT)
                TARB= TARB4H(NTGT)
            END IF
        ELSE
            TARB= TARB4H(NTGT)  ! use the 4He b.
C        c is proportional to Z**2 and A**(b-1), and is normalized to 4He.
            TARC= TARC4H(NTGT)*(IZP*IZP/4)*(IAP/4)**(TARB-1.)
        END IF

        THCUT= ABS(COS(THP))
        IF (THCUT .EQ. 0.) RETURN
C      Ecut= b/(1+b)[t*c*b/|cos(theta)|]**(1/b)
C       where t is the total target thickness
C       and b and c are range-energy parameters.
      ECUT=(TARB/(1.+TARB))*(TARTHICK(NTGT)*TARC*TARB/THCUT)**(1./TARB)

        END
