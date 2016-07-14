      SUBROUTINE SPACO(FCS)
C  COMPACTIFIES CHARACTER VBLE FCS, REMOVING SPACES
      CHARACTER (LEN=*) :: FCS
C
C
      NC=LEN(FCS)
C
      OUT: DO IC=1,NC-1
        DO ICC=IC,NC
          IF(FCS(ICC:ICC).NE.' ')THEN
            IF(ICC.NE.IC)THEN
              FCS(IC:IC)=FCS(ICC:ICC)
              FCS(ICC:ICC)=' '
            ENDIF
            CYCLE OUT
          ENDIF
        ENDDO
        EXIT
      ENDDO OUT
C
      END
