      use global
      INCLUDE 'NUCLS'
      COMMON/ZAN/Z,AN
      CHARACTER*14 FNAS
C
      EXTERNAL FUN
      PARAMETER (N=3)
      DIMENSION X(N),WK(N*N+4*N+1)
      DIMENSION XB(N)         !best
C
      integer :: ISEED=1234567
C
C
      call global_ranInit(iseed)

      FNAS=FNAME//'.SHP'
      CALL SPACO(FNAS)
      OPEN(17,FILE=FNAS,STATUS='UNKNOWN')
C
      DO 50 K=1,2
      IF(K.EQ.1)THEN
        Z=IZ1
        AN=IA1-IZ1
      ELSE
        Z=IZ2
        AN=IA2-IZ2
      ENDIF
      IF(Z+AN.EQ.0.OR.NOPOT)GOTO 50
C
      XB(1)=-.007-FCOU*.006   !starting values
      XB(2)=-.007
      XB(3)=0.157
      FOUTB=FUN(XB)
C
      NEVV=0
      NKIT=80
      KC=0
      DO KIT=1,NKIT
        SELECT CASE(KC)
        CASE(1)
          FS=.5
        CASE(2)
          FS=.25
        CASE DEFAULT
          FS=1.
        END SELECT
        KKIT=MODULO(KIT-1,INT(NKIT*.33))+1
        WK(1)=-.003D0/REAL(KKIT)**.5*FS
        WK(2)=.003D0/REAL(KKIT)**.5*FS
        WK(3)=.015D0/REAL(KKIT)**.5*FS
        WK(1)=-ABS(WK(1))  !to mark separate scales
C
        DO J=1,N
          X(J)=XB(J)+2D0*(getRan()-.5D0)*WK(J)
**          X(J)=XB(J)+RANG(ISEED)*WK(J)
        ENDDO
        IF(X(1).GT.0.)X(1)=-X(1)
        IF(X(2).GT.0.)X(2)=-X(2)
        X(3)=.12+MODULO(X(3)-.12,X(3)-X(2))
C
        ACC=5E-5/KIT*NKIT/6.
        NEV=800
C
        CALL SIMPLX(FUN,N,X,FOUT,NEV,ACC,WK)
        NEVV=NEVV+NEV
C
        KCB=KC
        IF(FOUT.LT.FOUTB)THEN
          XB=X
          FOUTB=FOUT
          KC=0
        ELSE
          KC=MODULO(KC+1,3)
        ENDIF
C
        WRITE(*,'(1X,I3,1X,I5,1X,F11.4,1X,2E15.6,1X,F10.7)')
     W    KCB,NEV,FOUTB,XB
      ENDDO
C
      WRITE(*,*)REAL(FOUTB)
      WRITE(*,*)REAL(XB)
C
      CALL FINAL(XB(1),XB(2),XB(3))
C               AMIP   AMIN   RHO
      WRITE(17,*)' '
C
 50   CONTINUE
C
      END


      SUBROUTINE FINAL(AMIPP,AMINN,RH0)
      INCLUDE 'NUCLS'
      PARAMETER(NEQN=7)
      DIMENSION Y(NEQN)
      COMMON/CAM/AMIP,AMIN
C
      EXTERNAL DRV,G
      DOUBLE PRECISION R,Y,ROUT,RELERR,ABSERR,WORK,RPAR
      PARAMETER(NRDENS=0)
      PARAMETER(LWORK=11*NEQN+8*NRDENS+21)
      DIMENSION WORK(LWORK)
      PARAMETER(LIWORK=NRDENS+21)
      DIMENSION IWORK(LIWORK)
C
      PARAMETER (ELQP=1.4400E-3)
      PARAMETER (ELQ=ELQP*FCOU)
C
      AMIP=AMIPP
      AMIN=AMINN
C
      R=0D0
      Y(1)=RH0
      Y(2)=0D0
      Y(3)=0D0
      Y(4)=0D0
      Y(5)=0D0
      Y(6)=0D0
      Y(7)=0D0
      IF(COU)THEN
        ROUT=20D0
        RELERR=1D-6
        ABSERR=1D-6
C
        ITOL=0
        IOUT=1
        WORK=0D0
        IWORK=0
        IWORK(3)=-1
        CALL DOP853(NEQN,DRV,R,Y,ROUT,RELERR,ABSERR,ITOL,G,IOUT,
     &    WORK,LWORK,IWORK,LIWORK,RPAR,IPAR,IDID)
C
        Z=Y(3)
        PHIC=Y(5)
        PHIR=Z*ELQ/R
        DPHI=PHIR-PHIC
C
        R=0D0
        Y(1)=RH0
        Y(2)=0D0
        Y(3)=0D0
        Y(4)=0D0
        Y(5)=DPHI
        AMIP=AMIP+DPHI
        Y(6)=0D0
        Y(7)=0D0
      ENDIF
C
      CALL PN(RHP,RHN,RH0,AMIP,AMIN,DPHI,REST,EDEN)
      WRITE(17,*)0.,RHP,RHN,RH0,DPHI
      WRITE(*,'(F5.2,4(1X,F6.4))')R,RHP,RHN,Y(1),Y(5)
C
      R=0D0
      ROUT=0D0
 10   CONTINUE
      ROUT=ROUT+.2D0
      RELERR=1D-6
      ABSERR=1D-6
C
      ITOL=0
      IOUT=1
      WORK=0D0
      IWORK=0
      IWORK(3)=-1
      CALL DOP853(NEQN,DRV,R,Y,ROUT,RELERR,ABSERR,ITOL,G,IOUT,
     &  WORK,LWORK,IWORK,LIWORK,RPAR,IPAR,IDID)
      CALL PN(RHP,RHN,REAL(Y(1)),AMIP,AMIN,REAL(Y(5)),REST,EDEN)
C
      IF(IDID.EQ.1.OR.IDID.EQ.2)
     W  WRITE(17,*)REAL(R),RHP,RHN,RHP+RHN,REAL(Y(5))
      WRITE(*,'(F5.2,4(1X,F6.4),1X,I2)')R,RHP,RHN,RHP+RHN,Y(5),IDID
      IF(R.LT.20D0.AND.IDID.EQ.1)GOTO 10
      WRITE(17,*)-10.,0.,0.,0.,0.
C
      E=Y(7)
      Z=Y(3)
      AN=Y(4)
C
      WRITE(*,*)' '
      WRITE(*,*)'Z/N/B ',Z,AN,E/(Z+AN)
      WRITE(*,*)'MP/MN ',AMIP,AMIN
C
      END


      FUNCTION FUN(X)
      DIMENSION X(*)
      COMMON/ZAN/Z,AN
**      COMMON/RHO/RHO
C
      CALL VALUES(ZZ,ANN,E,X(1),X(2),X(3))
C
      EN=E/(Z+AN)
      EN=MIN(EN,1E25)
      DZ=ZZ-Z
      ADZ=MIN(ABS(DZ),1E18)
      DN=ANN-AN
      ADN=MIN(ABS(DN),1E18)
      FUN=(ADZ+ADN)*10.
     F  +((0.5+ADZ+ADZ*ADZ*50.)*SQRT(ADZ)
     U  +(0.5+ADN+ADN*ADN*50.)*SQRT(ADN))*200./MAX(1.,AN+Z)
     U  +3.6E3*EN*(Z+AN)**1.75/(.7*(ZZ+ANN)+.3*(Z+AN))
      IF(X(2).GT.0.)FUN=FUN+(1E3*X(2))**2     !redundant if the other used
      IF(X(1).GT..1)FUN=FUN+(1E3*(X(1)-.1))**2  !redundant if the other used
      FUN=MIN(FUN,1E10)
*      WRITE(*,*)X(1),X(2),X(3),FUN
*      write(*,*)'a/z/en ',anN,zZ,EN
C
      END


      SUBROUTINE VALUES(Z,AN,E,AMIPP,AMINN,RH0)
      INCLUDE 'NUCLS'
      PARAMETER (NEQN=7)
      DIMENSION Y(NEQN)
      PARAMETER (THIRD=1./3.)
C
      EXTERNAL DRV,G
      DOUBLE PRECISION R,Y,ROUT,RELERR,ABSERR,WORK,RPAR
      PARAMETER(NRDENS=0)
      PARAMETER(LWORK=11*NEQN+8*NRDENS+21)
      DIMENSION WORK(LWORK)
      PARAMETER(LIWORK=NRDENS+21)
      DIMENSION IWORK(LIWORK)
C
      PARAMETER (ELQP=1.4400E-3)
      PARAMETER (ELQ=ELQP*FCOU)
      PARAMETER (H=.19733)
      COMMON/GEN/PFRHO,PI
      COMMON/CAM/AMIP,AMIN
C
      AMIP=AMIPP
      AMIN=AMINN
C
      PI=4.*ATAN(1.)
      PFRHO=2.*PI*H*(3./(4.*PI*4.))**THIRD
C
      R=0D0
      Y(1)=RH0       !density
      Y(2)=0D0       !1st derivative of the density
      Y(3)=0D0       !proton number
      Y(4)=0D0       !neutron number
      Y(5)=0D0       !Coulomb potential
      Y(6)=0D0       !1st derivative of Coulomb
      Y(7)=0D0       !net energy
C
      RELERR=1D-6
      ABSERR=1D-6
      ROUT=20D0
      ITOL=0
      IOUT=1
      WORK=0D0
      IWORK=0
      IWORK(3)=-1
      CALL DOP853(NEQN,DRV,R,Y,ROUT,RELERR,ABSERR,ITOL,G,IOUT,
     &  WORK,LWORK,IWORK,LIWORK,RPAR,IPAR,IDID)
      CALL PN(RHP,RHN,REAL(Y(1)),AMIP,AMIN,REAL(Y(5)),REST,EDEN)
C
      E=Y(7)
      Z=Y(3)
      AN=Y(4)
      IF(COU)THEN
        PHIC=Y(5)
        PHIR=Z*ELQ/R
        DPHI=PHIR-PHIC
        E=E+.5*DPHI*Z
      ENDIF
C
*      WRITE(*,*)' '
*      WRITE(*,*)Z,AN,E/(Z+AN)
**     WRITE(*,*)AMIP,AMIN
C
      END


      SUBROUTINE G(NR,ROLD,R,Y,NEQN,CON,ICOMP,ND,RPAR,IPAR,IRTRN)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      DIMENSION Y(*),CON(*),ICOMP(*)
C
      IF(R.GT.0.1D0.AND.Y(1).LE.0D0)IRTRN=-1
C
      END


      SUBROUTINE DRV(NEQN,R,Y,YP,RPAR,IPAR)
      DIMENSION Y(*),YP(*)
      DOUBLE PRECISION R,Y,YP,RPAR
      INCLUDE 'NUCLS'
      COMMON/CAM/AMIP,AMIN
      PARAMETER (CSUR=.067D0)  !*(1.-MO*.15D0)) !coef in surface energy in GeV*fm**5
      COMMON/GEN/PFRHO,PI
      PARAMETER (ELQP=1.4400E-3)
      PARAMETER (ELQ=ELQP*FCOU)
C
C
C  PROVISION TO STOP INTEGRATION ONCE NEGATIVE DENSITY IS REACHED
      IF(Y(1).LE.0D0)THEN
        YP(1)=0D0      !density
        YP(2)=0D0      !1st derivative of the density
        YP(3)=0D0      !proton number
        YP(4)=0D0      !neutron number
        YP(5)=Y(6)     !y(5) - Coulomb potential, y(6) - 1st drv of Coulomb pot
        IF(COU)THEN
          YP(6)=-2D0*YP(5)/MAX(R,1D-6)   !1st drv of Coulomb potential, used above
        ELSE                     !
          YP(6)=0D0
        ENDIF
        YP(7)=0D0      !energy
        RETURN
      ENDIF
C
C
      YP(1)=Y(2)                           !density
C
      CALL PN(RHP,RHN,REAL(Y(1)),AMIP,AMIN,REAL(Y(5)),REST,EDEN)
C
      YP(2)=.25D0/CSUR*(REST-AMIP-AMIN)      !2'nd density
      YP(2)=YP(2)-2D0*YP(1)/MAX(R,1D-6)     !correction for spherical
      FRR=SIGN(4D0*PI*R*R,Y(1))
      YP(3)=FRR*RHP                        !proton number
      YP(4)=FRR*RHN                        !neutron number
      YP(5)=Y(6)                           !Coulomb potential
      IF(COU)THEN
        YP(6)=-4D0*PI*ELQ*RHP              !2'nd Coulomb
        YP(6)=YP(6)-2D0*YP(5)/MAX(R,1D-6)
      ELSE
        YP(6)=0D0
      ENDIF
      EDEN=EDEN+CSUR*Y(2)**2
      YP(7)=FRR*EDEN                       !energy
C
      END


      SUBROUTINE PN(RHP,RHN,RHV,AMIP,AMIN,UC,REST,EDEN)
      EXTERNAL DMI,DMI1
      COMMON/CMI/AMID,RHVC
C
C  PROVISION FOR (NEAR-)NEGATIVE DENSITY
      IF(RHV.LE.1E-10)THEN
        RHP=0.
        RHN=0.
        REST=UC
        EDEN=0.
        RETURN
      ELSE
        RHVC=RHV
      ENDIF
C
      AMID=AMIP-AMIN-UC
      RE=0.
      AE=1E-8
      X=0.
      X1=1.
      CALL ZEROIN(DMI,X,X1,RE,AE,IFLAG)
**    IF(IFLAG.GT.2)WRITE(*,*)IFLAG,X
C
      CALL DMI1(X,RHP,RHN,REST,EDEN)
      REST=REST+UC
      EDEN=EDEN+.5*RHP*UC
C
      END


      FUNCTION DMI(X)
      INCLUDE 'NUCLS'
      PARAMETER(THIRD=1./3.)
      PARAMETER(AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
**    PARAMETER(AM0K=AM0*AM0)
C
      COMMON/GEN/PFRHO,PI
      COMMON/CMI/AMID,RHVC
C
      PARAMETER (AN0=.160)
      PARAMETER (UIZO=.097*2.)
      PARAMETER (FIZO=UIZO/AN0,FIZOH=.5*FIZO)
C
      RHPV=RHVC*X
      RHNV=RHVC-RHPV
C
      PFP=PFRHO*(RHPV+RHPV)**THIRD
      PFN=PFRHO*(RHNV+RHNV)**THIRD
C
      IF(LMI)THEN
*       PFK=PFP*PFP
*       EP=SQRT(AM0K+PFK)
*       EFP=PFK/(AM0+EP)
*       RHPS=RHPV*(1.-.3*PFK/AM0K)  ! old
C
*       PFK=PFN*PFN
*       EN=SQRT(AM0K+PFK)
*       EFN=PFK/(AM0+EN)
*       RHNS=RHNV*(1.-.3*PFK/AM0K)  ! old
C
        RHPS=.5*AS(RHPV+RHPV)
        RHNS=.5*AS(RHNV+RHNV)
C
        RHS=RHPS+RHNS
C
        US=UPO(RHS/AN0)
C
        AMS=AM0+US
        AMSK=AMS*AMS
C
        PFPK=PFP*PFP
        EP=SQRT(PFPK+AMSK)
C
        PFNK=PFN*PFN
        EN=SQRT(PFNK+AMSK)
C
        DE=(PFPK-PFNK)/(EP+EN)
      ELSE
        DE=AM0*(EKIM(PFP/AM0,RHVC/AN0)-EKIM(PFN/AM0,RHVC/AN0))
      ENDIF
C
**    DMI=AMID-EFP+EFN
      DMI=AMID-DE
**      IF(COU)DMI=DMI-FIZOH*(RHPV-RHNV)           !Jun modification
      IF(COU)DMI=DMI-4.*(X+X-1.)*SINT(RHVC/AN0)
C
      END


      SUBROUTINE DMI1(X,RHPV,RHNV,REST,EDEN)
      INCLUDE 'NUCLS'
      PARAMETER(THIRD=1./3.)
      PARAMETER(AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
**    PARAMETER(AM0K=AM0*AM0)
C
      COMMON/GEN/PFRHO,PI
      COMMON/CMI/AMID,RHVC
C
      PARAMETER (AN0=.160)
      PARAMETER (UIZO=.097*2.)
      PARAMETER (FIZO=UIZO/AN0,FIZOH=.5*FIZO)
C
      RHPV=RHVC*X
      RHNV=RHVC-RHPV
C
      PFP=PFRHO*(RHPV+RHPV)**THIRD
      PFN=PFRHO*(RHNV+RHNV)**THIRD
C
      IF(LMI)THEN
*       PFK=PFP*PFP
*       EP=SQRT(AM0K+PFK)
*       EFP=PFK/(AM0+EP)
*       RHPS=RHPV*(1.-.3*PFK/AM0K)     ! old
C
*       PFK=PFN*PFN
*       EN=SQRT(AM0K+PFK)
*       EFN=PFK/(AM0+EN)
*       RHNS=RHNV*(1.-.3*PFK/AM0K)     ! old
C
        RHPS=.5*AS(RHPV+RHPV)
        RHNS=.5*AS(RHNV+RHNV)
C
        RHS=RHPS+RHNS
C
        US=UPO(RHS/AN0)
C
        AMS=AM0+US
        AMSK=AMS*AMS
        USA=US*(AMS+AM0)
C
        PFPK=PFP*PFP
        EP=SQRT(PFPK+AMSK)
C
        PFNK=PFN*PFN
        EN=SQRT(PFNK+AMSK)
C
**      REST=EFP+EFN+US+US
        REST=(PFPK+USA)/(EP+AM0)+(PFNK+USA)/(EN+AM0)
C
        EDEN=.5*EKID(RHPV+RHPV)+.5*EKID(RHNV+RHNV)+EPO(RHS/AN0)
**      EDEN=.6*(RHPV*EFP+RHNV*EFN)+EPO(RHS/AN0)
      ELSE
        RHA0=RHVC/AN0
        REST=EKIM(PFP/AM0,RHA0)+EKIM(PFN/AM0,RHA0)
     R    +2.*(DUPNO(X)+UPO(RHA0))
        EDEN=EKIDM(X)+EPO(RHA0)
      ENDIF
C
**      IF(COU)EDEN=EDEN
**     E  +.25*FIZOH*(RHPV-RHNV)**2                       !Jun modification
      IF(COU)THEN
        EDEN=EDEN+(RHPV-RHNV)**2*SINT(RHVC/AN0)/RHVC
        REST=REST+2.*((RHPV-RHNV)/AN0)**2*DSIX(RHVC/AN0)
      ENDIF
C
      END


      INCLUDE 'EOSET'


      FUNCTION UPO(RTT)
C  OPTICAL POTENTIAL IN NONRELATIVISTIC MATTER IN GEV'S
C  RT IS THE DENSITY RATIO
      INCLUDE 'NUCLS'
      COMMON/CEOS/AES,BES,GES,GES1,APES,BPES
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      SAVE F1
C
      IF(FIRST)THEN
        F1=1./2.5**GES1
        FIRST=.FALSE.
      ENDIF
C
      RT=MIN(RTT,20.)
      IF(RT.GT.1E-5.AND.POT)THEN
        RTG=RT**GES1
        UPO=RT*(AES+BES*RTG)/(1.+F1*RTG)
      ELSE
        UPO=0.
      ENDIF
C
      END


      FUNCTION EPO(RTT)
      INCLUDE 'NUCLS'
C  POTENTIAL ENERGY DENSITY IN NONRELATIVISTIC MATTER IN GEV/FM**3
C  FOR THE OPTICAL POTENTIAL ABOVE
C  RT IS THE DENSITY RATIO
      PARAMETER (AN0=.160)
C
      PARAMETER(DRT=.02,NRT=800,NRT1=NRT-1,NSM=3)
      DIMENSION EPOR(0:NRT)
      DATA EPOR(0)/0./
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C
      IF(NOPOT)THEN
        EPO=0.
        RETURN
      ENDIF
C
      IF(FIRST)THEN
        AA=0.
        DO IRT=1,NRT
          DO ISM=1,NSM
            AA=AA+UPO(DRT*(IRT-1+(ISM-.5)/NSM))
          ENDDO
          EPOR(IRT)=AA/NSM*AN0/(IRT*IRT*DRT)
        ENDDO
        FIRST=.FALSE.
      ENDIF
C
      RT=MIN(RTT,20.)
      RTN=RT/DRT
      IR=RTN
      IR=MIN(IR,NRT1)
      IR=MAX(0,IR)
      IR1=IR+1
      RI=RTN-IR
      RI1=1.-RI
      EPO=RI1*EPOR(IR)+RI*EPOR(IR1)
      EPO=EPO*RT*RT
C
      END


      FUNCTION EKID(AN)
C  CALCULATES KINETIC ENERGY DENSITY
      PARAMETER(AN0=.160)
      PARAMETER(H=.19733)
      PARAMETER(AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
C
      PARAMETER(DRT=.02,NRT=800,NRT1=NRT-1)
      DIMENSION FAR(0:NRT)
      DATA FAR(0)/1./
C
      COMMON/CAMS/AMS,US
C
      EXTERNAL FUNI
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      SAVE F1,F3
C
      IF(FIRST)THEN
        PI=4.*ATAN(1.)
        F1=(PI+PI)*(3./(4.*4.*PI))**(1./3.)*H
        F3=1./3.
        F11=4./(H*(PI+PI))**3*4.*PI
        DO I=1,NRT
          ANO=AN0*I*DRT
          AAS=AS(ANO)
          US=UPO(AAS/AN0)
*         us=0.
          AMS=AM0+US
          PF=F1*ANO**F3
          CALL IGRAL(0.,PF,.5*PF,FUNI,E)
          E=E*F11-AAS*US
          FAR(I)=E/(.6*.5*PF*PF/AM0*ANO)
*         WRITE(*,*)I*DRT,FAR(I)
        ENDDO
        FIRST=.FALSE.
      ENDIF
C
      RTN=AN/(AN0*DRT)
      RTN=MIN(RTN,REAL(NRT1))
      IR=RTN
      IR=MIN(IR,NRT1)
      IR=MAX(0,IR)
      IR1=IR+1
      RI=RTN-IR
      RI1=1.-RI
      EKID=RI1*FAR(IR)+RI*FAR(IR1)
      PF=F1*AN**F3
      EKID=EKID*.6*.5*PF*PF/AM0*AN
C
      END


      FUNCTION AS(AN)
      PARAMETER(AN0=0.160)
C
      PARAMETER(DRT=.1,NRT=80,NRT1=NRT-1)
      DIMENSION FAR(0:NRT)
      DATA FAR(0)/1./
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      IF(FIRST)THEN
        DO I=1,NRT
          ANO=AN0*I*DRT
          FAR(I)=ASO(ANO,PF)/ANO
        ENDDO
        FIRST=.FALSE.
      ENDIF
C
      RTN=AN/(AN0*DRT)
      RTN=MIN(RTN,REAL(NRT1))
      IR=RTN
      IR=MIN(IR,NRT1)
      IR=MAX(0,IR)
      IR1=IR+1
      RI=RTN-IR
      RI1=1.-RI
      AS=RI1*FAR(IR)+RI*FAR(IR1)
      AS=AS*AN
C
      END


      FUNCTION ASO(AN,PF)
      PARAMETER(AN0=0.160)
      PARAMETER(H=.19733)
      PARAMETER(AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
C
      COMMON/CAMS/AMS,US
C
      SAVE F1,F3
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      IF(FIRST)THEN
        PI=4.*ATAN(1.)
        F1=(PI+PI)*(3./(4.*4.*PI))**(1./3.)*H
        F3=1./3.
        FIRST=.FALSE.
      ENDIF
C
      PF=F1*AN**F3
*     WRITE(*,*)'PF = ',PF
C
      RV=AN/AN0
      AMS=AM0+UPO(RV)
      ASO=AN*AMS/SQRT(AMS*AMS+PF*PF)
C
      K=0
 10   CONTINUE
      ASB=ASO
      AMS=AM0+UPO(ASO/AN0)
      ASO=RSR(PF)
      K=K+1
      IF(ABS(ASO-ASB).GT.1E-3*ASO)GOTO 10
C
*     WRITE(*,*)K,AN/AN0,ASO/AN0,UPO(ASO/AN0)
C
      END


      FUNCTION RSR(PF)
      EXTERNAL FUNS
      PARAMETER(H=.19733)
C
      SAVE F1
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      IF(FIRST)THEN
        PI=4.*ATAN(1.)
        F1=4./(H*(PI+PI))**3*4.*PI
        FIRST=.FALSE.
      ENDIF
C
      CALL IGRAL(0.,PF,.5*PF,FUNS,RSR)
      RSR=RSR*F1
C
      END


      FUNCTION FUNS(P)
      COMMON/CAMS/AMS,US
C
      PK=P*P
      FUNS=PK*AMS/SQRT(PK+AMS*AMS)
C
      END


      FUNCTION FUNI(P)
      PARAMETER(AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      COMMON/CAMS/AMS,US
C
      PK=P*P
      FUNI=PK*(PK+US*(AMS+AM0))/(SQRT(PK+AMS*AMS)+AM0)
*     funi=pk*(pk/(am0+am0)+us*ams/sqrt(pk+ams*ams))
C
      END


C  ROUTINES FOR MO-DEP POTENTIALS


      FUNCTION EKIM(PM,RVP)
C  CALCULATES RATIO EKIN/MASS IN THE MO-DEP CASE
C  RVP IS THE EFFECTIVE DENSITY RATIO
      PARAMETER(NRV=20,DXR=1./NRV)
      PARAMETER(NPM=20,DYP=1./NPM)
      PARAMETER(NPP=8)
      DIMENSION REK(0:NRV,0:NPM)
      PARAMETER(NRV1=NRV-1,NPM1=NPM-1)
C
      PARAMETER(RVC=1.,PMCC=.263)
C
      SAVE REK
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C
      IF(FIRST)THEN
        DO I=0,NRV
          IF(I.LT.NRV)THEN
            XR=I*DXR
            RVV=RVC*XR/(1.-XR)
          ELSE
            RVV=1E8
          ENDIF
          AMF=1./(1.+FXI(RVV))
          AMFK=AMF*AMF
          PMC=PMCC/AMF
C
          PMB=0.
          AIN=0.
          REK(I,0)=1.
          REK(I,NPM)=1.
          DO J=1,NPM1
            YP=J*DYP
            PMM=PMC*YP/(1.-YP)
            DPP=(PMM-PMB)/NPP
            DO JJ=1,NPP
              AIN=AIN+DPP*VEL((PMB+(JJ-.5)*DPP),RVV)
            ENDDO
            PMB=PMM
            PMK=PMM*PMM
            REK(I,J)=AIN*(AMF+SQRT(AMFK+PMK))/PMK
          ENDDO
        ENDDO
        FIRST=.FALSE.
      ENDIF
C
      XR=NRV*RVP/(RVP+RVC)
      I=XR
      I=MIN(I,NRV1)
      I1=I+1
      XI=XR-I
      XI1=1.-XI
C
      AMF=1./(1.+FXI(RVP))
      PMC=PMCC/AMF
      YP=NPM*PM/(PM+PMC)
      J=YP
      J=MIN(J,NPM1)
      J1=J+1
      YJ=YP-J
      YJ1=1.-YJ
C
      RE=XI1*(YJ1*REK(I,J)+YJ*REK(I,J1))
     R  +XI*(YJ1*REK(I1,J)+YJ*REK(I1,J1))
C
      PMK=PM*PM
      EKIM=RE*PMK/(AMF+SQRT(AMF*AMF+PMK))
C
      END


*      FUNCTION EKIDM(X)
*      PARAMETER(NP=40)
*C
*      COMMON/GEN/PFRHO,PI
*      COMMON/CMI/AMID,RHVC
*C
*      PARAMETER(AN0=.160)
*      PARAMETER(AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
*      PARAMETER(H=.19733)
*C
*      PARAMETER (THIRD=1./3.)
*C
*C
*      RHA0=RHVC/AN0
*C
*      RHPV=RHVC*X
*      RHNV=RHVC-RHPV
*C
*      pi=4.*atan(1.)
*      pfrho=2.*pi*h*(3./(4.*pi*4.))**third
*c
*      PFP=PFRHO*(RHPV+RHPV)**THIRD
*      PFN=PFRHO*(RHNV+RHNV)**THIRD
*C
*      PFPQ=PFP**3
*      PFNQ=PFN**3
*C
*      A=0.
*      DO I=1,NP
*        PP=PFP*(I-.5)/NP
*        PN=PFN*(I-.5)/NP
*        A=A+(PFPQ-PP**3)*VEL(PP/AM0,RHA0)*PFP/NP
*     A    +(PFNQ-PN**3)*VEL(PN/AM0,RHA0)*PFN/NP
*      ENDDO
*C
*      EKIDM=A/3.*2.*4.*PI/(2.*PI*H)**3
*C
*      END


      FUNCTION EKIDM(X)
C
      COMMON/GEN/PFRHO,PI
      COMMON/CMI/AMID,RHVC
C
      PARAMETER(AN0=.160)
      PARAMETER(AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER(H=.19733)
C
      PARAMETER (THIRD=1./3.)
C
      PARAMETER(NG=14)
      DIMENSION XG(NG),WG(NG)
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      SAVE XG,WG
C
C
      IF(FIRST)THEN
        PI=4.*ATAN(1.)
        PFRHO=2.*PI*H*(3./(4.*PI*4.))**THIRD
C
        CALL GAUSGN(NG,XG,WG)
        DO IG=1,NG      !renormalization for [0,1] interval
          XG(IG)=.5*(XG(IG)+1.)
          WG(IG)=.5*WG(IG)
        ENDDO
C
        FIRST=.FALSE.
      ENDIF
C
      RHA0=RHVC/AN0
C
      RHPV=RHVC*X
      RHNV=RHVC-RHPV
C
      PFP=PFRHO*(RHPV+RHPV)**THIRD
      PFN=PFRHO*(RHNV+RHNV)**THIRD
C
      PFPQ=PFP**3
      PFNQ=PFN**3
C
      A=0.
      DO IG=1,NG
        PP=PFP*XG(IG)
        PN=PFN*XG(IG)
        A=A+WG(IG)*((PFPQ-PP**3)*VEL(PP/AM0,RHA0)*PFP
     A    +(PFNQ-PN**3)*VEL(PN/AM0,RHA0)*PFN)
      ENDDO
C
      EKIDM=A/3.*2.*4.*PI/(2.*PI*H)**3
C
      END


*      FUNCTION DUPNO(X)
*      PARAMETER(NP=40)
*C
*      COMMON/GEN/PFRHO,PI
*      COMMON/CMI/AMID,RHVC
*C
*      PARAMETER(AN0=.160)
*      PARAMETER(AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
*      PARAMETER(H=.19733)
*C
*      PARAMETER (THIRD=1./3.)
*C
*C
*      RHA0=RHVC/AN0
*C
*      RHPV=RHVC*X
*      RHNV=RHVC-RHPV
*C
*      pi=4.*atan(1.)
*      pfrho=2.*pi*h*(3./(4.*pi*4.))**third
*c
*      PFP=PFRHO*(RHPV+RHPV)**THIRD
*      PFN=PFRHO*(RHNV+RHNV)**THIRD
*C
*      PFPQ=PFP**3
*      PFNQ=PFN**3
*C
*      A=0.
*      DO I=1,NP
*        PP=PFP*(I-.5)/NP
*        PN=PFN*(I-.5)/NP
*        A=A+(PFPQ-PP**3)*VELDR(PP/AM0,RHA0)*PFP/NP
*     A    +(PFNQ-PN**3)*VELDR(PN/AM0,RHA0)*PFN/NP
*      ENDDO
*C
*      DUPNO=A/3.*2.*4.*PI/(2.*PI*H)**3/AN0
*C
*      END


      FUNCTION DUPNO(X)
C
      COMMON/GEN/PFRHO,PI
      COMMON/CMI/AMID,RHVC
C
      PARAMETER(AN0=.160)
      PARAMETER(AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER(H=.19733)
C
      PARAMETER (THIRD=1./3.)
C
      PARAMETER(NG=14)
      DIMENSION XG(NG),WG(NG)
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      SAVE XG,WG
C
C
      IF(FIRST)THEN
        PI=4.*ATAN(1.)
        PFRHO=2.*PI*H*(3./(4.*PI*4.))**THIRD
C
        CALL GAUSGN(NG,XG,WG)
        DO IG=1,NG      !renormalization for [0,1] interval
          XG(IG)=.5*(XG(IG)+1.)
          WG(IG)=.5*WG(IG)
        ENDDO
C
        FIRST=.FALSE.
      ENDIF
C
      RHA0=RHVC/AN0
C
      RHPV=RHVC*X
      RHNV=RHVC-RHPV
C
      PFP=PFRHO*(RHPV+RHPV)**THIRD
      PFN=PFRHO*(RHNV+RHNV)**THIRD
C
      PFPQ=PFP**3
      PFNQ=PFN**3
C
      A=0.
      DO IG=1,NG
        PP=PFP*XG(IG)
        PN=PFN*XG(IG)
        A=A+WG(IG)*((PFPQ-PP**3)*VELDR(PP/AM0,RHA0)*PFP
     A    +(PFNQ-PN**3)*VELDR(PN/AM0,RHA0)*PFN)
      ENDDO
C
      DUPNO=A/3.*2.*4.*PI/(2.*PI*H)**3/AN0
C
      END


      FUNCTION VEL(PM,RVP)
C  CALCULATES VELOCITY IN THE MO-DEP CASE
      COMMON/CEOS/AES,BES,GES,GES1,APES,BPES
      PARAMETER(AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
C
      PMK=PM*PM
      VEL=PM/SQRT(PMK+1./(1.+FXI(RVP)/(1.+BPES*PMK)**2)**2)
C
      END


      FUNCTION VELDR(PM,RVP)
C  CALCULATES DERIVATIVE D_VELOCITY/D_RVP IN THE MO-DEP CASE
C  RVP IS THE EFFECTIVE DENSITY RATIO
      COMMON/CEOS/AES,BES,GES,GES1,APES,BPES
      PARAMETER(AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
C
      PMK=PM*PM
      PB=1./(1.+BPES*PMK)**2
      PA=1./(1.+FXI(RVP)*PB)
      VELDR=PM*PB*PA**3/SQRT(PMK+PA*PA)**3*FXIP(RVP)
C
      END


      FUNCTION FXI(X)
C  DENSITY PROFILE FUNCTION F/MOMENTUM DEPENDENCE
      COMMON/CEOS/AES,BES,GES,GES1,APES,BPES
C
      FXI=APES*X   ! 1.5*X/(1.+X)  !   X
C
      END


      FUNCTION FXIP(X)
C  DERIVATIVE OF FUNCTION FXI
C
      DX=MAX(.01,.03*X)
      DX=MIN(DX,.03)
      XF=X+DX
      XB=MAX(.1*DX,X-DX)
      FXIP=(FXI(XF)-FXI(XB))/(XF-XB)
C
      END


      FUNCTION SINTX(X)
C
C  INTERACTION SYMMETRY ENERGY DIVIDED BY NORMALIZED DENSITY
C
      SINTX=SINT(X)/MAX(X,1E-4)
C
      END


      FUNCTION DSIX(X)
C
C DRV OF INT SYM EN DIVIDED BY NORM DENSITY, W/RESPECT TO NORM DENSITY
C
      DX=MAX(.01,.03*X)
      DX=MIN(DX,.03)
      XF=X+DX
      XB=MAX(.1*DX,X-DX)
      DSIX=(SINTX(XF)-SINTX(XB))/(XF-XB)
C
      END
