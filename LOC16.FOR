      SUBROUTINE LAB(EEI,PZI,EEL,PZL,AMM,YL)
      COMMON/RAP/YBEAM,YCM,PF,RENAN,GG1,BB1,GG2,BB2,dmo
C
      AG1=MAX(ABS(EEI+PZI),1E-5*ABS(EEI))
      AG2=MAX(ABS(EEI-PZI),1E-5*ABS(EEI))
      YI=.5*LOG(AG1/AG2)
      YL=YCM+YI
      AMM=SQRT(AG1*AG2)
      EEL=GG2*(EEI-BB2*PZI)
      PZL=GG2*(PZI-BB2*EEI)
C
      END


      SUBROUTINE BREK(IP,PXN,PYN,PZN,EEN,DEN,IDN,PXE,PYE,PZE,EEE,IDE)
      use global
C  BREAKS A DELTA INTO A NUCLEON IDN AND A PION IDE
      INCLUDE 'PARTIC15'
      PARAMETER (AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER (AM0K=AM0*AM0)
      PARAMETER (AMEC=.13957,AMEN=.13496,AME=(AMEC+AMEC+AMEN)/3.)
      PARAMETER (AMEK=AME*AME)
      PARAMETER (THIRD=1./3.)
C
      COMMON/CSEED/ISEED
C
      IDP=ABS(ID(IP))
      IF(IDP.LE.6.OR.IDP.GE.13)THEN                         !not a delta
        IDN=IDP
        PXN=PX(IP)
        PYN=PY(IP)
        PZN=PZ(IP)
        EEN=EE(IP)
        DEN=PETOM*MDE(IP)          !Coulomb + isospin
c  here just regular iso/Coul contribution to 1-ptcle energy
        IDE=0
        RETURN
      ENDIF
C
      AMVI=AMV(IP)
      PXI=PX(IP)
      PYI=PY(IP)
      PZI=PZ(IP)
      EEI=EE(IP)
C
      EMVK=EEI*EEI-(PXI*PXI+PYI*PYI+PZI*PZI)
      EMV=SQRT(EMVK)
C
**    UUP2=EMVK/AMVI-AMVI
      UUP=EMV-AMVI
C
**    EM0K=AM0K+AM0*UUP2
      EM0K=(AM0+UUP)**2
C
      PCK=((EMVK-EM0K-AMEK)**2-4.*EM0K*AMEK)/(4.*EMVK)
      IF(PCK.GT.0.)THEN
        PC=SQRT(PCK)
      ELSE
        PC=0.
      ENDIF
*      EIC=SQRT(EM0K+PCK)
      EEC=SQRT(AMEK+PCK)
C
      CTHS=2.*getRan()-1.
      CALL NEWS(0.,PC,0.,PC,CTHS,PC,EEC,PXE,PYE,PZE
     S  ,PXI/EEI,PYI/EEI,PZI/EEI,EEI/EMV)
C
      EEE=SQRT(AMEK+PXE*PXE+PYE*PYE+PZE*PZE)
*      EEN=SQRT(EM0K+PXN*PXN+PYN*PYN+PZN*PZN)
C
*      PXE=PXI-PXN
*      PYE=PYI-PYN
*      PZE=PZI-PZN
*      EEE=EEI-EEN
      PXN=PXI-PXE
      PYN=PYI-PYE
      PZN=PZI-PZE
      EEN=EEI-EEE
C
      IF(IDP.EQ.7)THEN
        IDN=1
        IDE=13                               !pi plus
      ELSEIF(IDP.EQ.10)THEN
        IDN=2
        IDE=15                               !pi minus
      ELSE
        RS=getRan()
        IF(RS.LT.THIRD)THEN
          IF(IDP.EQ.8)THEN
            IDN=2
            IDE=13                           !pi plus
          ELSEIF(IDP.EQ.9)THEN
            IDN=1
            IDE=15                           !pi minus
          ELSE
            IDN=IDP-10
            IDE=14                           !pi zero
          ENDIF
        ELSE
          IF(IDP.LE.10)THEN
            IDN=IDP-7
            IDE=14                           !pi zero
          ELSEIF(IDP.EQ.11)THEN           !star +
            IDI=2
            IDK=13                           !pi plus
          ELSE                           !star 0
            IDI=1
            IDK=15                           !pi minus
          ENDIF
        ENDIF
      ENDIF
C
      DEN=PETOM*MDE(IP)          !Coulomb always put here on a nucleon
c  here just regular iso/Coul contribution to 1-ptcle energy
C
      END


      SUBROUTINE TEST(IDP,PXP,PYP,PZP,EEP,DEP,IPT,*,IPART,IOV)
C  TESTS A QUASIPARTICLE AGAINST
C  A PLASTIC BALL DEFINITION OF PARTICIPANT
C  AND ACCEPTED PROTON/DEUTERON
      INCLUDE 'PARTID'
      COMMON/RAP/YBEAM,YCM,PF,RENAN,GG1,BB1,GG2,BB2,dmo
      PARAMETER (TLMIN=.012)
      PARAMETER (FYR=.95)
      PARAMETER (DYR=.16)
      PARAMETER (DPT=.15)
      PARAMETER (B=.008,AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER (AMB=AM0-B)
      PARAMETER (BD=.002225,AMD=AM0+AM0-BD)
      PARAMETER (AMEC=.13957,AMEN=.13496,AME=(AMEC+AMEC+AMEN)/3.)
      PARAMETER (BT=.0086,AMT=AMP+AMN+AMN-BT)
      PARAMETER (B3E=.0080,AM3E=AMP+AMP+AMN-B3E)
      PARAMETER (AMTE=.5*(AMT+AM3E))
      PARAMETER (B4E=.0286,AM4E=4.*AM0-B4E)
      PARAMETER (DPF=.040)
C
      LOGICAL TAKE
      LOGICAL FIRST
C
      DATA FIRST/.TRUE./
C
      IF(FIRST)THEN
        GG=COSH(YCM)
        PCM=AMB*SINH(YCM)
        PCMR=PCM/GG
        AG=(PF+DPF)/MAX(PCMR,1E-3)
        IF(AG.LT..8)THEN
          ANGMIN=ASIN(AG)
          CTMAX=GG*COS(ANGMIN)/SIN(ANGMIN)
          CAMX=SQRT(1./(1.+1./(CTMAX*CTMAX)))
        ELSE
          ANGMIN=0.
          CTMAX=1E20
          CAMX=1.
        ENDIF
        YPR=YBEAM-YCM
        GG=COSH(YPR)
        PCM=AMB*SINH(YPR)
        PCMR=PCM/GG
        AG=(PF+DPF)/MAX(PCMR,1E-3)
        IF(AG.LT..8)THEN
          ANGMAX=ASIN(AG)
          CTMIN=-GG*COS(ANGMAX)/SIN(ANGMAX)
          CAMN=SQRT(1./(1.+1./(CTMIN*CTMIN)))
        ELSE
          ANGMAX=0.
          CTMIN=-1E20
          CAMN=1.
        ENDIF
        RENAN=2./(CAMX+CAMN)
        WRITE(*,*)'RENAN = ',RENAN
        YPPR=FYR*YBEAM
        FIRST=.FALSE.
      ENDIF
C
      IF(IDP.EQ.0.)RETURN 1
C
      IPART=0
      IOV=0
      IF(IPT.GT.0)RETURN
C  BOUND...
C
      IIDP=ABS(IDP)
      IF(IIDP.GT.6.AND.IIDP.LT.13)RETURN
      IAP=BAR(IIDP)
      IZP=ZPA(IIDP)
      AM=AMS(IIDP)
C
      Y=.5*LOG((EEP+PZP)/(EEP-PZP))
      YL=Y+YCM
C
      PTK=PXP**2+PYP**2
      PT=SQRT(PTK)
      IF(PT*CTMIN.LT.PZP.AND.PZP.LT.PT*CTMAX)IOV=1
C  THIS WAS OUR SIMULATION OF THE 'OVERLAP REGION'
      IF(IIDP.GT.13)RETURN       !no misidents for pions in filter ...
      TL=GG2*(EEP-DEP-BB2*PZP)-AM       !DEP term: Coulomb + isospin
      IF(TL.LE.0.)RETURN
C  NEGATIVE KINETIC ENERGY IN THE LAB
      TR=GG1*(EEP-DEP-BB1*PZP)-AM       !same
      IF(TR.LT.0.)RETURN
C  NEGATIVE KINETIC ENERGY IN THE ANTILAB
C
      IZP=MAX(IZP,1)                    !neutrons treated as protons
      PZL=GG2*(PZP-BB2*EEP)
      PL=SQRT(PTK+PZL*PZL)
      VL=PL/(AM+TL+GG2*DEP)             !DEP to make kinematic velocity...
      THL=ATAN2(PT,PZL)
      IAPP=IAP
      TLL=TL*1E3
      TLLP=TLL
      CALL SIMDAT_CLUSTERS(IAP,IZP,TLLP,VL,THL,TAKE)
      IF(.NOT.TAKE)RETURN
C
      IF(TL.LT.IAP*TLMIN)RETURN
      IF(IAPP.NE.IAP.OR.TLLP.NE.TLL)THEN
        TL=1E-3*TLLP
        IF(IAP.EQ.1)THEN
          AM=AM0
        ELSEIF(IAP.EQ.2)THEN
          AM=AMD
        ELSEIF(IAP.EQ.3)THEN
          AM=AMTE
        ELSEIF(IAP.EQ.4)THEN
          AM=AM4E
        ENDIF
        EL=TL+AM
        PL=SQRT(EL*EL-AM*AM)
        PZL=PL*COS(THL)
        PT=PL*SIN(THL)
        YL=.5*LOG((EL+PZL)/(EL-PZL))
      ENDIF
      IF(ABS(YL-YPPR).LT.DYR.AND.PT.LT.IAP*DPT)RETURN
C
      IPART=IZP
C
      END


      SUBROUTINE PSPEC(PKC,KCS)
      use global
      PARAMETER(NTR=8)
C  SEARCHES FOR MOMENTUM OF SPECTATOR NUCLEON
C  IN DEUTERON BREAKUP
      COMMON/CSPC/EM0K,PKMX,ECM,EDC,PIC
      COMMON/GETD/EDKC,PDKC,EMDK,EMD0K
     G  ,VNX,VPMN,VPMX,CTHD
      COMMON/CSEED/ISEED
C
      DATA XS/.5/
C
      CALL FUNSPC(XS,FS,S1,S2)
      VN=VNX
      VPN=VPMN
      VPX=VPMX
C
      DO 30 ITR=1,NTR
      XT=getRan()
      CALL FUNSPC(XT,FT,T1,T2)
      IF(FT.LT.FS)THEN
        IF(FT.LT.FS*getRan())GOTO 30
      ENDIF
      XS=XT
      FS=FT
      S1=T1
      S2=T2
      VN=VNX
      VPN=VPMN
      VPX=VPMX
 30   CONTINUE
C
      VNX=VN
      VPMN=VPN
      VPMX=VPX
      PKC=PKMX*XS
C
      S=(S1+S2)*getRan()
      IF(S.GT.S1)THEN
C  PN
        KCS=2
      ELSE
C  PP AND NN
        KCS=1
      ENDIF
C
      END


      SUBROUTINE FUNSPC(XS,FS,S1,S2)
      COMMON/CSPC/EM0K,PKMX,ECM,EDC,PIC
      COMMON/GETD/EDKC,PDKC,EMDK,EMD0K
     G  ,VNX,VPMN,VPMX,CTHD
C
      P3=XS*PKMX
      P3K=P3*P3
      E3=SQRT(EM0K+P3K)
      EQK=.25*((ECM-E3)**2-P3K)
      EQ=SQRT(EQK)
      AG=EQK-EM0K
      IF(AG.LE.0.)THEN
        FS=0.
        S1=0.                                  !???
        S2=0.
        RETURN
      ENDIF
      Q=SQRT(AG)
      FIP=P3*EQ*Q/E3
      EPCM=(EQ+EQ)*Q
      S1=SIG(EPCM,1,1)
      S2=SIG(EPCM,1,2)
      FS=FIP*(S1+S2)
      PMAX=SQRT(((EDC*E3+PIC*P3)**2-EMD0K)/EMDK)
      AG=((EDC*E3-PIC*P3)**2-EMD0K)/EMDK
      IF(AG.GT.0.)THEN
        PMIN=SQRT(AG)
      ELSE
        PMIN=0.
      ENDIF
      VPMN=VOLDE(PMIN)
      VPMX=VOLDE(PMAX)
      VNX=VPMN-VPMX
      FS=FS*VNX
C
      END


      SUBROUTINE NEWS(PICX,PICY,PICZ,PIC,CTHS,PICC,EICC
     S  ,PIFX,PIFY,PIFZ,BX,BY,BZ,GM)
      use global
C  BX,BY,BZ,GM ARE BETA (REGULAR) COMPONENTS OF CM IN THE LAB
C  PICX,PICY,PICZ,PIC   ARE COMPONENTS OF THE VECTOR IN THE CM THAT SETS
C  REFERENCE DIRECTION, PICC & EICC ARE CM VALUES FOR DEFLECTED PARTICLE,
C  CTHS IS RANDOM COSINE FROM THE OUTSIDE, PIF ARE FINAL COMPONENTS IN THE LAB
      COMMON/CSEED/ISEED
      LOGICAL FIRST
C
      DATA FIRST/.TRUE./
C
      IF(FIRST)THEN
        PI=4.*ATAN(1.)
        PIPI=PI+PI
        FIRST=.FALSE.
      ENDIF
C
      IF(PIC.GT.0.)THEN
        CTH=PICZ/PIC
        AG=1.-CTH*CTH
      ELSE
        CTH=1.
        AG=0.
      ENDIF
      IF(AG.GT.0.)THEN
        STH=SQRT(AG)
        COFI=PICX/(STH*PIC)
        SIFI=PICY/(STH*PIC)
      ELSE
        STH=0.
        COFI=1.
        SIFI=0.
      ENDIF
C
      AG=MAX(0.,1.-CTHS*CTHS)
      STHS=SQRT(AG)
C
      FIS=PIPI*getRan()
C
      PICXS=PICC*STHS*COS(FIS)
      PICYS=PICC*STHS*SIN(FIS)
      PICZS=PICC*CTHS
C
      PICZF=PICZS*CTH-PICXS*STH
      PICXF=PICXS*CTH+PICZS*STH
C
      PICYF=PICYS*COFI+PICXF*SIFI
      PICXF=PICXF*COFI-PICYS*SIFI
C
C  GETTING BACK TO THE LAB
      BPIC=BX*PICXF+BY*PICYF+BZ*PICZF
      BFPI=GM*(EICC+GM/(GM+1.)*BPIC)
      PIFX=PICXF+BX*BFPI
      PIFY=PICYF+BY*BFPI
      PIFZ=PICZF+BZ*BFPI
C
      END


      SUBROUTINE GEDT
      use global
      COMMON/GETD/EDKC,PDKC,EMDK,EMD0K
     G  ,VNX,VPMN,VPMX,CTHD
      PARAMETER (NK=8)
      COMMON/CSEED/ISEED
C
C
      V=getRan()*VNX+VPMX
C
      CTMX=-1.
      VMX=VPMX
      CTMN=1.
      VMN=VPMN
C
      DO K=1,NK
        CTH=.4*CTMX+.6*CTMN
        P=SQRT(((EDKC-CTH*PDKC)**2-EMD0K))/EMDK
        VV=VOLDE(P)
        IF(VV.GT.V)THEN
          CTMN=CTH
          VMN=VV
        ELSE
          CTMX=CTH
          VMX=VV
        ENDIF
      ENDDO
C
      DV=(V-VMX)/(VMN-VMX)
      CTHD=DV*CTMN+(1.-DV)*CTMX
C
      END


      SUBROUTINE GETT
      use global
      COMMON/GETTC/ETKMC,PTKMC,EMTK,EKMCK
     G  ,AGCO,VTNX,VTMN,VTMX,CTHT
      PARAMETER (NK=8)
      COMMON/CSEED/ISEED
C
C
      V=getRan()*VTNX+VTMX
C
      CTMX=-1.
      VMX=VTMX
      CTMN=1.
      VMN=VTMN
C
      DO K=1,NK
        CTH=.4*CTMX+.6*CTMN
        PK=(ETKMC-CTH*PTKMC)**2/EMTK-EKMCK
        VV=VOLTI(1.5*PK+AGCO)
        IF(VV.GT.V)THEN
          CTMN=CTH
          VMN=VV
        ELSE
          CTMX=CTH
          VMX=VV
        ENDIF
      ENDDO
C
      VVD=VMN-VMX             !precautions 9/20/92
      IF(VVD.GT.0.)THEN
        DV=(V-VMX)/VVD
        DV=MIN(1.,DV)
        DV=MAX(0.,DV)
      ELSE
        DV=0.
      ENDIF
      CTHT=DV*CTMN+(1.-DV)*CTMX
C
      END


      SUBROUTINE LOCVEt
      use global
C  CALCULATES LOCAL VELOCITIES
      COMMON/CLOPA/IX,IY,IZ,CALO
      LOGICAL CALO
      COMMON/CCLOPA/IXB,IXF,IYB,IYF,IZB,IZF,XC,YC,ZC,IIVOV
      INCLUDE 'PARTIC15'
      INCLUDE 'SIZE2'
      common/mut/pxpt,pypt,pzpt,empt,nopt
C
      common/hu/bxpt,bypt,bzpt,gmpt
C
      BXPT=0.
      BYPT=0.
      BZPT=0.
      GMPT=0.
      nopt=0.
C
      ISX=1
 30   CONTINUE
      IS=1
 40   CONTINUE
      DO 100 IIZ=IZB*ISX,IZF*ISX,ISX
      DO 90 IIY=IYB*IS,IYF*IS,IS
*      DO 80 IIX=IXB*ISX,IXF*ISX,ISX
*      DO 100 IIZ=IZ*ISX,IZ*ISX,ISX
*      DO 90 IIY=IY*IS,IY*IS,IS
      DO 80 IIX=IX*ISX,IX*ISX,ISX
      IIMX=IMX(IIX,IIY,IIZ)
      IF(IIMX.LT.0)GOTO 80
      IIMN=IMN(IIX,IIY,IIZ)
      DO 70 II=IIMN,IIMX
      IIP=IPO(II)
      IIDP=ID(IIP)
      IF(IIDP.LE.0)GOTO 70
      if(iidp.lt.3.and.NCS(iip).eq.0)goto 70
*      if(iidp.ge.3)goto 70
      BXPT=BXPT+PX(IIP)*ISX
      BYPT=BYPT+PY(IIP)*IS
      BZPT=BZPT+PZ(IIP)*ISX
      IF(LMO)THEN    ! quick and dirty 11/18/99
        GMPT=GMPT+SQRT(AMV(IIP)**2+PX(IIP)**2+PY(IIP)**2+PZ(IIP)**2)
      ELSE
        GMPT=GMPT+EE(IIP)
      ENDIF
      nopt=nopt+1
 70   CONTINUE
 80   CONTINUE
 90   CONTINUE
 100  CONTINUE
C
*     IF(SYMMY.AND.IS.EQ.1)THEN
      IF(IS.EQ.1)THEN
        IS=-1
        GOTO 40
      ENDIF
      IF(ISX.EQ.1)THEN
        ISX=-1
**      GOTO 30    ! commented out Aug 14 '96
      ENDIF
*c for b=0
*      rx=dwl*ix
*      ry=dwl*iy
*      r=sqrt(rx*rx+ry*ry)
*      if(r.eq.0.)then
*        bxpt=0.
*        bypt=0.
*      else
*        rx=rx/r
*        ry=ry/r
*        br=bxpt*rx+bypt*ry
*        bxpt=br*rx
*        bypt=br*ry
*      endif
C
      pxpt=bxpt
      pypt=bypt
      pzpt=bzpt
      empt=gmpt
      IF(gmpt.gt.0.)THEN
        BXPT=BXPT/GMPT
        BYPT=BYPT/GMPT
        BZPT=BZPT/GMPT
        GMPT=1./SQRT(1.-(BXPT*bxpt+BYPT*bypt+bzpt*bzpt))
      else
        gmpt=1.
      ENDIF
C
      END


      SUBROUTINE LOCVE
      use global
C  CALCULATES LOCAL VELOCITIES
      COMMON/CLOPA/IX,IY,IZ,CALO
      LOGICAL CALO
      COMMON/CCLOPA/IXB,IXF,IYB,IYF,IZB,IZF,XC,YC,ZC,IIVOV
      INCLUDE 'PARTIC15'
      INCLUDE 'SIZE2'
C
      COMMON/CNC/BXPA(2),BYPA(2),BZPA(2),GMPA(2),GMPA1(2)
     C  ,BXPA1,BYPA1,BZPA1,GMP1,NOPAUL1
     C  ,CX(3,2),CY(3,2),CZ(3,2)
     N  ,NOPAUL(2),TC(2),AMIC(2),FFA(2)
     N  ,TK(2)                           !for pions?
     C  ,BXPAN(2,2),BYPAN(2,2),BZPAN(2,2),GMPAN(2,2),GMPAN1(2,2)
     C  ,CXN(3,2,2),CYN(3,2,2),CZN(3,2,2)
     N  ,NOPALN(2,2),TCN(2,2),AMICN(2,2),FFAN(2,2)
     c  ,fmax
      LOGICAL NOPAUL,NOPALN,NOPAUL1
C
      COMMON/ENERGY/EKIN,EPOT,EPR,APR,ETOG,PARTI(NIDP),PARTIG(NIDP)
     E  ,ANPAR,ANPARG,ANPR(6),ANPRG(6)
     N  ,EKINT,EKINZ,ECINT,ECINZ
     N  ,EKINTG,EKINZG
     E  ,PPRX,PPRY,PPRZ,PPRZL,PGOX,PGOY,PGOZ,PGOZL
     R  ,GST,BGSTX,BGSTY,BGSTZ
     G  ,PRSKT0,PRSP0,PRSKL0,PRSXZ0
C
      COMMON/CSEED/ISEED
C
      COMMON/CPRI/PRI,TIM,IZCEN
      LOGICAL PRI
C
      CALO=.FALSE.
C
      BXPA1=0.
      BYPA1=0.
      BZPA1=0.
      gmp1=0.
      DO IPT=1,2
        BXPA(IPT)=0.
        BYPA(IPT)=0.
        BZPA(IPT)=0.
        GMPA(IPT)=0.
      ENDDO
C
      IS=1
 40   CONTINUE
      DO 100 IIZ=IZB,IZF
      DO 90 IIY=IYB*IS,IYF*IS,IS
      DO 80 IIX=IXB,IXF
      IIMX=IMX(IIX,IIY,IIZ)
      IF(IIMX.LT.0)GOTO 80
      IIMN=IMN(IIX,IIY,IIZ)
      DO 70 II=IIMN,IIMX
      IIP=IPO(II)
      IIDP=ID(IIP)
      IF(IIDP.LE.0)GOTO 70
      IPT=ABS(IPTI(IIP))
      IF(IIDP.GE.3.AND.IIDP.LE.6)THEN           !has to be adapted to clusters
        IPT=NINT((ABS(IPT)+.1*getRan())/BAR(IIDP))
      ENDIF
**    ipt=1
      BXPA(IPT)=BXPA(IPT)+PX(IIP)
      BYPA(IPT)=BYPA(IPT)+PY(IIP)*IS
      BZPA(IPT)=BZPA(IPT)+PZ(IIP)
      GMPA(IPT)=GMPA(IPT)+EE(IIP)
 70   CONTINUE
 80   CONTINUE
 90   CONTINUE
 100  CONTINUE
C
      IF(SYMMY.AND.IS.EQ.1)THEN
        IS=-1
        GOTO 40
      ENDIF
C
      NOPAUL1=GMPA(1).EQ.0..AND.GMPA(2).EQ.0.
      IF(.NOT.NOPAUL1)THEN
        BXPA1=BXPA(1)+BXPA(2)
        BYPA1=BYPA(1)+BYPA(2)
        BZPA1=BZPA(1)+BZPA(2)
        gmp1=GMPA(1)+GMPA(2)
        BXPA1=BXPA1/GMP1
        BYPA1=BYPA1/GMP1
        BZPA1=BZPA1/GMP1
        GMP1=1./SQRT(1.-
     G    (BXPA1**2+BYPA1**2+BZPA1**2))
      ENDIF
C
      DO IPT=1,2
        NOPAUL(IPT)=GMPA(IPT).EQ.0.
        IF(.NOT.NOPAUL(IPT))THEN
          BXPA(IPT)=BXPA(IPT)/GMPA(IPT)
          BYPA(IPT)=BYPA(IPT)/GMPA(IPT)
          BZPA(IPT)=BZPA(IPT)/GMPA(IPT)
          AG=BXPA(IPT)**2+BYPA(IPT)**2+BZPA(IPT)**2
          IF(AG.LT..9999)THEN
            GMPA(IPT)=1./SQRT(1.-AG)
          ELSE
            GMPA(IPT)=1.
            BXPA(IPT)=0.
            BYPA(IPT)=0.
            BZPA(IPT)=0.
          ENDIF
        ENDIF
      ENDDO
C
*      IF(PRI)THEN
*        PXSU=0.
*        PYSU=0.
*        PZSU=0.
*        ESU=0.
*        IS=1
* 200    CONTINUE
*        IIMX=IMX(IX,IY*IS,IZ)
*        IF(IIMX.LT.0)GOTO 280
*        IIMN=IMN(IX,IY*IS,IZ)
*        DO 270 II=IIMN,IIMX
*        IIP=IPO(II)
*        IIDP=ID(IIP)
*        IF(IIDP.LE.0)GOTO 270
*        PXSU=PXSU+PX(IIP)
*        PYSU=PYSU+PY(IIP)*IS
*        PZSU=PZSU+PZ(IIP)
*        ESU=ESU+EE(IIP)
* 270    CONTINUE
* 280    CONTINUE
*C
*        IF(SYMMY.AND.IS.EQ.1)THEN
*          IS=-1
*          GOTO 200
*        ENDIF
*C
*        AMAS=SQRT(ESU*ESU-(PXSU*PXSU+PYSU*PYSU+PZSU*PZSU))
*        IF(SYMMY)AMAS=AMAS*.5
*        ECINT=ECINT+.5*AMAS*(BXPA(1)**2+BYPA(1)**2)
*        ECINZ=ECINZ+.5*AMAS*BZPA(1)**2
*      ENDIF
C
      END


      SUBROUTINE LOCPA
CC CALCULATES LOCAL PARAMETERS
      COMMON/CLOPA/IX,IY,IZ,CALO
      LOGICAL CALO
      COMMON/CCLOPA/IXB,IXF,IYB,IYF,IZB,IZF,XC,YC,ZC,IIVOV
      COMMON/CLOPAC/IIXB,IIXF,IIYB,IIYF,IIZB,IIZF,IVOV,FPT
C
      INCLUDE 'PARTIC15'
      INCLUDE 'SIZE2'
C
      COMMON/CPA/RHOPAM,UUP
C
      DIMENSION PXPAN(2,2),PYPAN(2,2),PZPAN(2,2),EPAN(2,2)
      DIMENSION PXPA(2),PYPA(2),PZPA(2),EPA(2)
C
      COMMON/CNC/BXPA(2),BYPA(2),BZPA(2),GMPA(2),GMPA1(2)
     C  ,BXPA1,BYPA1,BZPA1,GMP1,NOPAUL1
     C  ,CX(3,2),CY(3,2),CZ(3,2)
     N  ,NOPAUL(2),TC(2),AMIC(2),FFA(2)
     N  ,TK(2)                           !for pions?
     C  ,BXPAN(2,2),BYPAN(2,2),BZPAN(2,2),GMPAN(2,2),GMPAN1(2,2)
     C  ,CXN(3,2,2),CYN(3,2,2),CZN(3,2,2)
     N  ,NOPALN(2,2),TCN(2,2),AMICN(2,2),FFAN(2,2)
     c  ,fmax
      LOGICAL NOPAUL,NOPALN,NOPAUL1
      DIMENSION PXXC(2),PYYC(2),PZZC(2),PXYC(2),PXZC(2),PYZC(2)
      DIMENSION PXXCN(2,2),PYYCN(2,2),PZZCN(2,2)
      DIMENSION PXYCN(2,2),PXZCN(2,2),PYZCN(2,2)
*      DIMENSION PKC(2),RHOC(2),NPT(2),PMO(2)
      DIMENSION PKC(2),RHOC(2),PMO(2)
      DIMENSION PKCN(2,2),RHOCN(2,2),PMON(2,2)
      COMMON/DU/NPT(2)                          !for pions?
      DIMENSION ANPTN(2,2),ANPT(2)
      LOGICAL REPEAT(2),REPETN(2,2)
      DIMENSION FFAO(2),FFAON(2,2)
C
      common/hu/bxpt,bypt,bzpt,gmpt
C
      PARAMETER (AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER (BD=.002225,AMD=AM0+AM0-BD)
      PARAMETER (AMD2=AMD+AMD)
      PARAMETER (AMEC=.13957,AMEN=.13496,AME=(AMEC+AMEC+AMEN)/3.)
      PARAMETER (AMVM=AM0+AME)
      PARAMETER (H=.19733)
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      logical grav
      common/cgrav/grav
c
*      common/xtrac/ipt,iidn,fii(3,2,2),phipai(3,2,2),thpai(3,2,2)
C
      LOGICAL COUNT
      COMMON/CCOUN/COUNT
C
      LOGICAL LOGI
C
      IF(FIRST)THEN
        FRHO=1./(NQU*VOLE)
        RHOPM1=RHOPAM/FRHO
        FIRST=.FALSE.
      ENDIF
C
      CALO=.FALSE.
      count=.true.
C
      IIXB=IXB
      IIXF=IXF
      IIYB=IYB
      IIYF=IYF
      IIZB=IZB
      IIZF=IZF
C
      DO IIDN=1,2
        DO IPT=1,2
          ANPTN(IPT,IIDN)=0.
        ENDDO
      ENDDO
      IS=1
      AIVOV=0.
 15   CONTINUE
      DO 50 IIZ=IIZB,IIZF
      IF(IIZ.NE.IZ)THEN
        FZFA=.5
      ELSE
        FZFA=1.
      ENDIF
      DO 40 IIY=IIYB*IS,IIYF*IS,IS
      IIYS=IIY*IS
      IF(IIYS.NE.IY)THEN
        FYFA=.5*FZFA
      ELSE
        FYFA=FZFA
      ENDIF
      DO 30 IIX=IIXB,IIXF
      IF(IIX.NE.IX)THEN
        FXFA=.5*FYFA
      ELSE
        FXFA=FYFA
      ENDIF
      AIVOV=AIVOV+FXFA
*     aivov=aivov+1.
      IIMX=IMX(IIX,IIY,IIZ)
      IF(IIMX.LT.0)GOTO 30
      IIMN=IMN(IIX,IIY,IIZ)
      DO 20 II=IIMN,IIMX
      IIP=IPO(II)
      IIDP=ID(IIP)
      IF(IIDP.LE.0)GOTO 20
      IF(IIDP.GE.3)GOTO 20
      IPT=ABS(IPTI(IIP))
      XL=XX(IIP)/DWL
      IF(NINT(XL).NE.IIX)GOTO 20
      IF(IIX.NE.IX)THEN
        WIX=1.5-ABS(IX-XL)
      ELSE
        WIX=1.
      ENDIF
      YL=YY(IIP)/DWL
      IF(NINT(YL).NE.IIY)GOTO 20
      IF(IIYS.NE.IY)THEN
        WIY=1.5-ABS(IY-YL*IS)
      ELSE
        WIY=1.
      ENDIF
      ZL=ZZ(IIP)/DLL
      IF(NINT(ZL).NE.IIZ)GOTO 20
      IF(IIZ.NE.IZ)THEN
        WIZ=1.5-ABS(IZ-ZL)
      ELSE
        WIZ=1.
      ENDIF
      ANPTN(IPT,IIDP)=ANPTN(IPT,IIDP)+WIX*WIY*WIZ
*     anptn(ipt,iidp)=anptn(ipt,iidp)+1.
 20   CONTINUE
 30   CONTINUE
 40   CONTINUE
 50   CONTINUE
C
      IF(SYMA.AND.IS.EQ.1)THEN
        IS=-1
        GOTO 15
      ENDIF
C
      DO IPT=1,2
        ANPT(IPT)=ANPTN(IPT,1)+ANPTN(IPT,2)
        NPT(IPT)=NINT(ANPT(IPT))
      ENDDO
C
      APONE=ANPT(1)+ANPT(2)
C
      DO IIDN=1,2
        DO IPT=1,2
          PXPAN(IPT,IIDN)=0.
          PYPAN(IPT,IIDN)=0.
          PZPAN(IPT,IIDN)=0.
          EPAN(IPT,IIDN)=0.
          RHOCN(IPT,IIDN)=0.
        ENDDO
      ENDDO
C
      IS=1
 55   CONTINUE
      DO 100 IIZ=IIZB,IIZF
      DO 90 IIY=IIYB*IS,IIYF*IS,IS
      IIYS=IIY*IS
      DO 80 IIX=IIXB,IIXF
      IIMX=IMX(IIX,IIY,IIZ)
      IF(IIMX.LT.0)THEN
        GOTO 80
      ENDIF
      IIMN=IMN(IIX,IIY,IIZ)
      DO 70 II=IIMN,IIMX
      IIP=IPO(II)
      IIDP=ID(IIP)
      IF(IIDP.LE.0)GOTO 70
      IF(IIDP.GE.3)GOTO 70
      EEIIP=EE(IIP)
      IPT=ABS(IPTI(IIP))
      XL=XX(IIP)/DWL
      IF(NINT(XL).NE.IIX)GOTO 70
      IF(IIX.NE.IX)THEN
        WIX=1.5-ABS(IX-XL)
      ELSE
        WIX=1.
      ENDIF
      YL=YY(IIP)/DWL
      IF(NINT(YL).NE.IIY)GOTO 70
      IF(IIYS.NE.IY)THEN
        WIY=1.5-ABS(IY-YL*IS)
      ELSE
        WIY=1.
      ENDIF
      ZL=ZZ(IIP)/DLL
      IF(NINT(ZL).NE.IIZ)GOTO 70
      IF(IIZ.NE.IZ)THEN
        WIZ=1.5-ABS(IZ-ZL)
      ELSE
        WIZ=1.
      ENDIF
      WII=WIX*WIY*WIZ
*     wii=1.
      EPAN(IPT,IIDP)=EPAN(IPT,IIDP)+EEIIP*WII
      PXPAN(IPT,IIDP)=PXPAN(IPT,IIDP)+PX(IIP)*WII
      PYPAN(IPT,IIDP)=PYPAN(IPT,IIDP)+PY(IIP)*WII*IS
      PZPAN(IPT,IIDP)=PZPAN(IPT,IIDP)+PZ(IIP)*WII
      RHOCN(IPT,IIDP)=RHOCN(IPT,IIDP)+AM0/EEIIP*WII
 70   CONTINUE
 80   CONTINUE
 90   CONTINUE
 100  CONTINUE
C
      IF(SYMA.AND.IS.EQ.1)THEN
        IS=-1
        GOTO 55
      ENDIF
C
      bxpt=pxpan(1,1)+pxpan(2,1)+pxpan(1,2)+pxpan(2,2)
      bypt=pypan(1,1)+pypan(2,1)+pypan(1,2)+pypan(2,2)
      bzpt=pzpan(1,1)+pzpan(2,1)+pzpan(1,2)+pzpan(2,2)
      gmpt=epan(1,1)+epan(2,1)+epan(1,2)+epan(2,2)
      if(gmpt.ne.0.)then
        bxpt=bxpt/gmpt
        bypt=bypt/gmpt
        bzpt=bzpt/gmpt
      endif
      gmpt=1./sqrt(1.-(bxpt**2+bypt**2+bzpt**2))
c
      DO IPT=1,2
        RHOC(IPT)=RHOCN(IPT,1)+RHOCN(IPT,2)
        IF(RHOC(IPT).LT.RHOPM1*AIVOV)THEN
          NOPAUL(IPT)=.TRUE.
        ELSE
          NOPAUL(IPT)=.FALSE.
          EPA(IPT)=EPAN(IPT,1)+EPAN(IPT,2)
          PXPA(IPT)=PXPAN(IPT,1)+PXPAN(IPT,2)
          PYPA(IPT)=PYPAN(IPT,1)+PYPAN(IPT,2)
          PZPA(IPT)=PZPAN(IPT,1)+PZPAN(IPT,2)
          BXPA(IPT)=PXPA(IPT)/EPA(IPT)
          BYPA(IPT)=PYPA(IPT)/EPA(IPT)
          BZPA(IPT)=PZPA(IPT)/EPA(IPT)
          GMPA(IPT)=1./SQRT(1.-(BXPA(IPT)**2
     G      +BYPA(IPT)**2+BZPA(IPT)**2))
          GMPA1(IPT)=GMPA(IPT)/(GMPA(IPT)+1.)
        ENDIF
      ENDDO
C
      IF(NOPAUL(1).AND.NOPAUL(2))RETURN
C
      IF(ASYII)THEN
        LOGI=.TRUE.
        DO IIDN=1,2
          DO IPT=1,2
            IF(2.*RHOCN(IPT,IIDN).LT.RHOPM1*AIVOV)THEN
              NOPALN(IPT,IIDN)=.TRUE.
C ... NO PAULI
            ELSE
              NOPALN(IPT,IIDN)=.FALSE.
              LOGI=.FALSE.
              BXPAN(IPT,IIDN)=PXPAN(IPT,IIDN)/EPAN(IPT,IIDN)
              BYPAN(IPT,IIDN)=PYPAN(IPT,IIDN)/EPAN(IPT,IIDN)
              BZPAN(IPT,IIDN)=PZPAN(IPT,IIDN)/EPAN(IPT,IIDN)
              GMPAN(IPT,IIDN)=1./SQRT(1.-(BXPAN(IPT,IIDN)**2
     G          +BYPAN(IPT,IIDN)**2+BZPAN(IPT,IIDN)**2))
              GMPAN1(IPT,IIDN)=GMPAN(IPT,IIDN)/(GMPAN(IPT,IIDN)+1.)
            ENDIF
          ENDDO
        ENDDO
        IF(LOGI)RETURN
      ENDIF
C
      PXTO=PXPA(1)+PXPA(2)
      PYTO=PYPA(1)+PYPA(2)
      PZTO=PZPA(1)+PZPA(2)
      ETO=EPA(1)+EPA(2)
C
      BXTO=PXTO/ETO
      BYTO=PYTO/ETO
      BZTO=PZTO/ETO
      GMTO=1./SQRT(1.-(BXTO*BXTO+BYTO*BYTO+BZTO*BZTO))
C
C  LOCAL FRAMES DETERMINED, NOW FOR THE SHAPES
      DO IPT=1,2
        PXXC(IPT)=0.
        PYYC(IPT)=0.
        PZZC(IPT)=0.
        PXYC(IPT)=0.
        PXZC(IPT)=0.
        PYZC(IPT)=0.
        PMO(IPT)=0.
        TK(IPT)=0.
        IF(ASYII)THEN
          DO IIDN=1,2
            PXXCN(IPT,IIDN)=0.
            PYYCN(IPT,IIDN)=0.
            PZZCN(IPT,IIDN)=0.
            PXYCN(IPT,IIDN)=0.
            PXZCN(IPT,IIDN)=0.
            PYZCN(IPT,IIDN)=0.
            PMON(IPT,IIDN)=0.
          ENDDO
        ENDIF
      ENDDO
C
      EMA=0.
      IS=1
 105  CONTINUE
      DO 140 IIZ=IIZB,IIZF
      DO 130 IIY=IIYB*IS,IIYF*IS,IS
      IIYS=IIY*IS
      DO 120 IIX=IIXB,IIXF
      IIMX=IMX(IIX,IIY,IIZ)
      IF(IIMX.LT.0)GOTO 120
      IIMN=IMN(IIX,IIY,IIZ)
      NII=0
      DO 110 II=IIMN,IIMX
      IIP=IPO(II)
      IIDP=ID(IIP)
      IF(IIDP.LE.0)GOTO 110
      IF(IIDP.GE.3)GOTO 110
      NII=NII+1
      IPT=ABS(IPTI(IIP))
      XL=XX(IIP)/DWL
      IF(NINT(XL).NE.IIX)GOTO 110
      IF(IIX.NE.IX)THEN
        WIX=1.5-ABS(IX-XL)
      ELSE
        WIX=1.
      ENDIF
      YL=YY(IIP)/DWL
      IF(NINT(YL).NE.IIY)GOTO 110
      IF(IIYS.NE.IY)THEN
        WIY=1.5-ABS(IY-YL*IS)
      ELSE
        WIY=1.
      ENDIF
      ZL=ZZ(IIP)/DLL
      IF(NINT(ZL).NE.IIZ)GOTO 110
      IF(IIZ.NE.IZ)THEN
        WIZ=1.5-ABS(IZ-ZL)
      ELSE
        WIZ=1.
      ENDIF
      WII=WIX*WIY*WIZ
*     wii=1.
      BPII=BXPA(IPT)*PX(IIP)+BYPA(IPT)*PY(IIP)*IS+BZPA(IPT)*PZ(IIP)
      BCPII=GMPA(IPT)*(EE(IIP)-GMPA1(IPT)*BPII)
      PIICX=PX(IIP)-BXPA(IPT)*BCPII
      PIICY=PY(IIP)*IS-BYPA(IPT)*BCPII
      PIICZ=PZ(IIP)-BZPA(IPT)*BCPII
      PXXC(IPT)=PXXC(IPT)+PIICX*PIICX*WII
      PYYC(IPT)=PYYC(IPT)+PIICY*PIICY*WII
      PZZC(IPT)=PZZC(IPT)+PIICZ*PIICZ*WII
      PXYC(IPT)=PXYC(IPT)+PIICX*PIICY*WII
      PXZC(IPT)=PXZC(IPT)+PIICX*PIICZ*WII
      PYZC(IPT)=PYZC(IPT)+PIICY*PIICZ*WII
      EIC=GMTO*EE(IIP)-(BXTO*PX(IIP)+BYTO*PY(IIP)*IS+BZTO*PZ(IIP))
      EMA=EMA+EIC*WII
      IF(ASYII)THEN
        BPII=BXPAN(IPT,IIDP)*PX(IIP)+BYPAN(IPT,IIDP)*PY(IIP)*IS
     I    +BZPAN(IPT,IIDP)*PZ(IIP)
        BCPII=GMPAN(IPT,IIDP)*(EE(IIP)-GMPAN1(IPT,IIDP)*BPII)
        PIICX=PX(IIP)-BXPAN(IPT,IIDP)*BCPII
        PIICY=PY(IIP)*IS-BYPAN(IPT,IIDP)*BCPII
        PIICZ=PZ(IIP)-BZPAN(IPT,IIDP)*BCPII
        PXXCN(IPT,IIDP)=PXXCN(IPT,IIDP)+PIICX*PIICX*WII
        PYYCN(IPT,IIDP)=PYYCN(IPT,IIDP)+PIICY*PIICY*WII
        PZZCN(IPT,IIDP)=PZZCN(IPT,IIDP)+PIICZ*PIICZ*WII
        PXYCN(IPT,IIDP)=PXYCN(IPT,IIDP)+PIICX*PIICY*WII
        PXZCN(IPT,IIDP)=PXZCN(IPT,IIDP)+PIICX*PIICZ*WII
        PYZCN(IPT,IIDP)=PYZCN(IPT,IIDP)+PIICY*PIICZ*WII
      ENDIF
 110  CONTINUE
 120  CONTINUE
 130  CONTINUE
 140  CONTINUE
C
      IF(SYMA.AND.IS.EQ.1)THEN
        IS=-1
        GOTO 105
      ENDIF
C
      DO IPT=1,2
*        iidn=0
        IF(.NOT.NOPAUL(IPT))CALL DIA(PXXC(IPT),PYYC(IPT),PZZC(IPT)
     I    ,PXYC(IPT),PXZC(IPT),PYZC(IPT),CX(1,IPT),CY(1,IPT)
     F    ,CZ(1,IPT),ANPT(IPT),PKC(IPT),NOPAUL(IPT),FFA(IPT))
        IF(ASYII)THEN
          DO IIDN=1,2
            IF(.NOT.NOPALN(IPT,IIDN))CALL DIA(PXXCN(IPT,IIDN)
     I        ,PYYCN(IPT,IIDN),PZZCN(IPT,IIDN),PXYCN(IPT,IIDN)
     I        ,PXZCN(IPT,IIDN),PYZCN(IPT,IIDN),CXN(1,IPT,IIDN)
     F        ,CYN(1,IPT,IIDN),CZN(1,IPT,IIDN),ANPTN(IPT,IIDN)
     F        ,PKCN(IPT,IIDN),NOPALN(IPT,IIDN),FFAN(IPT,IIDN))
          ENDDO
        ENDIF
      ENDDO
*     write(*,*)'pkc = ',(sqrt(5./3.*pkc(ipt)),ipt=1,2)
      IF(NOPAUL(1).AND.NOPAUL(2))RETURN
      IF(ASYII)THEN
        IF(NOPALN(1,1).AND.NOPALN(2,1).AND.NOPALN(1,2).AND.
     I    NOPALN(2,2))RETURN
      ENDIF
C
      EMA=EMA/APONE                           !average cm mass for deuteron
C                                             !calculation
      IS=1
 145  CONTINUE
      DO 180 IIZ=IIZB,IIZF
      DO 170 IIY=IIYB*IS,IIYF*IS,IS
      IIYS=IIY*IS
      DO 160 IIX=IIXB,IIXF
      IIMX=IMX(IIX,IIY,IIZ)
      IF(IIMX.LT.0)GOTO 160
      IIMN=IMN(IIX,IIY,IIZ)
      DO 150 II=IIMN,IIMX
      IIP=IPO(II)
      IIDP=ID(IIP)
      IF(IIDP.LE.0)GOTO 150
      IF(IIDP.GE.3)GOTO 150
      IPT=ABS(IPTI(IIP))
      XL=XX(IIP)/DWL
      IF(NINT(XL).NE.IIX)GOTO 150
      IF(IIX.NE.IX)THEN
        WIX=1.5-ABS(IX-XL)
      ELSE
        WIX=1.
      ENDIF
      YL=YY(IIP)/DWL
      IF(NINT(YL).NE.IIY)GOTO 150
      IF(IIYS.NE.IY)THEN
        WIY=1.5-ABS(IY-YL*IS)
      ELSE
        WIY=1.
      ENDIF
      ZL=ZZ(IIP)/DLL
      IF(NINT(ZL).NE.IIZ)GOTO 150
      IF(IIZ.NE.IZ)THEN
        WIZ=1.5-ABS(IZ-ZL)
      ELSE
        WIZ=1.
      ENDIF
      WII=WIX*WIY*WIZ
*     wii=1.
      BPII=BXPA(IPT)*PX(IIP)+BYPA(IPT)*PY(IIP)*IS+BZPA(IPT)*PZ(IIP)
      BCPII=GMPA(IPT)*(EE(IIP)-GMPA1(IPT)*BPII)
      PIICX=PX(IIP)-BXPA(IPT)*BCPII
      PIICY=PY(IIP)*IS-BYPA(IPT)*BCPII
      PIICZ=PZ(IIP)-BZPA(IPT)*BCPII
      PRK=0.
      DO I=1,3
        PCO=CX(I,IPT)*PIICX+CY(I,IPT)*PIICY+CZ(I,IPT)*PIICZ
        PRK=PRK+PCO*PCO
      ENDDO
      TK(IPT)=TK(IPT)+PRK/(2.*AMV(IIP))*WII
      PMO(IPT)=PMO(IPT)+SQRT(PRK)*WII
      IF(ASYII)THEN
        BPII=BXPAN(IPT,IIDP)*PX(IIP)+BYPAN(IPT,IIDP)*PY(IIP)*IS
     B    +BZPAN(IPT,IIDP)*PZ(IIP)
        BCPII=GMPAN(IPT,IIDP)*(EE(IIP)-GMPAN1(IPT,IIDP)*BPII)
        PIICX=PX(IIP)-BXPAN(IPT,IIDP)*BCPII
        PIICY=PY(IIP)*IS-BYPAN(IPT,IIDP)*BCPII
        PIICZ=PZ(IIP)-BZPAN(IPT,IIDP)*BCPII
        PRK=0.
        DO I=1,3
          PCO=CXN(I,IPT,IIDP)*PIICX+CYN(I,IPT,IIDP)*PIICY
     P      +CZN(I,IPT,IIDP)*PIICZ
          PRK=PRK+PCO*PCO
        ENDDO
        PMON(IPT,IIDP)=PMON(IPT,IIDP)+SQRT(PRK)*WII
      ENDIF
 150  CONTINUE
 160  CONTINUE
 170  CONTINUE
 180  CONTINUE
C
      IF(SYMA.AND.IS.EQ.1)THEN
        IS=-1
        GOTO 145
      ENDIF
C
CCC  SHAPES DETERMINED
      FPT=1./AIVOV
C
      DO IPT=1,2
        RHOC(IPT)=RHOC(IPT)*FPT*FRHO
        IF(NOPAUL(IPT).OR.RHOC(IPT).LE.RHOPAM)THEN
          NOPAUL(IPT)=.TRUE.
        ELSE
          TK(IPT)=TK(IPT)/ANPT(IPT)*2./3.
          PMO(IPT)=PMO(IPT)/ANPT(IPT)
          FFAO(IPT)=FFA(IPT)
          CALL FINDPA(PMO(IPT),PKC(IPT),TC(IPT),AMIC(IPT)
     C      ,RHOC(IPT),FFA(IPT))
        ENDIF
        IF(ASYII)THEN
          DO IIDN=1,2
            RHOCN(IPT,IIDN)=2.*RHOCN(IPT,IIDN)*FPT*FRHO
            IF(NOPALN(IPT,IIDN).OR.RHOCN(IPT,IIDN).LE.RHOPAM)THEN
              NOPALN(IPT,IIDN)=.TRUE.
            ELSE
              PMON(IPT,IIDN)=PMON(IPT,IIDN)/ANPTN(IPT,IIDN)
              FFAON(IPT,IIDN)=FFAN(IPT,IIDN)
              CALL FINDPA(PMON(IPT,IIDN),PKCN(IPT,IIDN)
     C          ,TCN(IPT,IIDN),AMICN(IPT,IIDN),RHOCN(IPT,IIDN)
     A          ,FFAN(IPT,IIDN))
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C
      EM0=AM0+UUP
 220  CONTINUE
      FMAX=0.
      DO IPT=1,2
        REPEAT(IPT)=.FALSE.
        IF(.NOT.NOPAUL(IPT))THEN
          CALL DSTR(F,BXPA(IPT)*EM0,BYPA(IPT)*EM0,BZPA(IPT)*EM0
     D      ,GMPA(IPT)*EM0,1,1.,.FALSE.)
          FMAX=MAX(FMAX,F)
          IF(F.GT.1.0001.AND.FFA(IPT).GT..0001)THEN
            REPEAT(IPT)=.TRUE.
            PMO(IPT)=PMO(IPT)*1.003
**          PKC(IPT)=PKC(IPT)*1.005
          ENDIF
        ENDIF
        IF(ASYII)THEN
          DO IIDN=1,2
            REPETN(IPT,IIDN)=.FALSE.
            IF(.NOT.NOPALN(IPT,IIDN))THEN
              CALL DSTR(F,BXPAN(IPT,IIDN)*EM0
     D          ,BYPAN(IPT,IIDN)*EM0,BZPAN(IPT,IIDN)*EM0
     D          ,GMPAN(IPT,IIDN)*EM0,IIDN,1.,.TRUE.)
              FMAX=MAX(FMAX,F)
              IF(F.GT.1.0001.AND.FFAN(IPT,IIDN).GT..0001)THEN
                REPETN(IPT,IIDN)=.TRUE.
                PMON(IPT,IIDN)=PMON(IPT,IIDN)*1.003
**              PKCN(IPT,IIDN)=PKCN(IPT,IIDN)*1.005
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C
      LOGI=REPEAT(1).OR.REPEAT(2)
      LOGI=LOGI.OR.(ASYII.AND.(REPETN(1,1).OR.REPETN(2,1).OR.
     L  REPETN(1,2).OR.REPETN(2,2)))
      IF(.NOT.LOGI)THEN
        IF(PENT)CALL ENCELL
        RETURN
      ENDIF
C
      DO IPT=1,2
        IF(.NOT.NOPAUL(IPT))THEN
          IF(REPEAT(IPT))THEN
            FFA(IPT)=FFAO(IPT)
            CALL FINDPA(PMO(IPT),PKC(IPT),TC(IPT)
     C        ,AMIC(IPT),RHOC(IPT),FFA(IPT))
            if(ffa(ipt).gt.1e5.or.grav)then
              write(*,*)' '
              write(*,*)ipt,' pmo = ',pmo(ipt),' pkc = ',pkc(ipt)
              write(*,*)'tc = ',tc(ipt),' amic = ',amic(ipt)
              write(*,*)'rhoc = ',rhoc(ipt),' em0 = ',em0
              write(*,*)' cx = ',(cx(ii,ipt),ii=1,3)
              write(*,*)' cy = ',(cy(ii,ipt),ii=1,3)
              write(*,*)' cz = ',(cz(ii,ipt),ii=1,3)
              write(*,*)'Anpt = ',Anpt(ipt)
              write(*,*)'beta = ',bxpa(ipt),bypa(ipt),bzpa(ipt)
              write(*,*)'ffa = ',ffa(ipt)
              write(*,*)' '
            endif
          ENDIF
        ENDIF
        IF(ASYII)THEN
          DO IIDN=1,2
            IF(.NOT.NOPALN(IPT,IIDN))THEN
              IF(REPETN(IPT,IIDN))THEN
                FFAN(IPT,IIDN)=FFAON(IPT,IIDN)
                CALL FINDPA(PMON(IPT,IIDN),PKCN(IPT,IIDN)
     C            ,TCN(IPT,IIDN),AMICN(IPT,IIDN),RHOCN(IPT,IIDN)
     A            ,FFAN(IPT,IIDN))
                if(ffan(ipt,iidn).gt.1e5.or.grav)then
                  write(*,*)' '
                  write(*,*)ipt,iidn,' pmon = ',pmon(ipt,iidn)
     w              ,' pkc = ',pkcn(ipt,iidn)
                  write(*,*)'tc = ',tcn(ipt,iidn)
     w              ,' amic = ',amicn(ipt,iidn)
                  write(*,*)'rhoc = ',rhocn(ipt,iidn),' em0 = ',em0
                  write(*,*)' cx = ',(cxn(ii,ipt,iidn),ii=1,3)
                  write(*,*)' cy = ',(cyn(ii,ipt,iidn),ii=1,3)
                  write(*,*)' cz = ',(czn(ii,ipt,iidn),ii=1,3)
                  write(*,*)'Anpt = ',Anptn(ipt,iidn)
                  write(*,*)'beta = ',bxpan(ipt,iidn)
     w              ,bypan(ipt,iidn),bzpan(ipt,iidn)
                  write(*,*)'ffa = ',ffan(ipt,iidn)
*                  do iv=1,3
*                    write(*,*)'f,th,ph ',fii(iv,ipt,iidn)
*     w                ,thpai(iv,ipt,iidn),phipai(iv,ipt,iidn)
*                  enddo
                  write(*,*)' '
                endif
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDDO
      GOTO 220
C
      END


      SUBROUTINE DIA(PXXC,PYYC,PZZC,PXYC,PXZC,PYZC,CX,CY,CZ,ANPT
     D  ,PKC,NOPAUL,FFA)
      DIMENSION CX(3),CY(3),CZ(3)
      LOGICAL NOPAUL
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      DIMENSION FF(3)
*     DATA CF3/4.5/,CF2/3.4/,CF31/3.5/
*      data cf3/4.0/,cf2/3.0/,cf31/3.1/
      data cf3/2.0/,cf2/1.5/,cf31/1.55/
*      common/xtrac/ipt,iidn,fii(3,2,2),phipai(3,2,2),thpai(3,2,2)
C
      IF(FIRST)THEN
        PI=4.*ATAN(1.)
        PIH=.5*PI
        FIRST=.FALSE.
      ENDIF
C
      IF(ANPT.LE.2.)GOTO 300  !added 9/11/1992
C
      AAA=-(PXXC+PYYC+PZZC)
      BBB=PXXC*(PYYC+PZZC)+PYYC*PZZC-(PXYC*PXYC+PXZC*PXZC+PYZC*PYZC)
      CCC=PXXC*(PYZC*PYZC-PYYC*PZZC)
     C  +(PYYC*PXZC-PXYC*PYZC)*PXZC+(PZZC*PXYC-PXZC*PYZC)*PXYC
C
      PPPS=SQRT(MAX(AAA*AAA-3.*BBB,0.))/3.
      QQQ=AAA*BBB/6.-.5*CCC-AAA*AAA*AAA/27.
C
      IF(PPPS.GT.1E-10)THEN
        AG=QQQ/(PPPS*PPPS*PPPS)
        AG=SIGN(MIN(1.,ABS(AG)),AG)
        ALPHA=ACOS(AG)
      ELSE
        ALPHA=0.
      ENDIF
C
      FF(3)=-2.*PPPS*COS((ALPHA-PI)/3.)-AAA/3.
      IF(FF(3).LT.1E-10*ANPT)GOTO 300
      FF(1)=2.*PPPS*COS(ALPHA/3.)-AAA/3.
      FF(2)=-2.*PPPS*COS((ALPHA+PI)/3.)-AAA/3.
C  FF(1) - MAJOR, FF(3) - MINOR
C
      FV=(FF(1)+FF(2)+FF(3))/3.
      IF(FV.LT.1E-9)GOTO 300
      IF(FF(3)/FF(1).LT.1E-5)GOTO 300  !9/15/1992
C
      DO I=1,3
        FFI=FF(I)
C
        DDD=PXYC*PXYC-(PXXC-FFI)*(PYYC-FFI)
        DDSF=PYZC*(PXXC-FFI)-PXYC*PXZC
        DDCF=PXZC*(PYYC-FFI)-PXYC*PYZC
        IF(ABS(DDSF*DDD).GT.1E-12)THEN   !abs modified from 0 on Jan 7, 2014
          PHIPA=ATAN2(DDSF*DDD,DDCF*DDD)
          THPA=ATAN(SQRT(DDSF*DDSF+DDCF*DDCF)/ABS(DDD))
        ELSE
          THPA=PIH
          IF(DDSF.NE.0.)THEN
            PHIPA=ATAN2(DDSF,DDCF)
          ELSEIF(DDCF.NE.0.)THEN
            PHIPA=0.
          ELSEIF(I.EQ.1)THEN
            PHIPA=0.
          ELSEIF(I.EQ.2)THEN
            PHIPA=PIH
          ELSE
            PHIPA=0.
            THPA=0.
          ENDIF
        ENDIF
C
*        if(iidn.ne.0)then
*        fii(i,ipt,iidn)=ff(i)
*        phipai(i,ipt,iidn)=phipa
*        thpai(i,ipt,iidn)=thpa
*        endif
        COTH=COS(THPA)
        CZ(I)=COTH
        SITHPA=SQRT(1.-COTH*COTH)
        COPPA=COS(PHIPA)
        SIPPA=SIN(PHIPA)
        CX(I)=SITHPA*COPPA
        CY(I)=SITHPA*SIPPA
      ENDDO
C
      PSNPT=1./SQRT(MAX(1.,REAL(ANPT-1.)))
C
      PKC=3.*FV/ANPT
C
      FFA=1.
C
      IF(ANPT.LE.3.)RETURN     !introduced 9/10/92
      IF(FF(1)-FF(3).LT.CF3*FF(1)*PSNPT)RETURN
      IF(FF(1)-FF(2).LT.CF2*FF(1)*PSNPT)THEN
        FVV=.5*(FF(1)+FF(2))
        FF(1)=FVV
        FF(2)=FVV
      ENDIF
      IF(FF(2)-FF(3).LT.CF2*FF(2)*PSNPT)THEN
        FVV=.5*(FF(2)+FF(3))
        FF(2)=FVV
        FF(3)=FVV
      ENDIF
      IF(FF(1)-FF(3).LT.CF31*FF(1)*PSNPT)RETURN
C
      DO I=1,3
        FAI=SQRT(FV/FF(I))
        CX(I)=CX(I)*FAI
        CY(I)=CY(I)*FAI
        CZ(I)=CZ(I)*FAI
        FFA=FFA*FAI
      ENDDO
C
      RETURN
C
 300  CONTINUE
      NOPAUL=.TRUE.
C
      END


      SUBROUTINE FINDPA(PMO,PKC,TC,AMIC,RHOC,FFA)
      COMMON/CPA/RHOPAM,UUP
      PARAMETER (NPMO=11)
      PARAMETER (NPMO2=NPMO-2)
      PARAMETER (PMOX=.6)
      PARAMETER (DPMO=PMOX/NPMO)
      PARAMETER (NR=30)
      PARAMETER (NR1=NR-1)
      PARAMETER (NR2=NR-2)
      PARAMETER (RNR1=NR1)
      PARAMETER (RMIN=16./15.)
C
      PARAMETER (AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER (AM02=AM0+AM0,AM0K=AM0*AM0)
C
      DIMENSION TPK(NR,NPMO),DMIPK(NR,NPMO),RHOQP(NR,NPMO)
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
c
      logical grav
      common/cgrav/grav
C
      IF(FIRST)THEN
        PI=4.*ATAN(1.)
        RMAX=3.*PI/8.
        DRR=(RMAX-RMIN)/NR
        OPEN(33,FILE='RHOM.DAT',STATUS='OLD')
        DO JPMO=1,NPMO
          READ(33,'()')
          DO JR=1,NR
            READ(33,'()')
            READ(33,*)TPK(JR,JPMO),DMIPK(JR,JPMO),RHOQP(JR,JPMO)
          ENDDO
        ENDDO
        CLOSE(33)
        FIRST=.FALSE.
      ENDIF
C
      PMOP=PMO
      PKCP=PKC
 5    PMOK=PMOP*PMOP
      RA=PKCP/PMOK
      XR=(RA-RMIN)/DRR
      IF(XR.LT.0.)THEN
        XR=0.
        RFA=SQRT(RMIN/RA)
        PKCP=RFA*PKCP
        PMOK=PMOK/RFA
        PMOP=SQRT(PMOK)
      ENDIF
      IF(XR.LT.RNR1)THEN
        JR=XR
        DR=XR-JR
      ELSE
        JR=NR2
        DR=1.
      ENDIF
      JR=JR+1
      JR1=JR+1
      DR1=1.-DR
C
      XP=PMOP/DPMO
      JP=XP
      IF(JP.LT.0)THEN
        JP=0
      ELSEIF(JP.GT.NPMO2)THEN
        JP=NPMO2
      ENDIF
      DP=XP-JP
      DP1=1.-DP
      JP=JP+1
      JP1=JP+1
C
      TPKC=(TPK(JR,JP)*DR1+TPK(JR1,JP)*DR)*DP1
     T  +(TPK(JR,JP1)*DR1+TPK(JR1,JP1)*DR)*DP
C
      DMIC=(DMIPK(JR,JP)*DR1+DMIPK(JR1,JP)*DR)*DP1
     T  +(DMIPK(JR,JP1)*DR1+DMIPK(JR1,JP1)*DR)*DP
C
      RHOQ=(RHOQP(JR,JP)*DR1+RHOQP(JR1,JP)*DR)*DP1
     T  +(RHOQP(JR,JP1)*DR1+RHOQP(JR1,JP1)*DR)*DP
C
      TC=TPKC*PKCP
      PF=4.*PMOP/3.
      PFK=PF*PF
**    AMIF=SQRT(AM0K+AM02*UUP+PFK)
      AMIF=SQRT((AM0+UUP)**2+PFK)
      AMIC=AMIF+DMIC*PKCP
      if(pmop.lt.1e6.and.rhoq.le.1e6)then
        RHOS=(PMOP*RHOQ)**3
        grav=.false.
      else
        rhos=0.
        write(*,*)'pmop = ',pmop,'  rhoq = ',rhoq
        write(*,*)'pmo = ',pmo,' pkc = ',pkc
        write(*,*)'jr = ',jr,' jr1 = ',jr1
        write(*,*)'jp = ',jp,' jp1 = ',jp1
        grav=.true.
      endif
C
C      IF(FFA*RHOC/RHOS.GT.1.)THEN
C        PMOP=PMOP*1.005
C        GOTO 5
C      ENDIF
CC
      IF(RHOS.GT.0.)THEN
        FFA=FFA*RHOC/RHOS
      ELSE
        FFA=0.
      ENDIF
C
      END


      SUBROUTINE TDEC
      use global
      COMMON/CSEED/ISEED
      PARAMETER (NIT=65)
C
      DATA ZP,YP,XP/3*.5/
C
**    k=0
      FP=WFUT(ZP,YP,XP)
      DO 30 IT=1,NIT
      Z=RNA(ZP)
      Y=RNA(YP)
      X=RNA(XP)
      FX=WFUT(Z,Y,X)
      IF(FX.LT.FP)THEN
        IF(FP*getRan().GT.FX)GOTO 30
      ENDIF
      ZP=Z
      YP=Y
      XP=X
      FP=FX
      k=k+1
 30   CONTINUE
C
      FP=WFUT(ZP,YP,XP)     !for setting final values
**    write(*,'(A,I3,A,3F7.4,G12.4)')'k = ',k,' *** ',zp,yp,xp,fp
C
      END


      FUNCTION RNA(X)
      use global
      COMMON/CSEED/ISEED
      PARAMETER(DX=.3)
C
      RNA=X+DX*(getRan()-.5)
      IF(RNA.GE.1.)THEN
        RNA=RNA-1.
      ELSEIF(RNA.LT.0.)THEN
        RNA=RNA+1.
      ENDIF
C
      END


      FUNCTION WFUT(Z,Y,X)
C  SUBINTEGRAL FUNCTION FOR METROPOLIS OF TRITON
C  BREAKUP.  SEE TEB.FOR OR TRIT4.FOR FOR CLARIFICATION.
C  Z AND Y RANGE FROM 0. TO 1.
      COMMON/CSPC/EM0K,PKMX,ECM,EDC,PIC                !for deuterons
      COMMON/CDBB/ECMK,EM2KK,EM4K2K,EMDKK,EM4KDK
      COMMON/GETTC/ETKMC,PTKMC,EMTK,EKMCK
     G  ,AGCO,VTNX,VTMN,VTMX,CTHT
      COMMON/WFUTC/EM0,EM2K,PROHE,ETC
      LOGICAL PROHE
C
      COMMON/WFUTR/SIGPP,SIGPN,EEKMC,PPKMC,PKMC,PTKMK,E12K,E12,P12
C
      PARAMETER(PCI=.4)
C
      WFUT=0.
C
      AG=EM0*ECM-EM2K
      IF(AG.LE.0.)RETURN
      PKMCMX=SQRT(AG)
      A34=1.+PCI/PKMCMX
      AZ=A34-Z
      PKMC=PCI*Z/AZ                   !momentum in the 2-ptcle cm
      PKMCK=PKMC*PKMC
      WU=PCI*A34/(AZ*AZ)*PKMCK
      EKMCK=4.*(EM0K+PKMCK)
      AG=((ECMK+EM2K-EKMCK)/(2.*ECM))**2-EM2K
      IF(AG.LE.0.)THEN
        WFUT=0.
        RETURN
      ENDIF
      PL34MX=SQRT(AG)
C
      PPKMC=PL34MX*Y                  !momentum in the 4-ptcle cm
      PPKMCK=PPKMC*PPKMC
      EEKMC=SQRT(EKMCK+PPKMCK)
      E12K=((ECM-EEKMC)**2-PPKMCK)/4.
      E12=SQRT(E12K)
      P12K=E12K-EM0K
      P12K=MAX(P12K,0.)
      P12=SQRT(P12K)
      EPCM=(E12+E12)*P12
      SIGPP=SIG(EPCM,1,1)
      SIGPN=SIG(EPCM,1,2)
      IF(PROHE)THEN
        WU=WU*(SIGPP+SIGPP+SIGPN)
      ELSE
        WU=WU*(SIGPP+SIGPN+SIGPN)
      ENDIF
      WU=WU*PL34MX*PPKMC/EEKMC*P12*E12
      ETKMC=ETC*EEKMC
      PTKMC=PIC*PPKMC
      PMINK=(ETKMC-PTKMC)**2/EMTK-EKMCK
      PMINK=MAX(0.,PMINK)
      PMIN=SQRT(PMINK)
      PMAXK=(ETKMC+PTKMC)**2/EMTK-EKMCK
      PMAX=SQRT(PMAXK)
      IF(PMAX.LE.PMIN)RETURN
      AT34=1.+PCI/(PMAX-PMIN)
      AX=AT34-X
      PTKM=PMIN+PCI*X/AX                 !2-ptcle momentum in triton cm
      PTKMK=PTKM*PTKM
      WU=PCI*A34/(AZ*AZ)*PKMCK
      WFUT=WU*PCI*AT34/(AX*AX)*PTKM*WAVETK(1.5*PTKMK+PKMCK+PKMCK)
C
      END


      FUNCTION WAVETK(PK)
      PARAMETER (PMX=1.02)
      PARAMETER (PMXK=PMX*PMX)
      PARAMETER (NP=120)
      PARAMETER (DP=PMX/NP)
C
      DIMENSION WAVT(0:NP)
C
      LOGICAL FIRST
C
      DATA FIRST/.TRUE./
C
      IF(FIRST)THEN
        OPEN(33,FILE='GENFIT.DAT',STATUS='OLD')
        DO I=0,NP
          READ(33,*)A,A,WAVT(I)
        ENDDO
        CLOSE(33)
        FIRST=.FALSE.
      ENDIF
C
      IF(PK.GT.0.)THEN
        X=SQRT(PK)/DP
      ELSE
        X=0.
      ENDIF
      I=X
      IF(I.GE.NP)THEN
        WAVETK=0.
        RETURN
      ENDIF
      DX=X-I
      WAVETK=WAVT(I)*(1.-DX)+WAVT(I+1)*DX
C
      END


      SUBROUTINE GETNU(F,PFX,PFY,PFZ,IDPF,ASYIP)
      LOGICAL ASYIP
      COMMON/CLOPA/IX,IY,IZ,CALO
      LOGICAL CALO
      COMMON/CCLOPA/IXB,IXF,IYB,IYF,IZB,IZF,XC,YC,ZC,IIVOV
      INCLUDE 'PARTIC15'
      INCLUDE 'SIZE2'
C
      PARAMETER (H=.19733)
C
      PARAMETER (AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
C
      COMMON/CSEED/ISEED
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      IF(FIRST)THEN
        PI=4.*ATAN(1.)
        PIPI=PI+PI
        FNUM=(PIPI*H)**3/(4.*NQU)
        DPL=.34/NQU**(1./6.)
        DPLK=DPL*DPL
        VOLP=DPL**3*4.*PI/3.
        FIRST=.FALSE.
      ENDIF
C
      ANUM=0.
      VOLI=0.
      IS=1
 10   CONTINUE
      DO 160 IIZ=IZB,IZF
      IF(IIZ.NE.IZ)THEN
        FZFA=.5
      ELSE
        FZFA=1.
      ENDIF
      DO 150 IIY=IYB,IYF
      IIYS=IIY*IS
      IF(IIY.NE.IY)THEN
        FYFA=.5*FZFA
      ELSE
        FYFA=FZFA
      ENDIF
      DO 140 IIX=IXB,IXF
      IF(IIX.NE.IX)THEN
        FXFA=.5*FYFA
      ELSE
        FXFA=FYFA
      ENDIF
      VOLI=VOLI+VOLE*FXFA
      IIMX=IMX(IIX,IIYS,IIZ)
      IF(IIMX.LT.0)GOTO 140
      IIMN=IMN(IIX,IIYS,IIZ)
      DO 120 II=IIMN,IIMX
      IIP=IPO(II)
      IIDP=ID(IIP)
      IF(IIDP.LE.0)GOTO 120
      IF(IIDP.GE.3)GOTO 120
      IF(ASYIP)THEN
        IF(IIDP.NE.IDPF)GOTO 120
      ENDIF
      DPX=PX(IIP)-PFX
      DPY=PY(IIP)*IS-PFY
      DPZ=PZ(IIP)-PFZ
      DPK=DPX*DPX+DPY*DPY+DPZ*DPZ
      IF(DPK.LE.DPLK)THEN
        XL=XX(IIP)/DWL
        IF(NINT(XL).NE.IIX)GOTO 120
        IF(IIX.NE.IX)THEN
          WIX=1.5-ABS(IX-XL)
        ELSE
          WIX=1.
        ENDIF
        YL=YY(IIP)/DWL
        IF(NINT(YL).NE.IIYS)GOTO 120
        IF(IIY.NE.IY)THEN
          WIY=1.5-ABS(IY-YL*IS)
        ELSE
          WIY=1.
        ENDIF
        ZL=ZZ(IIP)/DLL
        IF(NINT(ZL).NE.IIZ)GOTO 120
        IF(IIZ.NE.IZ)THEN
          WIZ=1.5-ABS(IZ-ZL)
        ELSE
          WIZ=1.
        ENDIF
        ANUM=ANUM+WIX*WIY*WIZ
      ENDIF
 120  CONTINUE
 140  CONTINUE
 150  CONTINUE
 160  CONTINUE
C
      IF(SYMA.AND.IS.EQ.1)THEN
        IS=-1
        GOTO 10
      ENDIF
C
      F=ANUM*FNUM/(VOLI*VOLP)
      IF(ASYIP)F=F+F
*     write(*,*)' f = ',f,' num = ',num
*     write(*,*)' voli = ',voli,' volp = ',volp
c
      end


      SUBROUTINE GETpi(F,PFX,PFY,PFZ,IDPF)
      COMMON/CLOPA/IX,IY,IZ,CALO
      LOGICAL CALO
      COMMON/CCLOPA/IXB,IXF,IYB,IYF,IZB,IZF,XC,YC,ZC,IIVOV
      INCLUDE 'PARTIC15'
      INCLUDE 'SIZE2'
C
      PARAMETER (H=.19733)
C
      PARAMETER (AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
C
      COMMON/CSEED/ISEED
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      IF(FIRST)THEN
        PI=4.*ATAN(1.)
        PIPI=PI+PI
        FNUM=(PIPI*H)**3/nQU
        DPL=.24/NQU**(1./6.)
        DPLK=DPL*DPL
        VOLP=DPL**3*4.*PI/3.
        FIRST=.FALSE.
      ENDIF
C
      ANUM=0.
      VOLI=0.
      IS=1
 10   CONTINUE
      DO 160 IIZ=IZB,IZF
      IF(IIZ.NE.IZ)THEN
        FZFA=.5
      ELSE
        FZFA=1.
      ENDIF
      DO 150 IIY=IYB,IYF
      IIYS=IIY*IS
      IF(IIY.NE.IY)THEN
        FYFA=.5*FZFA
      ELSE
        FYFA=FZFA
      ENDIF
      DO 140 IIX=IXB,IXF
      IF(IIX.NE.IX)THEN
        FXFA=.5*FYFA
      ELSE
        FXFA=FYFA
      ENDIF
      VOLI=VOLI+VOLE*FXFA
      IIMX=IMX(IIX,IIYS,IIZ)
      IF(IIMX.LT.0)GOTO 140
      IIMN=IMN(IIX,IIYS,IIZ)
      DO 120 II=IIMN,IIMX
      IIP=IPO(II)
      IF(ID(IIP).NE.IDPF)GOTO 120
      DPX=PX(IIP)-PFX
      DPY=PY(IIP)*IS-PFY
      DPZ=PZ(IIP)-PFZ
      DPK=DPX*DPX+DPY*DPY+DPZ*DPZ
      IF(DPK.LE.DPLK)THEN
        XL=XX(IIP)/DWL
        IF(NINT(XL).NE.IIX)GOTO 120
        IF(IIX.NE.IX)THEN
          WIX=1.5-ABS(IX-XL)
        ELSE
          WIX=1.
        ENDIF
        YL=YY(IIP)/DWL
        IF(NINT(YL).NE.IIYS)GOTO 120
        IF(IIY.NE.IY)THEN
          WIY=1.5-ABS(IY-YL*IS)
        ELSE
          WIY=1.
        ENDIF
        ZL=ZZ(IIP)/DLL
        IF(NINT(ZL).NE.IIZ)GOTO 120
        IF(IIZ.NE.IZ)THEN
          WIZ=1.5-ABS(IZ-ZL)
        ELSE
          WIZ=1.
        ENDIF
        ANUM=ANUM+WIX*WIY*WIZ
      ENDIF
 120  CONTINUE
 140  CONTINUE
 150  CONTINUE
 160  CONTINUE
C
      IF(SYMA.AND.IS.EQ.1)THEN
        IS=-1
        GOTO 10
      ENDIF
C
      F=ANUM*FNUM/(VOLI*VOLP)
*     write(*,*)' f = ',f,' num = ',anum
*     write(*,*)' voli = ',voli,' volp = ',volp
c
      end


      FUNCTION PIBAR(ANP,EN)
      PARAMETER(NE=9,NRO=6)
      PARAMETER(NE1=NE-1,NRO1=NRO-1)
      PARAMETER(EMIN=.9385,DE=.05)
      DIMENSION RA(0:NRO),PIBA(0:NRO,0:NE)
C
      DATA RA/0.,.1,.25,.5,1.,2.,3.5/
C
      DATA PIBA/7*0.,
     P  .050,.007,.0025,.002,.7E-3,.17E-4,.22E-11,
     I  .200,.099,.067, .050,.037, .026,  .024,
     B  .363,.221,.172, .138,.108, .103,  .091,
     A  .538,.364,.289, .235,.207, .182,  .175,
     P  .700,.482,.397, .349,.302, .271,  .250,
     I  .875,.628,.516, .438,.387, .356,  .330,
     B  1.02,.747,.634, .552,.494, .433,  .412,
     A  1.17,.890,.739, .640,.573, .516,  .493,
     P  1.30,1.05,.850, .762,.660, .591,  .557/
C
      PARAMETER(AN0=.160)
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      IF(FIRST)THEN
        DO I=1,NRO
          RA(I)=SQRT(RA(I)*AN0)
        ENDDO
        FIRST=.FALSE.
      ENDIF
C
      ER=(EN-EMIN)/DE
      IE=ER
      IF(IE.LT.0)THEN
        IE=0
        DER=0.
      ELSEIF(IE.GE.NE)THEN
        IE=NE1
        DER=ER-IE
      ELSE
        DER=ER-IE
      ENDIF
      IE1=IE+1
      DER1=1.-DER
C
      AN=SQRT(ANP)
      IR=0
      DO I=1,NRO
        IF(RA(I).GT.AN)GOTO 120
        IR=IR+1
      ENDDO
      IR=NRO1
      IR1=NRO
      DRO=1.
      GOTO 130
 120  CONTINUE
      IR1=IR+1
      DRO=(AN-RA(IR))/(RA(IR1)-RA(IR))
 130  CONTINUE
      DRO1=1.-DRO
C
      PIBAR=DRO1*DER1*PIBA(IR,IE)+DRO*DER1*PIBA(IR1,IE)
     P  +DRO1*DER*PIBA(IR,IE1)+DRO*DER*PIBA(IR1,IE1)
C
      END
