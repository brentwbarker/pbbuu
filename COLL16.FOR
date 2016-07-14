      SUBROUTINE COLLIDE
      use global
C  COLLIDES PSEUDOPARTICLES
      INCLUDE 'PARTIC15'
      INCLUDE 'SIZE2'
C
      PARAMETER (AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER (BD=.002225,AMD=AM0+AM0-BD)
      PARAMETER (AMD2=AMD+AMD,AMD4=AMD2+AMD2)
      PARAMETER (AMDK=AMD*AMD)
      PARAMETER (AM02=AM0+AM0,AM0K=AM0*AM0)
      PARAMETER (AMEC=.13957,AMEN=.13496,AME=(AMEC+AMEC+AMEN)/3.)
      PARAMETER (AMVM=AM0+AME)
      PARAMETER (AMVMK=AMVM*AMVM,AMVM2=AMVM+AMVM)
      PARAMETER (AMEK=AME*AME)
      PARAMETER (BT=.0086,AMT=AMP+AMN+AMN-BT)
      PARAMETER (B3E=.0080,AM3E=AMP+AMP+AMN-B3E)
      PARAMETER (AMTE=.5*(AMT+AM3E))
C
      PARAMETER (H=.19733)
C
      INCLUDE 'TSTEP'
C
*      PARAMETER (DICR=.5)   !max no. coll/ptcle/time with comp pro possible
*      COMMON/CDI/DINM
*      LOGICAL CODPRO,NOCOD
C
C  NO. OF POSITIVE PTCLE ID'S
      COMMON/CVMAX/VOLCM(NIDP,NIDP),VDMAX,VTMAX,GTMAX(7:12)
     C  ,VDPPMX,VDPNMX,VTPPMX,VTPNMX
C  MAXIMUM COLLISIONAL VOLUME (NORMALIZED TO CELL)
C  VOLM - THE LARGEST...
      COMMON/CHMAX/SIGHO(NIDP,NIDP),VOLCH(NIDP,NIDP),SIGEH(NIDP,NIDP)
C
      LOGICAL FIRST
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
      COMMON/CLOPA/IX,IY,IZ,CALO
      LOGICAL CALO
      COMMON/CCLOPA/IXB,IXF,IYB,IYF,IZB,IZF,XC,YC,ZC,IIVOV
C
      common/hu/bxpt,bypt,bzpt,gmpt
C
      COMMON/CPRI/PRI,TIM,IZCEN
      LOGICAL PRI
C
      COMMON/CPA/RHOPAM,UUP
      LOGICAL COLL
C
      COMMON/CSPC/EM0K,PKMX,ECM,EDC,PIC                !for deuterons
      COMMON/GETD/EDKC,PDKC,EMDK,EMD0K
     G  ,VNX,VPMN,VPMX,CTHD
      COMMON/CDBB/ECMK,EM2KK,EM4K2K,EMDKK,EM4KDK
C
      COMMON/GETTC/ETKMC,PTKMC,EMTK,EKMCK
     G  ,AGCO,VTNX,VTMN,VTMX,CTHT
C
      COMMON/WFUTC/EM0,EM2K,PROHE,ETC
      LOGICAL PROHE
C
      COMMON/WFUTR/SIGPP,SIGPN,EEKMC,PPKMC,PKMC,PTKMK,E12K,E12,P12
C
      COMMON/CSEED/ISEED
      COMMON/CLCOLL/KK2,KK2R,EET2,KK3,KK3R,EET3,KKB,KKBR,EETB
     C  ,KKV,KKVR,EETV,KKVB,KKVBR,EETVB
     C  ,KBRA,KKVE,KKVER,EETVE,KKEV
     C  ,KK4,KK4R,EET4,KKBT,KKBTR,EETBT,KBRAT
     C  ,KKPH,KKPHR,kkhi,eehit,qc4,pqck,pqckl
     C  ,KKVV,KKVVR,EETVV,KKVVA,KKVVAR,EETVVA
C
      LOGICAL NO2NS
C
      DIMENSION IMNC(NIDP),IMXC(NIDP)
      DIMENSION NUMC(NIDP)
C
      PARAMETER (AN0=.160)
C
      COMMON/CFREE/EFREE
      DATA FIRST/.TRUE./
      common/sma/f,s
      LOGICAL BREAKI,BREAK
      COMMON/CENTRO/ENGAIN,ENTROT,EDGAIN,EDTROT,DNCELL,DDCELL
     C  ,EBGAIN,EBTROT,DBCELL
     e  ,eptro,eptrog,anuc,anucg
C
      COMMON/CRPI/RHOPI,REFIN
C
      COMMON/COQU/FV,FD,FT,FG,FTD
C
      COMMON/CECL/IECL(NQU),NECL    !local stack for entropy calculation
C                                   !eventually intermediate reshuffling
C                                   !might be brought back ...
      LOGICAL SLICE,NODEPS
C
      DATA NJ1/0/
c
      logical inpo
      common/cinpo/inpo
c
      common/mixc/mmix
c
      LOGICAL STAR
      LOGICAL LHIGH
      PARAMETER(DHOL=12.*AME,DHOH=13.*AME)  ! brackts f/he cs getting on
c
      parameter(nbmax=40)
      dimension pfin(nbmax,0:3),amsf(nbmax),idih(nbmax)
c
*c  for Jorg
*      parameter(njorg=70)
*      common/cjorg/dejorg,anatem(njorg),anasuc(njorg)
*c  end for Jorg
C
      IF(FIRST)THEN
        FG=DT/H
        FV=DT/(NQU*VOLE)
        FG=FG*FRAT
        FV=FV*FRAT
        FD=.75/(NQU*VOLE)
        FT=.25/(NQU*VOLE)**2
        FTD=1./3./(NQU*VOLE)
        fpimx=1.
        ffpix=1.+fpimx
        CALL SETPIN
        CALL TRIMAX        !max collisional volume for tritons (move it out?)
        DO IDLS=1,NDLS
*          DO IE=1,NES
*            NDES(IE,IDLS)=0
*          ENDDO
          DO IZ=-NL,NL
            NDLZ(IZ,IDLS)=0
          ENDDO
        ENDDO
        CALL SETHI    ! reset collisional volume for h-e stuff
*c
*c  for Jorg
*        dejorg=.003
*c  end for Jorg
*c
        FIRST=.FALSE.
      ENDIF
*      if(tim.le.15.)then
*        kks=0.
*        st=0.
*      endif
*      ITIM=NINT(20.*TIM)
C
*      DO IE=1,NES
*        DO IDLS=2,NDLS
*          NDES(IE,IDLS)=NDES(IE,IDLS-1)
*        ENDDO
*        NDES(IE,1)=0
*      ENDDO
C
      DO IZ=-NL,NL
        DO IDLS=2,NDLS
          NDLZ(IZ,IDLS)=NDLZ(IZ,IDLS-1)
        ENDDO
        NDLZ(IZ,1)=0
      ENDDO
C
      KQ2R=0
      KQ3R=0
      KQBR=0
      KQ4R=0
      KQBTR=0
      KQVR=0
      KQVBR=0
      KQVER=0
      KQEV=0
      KQPHR=0.
      kqhi=0.
      KQVVR=0.
      KQVVAR=0.
C
      ENGAI=0.
      EDGAI=0.
      EBGAI=0.
C
      ENLOSS=0.
      EDLOSS=0.
      EBLOSS=0.
C
      IF(LMO)EFREE=0.
*c
*      npair=0
*      sigpr=0.
*      epair=0.
*      eone=0.
C
      DO 200 IZ=-NL,NL
*      IZB=MAX(-NL,IZ-NB)
*      IZF=MIN(NL,IZ+NB)
      IZB=MAX(-NL,IZ-1)
      IZF=MIN(NL,IZ+1)
      ZC=IZ*DLL
      DO 200 IY=-NT,NT
*      IYB=MAX(-NT,IY-NB)
*      IYF=MIN(NT,IY+NB)
      IYB=MAX(-NT,IY-1)
      IYF=MIN(NT,IY+1)
      IYN=IY*NTL1
      YC=IY*DWL
      DO 180 IX=-NT,NT
      IMXXYZ=IMX(IX,IY,IZ)
      IF(IMXXYZ.LE.0)THEN
C  IF THERE ARE NO PSEUDOPARTICLES IN A CELL, MAX IS PUT NEGATIVE...
*        KINT(IX,IY,IZ)=0
        GOTO 180
      ENDIF
C
*      IXB=MAX(-NT,IX-NB)
*      IXF=MIN(NT,IX+NB)
      IXB=MAX(-NT,IX-1)
      IXF=MIN(NT,IX+1)
*      IF(DEPRO)THEN
*        KINTO=0
*        NTOP=0
*        DO IIZ=IZB,IZF
*          DO IIY=IYB,IYF
*            DO IIX=IXB,IXF
*              IIMX=IMX(IIX,IIY,IIZ)
*              IF(IIMX.GT.0)THEN
*                NTOP=NTOP+IIMX-IMN(IIX,IIY,IIZ)+1
*                KINTO=KINTO+KINT(IIX,IIY,IIZ)
*              ENDIF
*            ENDDO
*          ENDDO
*        ENDDO
*        RKIN=REAL(KINTO)
*        RNTO=REAL(NTOP)
*        DIN=RKIN/RNTO
*        RDIN=SQRT(RKIN*(2.+RKIN/RNTO))/RNTO
*        DIN=DIN/DT
*        RDIN=RDIN/DT
*        DINM=DIN
**       DINM=DIN-RDIN
*        NOCOD=DINM.GT.DICR
*        CODPRO=.NOT.NOCOD
**       IF(DIN.NE.0.)THEN
**         WRITE(*,*)IX,IY,IZ
**         WRITE(*,*)DIN,RDIN,KINTO,NTOP
**       ENDIF
*      ENDIF
C
      KC2R=0
      KC3R=0
      KCBR=0
      KC4R=0
      KCBTR=0
      KCVR=0
      KCVBR=0
      KCVER=0
      KCEV=0
      KCPHR=0
      kchi=0
      KCVVR=0
      KCVVAR=0
C
      COLL=.FALSE.
      IMNXYZ=IMN(IX,IY,IZ)
      DO I=IMNXYZ,IMXXYZ
        IP=IPO(I)
        IPTI(IP)=ABS(IPTI(IP))            !will be used to mark interactions
      ENDDO
C
      IF(LMI)THEN
        FRXT=1.
      ELSE    ! energy renormalization moved here
        VSX=0.
        VSY=0.
        VSZ=0.
        VS2=0.
        VSXN=0.
        VSYN=0.
        VSZN=0.
        VS2N=0.
        NSUC=0
C
        RHOV=RHO(IX,IY,IZ)/AN0
        VXN=VXNO(IX,IY,IZ)
        VYN=VYNO(IX,IY,IZ)
        VZN=VZNO(IX,IY,IZ)
        AG=VXN*VXN+VYN*VYN+VZN*VZN
        IF(AG.LT..99999)THEN
          GNO=1./SQRT(1.-AG)
          BGNOX=GNO*VXN
          BGNOY=GNO*VYN
          BGNOZ=GNO*VZN
        ELSE
          GNO=1.
          BGNOX=0.
          BGNOY=0.
          BGNOZ=0.
        ENDIF
        UUN=UU(IX,IY,IZ)
C
        DO 100 I=IMNXYZ,IMXXYZ
          IP=IPO(I)
          IDP=ID(IP)
          IF(IDP.LE.0)GOTO 100
          EFREE=EFREE+EE(IP)
          PXI=PX(IP)
          PYI=PY(IP)
          PZI=PZ(IP)
          AMVI=AMV(IP)
          AMVIK=AMVI*AMVI
C
          BARP=BAR(IDP)
          if(barp.eq.0.)goto 100   ! 8/5/99
          IF(BARP.NE.0.)THEN
            EIN=SQRT(AMVIK+PXI*PXI+PYI*PYI+PZI*PZI)
          ELSE
            EIN=EE(IP)
          ENDIF
C
          CALL LOREN(-BGNOX,-BGNOY,-BGNOZ,GNO,PXI,PYI,PZI,EIN
     L      ,PCX,PCY,PCZ,EC)
C
          PCK=PCX*PCX+PCY*PCY+PCZ*PCZ
          ECNK=AMVIK+PCK
          ECN=SQRT(ECNK)
C
          VCXN=PCX/ECN
          VCYN=PCY/ECN
          VCZN=PCZ/ECN
C
          VSXN=VSXN+VCXN
          VSYN=VSYN+VCYN
          VSZN=VSZN+VCZN
          VS2N=VS2N+PCK/ECNK
          NSUC=NSUC+1
C
          IF(BARP.EQ.0.)THEN
            VSX=VSX+VCXN
            VSY=VSY+VCYN
            VSZ=VSZ+VCZN
            VS2=VS2+PCK/ECNK
          ELSE
            CALL LOREN(-BGNOX,-BGNOY,-BGNOZ,GNO,PXI,PYI,PZI,EE(IP)
     L        ,PCX,PCY,PCZ,EC)
            CALL PTNO(0.,0.,0.,RHOV
     A        ,UUN,AMVI,BARP,PCX,PCY,PCZ
     L        ,EC,PCC,ECC,VCX,VCY,VCZ,1)  ! Note: recalculates EE!
            VSX=VSX+VCX
            VSY=VSY+VCY
            VSZ=VSZ+VCZ
            VS2=VS2+VCX*VCX+VCY*VCY+VCZ*VCZ
            EE(IP)=EIN
          ENDIF
 100    CONTINUE
        IF(NSUC.GE.4)THEN
          VS2N=VS2N-(VSXN*VSXN+VSYN*VSYN+VSZN*VSZN)/NSUC
          VS2=VS2-(VSX*VSX+VSY*VSY+VSZ*VSZ)/NSUC
          FRXT=SQRT(VS2/VS2N)
          FRXT=FRXT**(1.001-3./MAX(NSUC,3))
          FRXT=MIN(FRXT,2.)
          FRXT=MAX(FRXT,.5)
        ELSE
          FRXT=1.
        ENDIF
      ENDIF
C
      IF(PENT)THEN
        NECL=0
        inpo=.true.
        dncell=0.        !modification 10/26/92
        ddcell=0.
        dbcell=0.
      ENDIF
      CALO=.TRUE.                   !local parameters must be eventually
      rhoxi=rho(ix,iy,iz)/AN0
C                                   !calculated
      XC=IX*DWL
      IIVOV=(IXF-IXB+1)*(IYF-IYB+1)*(IZF-IZB+1)
C
      IYXN=IYN+IX*NLL1
      IVXYZ=(IYXN+IZ)*NIDP                      !from GIVAL
C
      IQC=IMNXYZ
      DO IDC=1,NIDP
        IVC=IVXYZ+IDC
        IMNC(IDC)=IQC
 17     CONTINUE
        IF(IQC.GT.IMXXYZ)THEN
          IF(IMNC(IDC).LE.IMXXYZ)THEN
            IMXC(IDC)=IMXXYZ
          ELSE
            IMXC(IDC)=-1
          ENDIF
        ELSEIF(IVAL(IPO(IQC)).GT.IVC)THEN
          IF(IQC.GT.IMNC(IDC))THEN
            IMXC(IDC)=IQC-1
          ELSE
            IMXC(IDC)=-1
          ENDIF
        ELSE
          IQC=IQC+1
          GOTO 17
        ENDIF
        IF(IMXC(IDC).GT.0)THEN
          NUMC(IDC)=IMXC(IDC)-IMNC(IDC)+1
        ELSE
          NUMC(IDC)=0
        ENDIF
      ENDDO
C
      IF(LMI)THEN
        UUP=UU(IX,IY,IZ)
      ELSE
        UUP=0.
      ENDIF
      UUP2=UUP+UUP
      EM0=AM0+UUP
      EM0K=EM0*EM0
      EMDK=(AMD+UUP2)**2
      EMDKK=EMDK+EM0K
      EM4KDK=4.*EMDK*EM0K
      EMD0K=EMDK*EM0K
C
      EMT=AMTE+3.*UUP
      EMTK=EMT*EMT
      EMTKK=EMTK+EM0K
      EM4KTK=4.*EMTK*EM0K
C
      EM2K=4.*EM0K
      EM4K2K=4.*EM2K*EM0K
      EM2KK=EM2K+EM0K
C
      EM3K=9.*EM0K
      EM4K3K=4.*EM3K*EM0K
      EM3KK=EM3K+EM0K
C
      EMVM=AMVM+UUP
C
      IF(getRan().GT..5)THEN
        IDP1=1
        IDP2=NIDP
        IINC=1
      ELSE
        IDP1=NIDP
        IDP2=1
        IINC=-1
      ENDIF
c
      coll=coll.or.MMIX.EQ.0.OR.MMIX.EQ.1
      if(coll)call locpa
C
      DO 162 IDP=IDP1,IDP2,IINC
      IF(NUMC(IDP).LE.0)GOTO 162
      IF(IDP.EQ.16)GOTO 162
*     IF(IMXC(IDP).LT.0)GOTO 162
      DO 160 I=IMXC(IDP),IMNC(IDP),-1
      IP=IPO(I)
*     IF(ID(IP).LE.0)GOTO 160
      IF(ID(IP).NE.IDP)GOTO 160
*     ANCIP=ABS(ANC(IP))                    !has been destroyed/produced
**     IF(ANCIP.LT.0.)GOTO 160               !in this time step
      AMVI=AMV(IP)
      EM1=AMVI+BAR(IDP)*UUP
      EM1K=EM1*EM1
C
*      IF(ANCIP.GE.1.)THEN
*        ANCI=0.
*      ELSE
*        ANCI=ANCIP
*        GOTO 22
*      ENDIF
      IIPT=ABS(IPTI(IP))
      NCSI=ABS(NCS(IP))
      IF(NCSI.EQ.0)THEN
        IPT=IIPT
      ELSE
        IPT=0
      ENDIF
C
*      DO 21 JDP=1,IDP
*      IF(IMXC(JDP).LT.0)GOTO 21
*      DO 20 J=IMNC(JDP),MIN(IMXC(JDP),I-1)
*      JP=IPO(J)
*      IF(ID(JP).LE.0)GOTO 20
*      ANCJ=ABS(ANC(JP))
**     IF(ANCJ.LT.0.)GOTO 20
*      IF(ANCJ.NE.ANCI)GOTO 22
* 20   CONTINUE
* 21   CONTINUE
*      GOTO 180
* 22   CONTINUE
*C  QUICK EXIT IF EVERYTHING FROM ONE NUCLEUS AND NOTHING HAS
*C  COLLIDED BEFORE
C
      EEI=EE(IP)
      PXI=PX(IP)
      PYI=PY(IP)
      PZI=PZ(IP)
C                                             !both from the same nucleus
      BREAKI=.FALSE.
      IF(IDP.GE.3.AND.IDP.LE.5)THEN
        IF(CALO)CALL LOCPA
        CALL GEDF(PXI,PYI,PZI,EEI,BREAKI,IDP)
      ENDIF
C
      IF(NPION)GOTO 30
      IF(IDP.LE.6.OR.IDP.GE.13)GOTO 30
C  DELTA and N* BREAKUP
*      if(idp.eq.11.or.idp.eq.12)then
*        if(getRan().gt.1./60.)goto 30
*      endif
      RD=getRan()                   !single-ptcle number for decay
      rd=rd/ffpix
      IF(RD.GT.GTMAX(IDP))GOTO 30
      EM1=SQRT(EM1K)
      CALL WIDTH(EM1-EM0-AME,GAM,IDP,*30)
      GR=FG*EM1/EEI*GAM    !*FRXT !!lmo     !frxt commented out here
      IF(RD.GT.GR)GOTO 30
      PCK=((EM1K-EM0K-AMEK)**2-4.*EM0K*AMEK)/(4.*EM1K)
C
      IF(PCK.GT.0.)THEN
        PC=SQRT(PCK)
      ELSE
        GOTO 30
      ENDIF
      EIC=SQRT(EM0K+PCK)
C
      CTHS=2.*getRan()-1.
      CALL NEWS(0.,PC,0.,PC,CTHS,PC,EIC,PIFX,PIFY,PIFZ
     S  ,PXI/EEI,PYI/EEI,PZI/EEI,EEI/EM1)
C  FINAL ENERGY
      EIF=SQRT(EM0K+PIFX*PIFX+PIFY*PIFY+PIFZ*PIFZ)
C
      IF(IDP.EQ.7)THEN        !delta ++
        IDI=1
        IDK=13
      ELSEIF(IDP.EQ.10)THEN             !delta -
        IDI=2
        IDK=15
      ELSEIF(RD.LT.GR/3.)THEN
        IF(IDP.EQ.8)THEN               !delta +
          IDI=2
          IDK=13
        ELSEIF(IDP.EQ.9)THEN           !delta 0
          IDI=1
          IDK=15
        ELSE                           !stars
          IDI=IDP-10
          IDK=14
        ENDIF
      ELSE
        IF(IDP.LE.10)THEN               !delta +/0
          IDI=IDP-7
          IDK=14                           !pi zero
        ELSEIF(IDP.EQ.11)THEN           !star +
          IDI=2
          IDK=13                           !pi plus
        ELSE                           !star 0
          IDI=1
          IDK=15                           !pi minus
        ENDIF
      ENDIF
C
      IF(CALO)CALL LOCPA
c
      call getpi(fpi,PXI-PIFX,PYI-PIFY,PZI-PIFZ,idk)
      ffpit=1.+fpi
      if(getRan().gt.ffpit/ffpix)goto 30
c
      KKVE=KKVE+1
      CALL GETFF(PIFX,PIFY,PIFZ,EIF,*30,IDI)      !check whether nucleon OK
      KCVER=KCVER+1
      ID(IP)=IDI
      NUMC(IDP)=NUMC(IDP)-1
C
      COLL=.TRUE.
C
      PX(IP)=PIFX
      PY(IP)=PIFY
      PZ(IP)=PIFZ
      EE(IP)=EIF
      AMV(IP)=AM0
      NCS(IP)=-(NCSI+1)                 !minus because created
      ncs(ip)=-1       !!!!!
      IPTI(IP)=-IIPT
C
      KP=IEM(NEM)
      NEM=NEM-1          !entry from the stack
      PX(KP)=PXI-PIFX
      PY(KP)=PYI-PIFY
      PZ(KP)=PZI-PIFZ
      EE(KP)=SQRT(AMEK+PX(KP)**2+PY(KP)**2+PZ(KP)**2)
      EETVE=EETVE+EIF+EE(KP)-EEI
C
      IF(PENT)THEN                          !entry into the other stack
        NECL=NECL+1
        IECL(NECL)=KP
      ENDIF
C
      AMV(KP)=AME
      NCS(KP)=-1                        !minus because created
      ncs(kp)=-ncsi  !!!!!!
      IPTI(KP)=-IIPT
      ID(KP)=IDK
      iRfo(kp)=iRfo(ip)
C
      XX(KP)=XX(IP)
      YY(KP)=YY(IP)
      ZZ(KP)=ZZ(IP)
C
      GOTO 160
C
 30   CONTINUE
      IF(getRan().GT..5)THEN
        JDP1=1
        JDP2=IDP
        JINC=1
      ELSE
        JDP1=IDP
        JDP2=1
        JINC=-1
      ENDIF
      DO 152 JDP=JDP1,JDP2,JINC
      IF(NUMC(JDP).LE.0)GOTO 152
      IF(JDP.EQ.16)GOTO 152
      JMAX=IMXC(JDP)
*     IF(JMAX.LE.0)GOTO 152
      JMAX=MIN(JMAX,I-1)
      JMIN=IMNC(JDP)
      NJT=JMAX-JMIN+1
      IF(NJT.LE.0)GOTO 152
      BREAK=BREAKI.AND.JDP.LE.2
C  2-BODY NUMBER
      NO2NS=IDP.GE.3    !not two nucleons
      VOLIJ=MAX(VOLCM(IDP,JDP),VOLCH(IDP,JDP))
      ANJ=NJT*VOLIJ+.999
      NJ=ANJ
      IF(NJ.EQ.0)GOTO 152
      RSNO=REAL(NJ)/NJT
      IF(IDP.LE.2)THEN
        NJ1=NJ
*        IF(DEPRO.AND.CODPRO)THEN
        IF(DEPRO)THEN
          IF(IDP.EQ.JDP)THEN
            ANJ=VDPPMX
            AANJ=VTPPMX
          ELSE
            ANJ=VDPNMX
            AANJ=VTPNMX
          ENDIF
          ANJ=NJT*ANJ+.999
*          anj=1e4
          NJ2=ANJ                       !for deuterons
          RSNO2=REAL(NJ2)/NJT
          AANJ=NJT*AANJ+.999
*          aanj=1e4
          NJ3=AANJ                      !for tritons
          RSNO3=REAL(NJ3)/NJT
        ELSE
          NJ2=0
          NJ3=0
        ENDIF
        NJ=NJ1+NJ2+NJ3
        SLICE=.TRUE.
      ELSE
        SLICE=.FALSE.
      ENDIF
      IF(BREAK.OR.NJ.GT.NJT)THEN
        NJ=NJT
        RSNO=1.
        RSNO2=1.
        RSNO3=1.
        SLICE=.FALSE.
      ENDIF
      J=JMIN+NJT*getRan()
      BARJU=BAR(JDP)*UUP
C
      JJ=1
 1200 CONTINUE
*      NODEPS=NODEP.OR.NOCOD.OR.SLICE.AND.JJ.LE.NJ1
      NODEPS=NODEP.OR.SLICE.AND.JJ.LE.NJ1
      IF(J.LT.JMIN)J=JMAX
      IF(J.GT.JMAX)J=JMIN
      JP=IPO(J)
*     IF(ID(JP).LE.0)GOTO 150
      IF(ID(JP).NE.JDP)THEN
        IF(NJ.LT.NJT)THEN
          IF(SLICE)THEN
            IF(JJ.LE.NJ1)THEN
              NJ1=NJ1+1
            ELSEIF(JJ.LE.NJ1+NJ2)THEN
              NJ2=NJ2+1
            ENDIF
          ENDIF
          NJ=NJ+1
        ENDIF
        GOTO 150
      ENDIF
**     IF(ANCJP.LT.0.)GOTO 150            !has interacted in this time step
      JJPT=ABS(IPTI(JP))
      NCSJ=ABS(NCS(JP))
      IF(NCSJ.EQ.0)THEN
        IF(JJPT.EQ.IPT)GOTO 150    !must collide first w/s-one f/other nucleus
      ENDIF
C
      AMVJ=AMV(JP)
      EMS=AMVJ+BARJU
      EMSK=EMS*EMS
C
      PXJ=PX(JP)
      PYJ=PY(JP)
      PZJ=PZ(JP)
      EEJ=EE(JP)
C
      AG1=EEI*EEJ-PXI*PXJ-PYI*PYJ-PZI*PZJ
      AG2=EM1*EMS
      EPCMK=(AG1-AG2)*(AG1+AG2)
      IF(EPCMK.LE.0.)GOTO 150
      EPCM=SQRT(EPCMK)
C
      RS=RSNO*getRan()
C
      VOLPH=0.   !!!!!! immobilized photon production
      LHIGH=.FALSE.
      redf=1e30
      IF(.NOT.BREAK)THEN
        ECMK=(EEI+EEJ)**2-((PXI+PXJ)**2+(PYI+PYJ)**2+(PZI+PZJ)**2)
        ECM=SQRT(ECMK)
        DECMM=ECM-EM1-EMS
        IF(BAR(IDP).LE.1.5.AND.BAR(JDP).LE.1.5.AND.PION)THEN
          IF(DECMM.GT.DHOH)THEN
            LHIGH=.TRUE.
          ELSEIF(DECMM.GT.DHOL)THEN
            LHIGH=(getRan()*(DHOH-DHOL)).LT.(DECMM-DHOL)
          ENDIF
        ENDIF
        IF(LHIGH)THEN      !high en cross s.
          SIGO=SIGHO(IDP,JDP)
          SIGE=SIGEH(IDP,JDP)
        ELSEIF(IDP.GE.13.AND.JDP.LE.2)THEN    !pi-N only
          DECM=ECM-EM0-AME
          CALL SIGPIN(SIGO,SIGIE,SIGXX,DECM,IDP,JDP)
        ELSEIF(IDP.GE.7.AND.IDP.LE.12)THEN    !delta/N* - baryon (N/cluster/N*)
          IF(NRPION)THEN                  !correct N-Delta/N*
            IF(JDP.GE.7.AND.JDP.LE.12)THEN
              DECM=ECM-EMVM-EMVM
            ELSE
              DECM=ECM-EM0-EMVM
            ENDIF
            CALL SIGEP1(IDP,AMVI,JDP,AMVJ,DECM,
     C        SNN,SND,SNT,SDD,SIGIE,SIGE)
C             ! sigie - tot inelastic, sige - rough elastic
          ELSE                            !bad N-Delta/N*
            DECM=ECM-EM0-EMVM
            CALL SIGWP(SIGE,SGET,DECM,JDP,IDP)
            PCMK=EPCMK/ECMK
            SIGIE=SGET/PCMK
          ENDIF
          SIGO=SIGE+SIGIE
        ELSE                            !N/cluster-N/cluster or pi-cluster/N*
          SIGO=SIG(EPCM,IDP,JDP)
*          sigo=4.  !!!
*c Jorg!
**          if(idp.le.2.and.jdp.le.2)sigo=max(sigo,4.)
          SIGPH=SPNPH(EPCM,IDP,JDP)
          VOLPH=FVE*SIGPH
**  needs to be changed
*          if(idp.le.2.and.jdp.le.2)then
*            IF(CALO)CALL LOCPA
*            call loren(-bxpt*gmpt,-bypt*gmpt,-bzpt*gmpt,gmpt
*     l        ,pxi,pyi,pzi,eei,pcxi,pcyi,pczi,eeci)
*            call loren(-bxpt*gmpt,-bypt*gmpt,-bzpt*gmpt,gmpt
*     l        ,pxj,pyj,pzj,eej,pcxj,pcyj,pczj,eecj)
*            tcij=sqrt(pcxi**2+pcyi**2+pczi**2+am0k)
*     t        +sqrt(pcxj**2+pcyj**2+pczj**2+am0k)-2.*am0
*            redf=exp(-rhoxi*.6/(1.+(tcij/.15)**2))
**r            psig0=.243*rhoxi**.667
**r            redf=1./(1.+sigo*psig0/.7)
*          endif
*          sigo=min(sigo,5.0/frat)
*          if(rhoxi.gt.1.)sigo=3.0
*          sigo=min(sigo,10.) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*          if(idp.le.2)then   !jdp always le idp
*            npair=npair+1
*            ecmk=(eei+eej)**2-((pxi+pxj)**2+(pyi+pyj)**2+(pzi+pzj)**2)
*            ecm=sqrt(ecmk)
*            DPAIR=ecm-em0-em0
*            epair=epair+DPAIR
*            IEPA=DPAIR/DES+1
*            eone=eone+eei+eej-em0-em0
*          endif
        ENDIF
        FVE=FV*EPCM/(EEI*EEJ)  !*FRXT !lmo  ! frxt commented and below 8/5/99
        if(bar(idp).gt..5.and.bar(jdp).gt..5)fve=fve*frxt
        VOLC=FVE*SIGO
        NODEPS=NODEPS.OR.LHIGH
C  ! composite prod off f/h-e collisions; NN cs isotropic/inconsistent...
        IF(NO2NS.OR.NODEPS)THEN
          IF(RS.GE.VOLC+VOLPH)GOTO 150
        ENDIF
      ENDIF
C  CHECK WITH THE ACTUAL COLLISIONAL VOLUME
C
      EIJ=EEI+EEJ
      PIJX=PXI+PXJ
      PIJY=PYI+PYJ
      PIJZ=PZI+PZJ
C
      IF(.NOT.LHIGH.AND.IDP.GE.13.AND.JDP.LE.2)THEN
        VOLIE=FVE*SIGIE
        IF(RS.LT.VOLIE)GOTO 1300             !pion absorption
      ENDIF
C
      BX=PIJX/EIJ
      BY=PIJY/EIJ
      BZ=PIJZ/EIJ
      GM=1./SQRT(1.-(BX*BX+BY*BY+BZ*BZ))
C
      BPI=BX*PXI+BY*PYI+BZ*PZI
C
      EIC=GM*(EEI-BPI)
C
      BCPI=GM*(EEI-GM/(GM+1.)*BPI)
      PICX=PXI-BX*BCPI
      PICY=PYI-BY*BCPI
      PICZ=PZI-BZ*BCPI
C  SO NOW WE ARE IN THE C.M. SYSTEM
C
      PICK=PICX*PICX+PICY*PICY+PICZ*PICZ
      IF(PICK.EQ.0.)GOTO 150
      PIC=SQRT(PICK)    !2-body cm parameters
      IF(LHIGH)THEN
        VOLEE=FVE*SIGE
        IF(RS.LT.VOLEE)GOTO 1480
        GOTO 1490    ! moved here to allow for elastic f/failure 'n else
      ENDIF
C
      IF(NO2NS.OR.NODEPS)THEN                  !other than both nucleons
        IF(.NOT.NO2NS)GOTO 2450          !gets here if nodep option chosen
        IF(CALO)CALL LOCPA                    !must go somewhere...
        IF(IDP.GE.7.AND.IDP.LE.10)THEN
          IF(JDP.GE.7.AND.JDP.LE.10)GOTO 1445  ! new channel DD-->NN
        ENDIF
        IF(IDP.GE.3.AND.JDP.GE.3)GOTO 1480    !both weirdos go to elastic
C  IF NONE NUCLEON GOTO ELASTIC               !
        IF(IDP.GE.13.AND.JDP.LE.2)GOTO 1480  ! h-e elastic pion
        ECMK=EIJ*EIJ-(PIJX*PIJX+PIJY*PIJY+PIJZ*PIJZ)
        ECM=SQRT(ECMK)
        IF(BREAK.OR.IDP.GE.3.AND.IDP.LE.5)THEN
          IF(IDP.EQ.3)GOTO 1450               !testing for d breakup
          GOTO 2400                           !testing for t/3He breakup
        ELSE
          GOTO 1445                           !delta reabs/NO reshuffling
        ENDIF
      ENDIF
C
      IF(SLICE)THEN
        IF(JJ.GT.NJ1+NJ2)GOTO 1600
      ELSE
*        IF(RS.LT.VOLC.AND.3.*getRan().LT.1.)GOTO 2450
      ENDIF
C COLLIDE!?
C  IF NEEDED LOCAL PARAMETERS MUST BE CALLED
C  THIS LOOKS LIKE MAX. 3-BODY COLLISIONAL VOLUME ...
C
      IF(IDP.NE.JDP)THEN
        FVOLD=1.
        KDI=1
        KDF=2
      ELSE
        FVOLD=2.
        KDI=3-IDP
        KDF=KDI
      ENDIF
C  THESE ARE THE CASES: PN, PP, NN
C  THEN THE ATTACHED PARTICLES: N OR P, N, P
      VOLDM=FVOLD*VDMAX*SIGO
      VOLDMR=VOLDM/RSNO2
C
      IF(getRan().GT..5)THEN
        KDP1=KDI
        KDP2=KDF
        KINC=1
      ELSE
        KDP1=KDF
        KDP2=KDI
        KINC=-1
      ENDIF
C
*      DO 1442 KDP=KDF,KDI,-1
*      DO 1442 KDP=KDI,KDF
      DO 1442 KDP=KDP1,KDP2,KINC
      IF(NUMC(KDP).LE.0)GOTO 1442
      KMAX=IMXC(KDP)
*     IF(KMAX.LE.0)GOTO 1442
      KMIN=IMNC(KDP)
      NKT=KMAX-KMIN+1
      IF(VOLDMR.GE.1.)THEN
        NK=NKT
*        K=KMAX
*        K=KMIN
        RSDNO=RSNO2
      ELSE
        ANK=MAX((NKT+FVOLD-2.)*VOLDMR,0.)             !finite ptcle effect
        NK=ANK
        RLO=getRan()
        IF(RLO.LT.ANK-NK)NK=NK+1
        IF(NK.LE.0)GOTO 1442
*        K=KMIN+NKT*getRan()
        RSDNO=VOLDM
      ENDIF
      K=KMIN+NKT*getRan()
      RSDNO=RSDNO/FVOLD
C                                    !corr. for missing nucleons
      KK=1                 !counter
 1430 CONTINUE
*      IF(K.LT.KMIN)K=KMAX
      IF(K.GT.KMAX)K=KMIN
      IF(K.LT.KMIN)K=KMAX
      KP=IPO(K)
*     IF(ID(KP).LE.0)GOTO 1440
      IF(K.EQ.I.OR.K.EQ.J.OR.ID(KP).NE.KDP)THEN
        NK=MIN(NK+1,NKT)
        GOTO 1440
      ENDIF
**     IF(ANC(KP).LT.0.)GOTO 1440            !has interacted in this time step
C
      EEK=EE(KP)
      PXK=PX(KP)
      PYK=PY(KP)
      PZKK=PZ(KP)
C
      EIJK=EIJ+EEK
      PIJKX=PIJX+PXK
      PIJKY=PIJY+PYK
      PIJKZ=PIJZ+PZKK
C
      ECMK=EIJK*EIJK-(PIJKX*PIJKX+PIJKY*PIJKY+PIJKZ*PIJKZ)
      ECM=SQRT(ECMK)
C
      PKMXK=((ECMK-EM2KK)**2-EM4K2K)/(4.*ECMK)
      IF(PKMXK.LT.0.)THEN
        WRITE(*,*)'! PKMXK = ',PKMXK
        WRITE(10,*)'! PKMXK = ',PKMXK
        GOTO 1440
      ENDIF
      PKMX=SQRT(PKMXK)
C
      BBX=PIJKX/EIJK
      BBY=PIJKY/EIJK
      BBZ=PIJKZ/EIJK
C
      GGM=EIJK/ECM
C
      BPK=BBX*PXK+BBY*PYK+BBZ*PZKK
C
      EKC=GGM*(EEK-BPK)
C
      AG=EKC*EKC-EM0K
      IF(AG.GT.0.)THEN
        PKC=SQRT(AG)
      ELSE
        GOTO 1440
      ENDIF
C
      PDCK=((ECMK-EMDKK)**2-EM4KDK)/(4.*ECMK)
      IF(PDCK.LT.0.)GOTO 1440             !new: no possibility to make
      PDC=SQRT(PDCK)                      !     a deuteron
      EDC=SQRT(EMDK+PDCK)
C
      EDKC=EDC*EKC
      PDKC=PDC*PKC
      PMAX=SQRT(((EDKC+PDKC)**2-EMD0K)/EMDK)
      AG=((EDKC-PDKC)**2-EMD0K)/EMDK
      IF(AG.GT.0.)THEN
        PMIN=SQRT(AG)
      ELSE
        PMIN=0.
      ENDIF
C
      VPMN=VOLDE(PMIN)
      VPMX=VOLDE(PMAX)
      VNX=VPMN-VPMX
      VOLD=FD*EMDK*EIC/(EEK*ECM*PIC*PKC)*VNX
      VOLD=VOLD*FDEU(PKMX)*VOLC
      RSD=RSDNO*getRan()
      IF(RSD.GE.VOLD)GOTO 1440
      IF(CALO)CALL LOCPA
      KK3=KK3+1
C  TRANSFORMING THIS XTRA PTCLE TO 3-PTCLE CM
      BCPK=GGM*(EEK-GGM/(GGM+1.)*BPK)
      PKCX=PXK-BBX*BCPK
      PKCY=PYK-BBY*BCPK
      PKCZ=PZKK-BBZ*BCPK
      CALL GEDT
      CALL NEWS(PKCX,PKCY,PKCZ,PKC,CTHD,PDC,EDC,PKFX,PKFY,PKFZ
     S  ,BBX,BBY,BBZ,GGM)
      EKF=SQRT(EMDK+PKFX*PKFX+PKFY*PKFY+PKFZ*PKFZ)
C  THIS WAS DEUTERON...
C
C  NUCLEON THAT STAYS
      PIFX=PIJKX-PKFX
      PIFY=PIJKY-PKFY
      PIFZ=PIJKZ-PKFZ
C  FINAL ENERGIES
      EIF=SQRT(EM0K+PIFX*PIFX+PIFY*PIFY+PIFZ*PIFZ)
C
C  ID
      IF(IDP.NE.JDP)THEN
        IDI=KDP
      ELSE
        IDI=IDP
      ENDIF
      if(rhoxi.gt.0.95)goto 1440
*      if(rhoxi.gt.0.25)goto 1440
      if(rhoxi.gt..35)then         !modified 11/5/01 soft-cut
        if((rhoxi-.35).gt..6*getRan())goto 1440
      endif
      CALL GETF(PIFX,PIFY,PIFZ,EIF,*1440,IDI)
      CALL GETF(PKFX,PKFY,PKFZ,EKF,*1440,3)
C  DEUTERON MOVED
C
      KC3R=KC3R+1
*      goto 160
      EET3=EET3+EIF+EKF-EIJK
C
      COLL=.TRUE.
C
C  SUBSTITUTING THE NEW VALUES
      IF(IDI.EQ.IDP)THEN
        IIP=IP
        JJP=JP
        JJDP=JDP
      ELSE
        IIP=JP
        JJP=IP
        JJDP=IDP
      ENDIF
C
*     ID(IP)=IDI
*     IF(IDP.NE.IDI)NUMC(IDP)=NUMC(IDP)-1
C  NUCLEON THAT STAYS
      PX(IIP)=PIFX
      PY(IIP)=PIFY
      PZ(IIP)=PIFZ
      EE(IIP)=EIF
      NCS(IIP)=NCSI+1
      IPTI(IIP)=-IPTI(IIP)
C  NUCLEON THAT DISAPPEARS
      ID(JJP)=0
      NEM=NEM+1
      IEM(NEM)=JJP
      NUMC(JJDP)=NUMC(JJDP)-1
C  DEUTERON NOW
      PX(KP)=PKFX
      PY(KP)=PKFY
      PZ(KP)=PKFZ
      EE(KP)=EKF
      NCS(KP)=-(ABS(NCS(KP))+NCSJ+2)          !minus because created
      IPTI(KP)=-(JJPT+ABS(IPTI(KP)))
      ID(KP)=3
      AMV(KP)=AMD
      NUMC(KDP)=NUMC(KDP)-1
*     itfo(kp)=itim
      GOTO 160
 1440 CONTINUE
*      K=K-1
*      K=K+1
      K=K+KINC
      KK=KK+1
      IF(KK.LE.NK)GOTO 1430
 1442 CONTINUE                             !loop for d's
      IF(SLICE)GOTO 150                    !exit
 1600 CONTINUE
*      goto 1730
      VOLTM=VTMAX*SIGO
*      if(tim.ge.15.)voltm=1e6
      IF(getRan().GT..5)THEN
        KDP1=1
        KDP2=2
        KINC=1
      ELSE
        KDP1=2
        KDP2=1
        KINC=-1
      ENDIF
      IF(getRan().GT..5)THEN
        MINC=1
      ELSE
        MINC=-1
      ENDIF
C
*     DO 1720 KDP=2,1,-1
*      DO 1720 KDP=1,2
      DO 1720 KDP=KDP1,KDP2,KINC
      IF(NUMC(KDP).LE.0)GOTO 1720
      KMAX=IMXC(KDP)
*     IF(KMAX.LE.0)GOTO 1720
      KMIN=IMNC(KDP)
      NKT=KMAX-KMIN+1      !external will always go in full
      NK=NKT
      IF(IDP.EQ.KDP)NK=NK-1
      IF(JDP.EQ.KDP)NK=NK-1
      IF(NK.LE.0)GOTO 1720
*      DO 1710 MDP=KDP,1,-1
*      DO 1710 MDP=1,KDP
      IF(MINC.EQ.1)THEN
        MDP1=1
        MDP2=KDP
      ELSE
        MDP1=KDP
        MDP2=1
      ENDIF
      DO 1710 MDP=MDP1,MDP2,MINC
      IF(MDP.EQ.KDP)THEN
        NKMT=(NKT*(NKT-1))/2
        MMIN=KMIN
        MMAX=KMAX
        NMT=NKT
        IF(IDP.NE.JDP)THEN
          FVOLT=1.
        ELSEIF(MDP.EQ.IDP)THEN
          GOTO 1710
        ELSE
          FVOLT=2.
        ENDIF
      ELSE
        IF(NUMC(MDP).LE.0)GOTO 1710
        MMAX=IMXC(MDP)
*       IF(MMAX.LE.0)GOTO 1710
        MMIN=IMNC(MDP)
        NMT=MMAX-MMIN+1
        NKMT=NKT*NMT
        FVOLT=2.
      ENDIF
      VOLKM=VOLTM*FVOLT
      VOLKMR=VOLKM/RSNO3
*      volkmr=1e10
      IF(VOLKMR.GE.1.)THEN
        NKM=NKMT
*        K=KMAX
*        K=KMIN
        RSTNO=RSNO3
**      write(*,*)'voltm is 1'
      ELSE
        IF(MDP.EQ.KDP)THEN
          NKM=(NK*(NK-1))/2
        ELSE
          NM=NMT
          IF(IDP.EQ.MDP)NM=NM-1
          IF(JDP.EQ.MDP)NM=NM-1
          IF(NM.LE.0)GOTO 1710
          NKM=NK*NM
        ENDIF
        ANKM=NKM*VOLKMR
        NKM=ANKM
        RLO=getRan()
        IF(RLO.LT.ANKM-NKM)NKM=NKM+1
        IF(NKM.LE.0)GOTO 1710
*        K=KMIN+NKT*getRan()
        RSTNO=VOLKM
      ENDIF
      K=KMIN+NKT*getRan()
      RSTNO=RSTNO/FVOLT
      KKMM=0      !counter
*      K=K+1
*      K=K-1
      K=K-KINC
 1650 CONTINUE    !new K
*      K=K-1
*      K=K+1
      K=K+KINC
      KKMM=KKMM+1
      IF(KKMM.GT.NKM)GOTO 1710
      IF(K.LT.KMIN)K=KMAX
      IF(K.GT.KMAX)K=KMIN
      KP=IPO(K)
*     IF(ID(KP).LE.0)GOTO 1650
      IF(K.EQ.I.OR.K.EQ.J.OR.ID(KP).NE.KDP)THEN
        NKM=MIN(NKM+1,NKMT)
        GOTO 1650
      ENDIF
**     IF(ANC(KP).LT.0.)GOTO 1650
C
      EEK=EE(KP)
      PXK=PX(KP)
      PYK=PY(KP)
      PZKK=PZ(KP)
C
      EIJK=EIJ+EEK
      PIJKX=PIJX+PXK
      PIJKY=PIJY+PYK
      PIJKZ=PIJZ+PZKK
C
      KKMM=KKMM-1
*      M=MIN(MMAX,K-1)
*      M=M+1
*      M=MMIN
      IF(MINC.EQ.1)THEN
        M=MMIN
      ELSE
        M=MIN(MMAX,K-1)
      ENDIF
*      M=M-1
      M=M-MINC
 1660 CONTINUE
*      M=M-1
*      M=M+1
      M=M+MINC
      IF(M.LT.MMIN)GOTO 1650
      IF(M.GT.MIN(MMAX,K-1))GOTO 1650
      KKMM=KKMM+1
      IF(KKMM.GT.NKM)GOTO 1710
      MP=IPO(M)
*     IF(ID(MP).LE.0)GOTO 1660
      IF(M.EQ.I.OR.M.EQ.J.OR.ID(MP).NE.MDP)THEN
        NKM=MIN(NKM+1,NKMT)
        GOTO 1660
      ENDIF
**     IF(ANC(MP).LE.0.)GOTO 1660
C
      EEM=EE(MP)
      PXM=PX(MP)
      PYM=PY(MP)
      PZM=PZ(MP)
C
      EIJKM=EIJK+EEM
      PIJKMX=PIJKX+PXM
      PIJKMY=PIJKY+PYM
      PIJKMZ=PIJKZ+PZM
C
      ECMK=EIJKM*EIJKM-(PIJKMX*PIJKMX+PIJKMY*PIJKMY+PIJKMZ*PIJKMZ)
      ECM=SQRT(ECMK)
C
      PTCK=((ECMK-EMTKK)**2-EM4KTK)/(4.*ECMK)
      IF(PTCK.LT.0.)GOTO 1660
      PTC=SQRT(PTCK)
      ETC=SQRT(EMTK+PTCK)
C
      EEKM=EEK+EEM
      PXKM=PXK+PXM
      PYKM=PYK+PYM
      PZKM=PZKK+PZM
C
      EKMCK=EEKM*EEKM-(PXKM*PXKM+PYKM*PYKM+PZKM*PZKM)
C
      AGCO=.5*(EKMCK-EM2K)
C
      EEKMC=(EIJKM*EEKM-(PIJKMX*PXKM+PIJKMY*PYKM+PIJKMZ*PZKM))/ECM
C
      EEKMCK=EEKMC*EEKMC
      PPKMCK=EEKMCK-EKMCK
      IF(PPKMCK.LE.0.)GOTO 1660
      PPKMC=SQRT(PPKMCK)          !total in 4-ptcle cm
C
      ETKMC=ETC*EEKMC
      PTKMC=PTC*PPKMC
C
      PMINK=(ETKMC-PTKMC)**2/EMTK-EKMCK
      PMAXK=(ETKMC+PTKMC)**2/EMTK-EKMCK
C
      DECM=ECM-4.*EM0
C
      VTMN=VOLTI(1.5*PMINK+AGCO)
      VTMX=VOLTI(1.5*PMAXK+AGCO)
      VTNX=VTMN-VTMX
      VOLT=VTNX*FTRI(DECM)
      VOLT=VOLT*FT*3.*2.*VOLC*EIC/(2.*PIC)
      VOLT=VOLT/(ECM*PPKMC)*EM0K/(EEK*EEM)
*      kks=kks+1
*      st=st+volt
C
      RST=RSTNO*getRan()
      IF(RST.GE.VOLT)GOTO 1660
      IF(CALO)CALL LOCPA
      KK4=KK4+1
      CALL GETT
C
      BBX=PIJKMX/EIJKM
      BBY=PIJKMY/EIJKM
      BBZ=PIJKMZ/EIJKM
C
      GGM=EIJKM/ECM
C
      BPKM=BBX*PXKM+BBY*PYKM+BBZ*PZKM
      BCPKM=GGM*(EEKM-GGM/(GGM+1.)*BPKM)
      PPKMCX=PXKM-BBX*BCPKM
      PPKMCY=PYKM-BBY*BCPKM
      PPKMCZ=PZKM-BBZ*BCPKM
C
      CALL NEWS(PPKMCX,PPKMCY,PPKMCZ,PPKMC,CTHT,PTC,ETC
     S  ,PKFX,PKFY,PKFZ,BBX,BBY,BBZ,GGM)
      EKF=SQRT(EMTK+PKFX*PKFX+PKFY*PKFY+PKFZ*PKFZ)
C  ID'S
      IF(IDP.EQ.JDP)THEN
        IDI=IDP
        IF(KDP.EQ.MDP)THEN
          IDK=6-IDI
        ELSE
          IDK=3+IDI
        ENDIF
      ELSEIF(KDP.EQ.MDP)THEN
        IDI=KDP
        IDK=3+KDP
      ELSEIF(getRan().GT..5)THEN   !1/2 probability for making t/3He
        IDI=1
        IDK=5
      ELSE
        IDI=2
        IDK=4
      ENDIF
C
C  NUCLEON THAT STAYS
      PIFX=PIJKMX-PKFX
      PIFY=PIJKMY-PKFY
      PIFZ=PIJKMZ-PKFZ
C  FINAL ENERGIES
      EIF=SQRT(EM0K+PIFX*PIFX+PIFY*PIFY+PIFZ*PIFZ)
C
      if(rhoxi.gt.0.95)goto 1660
*      if(rhoxi.gt.0.25)goto 1660
      if(rhoxi.gt..35)then         !modified 11/5/01 soft-cut
        if((rhoxi-.35).gt..6*getRan())goto 1660
      endif
      CALL GETF(PIFX,PIFY,PIFZ,EIF,*1660,IDI)
C  NUCLEON THAT STAYS
      CALL GETF(PKFX,PKFY,PKFZ,EKF,*1660,IDK)
C  TRITON MOVED
      KC4R=KC4R+1
      EET4=EET4+EIF+EKF-EIJKM
C
      COLL=.TRUE.
C
C  SUBSTITUTING THE NEW VALUES
      IF(IDI.EQ.IDP)THEN
        IIP=IP
        JJP=JP
        JJDP=JDP
      ELSE
        IIP=JP
        JJP=IP
        JJDP=IDP
      ENDIF
C
C  NUCLEON THAT STAYS
      PX(IIP)=PIFX
      PY(IIP)=PIFY
      PZ(IIP)=PIFZ
      EE(IIP)=EIF
*      ID(IP)=IDI
*      IF(IDI.NE.IDP)NUMC(IDP)=NUMC(IDP)-1
      NCS(IIP)=NCSI+1
      IPTI(IIP)=-IPTI(IIP)
C  NUCLEONS THAT DISAPPEAR
      ID(JJP)=0
      NEM=NEM+1
      IEM(NEM)=JJP
      NUMC(JJDP)=NUMC(JJDP)-1
      ID(MP)=0
      NEM=NEM+1
      IEM(NEM)=MP
      NUMC(MDP)=NUMC(MDP)-1
C  TRITON/3HE NOW
      PX(KP)=PKFX
      PY(KP)=PKFY
      PZ(KP)=PKFZ
      EE(KP)=EKF
      NCS(KP)=-(ABS(NCS(KP))+ABS(NCS(MP))+NCSJ+3)      !minus because created
      IPTI(KP)=-(ABS(IPTI(KP))+ABS(IPTI(MP))+JJPT)
      ID(KP)=IDK
      AMV(KP)=AMTE
      NUMC(KDP)=NUMC(KDP)-1
*     itfo(kp)=itim
      GOTO 160
 1710 CONTINUE
 1720 CONTINUE
 1730 continue
      IF(SLICE.OR.RS.GE.VOLC+VOLPH)GOTO 150      !exit
 2450 CONTINUE                 !2 nucleons if no clusters come here
      IF(CALO)CALL LOCPA
C
      IF(RS.LE.VOLC)GOTO 2480
      KKPH=KKPH+1
C  PHOTON PRODUCTION IN PN BREMSSTRAHLUNG
      CALL GENPH(OMPH,2.*(EIC-EM0))
      CTHS=2.*getRan()-1.
      CALL NEWS(PICX,PICY,PICZ,PIC,CTHS,OMPH,OMPH,PKFX,PKFY,PKFZ
     N  ,BX,BY,BZ,GM)                           !new lab momentum of a photon
      EKF=SQRT(PKFX*PKFX+PKFY*PKFY+PKFZ*PKFZ)
      PIJFX=PIJX-PKFX
      PIJFY=PIJY-PKFY
      PIJFZ=PIJZ-PKFZ
      EIJF=EIJ-EKF
      BBX=PIJFX/EIJF
      BBY=PIJFY/EIJF
      BBZ=PIJFZ/EIJF
      EECMK=EIJF*EIJF-(PIJFX*PIJFX+PIJFY*PIJFY+PIJFZ*PIJFZ)
      EECM=SQRT(EECMK)
      GGM=EIJF/EECM
      AG=.25*EECMK-EM0K
      IF(AG.GE.0.)THEN
        PIFC=SQRT(AG)
      ELSE
        GOTO 150
      ENDIF
      CT=2.*getRan()-1.
      CALL NEWS(0.,0.,PIFC,PIFC,CT,PIFC,.5*EECM,PIFX,PIFY,PIFZ
     S  ,BBX,BBY,BBZ,GGM)
      EIF=SQRT(EM0K+PIFX*PIFX+PIFY*PIFY+PIFZ*PIFZ)
      CALL GETF(PIFX,PIFY,PIFZ,EIF,*150,IDP)
      PJFX=PIJFX-PIFX
      PJFY=PIJFY-PIFY
      PJFZ=PIJFZ-PIFZ
      EJF=SQRT(EM0K+PJFX*PJFX+PJFY*PJFY+PJFZ*PJFZ)
      CALL GETF(PJFX,PJFY,PJFZ,EJF,*150,JDP)
C
      KP=IEM(NEM)
      NEM=NEM-1          !entry from the stack
      PX(KP)=PKFX
      PY(KP)=PKFY
      PZ(KP)=PKFZ
      EE(KP)=EKF
C
      AMV(KP)=0.
      NCS(KP)=-1
      IPTI(KP)=-IIPT
C  ENTROPY IGNORED FOR PHOTON
      ID(KP)=16
C  POSITIONS SAME AS OF PROTON
      XX(KP)=XX(IP)
      YY(KP)=YY(IP)
      ZZ(KP)=ZZ(IP)
      MDE(KP)=0.
      mdep(kp)=0.
C
      if(ekf.gt..025)KCPHR=KCPHR+1
C
      IIP=IP
      JJP=JP
C
      GOTO 1525
C
 2480 CONTINUE
**      goto 1480
      IF(OLAST)GOTO 1480                       !option no inelastic ...
C  NEW INELASTIC CROSS SECTION F/NN AND ELSE...
      ECM=EIC+EIC
      DECM=ECM-EM0-EM0
      CALL CIBAR(IDP,AM0,JDP,AM0,DECM,SNN,SND,SNT,SDD,SIGIT)
C  ! sigit - net inelastic
      RSS=getRan()*SIGO
      IF(RSS.GE.SIGIT)GOTO 1480                 !goto elastic
C
      IF(RSS.GE.SDD)GOTO 1270                    !not DD production
      KKVV=KKVV+1
C
      DMI=DECM-AME-AME
      IF(DMI.LE.0.)GOTO 150
C  KINEMATICS
      CALL DDMS(AMVIF,AMVJF,DMI)
      EMVIFK=(AMVIF+UUP)**2                        !dressed mass
      EMVJFK=(AMVJF+UUP)**2                        !dressed mass
      ECMK=ECM*ECM
      PIFCK=((ECMK-EMVIFK-EMVJFK)**2-4.*EMVIFK*EMVJFK)/(4.*ECMK)
      IF(PIFCK.LE.0.)GOTO 150
      PIFC=SQRT(PIFCK)
C  NOW THE ID'S; INITIAL-STATE THRESHOLD
      CALL IBAR(IDP,JDP,IDI,IDJ,3,DECM)
C  SCATTERING ANGLE
      CALL THBARN(CTHS,DECM,IDP,JDP,AM0,AM0,IDI,IDJ,AMVIF,AMVJF)
      EIFC=SQRT(PIFCK+EMVIFK)
      CALL NEWS(PICX,PICY,PICZ,PIC,CTHS,PIFC,EIFC,PIFX,PIFY,PIFZ
     N  ,BX,BY,BZ,GM)                !new lab momentum of the i'th delta
C  FINAL ENERGY
      EIF=SQRT(EMVIFK+PIFX*PIFX+PIFY*PIFY+PIFZ*PIFZ)  ! first delta energy
      AMV(IP)=AMVIF
C
      ID(IP)=IDI
      NUMC(IDP)=NUMC(IDP)-1
      ID(JP)=IDJ
      NUMC(JDP)=NUMC(JDP)-1
C
C  THE OTHER DELTA
      PJFX=PIJX-PIFX
      PJFY=PIJY-PIFY
      PJFZ=PIJZ-PIFZ
      EJF=SQRT(EMVJFK+PJFX*PJFX+PJFY*PJFY+PJFZ*PJFZ)  !delta energy
      AMV(JP)=AMVJF
C
      KCVVR=KCVVR+1
      EETVV=EETVV+EIF+EJF-EIJ
C  NDLZ=???   UNKNOWN PURPOSE
C
      IIP=IP
      JJP=JP
C
      ncs(Ip)=-1      !!!!!!!!!!!
      ncs(jp)=-1      !!!!!!!!!!!
c     goto 1290
C
      GOTO 1525                       !goto substitution of mo/en, coll. no.
C
 1270 CONTINUE  ! NN-->ND/N*
      KKV=KKV+1
      STAR=RSS.GT.(SDD+SND)
      DMI=ECM-EM0-EMVM
      IF(DMI.LE.0.)GOTO 150
      IF(NRPION)THEN
        IF(STAR)THEN
          CALL XMS(AMVJF,DMI)
        ELSE
          CALL DMS(AMVJF,DMI)
        ENDIF
      ELSE
        CALL GETMAZ(AMVJF,DMI,STAR)          !mass of a delta or N*
      ENDIF
      EMVJFK=(AMVJF+UUP)**2                        !dressed mass
      ECMK=ECM*ECM
      PIFCK=((ECMK-EM0K-EMVJFK)**2-4.*EM0K*EMVJFK)/(4.*ECMK)
      IF(PIFCK.LE.0.)GOTO 150
      PIFC=SQRT(PIFCK)
C  NOW THE ID'S; INITIAL-STATE THRESHOLD
      IF(STAR)THEN
        CALL IBAR(IDP,JDP,IDI,IDJ,2,DECM)
      ELSE
        CALL IBAR(IDP,JDP,IDI,IDJ,1,DECM)
      ENDIF
C  THE RESONANCE ALWAYS 2ND PTCLE
      IF(IDI.GT.6)THEN
        III=IDI
        IDI=IDJ
        IDJ=III
      ENDIF
C  SCATTERING ANGLE
      CALL THBARN(CTHS,DECM,IDP,JDP,AM0,AM0,IDI,IDJ,AM0,AMVJF)
      EIFC=SQRT(PIFCK+EM0K)
      CALL NEWS(PICX,PICY,PICZ,PIC,CTHS,PIFC,EIFC,PIFX,PIFY,PIFZ
     N  ,BX,BY,BZ,GM)                           !new lab momentum of a nucleon
C  FINAL ENERGY
      EIF=SQRT(EM0K+PIFX*PIFX+PIFY*PIFY+PIFZ*PIFZ)
      CALL GETFF(PIFX,PIFY,PIFZ,EIF,*150,IDI)      !check whether nucleon OK
      ID(IP)=IDI
      IF(IDI.NE.IDP)NUMC(IDP)=NUMC(IDP)-1
      ID(JP)=IDJ
      NUMC(JDP)=NUMC(JDP)-1
C  DELTA
      PJFX=PIJX-PIFX
      PJFY=PIJY-PIFY
      PJFZ=PIJZ-PIFZ
      EJF=SQRT(EMVJFK+PJFX*PJFX+PJFY*PJFY+PJFZ*PJFZ)  !delta energy
      AMV(JP)=AMVJF
C
      KCVR=KCVR+1
      EETV=EETV+EIF+EJF-EIJ
      NDLZ(IZ,1)=NDLZ(IZ,1)+1
*     IF(IEPA.GE.1.AND.IEPA.LE.NES)NDES(IEPA,1)=NDES(IEPA,1)+1
C
      IIP=IP
      JJP=JP
c
      ncs(jp)=-1      !!!!!!!!!!!
      goto 1292
C
 1290 CONTINUE
      NCS(JP)=-(NCSJ+1)
 1292 continue
      NCS(IP)=-(NCSI+1)
C
      GOTO 1525                       !goto substitution of mo/en, coll. no.
C
 1300 CONTINUE    !pion absorption
      KCEV=KCEV+1
      COLL=.TRUE.
      IF(CALO)CALL LOCPA
C
      EE(JP)=EIJ
      PX(JP)=PIJX
      PY(JP)=PIJY
      PZ(JP)=PIJZ
C
      NCS(JP)=-(NCSJ+1)             !minus because created
      ncs(jp)=-(ncsi+1)             !!!!!!!!!
      IPTI(JP)=-JJPT
C
      RSS=getRan()*SIGIE
      IF(RSS.LT.SIGXX)THEN
        ID(JP)=JDP+IDP-4
      ELSE
        ID(JP)=JDP+IDP-7
      ENDIF
      NUMC(JDP)=NUMC(JDP)-1
C
      AMV(JP)=SQRT(ECMK)-UUP
C  PION THAT DISAPPEARS
      ID(IP)=0
      NEM=NEM+1
      IEM(NEM)=IP
      NUMC(IDP)=NUMC(IDP)-1
      GOTO 160
C
 1445 CONTINUE                        !delta-delta and N-delta/N* interaction
      RSS=getRan()*SIGO
      IF(RSS.GT.SIGIE)GOTO 1480
*      if(idp.ge.11.and.idp.le.12.or.jdp.ge.11.and.jdp.le.12)then
*        if(getRan().gt.1./60.)goto 1480
*      endif
C
      IF(JDP.GT.6)THEN
        KKVVA=KKVVA+1
      ELSE
        KKVB=KKVB+1
      ENDIF
C
      PIFCK=.25*ECMK-EM0K
      IF(PIFCK.LE.0.)GOTO 150
      PIFC=SQRT(PIFCK)
C  NOW THE ID'S; INITIAL-STATE THRESHOLD
      CALL IBAR(IDP,JDP,IDI,IDJ,0,DECM)
C  SCATTERING ANGLE
      DMI=ECM-EM0-EM0
      CALL THBARN(CTHS,DMI,IDP,JDP,AMVI,AMVJ,IDI,IDJ,AM0,AM0)
      CALL NEWS(0.,0.,PIFC,PIFC,CTHS,PIFC,.5*ECM,PIFX,PIFY,PIFZ
     N  ,BX,BY,BZ,GM)
C  FINAL ENERGY
      EIF=SQRT(EM0K+PIFX*PIFX+PIFY*PIFY+PIFZ*PIFZ)
      CALL GETFF(PIFX,PIFY,PIFZ,EIF,*150,IDI) !chck whthr would-be nucleon ok
C  OTHER NUCLEON
      PJFX=PIJX-PIFX
      PJFY=PIJY-PIFY
      PJFZ=PIJZ-PIFZ
      EJF=SQRT(EM0K+PJFX*PJFX+PJFY*PJFY+PJFZ*PJFZ)  !former delta energy
      CALL GETFF(PJFX,PJFY,PJFZ,EJF,*150,IDJ)       !chck whthr othr nucleon OK
C
      IF(JDP.GT.6)THEN
        KCVVAR=KCVVAR+1
        EETVVA=EETVVA+EIF+EJF-EIJ
      ELSE
        KCVBR=KCVBR+1
        EETVB=EETVB+EIF+EJF-EIJ
      ENDIF
C
      ID(IP)=IDI
      IF(IDI.NE.IDP)NUMC(IDP)=NUMC(IDP)-1
      ID(JP)=IDJ
      IF(IDJ.NE.JDP)NUMC(JDP)=NUMC(JDP)-1
C
      AMV(IP)=AM0
      AMV(JP)=AM0
C
      IIP=IP
      JJP=JP
C
      GOTO 1290
C
 2400 CONTINUE
C  TRITON/3HE BREAKUP
      PKMXK=((ECMK-EM3KK)**2-EM4K3K)/(4.*ECMK)
      IF(PKMXK.LE.0.)GOTO 1480
      PKMX=SQRT(PKMXK)
      IF(.NOT.BREAK)THEN
        SIGB=SIGTB(PKMX,IDP,JDP)
        redf=-1.
        RSS=getRan()*SIGO
        IF(RSS.GT.SIGB)GOTO 1480
      ENDIF
      IF(.NOT.BREAK)KKBT=KKBT+1
      ETC=EIC
      PROHE=IDP-JDP.EQ.3
      CALL TDEC                !kinematic vbles in decay
      RT=getRan()
      JDO=3-JDP
      IF(PROHE)THEN
        SIGNO=SIGPP+SIGPP+SIGPN
        IF(RT.LT.SIGPN/SIGNO)THEN
          IDII=JDO
          IDK=JDP
        ELSE
          IDII=JDP
          IDK=JDO
        ENDIF
        IDM=JDP
      ELSE
        SIGNO=SIGPP+SIGPN+SIGPN
        IF(RT.LT.SIGPP/SIGNO)THEN
          IDII=JDP
          IDK=JDO
        ELSE
          IDII=JDO
          IDK=JDP
        ENDIF                 !id's done
        IDM=JDO
      ENDIF
C
      ETKM=SQRT(EKMCK+PTKMK)
      CTHT=(ETKMC-ETKM*EMT)/PTKMC
      IF(CTHT.GE.1.)THEN
        CTHT=.99999
      ELSEIF(CTHT.LE.-1.)THEN
        CTHT=-.99999
      ENDIF
      CALL NEWS(PICX,PICY,PICZ,PIC,CTHT,PPKMC,EEKMC
     N  ,PPKMFX,PPKMFY,PPKMFZ,BX,BY,BZ,GM)
      EEKMF=SQRT(EKMCK+PPKMFX*PPKMFX+PPKMFY*PPKMFY+PPKMFZ*PPKMFZ)
      EKMC=SQRT(EKMCK)
      BBX=PPKMFX/EEKMF
      BBY=PPKMFY/EEKMF
      BBZ=PPKMFZ/EEKMF
      GGM=EEKMF/EKMC
      CT=2.*getRan()-1.
      CALL NEWS(0.,0.,PKMC,PKMC,CT,PKMC,.5*EKMC,PKFX,PKFY,PKFZ
     S  ,BBX,BBY,BBZ,GGM)
      EKF=SQRT(EM0K+PKFX*PKFX+PKFY*PKFY+PKFZ*PKFZ)
      CALL GETF(PKFX,PKFY,PKFZ,EKF,*150,IDK)
      PMFX=PPKMFX-PKFX
      PMFY=PPKMFY-PKFY
      PMFZ=PPKMFZ-PKFZ
      EMF=SQRT(EM0K+PMFX*PMFX+PMFY*PMFY+PMFZ*PMFZ)
      CALL GETF(PMFX,PMFY,PMFZ,EMF,*150,IDM)
      PPIJFX=PIJX-PPKMFX
      PPIJFY=PIJY-PPKMFY
      PPIJFZ=PIJZ-PPKMFZ
      EEIJF=SQRT(4.*E12K+PPIJFX*PPIJFX+PPIJFY*PPIJFY+PPIJFZ*PPIJFZ)
      BBX=PPIJFX/EEIJF
      BBY=PPIJFY/EEIJF
      BBZ=PPIJFZ/EEIJF
      GGM=EEIJF/(E12+E12)
      CT=2.*getRan()-1.
      CALL NEWS(0.,0.,P12,P12,CT,P12,E12,PIFX,PIFY,PIFZ
     S  ,BBX,BBY,BBZ,GGM)
      EIF=SQRT(EM0K+PIFX*PIFX+PIFY*PIFY+PIFZ*PIFZ)
      CALL GETF(PIFX,PIFY,PIFZ,EIF,*150,IDII)
      PJFX=PPIJFX-PIFX
      PJFY=PPIJFY-PIFY
      PJFZ=PPIJFZ-PIFZ
      EJF=SQRT(EM0K+PJFX*PJFX+PJFY*PJFY+PJFZ*PJFZ)
      CALL GETF(PJFX,PJFY,PJFZ,EJF,*150,JDP)
      EETBT=EETBT+EIF+EJF+EMF+EKF-EIJ
      IF(.NOT.BREAK)KCBTR=KCBTR+1
      ID(IP)=IDII
      NUMC(IDP)=NUMC(IDP)-1
*     itfo(ip)=itim
      IF(BREAK)THEN
        NCS(JP)=MAX(NCSJ-1,0)
        NCSII=MAX(NCSI-1,0)
        KK4=KK4-1
        KC4R=KC4R-1
        KBRAT=KBRAT+1
      ELSE
        NCS(JP)=NCSJ+1
        NCSII=NCSI+1
      ENDIF
      IPTI(JP)=-JJPT
      IPTI(IP)=-IIPT/3
      NCS(IP)=-NCSI/3             !minus
      AMV(IP)=AM0
C  INTERACTING NUCLEON FROM TRITON/3HE
C
      IIP=IP
      JJP=JP            !for substitution later
C
      MP=IEM(NEM)
      NEM=NEM-1
C  ENTRY FROM STACK
      NCSII=NCSII-ABS(NCS(IP))         !minus
      NCS(MP)=-NCSII/2                 !minus because created
      IIPTI=IIPT+IPTI(IP)
      IPTI(MP)=-IIPTI/2
      PX(MP)=PMFX
      PY(MP)=PMFY
      PZ(MP)=PMFZ
      EE(MP)=EMF
      AMV(MP)=AM0
      ID(MP)=IDM
*     itfo(mp)=itim
C  POSITIONS SAME AS OF TRITON
      XX(MP)=XX(IP)
      YY(MP)=YY(IP)
      ZZ(MP)=ZZ(IP)
C
      IF(PENT)THEN
        NECL=NECL+1
        IECL(NECL)=MP
      ENDIF
C
      KP=IEM(NEM)
      NEM=NEM-1
C  ENTRY
*     ANC(KP)=ANCIP
*     ANC(KP)=-ANC(KP)
      NCS(KP)=-(NCSII-ABS(NCS(MP)))          !minus
      IPTI(KP)=-(IIPTI+IPTI(MP))
*     itfo(kp)=itim
      GOTO 1470
C
 1450 CONTINUE
C  DEUTERON BREAKUP
      IIP=JP
      JJP=IP
      EDC=EIC
C  IIP IS FREE NUCLEON, JJP WILL BE INTERACTING NUCLEON OUT OF THE DEUTERON
      PKMXK=((ECMK-EM2KK)**2-EM4K2K)/(4.*ECMK)
      IF(PKMXK.LE.0.)GOTO 1480                  !no breakup
*      goto 1480
      PKMX=SQRT(PKMXK)                          !needed in called routines
      IF(.NOT.BREAK)THEN
        SIGB=SIGDB(PKMX)                          !this was for on-shell d's
        redf=-1.
C       CALL SIGDBB(SIGB)                         !off-shell d's
        RSS=getRan()*SIGO
        IF(RSS.GT.SIGB)GOTO 1480
      ENDIF
      CALL PSPEC(PKC,KCS)                !... needs pkmx in particular
      EKC=SQRT(PKC*PKC+EM0K)
      EDKC=EDC*EKC
      PDKC=PIC*PKC
      CALL GEDT
      CALL NEWS(PICX,PICY,PICZ,PIC,CTHD,PKC,EKC
     S  ,PKFX,PKFY,PKFZ,BX,BY,BZ,GM)
      EKF=SQRT(EM0K+PKFX*PKFX+PKFY*PKFY+PKFZ*PKFZ)
      IF(.NOT.BREAK)KKB=KKB+1
      PIJFX=PIJX-PKFX
      PIJFY=PIJY-PKFY
      PIJFZ=PIJZ-PKFZ
      EIJF=EIJ-EKF
      BBX=PIJFX/EIJF
      BBY=PIJFY/EIJF
      BBZ=PIJFZ/EIJF
      EECMK=EIJF*EIJF-(PIJFX*PIJFX+PIJFY*PIJFY+PIJFZ*PIJFZ)
      EECM=SQRT(EECMK)
      GGM=EIJF/EECM
      AG=.25*EECMK-EM0K
      IF(AG.GE.0.)THEN
        PIFC=SQRT(AG)
      ELSE
        GOTO 150
      ENDIF
      CT=2.*getRan()-1.
      CALL NEWS(0.,0.,PIFC,PIFC,CT,PIFC,.5*EECM,PIFX,PIFY,PIFZ
     S  ,BBX,BBY,BBZ,GGM)
      EIF=SQRT(EM0K+PIFX*PIFX+PIFY*PIFY+PIFZ*PIFZ)
      CALL GETF(PIFX,PIFY,PIFZ,EIF,*150,JDP)
      PJFX=PIJFX-PIFX
      PJFY=PIJFY-PIFY
      PJFZ=PIJFZ-PIFZ
      EJF=SQRT(EM0K+PJFX*PJFX+PJFY*PJFY+PJFZ*PJFZ)
C  ID'S
      IF(KCS.EQ.1)THEN
        IDJJ=JDP
        IF(JDP.EQ.1)THEN
          IDK=2
        ELSE
          IDK=1
        ENDIF
      ELSE
        IDK=JDP
        IF(JDP.EQ.1)THEN
          IDJJ=2
        ELSE
          IDJJ=1
        ENDIF
      ENDIF
      CALL GETF(PJFX,PJFY,PJFZ,EJF,*150,IDJJ)
C  MOVED SPECTATOR
      CALL GETF(PKFX,PKFY,PKFZ,EKF,*150,IDK)
      ID(IP)=IDJJ
      NUMC(IDP)=NUMC(IDP)-1
*     itfo(ip)=itim
C  THIS WAS THE SPECTATOR
      IF(.NOT.BREAK)KCBR=KCBR+1
      EETB=EETB+EIF+EJF+EKF-EIJ
C  SUBSTITUTING THE NEW VALUES
      IF(BREAK)THEN
        NCS(JP)=MAX(NCSJ-1,0)
        NCSII=MAX(NCSI-1,0)
        KK3=KK3-1
        KC3R=KC3R-1
        KBRA=KBRA+1
      ELSE
        NCS(JP)=NCSJ+1
        NCSII=NCSI+1
      ENDIF
      IPTI(JP)=-JJPT
C  NUCLEON FREE FROM THE BEGINNING RETAINS ITS IDENTITY
      IPTI(IP)=-IIPT/2
      NCS(IP)=-NCSI/2                !minus - created
      AMV(IP)=AM0
C  INTERACTING NUCLEON FROM THE DEUTERON
C
      KP=IEM(NEM)
      NEM=NEM-1
C  ENTRY FROM THE STACK FOR SPECTATOR
      NCS(KP)=-(NCSI-ABS(NCS(IP)))
      IPTI(KP)=-(IIPT+IPTI(IP))
 1470 CONTINUE
      PX(KP)=PKFX
      PY(KP)=PKFY
      PZ(KP)=PKFZ
      EE(KP)=EKF
      AMV(KP)=AM0
      ID(KP)=IDK
*     itfo(kp)=itim
C  POSITIONS SAME AS FOR DEUTERON
      XX(KP)=XX(IP)
      YY(KP)=YY(IP)
      ZZ(KP)=ZZ(IP)
C
      IF(PENT)THEN                          !entry into the other stack
        NECL=NECL+1
        IECL(NECL)=KP
      ENDIF
C
      COLL=.TRUE.
C  CLUMSY
      GOTO 1530
C
 1490 CONTINUE
      CALL DSTME(IDP,EEI,PXI,PYI,PZI,JDP,EEJ,PXJ,PYJ,PZJ
     D  ,UUP,LHIGH,NFIH,IDIH,PFIN,AMSF)
      IF(.NOT.LHIGH)GOTO 1480                ! elastic if failure
      COLL=.TRUE.
      KCHI=KCHI+1
      NPIB=0
      NSTB=0
      IF(IDP.GE.7.AND.IDP.LE.12)THEN
        NSTB=1
      ELSEIF(IDP.GE.13.AND.IDP.LE.15)THEN
        NPIB=1
      ENDIF
      IF(JDP.GE.7.AND.JDP.LE.12)THEN
        NSTB=NSTB+1
      ELSEIF(JDP.GE.13.AND.JDP.LE.15)THEN
        NPIB=NPIB+1
      ENDIF
      NPIA=0
      NSTA=0
      DO IFIH=1,NFIH
        IDI=IDIH(IFIH)
        IF(IDI.GE.7.AND.IDI.LE.12)THEN
          NSTA=NSTA+1
        ELSEIF(IDI.GE.13.AND.IDI.LE.15)THEN
          NPIA=NPIA+1
        ENDIF
        IF(IFIH.EQ.1)THEN
          KP=IP
        ELSEIF(IFIH.EQ.2)THEN
          KP=JP
        ELSE
          KP=IEM(NEM)
          NEM=NEM-1          !entry from the stack
          IF(PENT)THEN
            NECL=NECL+1
            IECL(NECL)=KP
          ENDIF
        ENDIF
        EE(KP)=PFIN(IFIH,0)
        PX(KP)=PFIN(IFIH,1)
        PY(KP)=PFIN(IFIH,2)
        PZ(KP)=PFIN(IFIH,3)
        IF(IFIH.LE.2)THEN
          IPTI(KP)=-ABS(IPTI(KP))
          NCS(KP)=ABS(NCS(KP))+1
          IF(IFIH.EQ.1)THEN
            IF(IDI.NE.IDP)THEN
              NUMC(IDP)=NUMC(IDP)-1
              IF(IDI.GT.2)NCS(KP)=-NCS(KP)
            ENDIF
          ELSE
            IF(IDI.NE.JDP)THEN
              NUMC(JDP)=NUMC(JDP)-1
              IF(IDI.GT.2)NCS(KP)=-NCS(KP)
            ENDIF
          ENDIF
        ELSE
          IPTI(KP)=-INT(1.+2.*getRan())
          XX(KP)=XX(IP)+(XX(JP)-XX(IP))*(IFIH-2.)/(NFIH-1)
          YY(KP)=YY(IP)+(YY(JP)-YY(IP))*(IFIH-2.)/(NFIH-1)
          ZZ(KP)=ZZ(IP)+(ZZ(JP)-ZZ(IP))*(IFIH-2.)/(NFIH-1)
        ENDIF
        ID(KP)=IDI
        AMV(KP)=AMSF(IFIH)
        EEHIT=EEHIT+EE(KP)
      ENDDO
      EEHIT=EEHIT-EIJ
      IF(NPIA.GT.NPIB)THEN
        KKVE=KKVE+NPIA-NPIB
        KCVER=KCVER+NPIA-NPIB
      ELSEIF(NPIB.GT.NPIA)THEN
        KCEV=KCEV+NPIB-NPIA
      ENDIF
      IF(NSTA.GT.NSTB)THEN
        KKV=KKV+NSTA-NSTB
        KCVR=KCVR+NSTA-NSTB
      ELSEIF(NSTB.GT.NSTA)THEN
        KKVB=KKVB+NSTB-NSTA
        KKVBR=KKVBR+NSTB-NSTA
      ENDIF
      GOTO 160
C
 1480 CONTINUE                                !purely elastic
*     goto 150
      KK2=KK2+1
      IF(CALO)CALL LOCPA
C
C  LORENTZ TRANSFORMATION
C  FROM R. HAGEDORN, RELATIVISTIC KINEMATICS
C
C  RATHER THAN FROM ENERGY, TO BE ON THE SAFE SIDE
C  NOW THE SCATTERING ANGLES
*
*      if(idp.le.2.and.jdp.le.2)then
*        if(getRan().gt.redf)goto 150
*      endif
*
      IF(NO2NS)THEN
**        CTHS=2.*getRan()-1.
        BPK=2.*6.0*PICK
        RSEE=getRan()
        IF(BPK.LE.10.)THEN
          CTHS=LOG(RSEE*(EXP(2.*BPK)-1.)+1.)/BPK-1.
        ELSE
          IF(RSEE.GT.0.)THEN
            CTHS=LOG(RSEE)/BPK+1.
            CTHS=MAX(CTHS,-1.)
          ELSE
            CTHS=-1.
          ENDIF
        ENDIF
        IF(LCSM)THEN
C  IN-MEDIUM CS PATCH-UP FOR COMPOSITES
C  NOTE THAT BREAK-UP NOT TOUCHED, SO ELASTIC SUPPRESSED MORE
C  (in practice, the increased suppression of elastic makes no difference)
          if(redf.eq.-1.)then
            sigbu=sigb
          else
            sigbu=0.
          endif
          IF(BPK.LE.10)THEN
            CSDe=(SIGO-sigbu)*BPK*.079577*COSH(BPK*CTHS)/SINH(BPK)
          ELSE
            CSDe=(SIGO-sigbu)*BPK*.079577*EXP(BPK*(CTHS-1.))
          ENDIF
          csdb=.079577*sigbu
          sigof=(csde+csdb)*12.57
          if(rhoxi.gt.0.)then
            sigmax=1./(rhoxi*an0)**.667  *.85  !.85
            if(sigof/sigmax.gt..01)then
              redf=tanh(sigof/sigmax)/(sigof/sigmax)
              redf=((csde+csdb)*redf-csdb)/max(csde,.001)
              if(getRan().gt.redf)goto 150
            endif
          endif
        ENDIF
      ELSE
**      CALL THETA(CTHS,EPCM,IDP,JDP)
**      CALL THETA1(CTHS,EIC-EM0)
        CALL THETAN(CTHS,EPCM,IDP,JDP)
        IF(LCSM)THEN
          csd=csdif(epcm,cths,max(0.,sigo-sigit),idp,jdp)
          sigof=csd*12.57
          sigmax=1./(rhoxi*an0)**.667  *.85  !.85
          if(sigof/sigmax.gt..05)then
            redf=tanh(sigof/sigmax)/(sigof/sigmax)
            if(getRan().gt.redf)goto 150
          endif
        ENDIF
      ENDIF
***      CTHS=2.*getRan()-1.                   !!!!!!!!!!!!!!!!!
*c  for Jorg
*      if(pic.lt..05)goto 150
*      ecm=eic+eic
*      decm=ecm-em0-em0
*      ijorg=nint(decm/dejorg+.5)
*      ijorg=max(ijorg,1)
*      if(ijorg.le.70)anatem(ijorg)=anatem(ijorg)+1.
*c  end of for Jorg
C
C  NEW SCATTERING ANGLES
      CALL NEWS(PICX,PICY,PICZ,PIC,CTHS,PIC,EIC,PIFX,PIFY,PIFZ
     S  ,BX,BY,BZ,GM)
C
C  OTHER PARTICLE
C
      PJFX=PIJX-PIFX
      PJFY=PIJY-PIFY
      PJFZ=PIJZ-PIFZ
C
C  ENERGIES
      EIF=SQRT(EM1K+PIFX*PIFX+PIFY*PIFY+PIFZ*PIFZ)
      EJF=SQRT(EMSK+PJFX*PJFX+PJFY*PJFY+PJFZ*PJFZ)
C
**Jorg!!
*      NAJO(IP)=NAJO(IP)+1
*      NAJO(JP)=NAJO(JP)+1
**Jorg!!
      IF(IDP.LE.6)CALL GETFF(PIFX,PIFY,PIFZ,EIF,*150,IDP)
      IF(JDP.LE.6)CALL GETFF(PJFX,PJFY,PJFZ,EJF,*150,JDP)
      KC2R=KC2R+1
**Jorg!!
*      NAsu(IP)=NAsu(IP)+1
*      NAsu(JP)=NAsu(JP)+1
*      goto 150
**Jorg!!
C
C  SUBSTITUTING NEW VALUES
      qc4=qc4+pic**4*(1.-cths*cths)
      pqck=pqck+pic**2*((pijx*(pifx-pxi)+pijy*(pify-pyi)
     p  +pijz*(pifz-pzi))**2+.5*(1.-cths)*(pijx*(pxi-pxj)
     q  +pijy*(pyi-pyj)+pijz*(pzi-pzj))*(pijx*(pifx-pjfx)
     c  +pijy*(pify-pjfy)+pijz*(pifz-pjfz)))
      if(lmo)then
        CALL LOREN(-BGNOX,-BGNOY,-BGNOZ,GNO,PijX,PijY,PijZ,EIj
     L    ,PijcX,PijcY,PijCZ,EijC)
        pqckl=pqckl+pic**2*((pijcx*(pifx-pxi)+pijcy*(pify-pyi)
     p  +pijcz*(pifz-pzi))**2+.5*(1.-cths)*(pijcx*(pxi-pxj)
     q  +pijcy*(pyi-pyj)+pijcz*(pzi-pzj))*(pijcx*(pifx-pjfx)
     c  +pijcy*(pify-pjfy)+pijcz*(pifz-pjfz)))
      endif
      EET2=EET2+EIF+EJF-EIJ
*      IF(.NOT.NO2NS)THEN
*        IF(IEPA.GE.1.AND.IEPA.LE.NES)NDES(IEPA,1)=NDES(IEPA,1)+1
*      ENDIF
C
      IIP=IP
      JJP=JP
*     if(idp.gt.2.and.jdp.gt.2)write(*,*)'ijsc',idp,jdp,sigo*10.,cths
c
*      npair=npair+1
*      sigpr=sigpr+1./sigo
C
      NCS(JP)=NCS(JP)+1
      NCS(IP)=NCS(IP)+1
*c  for Jorg
*      if(ijorg.le.70)anasuc(ijorg)=anasuc(ijorg)+1.
*c  end of for Jorg
C
 1525 CONTINUE
      IPTI(IIP)=-ABS(IPTI(IIP))           !substituting the values
      IPTI(JJP)=-ABS(IPTI(JJP))
C
 1530 CONTINUE
      COLL=.TRUE.
C
      PX(IIP)=PIFX
      PY(IIP)=PIFY
      PZ(IIP)=PIFZ
      EE(IIP)=EIF
C
      PX(JJP)=PJFX
      PY(JJP)=PJFY
      PZ(JJP)=PJFZ
      EE(JJP)=EJF
C
      GOTO 160
C
 150  CONTINUE
      J=J+JINC
      JJ=JJ+1
      IF(JJ.LE.NJ)GOTO 1200
 152  CONTINUE
 160  CONTINUE
 162  CONTINUE
C
      KQ2R=KQ2R+KC2R
      KQ3R=KQ3R+KC3R
      KQBR=KQBR+KCBR
      KQ4R=KQ4R+KC4R
      KQBTR=KQBTR+KCBTR
      KQVR=KQVR+KCVR
      KQVBR=KQVBR+KCVBR
      KQVER=KQVER+KCVER
      KQEV=KQEV+KCEV
      KQPHR=KQPHR+KCPHR
      kqhi=kqhi+kchi
      KQVVR=KQVVR+KCVVR
      KQVVAR=KQVVAR+KCVVAR
C
*      KINT(IX,IY,IZ)=KC2R+KC2R+MAX(KC3R+KC3R,0)+KCBR*3
*     K  +MAX(KC4R+KC4R,0)+KCBTR*4+KCVR+KCVR+KCVBR+KCVBR
*     I  +KCVER+KCVER+KCEV
C
      IF(.NOT.PENT)GOTO 180
      inpo=.false.
      IF(COLL)THEN
        ENLOSS=ENLOSS+DNCELL
        EDLOSS=EDLOSS+DDCELL
        EBLOSS=EBLOSS+DBCELL
        CALL LOCPA
        ENGAI=ENGAI+DNCELL
        EDGAI=EDGAI+DDCELL
        EBGAI=EBGAI+DBCELL
      ENDIF
 180  CONTINUE
 200  CONTINUE
C
      KK2R=KK2R+KQ2R
      KK3R=KK3R+KQ3R
      KKBR=KKBR+KQBR
      KK4R=KK4R+KQ4R
      KKBTR=KKBTR+KQBTR
      KKVR=KKVR+KQVR
      KKVBR=KKVBR+KQVBR
      KKVER=KKVER+KQVER
      KKEV=KKEV+KQEV
      KKPHR=KKPHR+KQPHR
      kkhi=kkhi+kqhi
      KKVVR=KKVVR+KQVVR
      KKVVAR=KKVVAR+KQVVAR
C
      IF(PRI)THEN
        write(*,*)'qc4 = ',qc4/nqu,' pqck = ',pqck/nqu
     w    ,' pqckl = ',pqckl/nqu
        if(pion)
     i    WRITE(*,'(A12,F10.3,2X,A8,E10.3,2X,A5,F8.4)')
     W    'HI COLL = ',KKhi/FLOAT(NQU)
     R    ,'E TSF = ',EEhit/MAX(1,KKhi+KKhi)
     W    ,'DV = ',KQhi/(DT*NQU)
        WRITE(*,'(A12,F10.3,A1,F12.3,2X,A8,E10.3,2X,A5,F8.4)')
     W    '2-B COLL = ',KK2R/FLOAT(NQU),'/'
     R    ,KK2/FLOAT(NQU),'E TSF = ',EET2/MAX(1,KK2R+KK2R)
     W    ,'DV = ',KQ2R/(DT*NQU)
        IF(DEPRO)THEN
          WRITE(*,'(A12,F10.3,A1,F12.3,2X,A8,E10.3,2X,A5,F8.4)')
     W      '  D CR   = ',KK3R/FLOAT(NQU),'/'
     W      ,KK3/FLOAT(NQU),'E TSF = ',EET3/MAX(1,KK3R*3)
     R      ,'DV = ',KQ3R/(DT*NQU)
          WRITE(*,'(A12,F10.3,A1,F12.3,2X,A8,E10.3,2X,A5,F8.4)')
     W      ' D BREAK = ',KKBR/FLOAT(NQU),'/'
     W      ,KKB/FLOAT(NQU),'E TSF = ',EETB/MAX(1,KKBR*3)
     R      ,'DV = ',KQBR/(DT*NQU)
          WRITE(*,'(A12,F10.3,A1,F12.3,2X,A8,E10.3,2X,A5,F8.4)')
     W      'T/3HE CR = ',KK4R/FLOAT(NQU),'/'
     W      ,KK4/FLOAT(NQU),'E TSF = ',EET4/MAX(1,KK4R*4)
     R      ,'DV = ',KQ4R/(DT*NQU)
          WRITE(*,'(A12,F10.3,A1,F12.3,2X,A8,E10.3,2X,A5,F8.4)')
     W      'T/3HE BR = ',KKBTR/FLOAT(NQU),'/'
     W      ,KKBT/FLOAT(NQU),'E TSF = ',EETBT/MAX(1,KKBTR*4)
     R      ,'DV = ',KQBTR/(DT*NQU)
          WRITE(*,*)'ABNORMAL BREAKS = ',KBRA/FLOAT(NQU)
     W      ,KBRAT/FLOAT(NQU)
        ENDIF
        WRITE(*,'(A12,F10.3,A1,F12.3,2X,A8,E10.3,2X,A5,F8.4)')
     W    'DLT CR   = ',KKVR/FLOAT(NQU),'/'
     W    ,KKV/FLOAT(NQU),'E TSF = ',EETV/MAX(1,KKVR*2)
     R    ,'DV = ',KQVR/(DT*NQU)
        WRITE(*,'(A12,F10.3,A1,F12.3,2X,A8,E10.3,2X,A5,F8.4)')
     W    'DLT ABS  = ',KKVBR/FLOAT(NQU),'/'
     W    ,KKVB/FLOAT(NQU),'E TSF = ',EETVB/MAX(1,KKVBR*2)
     R    ,'DV = ',KQVBR/(DT*NQU)
        WRITE(*,'(A12,F10.3,A1,F12.3,2X,A8,E10.3,2X,A5,F8.4)')
     W    '2DLT CR  = ',KKVVR/FLOAT(NQU),'/'
     W    ,KKVV/FLOAT(NQU),'E TSF = ',EETVV/MAX(1,KKVVR*2)
     R    ,'DV = ',KQVVR/(DT*NQU)
        WRITE(*,'(A12,F10.3,A1,F12.3,2X,A8,E10.3,2X,A5,F8.4)')
     W    '2DLT ABS = ',KKVVAR/FLOAT(NQU),'/'
     W    ,KKVVA/FLOAT(NQU),'E TSF = ',EETVVA/MAX(1,KKVVAR*2)
     R    ,'DV = ',KQVVAR/(DT*NQU)
        IF(PION)THEN
          WRITE(*,'(A12,F10.3,A1,F12.3,2X,A8,E10.3,2X,A5,F8.4)')
     W      'PI PROD  = ',KKVER/FLOAT(NQU),'/'
     W      ,KKVE/FLOAT(NQU),'E TSF = ',EETVE/MAX(1,KKVER*3)
     R      ,'DV = ',KQVER/(DT*NQU)
          WRITE(*,'(A12,F10.3,2X,A5,F8.4)')'PI ABS   = '
     W      ,KKEV/FLOAT(NQU),'DV = ',KQEV/(DT*NQU)
*          if(tim.ge.15.and.
*     k      kks.gt.0)write(*,*)st/(kks*dt)*(nqu*vole)**3
          IF(PHOTR.NE.0.)THEN
            AA=PHOTR*NQU
            WRITE(*,'(A12,F10.5,A1,F12.5)')
     I        'PHO PRO  = ',KKPHR/AA,'/',KKPH/AA
          ENDIF
        ENDIF
        REFIN=KQ2R*2
        IF(DEPRO)THEN
          REFIN=REFIN+KQ3R*2+KQBR*3
          REFIN=REFIN+KQ4R*2+KQBTR*4
        ENDIF
        REFIN=REFIN+KQVR*2+KQVBR*2
        IF(PION)THEN
          REFIN=REFIN+KQVER*2
          REFIN=REFIN+KQEV
        ENDIF
        REFIN=REFIN/(DT*NQU)
      ENDIF
*      if(pri)then
*        if(sigpr.ne.0.)then
*        if(npair.ne.0)then
*          write(*,*)'sigpr = ',npair/sigpr*10.,' mb'
*          epair=epair/npair
*          eone=eone/(npair+npair)
*          write(*,*)'eone = ',eone,' epair = ',epair
*        endif
*      endif
C
      IF(MMIX.EQ.0)THEN
        ENREM=ENGAI
        EDREM=EDGAI
        EBREM=EBGAI
      ELSEIF(MMIX.EQ.1)THEN
        ENGAI=ENGAI+ENLOSS-ENREM
        EDGAI=EDGAI+EDLOSS-EDREM
        EBGAI=EBGAI+EBLOSS-EBREM
      ENDIF
C
      ENGAIN=ENGAI-ENLOSS
      EDGAIN=EDGAI-EDLOSS
      EBGAIN=EBGAI-EBLOSS
C
      END
