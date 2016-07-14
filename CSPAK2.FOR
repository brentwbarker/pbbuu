C  ON TOP OF THE *ELASTIC* NN PACKAGE WE PIGGYBACK THE
C  INELASTIC BB <--> BB
c  Header for testing
c      PARAMETER(NTR=10000)
c      PARAMETER(NC=8)
c      DIMENSION DSC(-NC:NC)
c      PARAMETER(DC=2./(NC+NC+1))
cC
c      PARAMETER (AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
c      PARAMETER (AMEC=.13957,AMEN=.13496,AME=(AMEC+AMEC+AMEN)/3.)
cC
c      INCLUDE 'PARTID.'
c      COMMON/CSEED/ISEED
cC
cC
c      ISEED=12323123
cC
c 10   CONTINUE
c      WRITE(*,*)'Enter IDP,JDP: '
c      READ(*,*)IDP,JDP
c      WRITE(*,*)'Enter IDPN,JDPN: '
c      READ(*,*)IDPN,JDPN
c      WRITE(*,*)'Enter lab en: '
c      READ(*,*)TL
cC
c      S=(AMS(IDP)+AMS(JDP))**2+2.*TL*AMS(JDP)
c      SS=SQRT(S)
c      DMI=SS-AM0-AM0
cC
c      DO IC=-NC,NC
c        DSC(IC)=0.
c      ENDDO
cC
c      DO IT=1,NTR
c        IF(IDPN.GE.7.AND.JDPN.GE.7)THEN
c          CALL DDMS(AMVIN,AMVJN,DMI-AME-AME)
c        ELSE
c          IF(IDPN.GE.11)THEN
c            CALL XMS(AMVIN,DMI-AME)
c          ELSEIF(IDPN.GE.7)THEN
c            CALL DMS(AMVIN,DMI-AME)
c          ELSE
c            AMVIN=AM0
c          ENDIF
c          IF(JDPN.GE.11)THEN
c            CALL XMS(AMVJN,DMI-AME)
c          ELSEIF(JDPN.GE.7)THEN
c            CALL DMS(AMVJN,DMI-AME)
c          ELSE
c            AMVJN=AM0
c          ENDIF
c        ENDIF
c        CALL THBARN(CTH,DMI,IDP,JDP,AMS(IDP),AMS(JDP)
c     C    ,IDPN,JDPN,AMVIN,AMVJN)
c*       call theta2(cth,dmi-ame)
c        IC=NINT(CTH/DC)
c        DSC(IC)=DSC(IC)+1.
c      ENDDO
cC
c      DO IC=-NC,NC
c        CC=IC*DC
c        WRITE(*,*)CC,DSC(IC)
c      ENDDO
cC
c      GOTO 10
cC
c      END
c
c
c      subroutine theta2(cth,decm)
c      parameter (amp=.9383,amn=.9396,am0=.5*(amp+amn))
c      parameter (amec=.13957,amen=.13496,ame=(amec+amec+amen)/3.)
c      parameter (amvm=am0+ame)
c      parameter (ecmm=am0+amvm)
c      common/cseed/iseed
cc
c      ecm=decm+ecmm
cc
c      if(ecm.lt.2.14)then
c        b1=.5
c        b3=0.
c      elseif(ecm.gt.2.4)then
c        b1=.06
c        b3=.4
c      else
c        b1=29.03+ecm*(-23.75+ecm*4.865)
c        b3=-30.33+ecm*(25.53-ecm*5.301)
c      endif
cc
c      rs=getRan()
c      if(b3.ne.0.)then
c        p=b1/(3.*b3)
c        q=(b1+b3)*(rs-.5)/b3
c        sp=sqrt(p)
c        phi=asinh(q/(p*sp))
c        cth=-2.*sp*sinh(phi/3.)
c      else
c        cth=1.-2.*rs
c      endif
cc
c      end
c
c
c      function asinh(x)
cc
c      if(x.ge.0.)then
c        asinh=log(sqrt(x*x+1.)+x)
c      else
c        asinh=-log(sqrt(x*x+1.)-x)
c      endif
cc
c      end
c
c
c      PARAMETER (AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
c      PARAMETER(AM0K=AM0*AM0)
c      PARAMETER (AM02=AM0+AM0)
c      PARAMETER (AMEC=.13957,AMEN=.13496,AME=(AMEC+AMEC+AMEN)/3.)
cC
c      INCLUDE 'PARTID.'
c      COMMON/CSEED/ISEED
cC
c      ISEED=12345671
cC
c      DO I=1,50
c        TL=I*.1
c        S=4.*AM0K+AM02*TL
c        SS=SQRT(S)
c        DECM=SS-AM02
c        CALL CIBAR(1,AM0,1,AM0,DECM,SNN,SND,SNT,SDD,SIL)
c        WRITE(*,*)' '
c        WRITE(*,'(1X,A,F6.3,4F6.2)')'PP TL IL/ND/NT/DD',TL,SIL*10.
c     W    ,SND*10.,SNT*10.,SDD*10.
c        CALL CIBAR(1,AM0,2,AM0,DECM,SNN,SND,SNT,SDD,SIL)
c        WRITE(*,'(1X,A,F6.3,4F6.2)')'PN TL IL/ND/NT/DD',TL,SIL*10.
c     W    ,SND*10.,SNT*10.,SDD*10.
c        CALL SIGI(SIG11,SIG10,SIGX,.5*SS-AM0)       !inelastic cross section
c        SIGE=SIG11+.5*SIG10
c        SIGIT=SIGE+SIGX  !+SIGE
c        WRITE(*,*)TL,SIL*10.,SIGIT*10.
c      ENDDO
cC
c      DO I=1,4
c        AM2=AM0+AME+.1+.2*(I-1)
c        AM2K=AM2*AM2
c        DO J=1,4
c          SS=AM0+AM2+.12*J
c          S=SS*SS
c          E2=(S-AM0K-AM2K)/(2.*AM0)
c          T2=E2-AM2
c          DECM=SS-(AM0+AM0+AME)
c          PNDK=((S-AM0K-AM2K)**2-4.*AM0K*AM2K)/(4.*S)
c          PND=SQRT(PNDK)
c          EPCM=SS*PND
cC N*  N
c          JDP=2
c          IDP=12
c          CALL SIGEP(SIGE,SGET,DECM,JDP,IDP)
c          SIGIE=SGET/EPCM
c          CALL CIBAR(2,AM0,12,AM2,DECM,SNN,SND,SNT,SDD,SIL)
c          WRITE(*,*)T2,AM2,SIL*10.,SIGIE*10.
c        ENDDO
c      ENDDO
cC
c      DO J=1,4
c        AM1=AM0+AME+(J-.5)*.1
c        DO I=1,4
c          AM2=AM1+(I-.5)*.0001
c          SS=AM1+AM2+.25
c*         CALL BARBAR(-1.5,1,AM1,1.5,1,AM2,SS,SNN,SND,SDD,SIL)
c          DECM=SS-(AM0+AM0+AME+AME)
c          CALL CIBAR(7,AM1,10,AM2,DECM,SNN,SND,SNT,SDD,SIL)
c          WRITE(*,*)SS,AM1,AM2,SIL*10.
c        ENDDO
c      ENDDO
cC
c      DO I=1,8
c        IDP=I
c        IF(I.GT.2)IDP=IDP+4
c        DO J=1,8
c          JDP=J
c          IF(J.GT.2)JDP=JDP+4
c          DO KCS=0,3
c            DECM=AM0+AME+4.*AME*getRan()
c            CALL IBAR(IDP,JDP,IDPN,JDPN,KCS,DECM)
c            IF(TIZO(IDP)+TIZO(JDP).NE.TIZO(IDPN)+TIZO(JDPN))THEN
c              WRITE(*,*)'I/J/IN/JN/KC',IDP,JDP,IDPN,JDPN,KCS
c            ENDIF
c          ENDDO
c        ENDDO
c      ENDDO
cC
c      WRITE(*,*)' '
c      DO K=1,15
c        SS=3.0
c        DMI=SS-(AM0+AM0+AME)
c        CALL DMS(AMV,DMI)
c        CALL XMS(AMX,DMI)
c        DMI=DMI-AME
c        CALL DDMS(AMV1,AMV2,DMI)
c        DMI=SS-AM02
c        CALL THBAR(CTH,DMI,AM0,AM0,AMV1,AMV2)  ????
c        WRITE(*,*)AMV,AMX,AMV1,AMV2,CTH
c      ENDDO
cC
c      END


      SUBROUTINE THBARN(CTH,DMI,IDP,JDP,AMI1,AMI2
     C  ,IDPN,JDPN,AMI1N,AMI2N)
      use global
C
C  THE PROCEDURE RETURNS COSINE CTH OF THE POLAR CM SCATTERING ANGLE,
C    FOR THE USE IN PRIMARLY INELASTIC BINARY BARYON-PROCESSES,
C    GIVEN NET CM ENERGY DMI ABOVE 2 NUCLEON MASSES AND
C    INITIAL-STATE ID'S IDP/JDP AND MASSES AMI1/AMI2
C    AND FINAL-STATE IDPN/JDPN,AMI1N/AMI2N
C
      INCLUDE 'PARTID'
      COMMON/CSEED/ISEED
      DOUBLE PRECISION BBT,BB,EBB,S,RS
      DOUBLE PRECISION AMI1K,AMI2K,AMI1NK,AMI2NK,PKI,PKIN
C
      PARAMETER(BBT=6D0)
C
      PARAMETER (AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER (AM02=AM0+AM0)
C
C
      SS=AM02+DMI
      S=SS*SS
C
      IF(AMI1N+AMI2N.GT.AMI1+AMI2)THEN
        IDM=MIN(IDP,JDP)
        JDM=MAX(IDP,JDP)
        IDX=MIN(IDPN,JDPN)
        JDX=MAX(IDPN,JDPN)
      ELSE
        IDX=MIN(IDP,JDP)
        JDX=MAX(IDP,JDP)
        IDM=MIN(IDPN,JDPN)
        JDM=MAX(IDPN,JDPN)
      ENDIF
C
      AMI1K=AMI1*AMI1
      AMI2K=AMI2*AMI2
      PKI=((S-AMI1K-AMI2K)**2-4D0*AMI1K*AMI2K)/(4D0*S)
      if(pki.lt.0.)then
        write(*,*)'pki',pki
        write(*,*)'idp/jdp',idp,jdp
        write(*,*)'ss,ami1,ami2',ss,ami1,ami2
        stop
      endif
      AMI1NK=AMI1N*AMI1N
      AMI2NK=AMI2N*AMI2N
      PKIN=((S-AMI1NK-AMI2NK)**2-4D0*AMI1NK*AMI2NK)/(4D0*S)
      if(pkin.lt.0.)then
        write(*,*)'pkin',pkin
        write(*,*)'idp/jdp',idp,jdp
        write(*,*)'idpn/jdpn',idpn,jdpn
        write(*,*)'ss,ami1n,ami2n',ss,ami1n,ami2n
        stop
      endif
      BB=BBT*2D0*SQRT(PKI*PKIN)
*     write(*,*)'bb = ',bb
      EBB=EXP(BB)
      RS=getRan()
      CTH=LOG(EBB*(1D0-RS)+RS/EBB)/BB
      IF(ABS(CTH).GT.1.)CTH=1.   ! least damage
C
C  FIGURING OUT WHICH ONE IS FORWARD
      IF(IDP.EQ.JDP)GOTO 100 ! symmetrization
      IF(IDPN.EQ.JDPN)GOTO 100 ! symmetrization
      IF(JDM.LE.2.AND.IDX.LE.2.AND.JDX.GE.7)GOTO 100  !1 delta or N* production
      IF(IDP.EQ.IDPN)RETURN
      IF(JDP.EQ.JDPN)RETURN
      IF(IDP.EQ.JDPN.OR.JDP.EQ.IDPN)THEN
        CTH=-CTH
        RETURN
      ENDIF
      IF(TIZO(IDP)*TIZO(IDPN).LT.0.)CTH=-CTH
      RETURN
C
 100  CONTINUE
      IF(getRan().GT..5)CTH=-CTH
C
      END


      SUBROUTINE IBAR(IDP,JDP,IDPN,JDPN,KCS,DECM)
      use global
C
C  THE PROCEDURE FINDS NEW IDS IN INELASTIC BARYON-BARYON
C    INTERACTIONS
C  INPUT: IDP, JDP, KCS, DECM
C    DECM - CM ENERGY MINUS INITIAL-STATE THRESHOLD
C    KCS - FINAL-STATE: KCS=0 NN, 1 ND, 2 NX, 3 DD
C  OUPUT: IDPN, JDPN
C
      INCLUDE 'PARTID'
      COMMON/CSEED/ISEED
C
      PARAMETER (AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER (AM02=AM0+AM0)
      PARAMETER (AMEC=.13957,AMEN=.13496,AME=(AMEC+AMEC+AMEN)/3.)
      PARAMETER(SSMI=AM0+AM0+AME)
C
C
      IDPN=IDP
      JDPN=JDP
C
      IF(IDP.GE.3.AND.IDP.LE.6)RETURN  ! no clusters
      IF(JDP.GE.3.AND.JDP.LE.6)RETURN  ! no clusters
C
      MINI=MIN(IDP,JDP)
      MAXI=MAX(IDP,JDP)
      IF(MINI.GE.7.AND.MAXI.GE.11)RETURN ! no DN* or N*N*
C
      TIZOI=TIZO(IDP)
      TIZOJ=TIZO(JDP)
      TIZOS=TIZOI+TIZOJ
      IF(ABS(TIZOS).GT.1.1)RETURN  !cs only in I=0 and I=1
C
      SS=DECM+AM02
      IF(IDP.GE.7)SS=SS+AME
      IF(JDP.GE.7)SS=SS+AME
C
      IF(SS.LE.SSMI)RETURN              !below threshold
C
      IF(KCS.EQ.3)THEN        ! -->DD
        IF(MAXI.GT.2)RETURN   ! only from NN
        IF(SS.LE.SSMI+AME)RETURN        !below threshold
        IF(IDP.EQ.JDP)THEN    ! from pp or nn
          IF(getRan().LT..4)THEN
            IDPN=IDP+7        ! same iso
            JDPN=JDP+7
          ELSE
            IDPN=IDP+6        ! changed
            JDPN=JDP+8
          ENDIF
        ELSE                  ! from pn
          PND0=PND0D(SS)
          PNDM=PNDMD(SS)
          IF((PNDM+PND0)*getRan().LT.PNDM)THEN
            IDPN=4+3*IDP      ! D-D++
            JDPN=4+3*JDP
          ELSE
            IDPN=7+IDP        ! D0D+
            JDPN=7+JDP
          ENDIF
        ENDIF
      ELSEIF(KCS.EQ.2)THEN    ! -->NX
        IF(MAXI.GT.2)RETURN   ! only from NN
        IF(getRan().GT..5)THEN
          IDPN=10+IDP
        ELSE
          JDPN=10+JDP
        ENDIF
      ELSEIF(KCS.EQ.1)THEN    ! -->ND
        IF(MAXI.GT.2)RETURN   ! inelastic only from NN
        RS=getRan()
        IF(IDP.EQ.JDP)THEN    ! from pp or nn
          IF(RS.LT..125)THEN
            IDPN=7+IDP        ! pD+ or nD0
          ELSEIF(RS.LT..25)THEN
            JDPN=7+JDP
          ELSEIF(RS.LT..625)THEN
            IDPN=3-IDP        ! nD++ or pD-
            JDPN=4+3*JDP
          ELSE
            JDPN=4+3*JDP
            IDPN=3-IDP
          ENDIF
        ELSE                  ! from np
          IF(RS.LT..5)THEN
            IDPN=7+IDP
          ELSE
            JDPN=7+JDP
          ENDIF
        ENDIF
      ELSE                    ! --> NN from DD, NX, or ND
        IF(ABS(TIZOS).LT..1)THEN     ! --> np
          IF(TIZOI.LT.0.)THEN
            IDPN=1
            JDPN=2
          ELSE
            IDPN=2
            JDPN=1
          ENDIF
        ELSE
          IF(TIZOS.GT.0.)THEN
            IDPN=1
            JDPN=1
          ELSE
            IDPN=2
            JDPN=2
          ENDIF
        ENDIF
      ENDIF
C
      END


      SUBROUTINE CIBAR(IDP,AM1,JDP,AM2,DECM,SNN,SND,SNT,SDD,SIL)
C  CALCULATES INELASTIC BARYON-BARYON CROSS SECTION IN FM**2
C  IDP,JDP - PTCLE IDS; AM1,AM2 - VACUUM MASSES
C  DECM - CM ENERGY MINUS INITIAL-STATE THRESHOLD
C  SIL - NET INELASTIC CROSS SECTION
C  SNN - NUCLEON-NUCLEON CHANNEL, SND - NUCLEON-DELTA
C  SNT - NUCLEON-N*, SDD - TWO DELTAS
C
      INCLUDE 'PARTID'
C
      PARAMETER (AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER (AM02=AM0+AM0)
      PARAMETER (AMEC=.13957,AMEN=.13496,AME=(AMEC+AMEC+AMEN)/3.)
      PARAMETER(SSMI=AM0+AM0+AME)
C
C
      SNN=0.
      SND=0.
      SNT=0.
      SDD=0.
      SIL=0.
C
      IF(IDP.GE.3.AND.IDP.LE.6)RETURN  ! no clusters
      IF(JDP.GE.3.AND.JDP.LE.6)RETURN  ! no clusters
C
      MINI=MIN(IDP,JDP)
      MAXI=MAX(IDP,JDP)
      IF(MINI.GE.7.AND.MAXI.GE.11)RETURN ! no DN* or N*N*
C
      TIZOI=TIZO(IDP)
      TIZOJ=TIZO(JDP)
      TIZOS=TIZOI+TIZOJ
      IF(ABS(TIZOS).GT.1.1)RETURN  !cs only in I=0 and I=1
C
      SS=DECM+AM02
      IF(IDP.GE.7)SS=SS+AME
      IF(JDP.GE.7)SS=SS+AME
C
      IF(SS.LE.SSMI)RETURN              !below threshold
C
      IF(MAXI.LE.2)THEN   ! NN
        SND=4./3.*PPND(SS)+PPDUP(SS)  !pp->ND=4/3*pp->nD++; deut-pi put on top
        IF(IDP.EQ.JDP)THEN
          SNT=PPPPS(SS)               ! -->NN*
        ELSE
          SND=.5*SND   ! only 1/2 of I=1
          SNT=PNPNS(SS)*2.            !--> NP* AND N*P
        ENDIF
        IF(IDP.EQ.JDP)THEN
          SDD=5./3.*PPD0D(SS)       ! pp-->DD 5/3 larger than pp-->D0D++
        ELSE
          SDD=PND0D(SS)+PNDMD(SS)
        ENDIF
        CALL RENN(RENX,RENDD,IDP,JDP,SS)
        SDD=SDD*RENDD          !remainder put into DD and NN*
        SNT=SNT*RENX
        SIL=SND+SNT+SDD
        RETURN   ! exit NN case here
      ELSEIF(MINI.LE.2.AND.MAXI.LE.10)THEN   ! ND
        RDPP=RDNPP(SS,AM1,AM2)
        SNN=RDPP*PPND(SS)              ! nD++ and pD-  into pp or nn
        ASI=ABS(TIZOI-TIZOJ)
        IF(ASI.LT..1)THEN
          SNN=SNN/3.                   ! pD+ and nD0   into pp or nn
        ELSEIF(ABS(ASI-1.).LT..1)THEN
          SNN=(SNN+SNN)/3.             ! pD0 and nD+   into np
        ENDIF
      ELSEIF(MINI.LE.2)THEN                  ! NN*
        RXPP=RXPPP(SS,AM1,AM2)
        ASI=ABS(TIZOI-TIZOJ)
        IF(ASI.LT..1)THEN
          CALL RENN(RENX,RENDD,1,1,SS)
          SNN=RXPP*PPPPS(SS)*RENX    !  --> pp or nn
        ELSE
          CALL RENN(RENX,RENDD,1,2,SS)
          SNN=RXPP*PNPNS(SS)*2.*RENX !  --> pn or np
        ENDIF
      ELSE                                   ! DD
        RDPP=RDDPP(SS,AM1,AM2)
        ASI=ABS(TIZOI-TIZOJ)
        IF(ASI.LT..1)THEN
          CALL RENN(RENX,RENDD,1,1,SS)
          SNN=RDPP*PPD0D(SS)/3.*RENDD          ! D+D+ to pp and D0D0 to nn
        ELSEIF(ABS(TIZOS-1.).LT..1)THEN
          CALL RENN(RENX,RENDD,1,1,SS)
          SNN=RDPP*.5*PPD0D(SS)*RENDD          ! D0D++ to pp and D-D+ to nn
        ELSEIF(ASI.GT.1.1)THEN
          CALL RENN(RENX,RENDD,1,2,SS)
          SNN=RDPP*PNDMD(SS)*RENDD             ! D-D++ to np
        ELSE
          CALL RENN(RENX,RENDD,1,2,SS)
          SNN=RDPP*PND0D(SS)*RENDD             ! D0D+ to np
        ENDIF
      ENDIF
      SIL=SNN
C
      END


      SUBROUTINE RENN(RENX,RENDD,IDP,JDP,X)
C  RENORMALIZATION OF THE CROSS SECTION FOR NN--> NX AND DD
C    TO SATURATE NET NN INELASTIC AND GIVE GOOD PI MULT
C    X -- SQRT(S), RENX -- FACTOR FOR NX, RENDD -- FOR DD
C
      PARAMETER(AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER(AM0K=AM0*AM0)
      PARAMETER(AMEC=.13957,AMEN=.13496,AME=(AMEC+AMEC+AMEN)/3.)
C
      PARAMETER(SSMI=AM0+AM0+AME,SSMIP=SSMI+AME)
      PARAMETER(SSMX=4.5)
C
      PARAMETER(NMI=40)
      DIMENSION RNX(0:NMI,2),RND(0:NMI,2)
      PARAMETER(NMI2=(NMI+1)*2)
      DATA RNX/NMI2*1./RND/NMI2*1./
C
      PARAMETER (TCRI=.85)
      PARAMETER (NTLI=6)
      DIMENSION TLI(NTLI),CSPP(NTLI),CSPN(NTLI)
      DATA TLI/ .75,1.  ,1.25,1.5,2.11,2.6 /  ! add more on an occasion
      DATA CSPP/21.,23.5,24.5,24.,28. ,30.3/
      DATA CSPN/11.,12. ,13. ,15.,28. ,30.3/
! note: if you add, you may need to recreate data files w/NN cross sections
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      IF(FIRST)THEN
        DSS=(SSMX-SSMI)/NMI
        DO I=1,NMI
          SS=SSMI+DSS*I
          PK=.25*SS*SS-AM0K
          TLA=(PK+PK)/AM0
          IF(TLA.GT.TCRI)THEN
            DO IT=1,NTLI
              IF(TLA.LE.TLI(IT))THEN
                ITT=IT
                DTL=TLI(IT)-TLA
                GOTO 20
              ENDIF
            ENDDO
            ITT=NTLI
            DTL=0.
 20         CONTINUE
            ITT1=ITT-1
            DTL=DTL/(TLI(ITT)-TLI(ITT1))
            DTL1=1.-DTL
            DO K=1,2
C
              IF(K.EQ.1)THEN
                SILE=(DTL*CSPP(ITT1)+DTL1*CSPP(ITT))*.1  ! data in mbs
              ELSE
                SILE=(DTL*CSPN(ITT1)+DTL1*CSPN(ITT))*.1
              ENDIF
C
              IF(SS.LT.SSMIP)THEN
                ANE=1.
              ELSE
                ANE=1.+(SS-SSMIP)*.71   ! pi mult in inelastic
**              DSMIP=SS-SSMIP
**              ANE=1.+.90*DSMIP*SQRT(DSMIP)/(1.+.35*DSMIP)
              ENDIF
C
              SND=4./3.*PPND(SS)+PPDUP(SS)
C                                    !pp->ND=4/3*pp->nD++; deut-pi put on top
              IF(K.EQ.1)THEN
                SNT=PPPPS(SS)              ! -->NN*
              ELSE
                SND=.5*SND   ! only 1/2 of I=1
                SNT=PNPNS(SS)*2.            !--> NP* AND N*P
              ENDIF
C
              IF(K.EQ.1)THEN
                SDD=5./3.*PPD0D(SS)       ! pp-->DD 5/3 larger than pp-->D0D++
              ELSE
                SDD=PND0D(SS)+PNDMD(SS)
              ENDIF
C
              SIL=SND+SNT+SDD
              IF(SILE.GT.SIL)THEN
                RNXI=((2.-ANE)*SILE-SND)/SNT
                IF(RNXI.GT.0.)THEN
                  RNX(I,K)=RNXI
                  RND(I,K)=(ANE-1.)*SILE/SDD
                ELSE
                  RNX(I,K)=0.
                  RND(I,K)=(SILE-SND)/SDD
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ENDDO
        FIRST=.FALSE.
      ENDIF
C
      IF(IDP.EQ.JDP)THEN
        K=1
      ELSE
        K=2
      ENDIF
C
      XS=X-SSMI
      RMS=XS/DSS
      IX=RMS
      IF(IX.LT.0)THEN
        RENX=1.
        RENDD=1.
        RETURN
      ENDIF
      IF(IX.LT.NMI)THEN
        IX1=IX+1
        RI=RMS-IX
        RI1=1.-RI
        RENX=RI1*RNX(IX,K)+RI*RNX(IX1,K)
        RENDD=RI1*RND(IX,K)+RI*RND(IX1,K)
      ELSE
        RENX=RNX(NMI,K)
        RENDD=RND(NMI,K)
      ENDIF
C
      END


      FUNCTION RDDPP(SS,AM1,AM2)
C
C  THE FUNCTION CALCULATES D+D+-->PP TO INVERSE CROSS SECTION RATIO
C    C.F. DANIELEWICZ/BERTSCH  NPA533(91)712
C    SS -- SQRT(S), AM1 AND AM2 -- MASSES OF THE TWO DELTAS
C
      PARAMETER(NMI=100,AAMX=4.)
      DIMENSION DNI(NMI)
C
      COMMON/CGDN/SG,AM1K         ! s for the subintegral function
      EXTERNAL GGDDV
C
      PARAMETER(AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER(AM0K=AM0*AM0)
      PARAMETER(AMEC=.13957,AMEN=.13496,AME=(AMEC+AMEC+AMEN)/3.)
C
      PARAMETER(AAMI=AM0+AME)
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      IF(FIRST)THEN
        PI=4.*ATAN(1.)
        DMI=(AAMX-AAMI)/NMI
        DO I=1,NMI
          AMX=AAMI+I*DMI
          SG=(AAMI+AMX)**2   !needed by GGDDV and GDDV; note: GGDDV changes AM1K
          CALL IGRAL(AAMI,AMX,.1,GGDDV,A)
          DNI(I)=2.*A/(PI+PI)**2
        ENDDO
        FIRST=.FALSE.
      ENDIF
C
      AMX=SS-AAMI
      RMX=(AMX-AAMI)/DMI
      IX=RMX
      IF(IX.LE.0)THEN
        RDDPP=0.
        RETURN
      ENDIF
      IF(IX.LT.NMI)THEN
        IX1=IX+1
        RI=RMX-IX
        RI1=1.-RI
        DN=RI1*DNI(IX)+RI*DNI(IX1)
      ELSE
        DN=DNI(NMI)
      ENDIF
C
      AM1K=AM1*AM1
      AM2K=AM2*AM2
      S=SS*SS
      PDDK=((S-AM1K-AM2K)**2-4.*AM1K*AM2K)/(4.*S)
      PDD=SQRT(MAX(PDDK,1E-5))
      PNNK=((S-AM0K-AM0K)**2-4.*AM0K*AM0K)/(4.*S)
C
      RDDPP=AM1*AM2*4./16.*PNNK/PDD/DN
      rddpp=rddpp*fdfug(am1)*fdfug(am2)
C
      END


      FUNCTION GGDDV(AM1)
C  SUBINTEGRAL FUNCTION IN THE 1'ST INTEGRAL FOR DD-->NN
      EXTERNAL GDDV
      COMMON/CGDN/SG,AM1K         ! s for the subintegral function
C
      PARAMETER(AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER(AM0K=AM0*AM0)
      PARAMETER (AMEC=.13957,AMEN=.13496,AME=(AMEC+AMEC+AMEN)/3.)
      PARAMETER (AMEK=AME*AME)
      PARAMETER (AM0EK=AM0K+AMEK,AM0E4=4.*AM0K*AMEK)
      PARAMETER (AMV=1.232)
      PARAMETER (AMVK=AMV*AMV)
C
      PARAMETER (QRK=((AMVK-AM0EK)**2-AM0E4)/(4.*AMVK))
C
      PARAMETER (BET=.3)
      PARAMETER (BETK=BET*BET)
      PARAMETER (FORM=BETK+QRK)
C
      PARAMETER (FDK=.37)
      PARAMETER (FG=2./3.*FDK*AM0/AMEK)
C
      PARAMETER(AAMI=AM0+AME)
C
C
      AM1K=AM1*AM1
      AMX=SQRT(SG)-AM1
      IF(AMX.GT.AAMI)THEN
        CALL IGRAL1(AAMI,AMX,.5*GMV,GDDV,A)
      ELSE
        GOTO 200
      ENDIF
C
      QK=((AM1K-AM0EK)**2-AM0E4)/(4.*AM1K)
      IF(QK.LT.0.)GOTO 200
C
      GMV=FG*QK*SQRT(QK)/AM1
      GMV=GMV*(FORM/(BETK+QK))**2   !not v. good, but lts stick w/it f/moment
*     gmv=.12   !!!!!
C
      GGDDV=A*AM1*GMV/((AM1-AMV)**2+.25*GMV*GMV)
      ggddv=ggddv*fdfug(am1)
      RETURN
C
 200  CONTINUE
      GGDDV=0.
C
      END


      FUNCTION GDDV(AM)
C  SUBINTEGRAL FUNCTION FOR DD CROSS SECTIONS
      COMMON/CGDN/SG,AM1K         ! s for the subintegral function
      PARAMETER(AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER(AM0K=AM0*AM0)
      PARAMETER (AMEC=.13957,AMEN=.13496,AME=(AMEC+AMEC+AMEN)/3.)
      PARAMETER (AMEK=AME*AME)
      PARAMETER (AM0EK=AM0K+AMEK,AM0E4=4.*AM0K*AMEK)
      PARAMETER (AMV=1.232)
      PARAMETER (AMVK=AMV*AMV)
C
      PARAMETER (QRK=((AMVK-AM0EK)**2-AM0E4)/(4.*AMVK))
C
      PARAMETER (BET=.3)
      PARAMETER (BETK=BET*BET)
      PARAMETER (FORM=BETK+QRK)
C
      PARAMETER (FDK=.37)
      PARAMETER (FG=2./3.*FDK*AM0/AMEK)
C
C
      AMK=AM*AM
      QK=((AMK-AM0EK)**2-AM0E4)/(4.*AMK)
      IF(QK.LT.0.)GOTO 200
C
      GMV=FG*QK*SQRT(QK)/AM
      GMV=GMV*(FORM/(BETK+QK))**2   !not v. good, but lts stick w/it f/moment
*     gmv=.12   !!!!!
C
      PK=((SG-AM1K-AMK)**2-4.*AM1K*AMK)/(4.*SG)
      IF(PK.LT.0.)GOTO 200
C
      GDDV=AM*SQRT(PK)*GMV/((AM-AMV)**2+.25*GMV*GMV)
      gddv=gddv*fdfug(am)
      RETURN
C
 200  CONTINUE
      GDDV=0.
C
      END


      SUBROUTINE DDMS(AMV1,AMV2,DMI)
      use global
C
C  RETURNS DELTA MASSES AMV1/AMV2 GIVEN CM ENERGY DMI ABOVE THRESHOLD
C    IN A REACTION NN-->DD
      PARAMETER(AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER(AMEC=.13957,AMEN=.13496,AME=(AMEC+AMEC+AMEN)/3.)
      PARAMETER(AMV0=1.232)
      PARAMETER(GMV=.120)
C
      PARAMETER(AAMI=AM0+AME)
      PARAMETER(SMI=AAMI+AAMI)
C
      PARAMETER(NIT=150)
C
      COMMON/CSEED/ISEED
C
C
      SS=SMI+DMI
      AMX=AAMI+DMI
      DM=MIN(1.1*GMV,.6*DMI)
      AMV1=AMV0-.5*GMV    ! choosing a start for Metropolis
      IF(AMV1.GE.AMX)THEN
        AMV1=AAMI+DMI*getRan()
      ELSEIF(getRan().GT..5)THEN
        AMM=AMV1+GMV
        IF(AMM.LT.AMX)AMV1=AMM
      ENDIF
C
      AMX2=SS-AMV1
      AMV2=AMV0-.5*GMV
      IF(AMV2.GE.AMX2)THEN
        AMV2=AAMI+(AMX2-AAMI)*getRan()
      ELSEIF(getRan().GT..5)THEN
        AMM=AMV2+GMV
        IF(AMM.LT.AMX2)AMV2=AMM
      ENDIF
C
      FV=GDDALL(SS,AMV1,AMV2)
      DO 30 IT=1,NIT
        AM1=AMV1+DM*(getRan()-.5)
        IF(AM1.LT.AAMI)THEN
          AM1=AM1+DMI
        ELSEIF(AM1.GT.AMX)THEN
          AM1=AM1-DMI
        ENDIF
C
        AM2=AMV2+DM*(getRan()-.5)
        IF(AM2.LT.AAMI)THEN
          AM2=AM2+DMI
        ELSEIF(AM2.GT.AMX)THEN
          AM2=AM2-DMI
        ENDIF
C
        AM12=AM1+AM2
        IF(AM12.GT.SS)THEN
          D12=.5*(AM1-AM2)
          T12=SS-.5*AM12
          AM1=T12+D12
          AM2=T12-D12
        ENDIF
C
        FS=GDDALL(SS,AM1,AM2)
        IF(FS.LT.FV)THEN
          IF(FV*getRan().GT.FS)GOTO 30
        ENDIF
        AMV1=AM1
        AMV2=AM2
        FV=FS
 30   CONTINUE
C
      IF(getRan().GT..5)THEN
        AA=AMV1
        AMV1=AMV2
        AMV2=AA
      ENDIF
C
      END


      FUNCTION GDDALL(SS,AM1,AM2)
C  WEIGHTING FUNCTION FOR DD MASS DETERMINATION
C
      PARAMETER(AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER(AM0K=AM0*AM0)
      PARAMETER (AMEC=.13957,AMEN=.13496,AME=(AMEC+AMEC+AMEN)/3.)
      PARAMETER (AMEK=AME*AME)
      PARAMETER (AM0EK=AM0K+AMEK,AM0E4=4.*AM0K*AMEK)
      PARAMETER (AMV=1.232)
      PARAMETER (AMVK=AMV*AMV)
C
      PARAMETER (QRK=((AMVK-AM0EK)**2-AM0E4)/(4.*AMVK))
C
      PARAMETER (BET=.3)
      PARAMETER (BETK=BET*BET)
      PARAMETER (FORM=BETK+QRK)
C
      PARAMETER (FDK=.37)
      PARAMETER (FG=2./3.*FDK*AM0/AMEK)
C
C
      AM1K=AM1*AM1
      QK1=((AM1K-AM0EK)**2-AM0E4)/(4.*AM1K)
      IF(QK1.LT.0.)GOTO 200
C
      GMV1=FG*QK1*SQRT(QK1)/AM1
      GMV1=GMV1*(FORM/(BETK+QK1))**2   !not v. good, but lts stick w/it f/moment
C
      AM2K=AM2*AM2
      QK2=((AM2K-AM0EK)**2-AM0E4)/(4.*AM2K)
      IF(QK2.LT.0.)GOTO 200
C
      GMV2=FG*QK2*SQRT(QK2)/AM2
      GMV2=GMV2*(FORM/(BETK+QK2))**2   !not v. good, but lts stick w/it f/moment
C
      S=SS*SS
      PK=((S-AM1K-AM2K)**2-4.*AM1K*AM2K)/(4.*S)
      P=SQRT(MAX(PK,0.))
C
      GDDALL=P*AM1*GMV1/((AM1-AMV)**2+.25*GMV1*GMV1)
      GDDALL=GDDALL*AM2*GMV2/((AM2-AMV)**2+.25*GMV2*GMV2)
      gddall=gddall*fdfug(am1)*fdfug(am2)
      RETURN
C
 200  CONTINUE
      GDDALL=0.
C
      END


      function fdfug(am)
      PARAMETER (AMV=1.232)
      if(am.lt.amv)then
        fdfug=1.
      else
        fdfug=exp(7.*(am-amv))
      endif
      end


      FUNCTION RDNPP(SS,AM1,AM2)
C
C  THE FUNCTION CALCULATES ND++-->PP TO INVERSE CROSS SECTION RATIO
C    FROM DETAILED BALANCE RELATION BY DANIELEWICZ/BERTSCH
C    NPA533(91)712; SS -- SQRT(S), AM1 AND AM2 -- MASSES OF
C    THE NUCLEON AND DELTA
C
      PARAMETER(NMI=100,AAMX=4.)
      DIMENSION DNI(NMI)
C
      COMMON/CGDN/SG,AM1K         ! s for the subintegral function
      EXTERNAL GDN
C
      PARAMETER(AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER(AM0K=AM0*AM0)
      PARAMETER(AMEC=.13957,AMEN=.13496,AME=(AMEC+AMEC+AMEN)/3.)
C
      PARAMETER(AAMI=AM0+AME)
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      IF(FIRST)THEN
        PI=4.*ATAN(1.)
        DMI=(AAMX-AAMI)/NMI
        DO I=1,NMI
          AMX=AAMI+I*DMI
          SG=(AM0+AMX)**2
          CALL IGRAL(AAMI,AMX,.2,GDN,A)
          DNI(I)=A/(PI+PI)
        ENDDO
        FIRST=.FALSE.
      ENDIF
C
      AMX=SS-AM0
      RMX=(AMX-AAMI)/DMI
      IX=RMX
      IF(IX.LE.0)THEN
        RDNPP=0.
        RETURN
      ENDIF
      IF(IX.LT.NMI)THEN
        IX1=IX+1
        RI=RMX-IX
        RI1=1.-RI
        DN=RI1*DNI(IX)+RI*DNI(IX1)
      ELSE
        DN=DNI(NMI)
      ENDIF
C
      AM1K=AM1*AM1
      AM2K=AM2*AM2
      S=SS*SS
      PNDK=((S-AM1K-AM2K)**2-4.*AM1K*AM2K)/(4.*S)
      PND=SQRT(MAX(PNDK,1E-5))
      PNNK=((S-AM0K-AM0K)**2-4.*AM0K*AM0K)/(4.*S)
C
      RDNPP=AM1*AM2/AM0*2./4./2.*PNNK/PND/DN
C
      END


      FUNCTION RXPPP(SS,AM1,AM2)
C
C  THE FUNCTION CALCULATES PP*-->PP TO INVERSE CROSS SECTION RATIO
C    FROM DETAILED BALANCE RELATION BY DANIELEWICZ/BERTSCH
C    NPA533(91)712; SS -- SQRT(S), AM1 AND AM2 -- MASSES OF
C    THE NUCLEON AND DELTA
C
      PARAMETER(NMI=100,AAMX=4.)
      DIMENSION DNI(NMI)
C
      COMMON/CGDN/SG,AM1K         ! s for the subintegral function
      EXTERNAL GXN
C
      PARAMETER(AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER(AM0K=AM0*AM0)
      PARAMETER(AMEC=.13957,AMEN=.13496,AME=(AMEC+AMEC+AMEN)/3.)
C
      PARAMETER(AAMI=AM0+AME)
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      IF(FIRST)THEN
        PI=4.*ATAN(1.)
        DMI=(AAMX-AAMI)/NMI
        DO I=1,NMI
          AMX=AAMI+I*DMI
          SG=(AM0+AMX)**2
          CALL IGRAL(AAMI,AMX,.2,GXN,A)
          DNI(I)=A/(PI+PI)
        ENDDO
        FIRST=.FALSE.
      ENDIF
C
      AMX=SS-AM0
      RMX=(AMX-AAMI)/DMI
      IX=RMX
      IF(IX.LE.0)THEN
        RXPPP=0.
        RETURN
      ENDIF
      IF(IX.LT.NMI)THEN
        IX1=IX+1
        RI=RMX-IX
        RI1=1.-RI
        DN=RI1*DNI(IX)+RI*DNI(IX1)
      ELSE
        DN=DNI(NMI)
      ENDIF
C
      AM1K=AM1*AM1
      AM2K=AM2*AM2
      S=SS*SS
      PPXK=((S-AM1K-AM2K)**2-4.*AM1K*AM2K)/(4.*S)
      PPX=SQRT(MAX(PPXK,1E-5))
      PNNK=((S-AM0K-AM0K)**2-4.*AM0K*AM0K)/(4.*S)
C
      RXPPP=AM1*AM2/AM0/2.*PNNK/PPX/DN
C
      END


      SUBROUTINE DMS(AMV,DMI)
      use global
C
C  RETURNS FREE DELTA MASS AMV GIVEN CM ENERGY DMI ABOVE THRESHOLD
C    IN A REACTION NN-->ND
      PARAMETER(AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER(AMEC=.13957,AMEN=.13496,AME=(AMEC+AMEC+AMEN)/3.)
      PARAMETER(AMV0=1.232)
      PARAMETER(GMV=.120)
C
      PARAMETER(AAMI=AM0+AME)
      PARAMETER(SMI=AM0+AAMI)
C
      PARAMETER (NIT=29)
C
      COMMON/CGDN/SG,AM1K         ! s for the subintegral function
      COMMON/CSEED/ISEED
C
      SS=SMI+DMI
      AMX=AAMI+DMI
      DM=MIN(1.2*GMV,DMI)
      AMV=AMV0-.5*GMV    ! choosing a starting for Metropolis
      IF(AMV.GE.AMX)THEN
        AMV=AAMI+DMI*getRan()
      ELSEIF(getRan().GT..5)THEN
        AMM=AMV+GMV
        IF(AMM.LT.AMX)AMV=AMM
      ENDIF
C
      SG=SS*SS
      FV=GDN(AMV)
      DO 30 IT=1,NIT
        AM=AMV+DM*(getRan()-.5)
        IF(AM.LT.AAMI)THEN
          AM=AM+DMI
        ELSEIF(AM.GT.AMX)THEN
          AM=AM-DMI
        ENDIF
        FS=GDN(AM)
        IF(FS.LT.FV)THEN
          IF(FV*getRan().GT.FS)GOTO 30
        ENDIF
        AMV=AM
        FV=FS
 30   CONTINUE
C
      END


      FUNCTION GDN(AM)
C  SUBINTEGRAL FUNCTION FOR ND CROSS SECTIONS
      COMMON/CGDN/SG,AM1K         ! s for the subintegral function
      PARAMETER(AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER(AM0K=AM0*AM0)
      PARAMETER (AMEC=.13957,AMEN=.13496,AME=(AMEC+AMEC+AMEN)/3.)
      PARAMETER (AMEK=AME*AME)
      PARAMETER (AM0EK=AM0K+AMEK,AM0E4=4.*AM0K*AMEK)
      PARAMETER (AMV=1.232)
      PARAMETER (AMVK=AMV*AMV)
C
      PARAMETER (QRK=((AMVK-AM0EK)**2-AM0E4)/(4.*AMVK))
C
      PARAMETER (BET=.3)
      PARAMETER (BETK=BET*BET)
      PARAMETER (FORM=BETK+QRK)
C
      PARAMETER (FDK=.37)
      PARAMETER (FG=2./3.*FDK*AM0/AMEK)
C
C
      AMK=AM*AM
      QK=((AMK-AM0EK)**2-AM0E4)/(4.*AMK)
      IF(QK.LT.0.)GOTO 200
C
      GMV=FG*QK*SQRT(QK)/AM
      GMV=GMV*(FORM/(BETK+QK))**2   !not v. good, but lts stick w/it f/moment
C
      PK=((SG-AM0K-AMK)**2-4.*AM0K*AMK)/(4.*SG)
      IF(PK.LT.0.)GOTO 200
C
      GDN=AM*SQRT(PK)*GMV/((AM-AMV)**2+.25*GMV*GMV)
      RETURN
C
 200  CONTINUE
      GDN=0.
C
      END


      SUBROUTINE XMS(AMV,DMI)
      use global
C
C  RETURNS FREE DELTA MASS AMV GIVEN CM ENERGY DMI ABOVE THRESHOLD
C    IN A REACTION NN-->ND
      PARAMETER(AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER(AMEC=.13957,AMEN=.13496,AME=(AMEC+AMEC+AMEN)/3.)
      PARAMETER(AMV0=1.440)
      PARAMETER(GMV=.200)
C
      PARAMETER(AAMI=AM0+AME)
      PARAMETER(SMI=AM0+AAMI)
C
      PARAMETER (NIT=29)
C
      COMMON/CGDN/SG,AM1K         ! s for the subintegral function
      COMMON/CSEED/ISEED
C
      SS=SMI+DMI
      AMX=AAMI+DMI
      DM=MIN(1.2*GMV,DMI)
      AMV=AMV0-.5*GMV    ! choosing a starting for Metropolis
      IF(AMV.GE.AMX)THEN
        AMV=AAMI+DMI*getRan()
      ELSEIF(getRan().GT..5)THEN
        AMM=AMV+GMV
        IF(AMM.LT.AMX)AMV=AMM
      ENDIF
C
      SG=SS*SS
      FV=GXN(AMV)
      DO 30 IT=1,NIT
        AM=AMV+DM*(getRan()-.5)
        IF(AM.LT.AAMI)THEN
          AM=AM+DMI
        ELSEIF(AM.GT.AMX)THEN
          AM=AM-DMI
        ENDIF
        FS=GXN(AM)
        IF(FS.LT.FV)THEN
          IF(FV*getRan().GT.FS)GOTO 30
        ENDIF
        AMV=AM
        FV=FS
 30   CONTINUE
C
      END


      FUNCTION GXN(AM)
C  SUBINTEGRAL FUNCTION FOR NN* CROSS SECTIONS
      COMMON/CGDN/SG,AM1K         ! s for the subintegral function
      PARAMETER(AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER(AM0K=AM0*AM0)
      PARAMETER (AMEC=.13957,AMEN=.13496,AME=(AMEC+AMEC+AMEN)/3.)
      PARAMETER (AMEK=AME*AME)
      PARAMETER (AM0EK=AM0K+AMEK,AM0E4=4.*AM0K*AMEK)
      PARAMETER (AMVS=1.440)
      PARAMETER (AMVSK=AMVS*AMVS)
      PARAMETER (GDV=.200)
C
      PARAMETER (QRK=((AMVSK-AM0EK)**2-AM0E4)/(4.*AMVSK))
C
C
      AMK=AM*AM
      QK=((AMK-AM0EK)**2-AM0E4)/(4.*AMK)
      IF(QK.LT.0.)GOTO 200
C
      RRK=QK/QRK
      GDS=GDV*RRK*SQRT(RRK)
C
      AMK=AM*AM
      PK=((SG-AM0K-AMK)**2-4.*AM0K*AMK)/(4.*SG)
      IF(PK.LT.0.)GOTO 200
C
      GXN=AM*SQRT(PK)*GDS/((AM-AMVS)**2+.25*GDS*GDS)
      RETURN
C
 200  CONTINUE
      GXN=0.
C
      END


      FUNCTION PPND(XX)
C
C  THE FUNCTION CALCULATES PP-->ND++ CROSS SECTION IN FM**2
C    ACCORDING TO THE PARAMETRIZATION BY HUBER AND AICHELIN
C    NPA573(1994)587; XX -- SQRT(S)
C
      X=MIN(XX,4.5)
      IF(X.LE.2.014)THEN
        A=0.
      ELSEIF(X.LT.2.20)THEN
        A=1341.*(X-2.014)**2.819
      ELSEIF(X.LT.2.45)THEN
        A=18.51-235.2*(X-2.370)**2
      ELSE
        A=1591./X**4.957
      ENDIF
C
      PPND=.1*A
C
      END


      FUNCTION PPPPS(XX)
C
C  THE FUNCTION CALCULATES PP-->PP* CROSS SECTION IN FM**2
C    ACCORDING TO THE PARAMETRIZATION BY HUBER AND AICHELIN
C    NPA573(1994)587; XX -- SQRT(S)
C
      X=MIN(XX,4.5)
      IF(X.LE.2.0158)THEN
        A=0.
      ELSEIF(X.LT.2.35)THEN
        A=1.704*(X-2.0158)**2.225
      ELSEIF(X.LT.2.60)THEN
        A=1.015*X-2.241
      ELSE
        A=0.6773-0.1356*ABS(X-3.877)**3.014
      ENDIF
C
      PPPPS=.1*A
C
      END


      FUNCTION PNPNS(XX)
C
C  THE FUNCTION CALCULATES PN-->PN* CROSS SECTION IN FM**2
C    ACCORDING TO THE PARAMETRIZATION BY HUBER AND AICHELIN
C    NPA573(1994)587; XX -- SQRT(S)
C
      X=MIN(XX,4.5)
      IF(X.LE.2.016)THEN
        A=0.
      ELSEIF(X.LT.2.33)THEN
        A=1.361*(X-2.016)**2.091
      ELSEIF(X.LT.2.53)THEN
        A=0.8031*X-1.752
      ELSEIF(X.LT.3.04)THEN
        A=0.3404-0.3272*(X-2.945)**2
      ELSE
        A=0.5326-0.06395*X
      ENDIF
C
      PNPNS=.1*A
C
      END


      FUNCTION PNDMD(XX)
C
C  THE FUNCTION CALCULATES NP-->D-D++ CROSS SECTION IN FM**2
C    ACCORDING TO THE PARAMETRIZATION BY HUBER AND AICHELIN
C    NPA573(1994)587; XX -- SQRT(S)
C
      X=MIN(XX,4.5)
      IF(X.LE.2.152)THEN
        A=0.
      ELSEIF(X.LT.2.412)THEN
        A=239.6*(X-2.152)**5.148
      ELSEIF(X.LT.2.60)THEN
        A=4.738*X-11.21
      ELSEIF(X.LT.2.98)THEN
        A=1.395-3.615*(X-2.884)**2
      ELSEIF(X.LT.3.69)THEN
        A=3.517-0.7197*X
      ELSE
        A=9.358/X**1.242-0.9878
      ENDIF
C
      PNDMD=.1*A
**      pndmd=000.
C
      END


      FUNCTION PND0D(XX)
C
C  THE FUNCTION CALCULATES NP-->D0D+ CROSS SECTION IN FM**2
C    ACCORDING TO THE PARAMETRIZATION BY HUBER AND AICHELIN
C    NPA573(1994)587; XX -- SQRT(S)
C
      X=MIN(XX,4.5)
      IF(X.LE.2.152)THEN
        A=0.
      ELSEIF(X.LT.2.412)THEN
        A=144.8*(X-2.152)**5.083
      ELSEIF(X.LT.2.58)THEN
        A=2.841*X-6.707
      ELSEIF(X.LT.2.98)THEN
        A=0.7819-1.777*(X-2.869)**2
      ELSEIF(X.LT.3.26)THEN
        A=1.894-0.3798*X
      ELSE
        A=8.371/X**1.926-0.2035
      ENDIF
C
      PND0D=.1*A
**      pnd0d=000.
C
      END


      FUNCTION PPD0D(XX)
C
C  THE FUNCTION CALCULATES PP-->D0D++ CROSS SECTION IN FM**2
C    ACCORDING TO THE PARAMETRIZATION BY HUBER AND AICHELIN
C    NPA573(1994)587; XX -- SQRT(S)
C
      X=MIN(XX,4.5)
      IF(X.LE.2.152)THEN
        A=0.
      ELSEIF(X.LT.2.44)THEN
        A=150.4*(X-2.152)**5.299
      ELSEIF(X.LT.2.63)THEN
        A=2.794*X-6.626
      ELSEIF(X.LT.2.98)THEN
        A=0.9205-2.763*(X-2.898)**2
      ELSEIF(X.LT.3.24)THEN
        A=2.192-0.4320*X
      ELSE
        A=6.898/X**1.417-0.5140
      ENDIF
C
      PPD0D=.1*A
**      ppd0d=000.
C
      END


      FUNCTION PPDUP(XX)
C
C  THE FUNCTION CALCULATES PP-->DPI+ CROSS SECTION IN FM**2
C    ACCORDING TO THE PARAMETRIZATION BY VERWEST AND ARNDT
C    NPA573(1994)587; XX -- SQRT(S)
C
C
      PARAMETER (H=.19733)
C
      PARAMETER (AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER (AMEC=.13957,AMEN=.13496,AME=(AMEC+AMEC+AMEN)/3.)
      PARAMETER (AM0K=AM0*AM0)
      PARAMETER (BD=.002225,AMD=AM0+AM0-BD)
C
      PARAMETER(ALPHA=6.03)
      PARAMETER(BETA=1.7)
      PARAMETER(AMV0=1.203)
      PARAMETER(AMV0K=AMV0*AMV0)
      PARAMETER(GAM=.1343)
      PARAMETER(GAMK0=GAM*GAM*AMV0K)
C
      PARAMETER(P0K=.25*(AM0+AMV0)**2-AM0K)
C
      LOGICAL FIRST
C
      DATA FIRST/.TRUE./
C
      IF(FIRST)THEN
        PI=4.*ATAN(1.)
        FSIG=PI*H*H*.5*ALPHA*GAMK0
        FIRST=.FALSE.
      ENDIF
C
      S=XX*XX
      PK=.25*S-AM0K
      SPIN=(XX-AM0)**2
      PRK=(S-(AMD-AME)**2)*(S-(AMD+AME)**2)/(4.*S)
C
      IF(PK.GT.0.)THEN
        PPDUP=FSIG/PK*(PRK/P0K)**BETA/((SPIN-AMV0K)**2+GAMK0)
      ELSE
        PPDUP=0.
      ENDIF
C
      END


      SUBROUTINE IGRAL1(A,B,C,FUN,RES)
C  INTEGRATES FUN FROM A TO B
C  A AND B MAY BE CLOSE TO +- INFINITY
C  C IS THE CHARACTERISTIC RANGE OF VARIATION OF FUN
C  RES IS RESULT
      EXTERNAL FUN
CT  TESTING PROGRAM
CT      EXTERNAL FUN
CT      CALL IGRAL(-1.,1.,1.,FUN,RES)
CT      WRITE(*,*)RES
CT      END
CT      FUNCTION FUN(Z)
CT      FUN=Z*Z
CT      END
      PARAMETER(NG=18)
      DIMENSION XG(NG),WG(NG)
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      SAVE XG,WG
C
      IF(FIRST)THEN
        CALL GAUSGN(NG,XG,WG)
        FIRST=.FALSE.
      ENDIF
C
      CC=ABS(C)
      IF(CC.EQ.0.)CC=1.
      CCK=CC*CC
C
      IF(ABS(A).LT.1E20)THEN
        X1=2.*A/(SQRT(CCK+4.*A*A)+CC)
      ELSE
        X1=SIGN(1.,A)
      ENDIF
C
      IF(ABS(B).LT.1E20)THEN
        X2=2.*B/(SQRT(CCK+4.*B*B)+CC)
      ELSE
        X2=SIGN(1.,B)
      ENDIF
C
      XC=.5*(X1+X2)
      XA=.5*(X2-X1)
C
      RES=0.
      DO I=1,NG
        X=XC+XA*XG(I)
        XK=X*X
        Z=CC*X/(1.-XK)
        RES=RES+FUN(Z)*WG(I)*(1.+XK)/(1.-XK)**2
      ENDDO
      RES=RES*CC*XA
C
      END


      SUBROUTINE IGRAL(A,B,C,FUN,RES)
C  INTEGRATES FUN FROM A TO B
C  A AND B MAY BE CLOSE TO +- INFINITY
C  C IS THE CHARACTERISTIC RANGE OF VARIATION OF FUN
C  RES IS RESULT
      EXTERNAL FUN
CT  TESTING PROGRAM
CT      EXTERNAL FUN
CT      CALL IGRAL(-1.,1.,1.,FUN,RES)
CT      WRITE(*,*)RES
CT      END
CT      FUNCTION FUN(Z)
CT      FUN=Z*Z
CT      END
      PARAMETER(NG=18)
      DIMENSION XG(NG),WG(NG)
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      SAVE XG,WG
C
      IF(FIRST)THEN
        CALL GAUSGN(NG,XG,WG)
        FIRST=.FALSE.
      ENDIF
C
      CC=ABS(C)
      IF(CC.EQ.0.)CC=1.
      CCK=CC*CC
C
      IF(ABS(A).LT.1E20)THEN
        X1=2.*A/(SQRT(CCK+4.*A*A)+CC)
      ELSE
        X1=SIGN(1.,A)
      ENDIF
C
      IF(ABS(B).LT.1E20)THEN
        X2=2.*B/(SQRT(CCK+4.*B*B)+CC)
      ELSE
        X2=SIGN(1.,B)
      ENDIF
C
      XC=.5*(X1+X2)
      XA=.5*(X2-X1)
C
      RES=0.
      DO I=1,NG
        X=XC+XA*XG(I)
        XK=X*X
        Z=CC*X/(1.-XK)
        RES=RES+FUN(Z)*WG(I)*(1.+XK)/(1.-XK)**2
      ENDDO
      RES=RES*CC*XA
C
      END


      SUBROUTINE GAUSGN(NP,X,W)
C
C  ROUTINE EVALUATES POINTS AND WEIGHTS FOR NP-POINT GAUSS QUADRATURE
C
C  P. DANIELEWICZ, AUGUST 9, 1983
C                  DECEMBER 6, 1987
C
C  REFERENCE - P.J. DAVIS AND P. RABINOWITZ, NUMERICAL INTEGRATION,
C              BLAISDELL PUB. CO., WALTHAM MASS., 1967, P. 44.
C
C  --INPUT--
C   NP- NUMBER OF POINTS
C
C  --OUTPUT--
C   X - POINTS
C   W - WEIGHTS
C
      DOUBLE PRECISION PREC,PREC2
      PARAMETER (PREC=1D-10,PREC2=PREC*PREC)
C  PREC - NUMBER .LT. SINGLE PRECISION AND .GT. DOUBLE
      DIMENSION X(*),W(*)
      DOUBLE PRECISION XX,DX,PNX,PN1X,PPX
      PARAMETER (PI=3.1415 92653 58979)
      DIMENSION ZJ0(20)
C
C  VALUES OF THE FIRST TWENTY ZEROS OF THE ZEROTH ORDER BESSEL FUNCTION
C  FROM M. ABRAMOWITZ AND I.A. STEGUN, HANDBOOK OF MATHEMATICAL
C  FUNCTIONS, DOVER, N.Y., 1972, P. 409.
      DATA ZJ0
     1  / 2.40482 55577  , 5.52007 81103
     2  , 8.65372 79129  ,11.79153 44391
     3  ,14.93091 77086  ,18.07106 39679
     4  ,21.21163 66299  ,24.35247 15308
     5  ,27.49347 91320  ,30.63460 64684
     6  ,33.77582 02136  ,36.91709 83537
     7  ,40.05842 57646  ,43.19979 17132
     8  ,46.34118 83717  ,49.48260 98974
     9  ,52.62405 18411  ,55.76551 07550
     O  ,58.90698 39261  ,62.04846 91902/
C
C
      C4=(1.-(2./PI)**2)*.25
      FAC=.5*(1./(NP+.5)+1./SQRT((NP+.5)**2+C4))
C
      NP1=NP+1
      NPP=NP1/2
C
C  STARTING VALUES FOR POINTS
      M=MIN0(NPP,20)
      DO 10 J=1,M
 10   X(J)=-COS(FAC*ZJ0(J))
      IF(NPP.LE.20)GOTO 30
C
C  EVALUATION OF EXTRA BESSEL ZEROS, FROM ABRAMOWITZ..., P. 371
      B=19.75*PI
      F1=-4.*31./(3.*8.**3)
      F2=32.*3779./(15.*8.**5)
C
      M=M+1
      DO 20 J=M,NPP
      B=B+PI
      BK=B*B
      AG=((F2/BK+F1)/BK+.125)/B+B
 20   X(J)=-COS(FAC*AG)
C
 30   DO 60 J=1,NPP
      XX=X(J)
C  ITERATION FOR ACCURATE POINTS
C  CALL FOR NP-1 AND NP LEGENDRE POLYNOMIALS
 40   CALL PLNXZZ(NP,XX,PN1X,PNX)
C  DERIVATIVE
      PPX=NP*(PN1X-XX*PNX)/(1D0-XX*XX)
      DX=-PNX/PPX
      XX=XX+DX
C  TO PRECLUDE UNDERFLOW IN CERTAIN CASES
      IF(ABS(XX).LT.PREC2)THEN
        XX=0D0
      ELSEIF(ABS(DX).GT.PREC*ABS(XX))THEN
        GOTO 40
      ENDIF
      X(J)=XX
      X(NP1-J)=-XX
      W(J)=2D0*(1D0-XX*XX)/(NP*PN1X)**2
 60   W(NP1-J)=W(J)
C
      END


      SUBROUTINE PLNXZZ(N,X,PN1,PN)
C
C  ROUTINE EVALUATES N-1 AND N LEGENDRE POLYNOMIAL AT X, N.GE.1,
C  FROM DAVIS...
C
      DOUBLE PRECISION X,PN,PN1,PN2,PBJ
C
C
      PN1=1D0
      PN=X
      IF(N.EQ.1)RETURN
C
      DO 10 J=2,N
      PBJ=1D0/J
      PN2=PN1
      PN1=PN
 10   PN=(2D0-PBJ)*X*PN1+(PBJ-1D0)*PN2
C
      END



C  PACKAGE THAT CALCULATES AND SAMPLES NN CROSS SECTION
c  Header for testing
c      PARAMETER (AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
c      PARAMETER (AM0K=AM0*AM0)
c      PARAMETER (AM0Q=AM0K*AM0K)
c      PARAMETER(NTRY=2000)
c      COMMON/CSEED/ISEED
cC
c      PI=4.*ATAN(1.)
cC
c      ISEED=12345791
cc
c*     do ie=1,10
c*       tlab=ie*.01
c*       em=tlab/am0
c*       write(*,*)tlab,cspnel(em,.2),cspnel(em,-.2)
c*       PLABK=(AM0+TLAB)**2-AM0K
c*       ECMK=(AM0+AM0+TLAB)**2-PLABK
c*       EICK=ECMK*.25
c*       PK=EICK-AM0K
c*       EPCM=SQRT(ECMK*PK)
c*       SIGEL=SIG(EPCM,1,2)*10.
c*       CSD1=CSDIF(EPCM,.2,SIGEL,1,2)
c*       CSD2=CSDIF(EPCM,-.2,SIGEL,1,2)
c*       write(*,*)tlab,csd1,csd2
c*     enddo
c*     stop
cC
c 10   CONTINUE
c      WRITE(*,*)'Enter Tlab, Angle, PP/NP: '
c      READ(*,*)TLAB,THD,K
c      TH=THD/180.*PI
c      CTH=COS(TH)
c      PLABK=(AM0+TLAB)**2-AM0K
c      ECMK=(AM0+AM0+TLAB)**2-PLABK
c      EICK=ECMK*.25
c      PK=EICK-AM0K
c      EPCM=SQRT(ECMK*PK)
c      SIGEL=SIG(EPCM,1,K)*10.
c      CSD=CSDIF(EPCM,CTH,SIGEL,1,K)
c*     IF(K.EQ.1)CSD=CSD+CSD
c*     WRITE(*,*)CSD,SIGEL/(4.*PI)
c      CTHF=MIN(1.,CTH+.1)
c      CTHB=MAX(-1.,CTH-.1)
c      KT=0
c      DO I=1,NTRY
c        CALL THETAN(CT,EPCM,1,K)
c        IF(ABS(CTH-CT).LT..1)KT=KT+1
c      ENDDO
c      CSDI=SIGEL/(2.*PI)*KT/NTRY/(CTHF-CTHB)
c      WRITE(*,*)CSD,CSDI
c      GOTO 10
cC
c      END
c
c
c      BLOCK DATA CSNORM
c      COMMON/RN/RN(6,6)
c      DATA RN(1,1)/1./RN(1,2)/1./RN(2,2)/1./
c      DATA RN(1,3)/.96/RN(2,3)/.96/
c      DATA RN(3,3)/.89/
c      DATA RN(1,6)/.85/RN(2,6)/.85/
c      DATA RN(3,6)/.75/
c      DATA RN(6,6)/.29/
c      DATA ((RN(I,J),I=1,2),J=4,5)/4*.90/       !check
c      DATA (RN(3,J),J=4,5)/2*.79/
c      DATA ((RN(I,J),I=4,5),J=4,5)/4*.69/
c      DATA RN(4,6)/.65/RN(5,6)/.65/
cC  NORMALIZATION OF TOTAL CROSS SECTION FOR CLUSTER-CLUSTER TO
cC  EVERY NUCLEON WITH EVERY NUCLEON (I.E. PROPORTIONAL TO A1*A2)
cC
c      END
c
c
c      BLOCK DATA ZEES            !!! NEEDS TO BE MODIFIED IF ID'S CHANGED !!!
c      INCLUDE 'PARTIC15.'
c      DATA   ZPA/1.,0.,1.,2.,1.,2.               !charge
cC                p  n  d 3He t 4He
c     Z  ,2.,1.,0.,-1.,1. ,0., 1., 0., -1., 0./
cC       D++ D+ D0 D- N*+ N*- pi+ pi0  pi-  gm
cC
c      DATA TIZO/.5,-.5,0.,.5,-.5,0.              !isospin
cC                p  n  d 3He  t 4He
c     T  ,1.5,.5,-.5,-1.5,.5,-.5,1.,0.,-1., 0./
cC       D++ D+ D0 D- N*+ N*- pi+ pi0  pi-  gm
cC
c      DATA   BAR/1.,1.,2.,3.,3.,4.               !baryon number
cC                p  n  d 3He t 4He
c     Z  ,1.,1.,1.,1.,1. ,1., 0., 0., 0., 0./
cC       D++ D+ D0 D- N*+ N*- pi+ pi0 pi- gm
cC
c      PARAMETER (B=.008,AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
c      PARAMETER (AMB=AM0-B)
c      PARAMETER (BD=.002225,AMD=AM0+AM0-BD)
c      PARAMETER (AMEC=.13957,AMEN=.13496,AME=(AMEC+AMEC+AMEN)/3.)
c      PARAMETER (BT=.0086,AMT=AMP+AMN+AMN-BT)
c      PARAMETER (B3E=.0080,AM3E=AMP+AMP+AMN-B3E)
c      PARAMETER (AMTE=.5*(AMT+AM3E))
c      PARAMETER (B4E=.0286,AM4E=4.*AM0-B4E)
c      PARAMETER (AMV0=1.232)
c      PARAMETER (AMX=1.440)
cC
c      DATA AMS/2*AM0,AMD,2*AMTE,AM4E,4*AMV0,2*AMX,3*AME,0./       !fixed mass
cC              N's    D  T&3HE  4HE  DELTA   N*    PI's gm
cC
c      END
c
c
c      FUNCTION SIG(EpPCM,IDP,JDP)
c      INCLUDE 'PARTID.'
cC  JUST PLAIN CROSS SECTION IN FM**2,  (N OR CLUSTER) - (N OR CLUSTER)
cC  BUT THE ARGUMENT IS WEIRD:
cC  EPCM IS THE TOTAL CM ENERGY TIMES CM
cC  MOMENTUM OF ONE PARTICLE IN GEV/C
cC  ((THIS IS THE EASIEST KINEMATIC COMBINATION THAT GIVES SOME RESOLUTION))
c      PARAMETER (NPCM=110,DPCM=.01)
c      PARAMETER (NPCM1=NPCM+1)
c      DIMENSION SIGPPR(0:NPCM1),SIGPNR(0:NPCM1)
cC  ONE LINE OF THE VECTOR WILL BE WASTED ...
c      DIMENSION SIGPP(0:NPCM1),SIGPN(0:NPCM1)
cC
c      PARAMETER (PCMX=NPCM*DPCM)
cC
c      PARAMETER (AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
c      PARAMETER (AM0K=AM0*AM0)
c      PARAMETER (AM0Q=AM0K*AM0K)
cC
c      COMMON/RN/RN(6,6)
cC
c      LOGICAL FIRST
cC
c      DATA FIRST/.TRUE./
cC
c      IF(FIRST)THEN
c        OPEN(33,FILE='CSCALL.DAT',STATUS='OLD')
c        DO I=0,NPCM1
c          READ(33,*)A,A,SIGPPR(I),SIGPNR(I)
c        ENDDO
c        CLOSE(33)
cC
cC  STEPS IN EPCM...
c        PCMXK=PCMX*PCMX
c        E0MX=SQRT(AM0K+PCMXK)
c        EPCMX=(E0MX+E0MX)*PCMX
c        DEPCM=EPCMX/NPCM
cC
c        DO I=0,NPCM
c          EP=I*DEPCM
c          PCMK=.5*(SQRT(AM0Q+EP*EP)-AM0K)
c          PCM=SQRT(PCMK)
c          X=PCM/DPCM
c          ICM=X
c          DX=X-ICM
c          IF(ICM.GE.NPCM)THEN
c            ICM=NPCM
c            DX=0.
c          ENDIF
c          SIGPP(I)=.1*(SIGPPR(ICM)*(1.-DX)+SIGPPR(ICM+1)*DX)
c          SIGPN(I)=.1*(SIGPNR(ICM)*(1.-DX)+SIGPNR(ICM+1)*DX)
c        ENDDO
c        SIGPP(NPCM1)=.1*SIGPPR(NPCM1)
c        SIGPN(NPCM1)=.1*SIGPNR(NPCM1)
cC  IMPOSSIBLE..:  DD COMPLETED
c        FIRST=.FALSE.
c        fepcm=4.*am0k*.065**2
c      ENDIF
cC
c      BARI=BAR(IDP)
c      BARJ=BAR(JDP)
c      BARIJ=BARI*BARJ
c      epcm=sqrt(eppcm*eppcm+fepcm*barij*barij
c     e  *(bari+barj-2.)/(bari+barj))
c      X=EPCM/(DEPCM*BARIJ)
c      ICM=X
c      DX=X-ICM
c      IF(ICM.GE.NPCM)THEN
c        ICM=NPCM
c        DX=0.
c      ENDIF
cC
c      ZI=ZPA(IDP)
c      UI=BARI-ZI
c      ZJ=ZPA(JDP)
c      UJ=BARJ-ZJ
c      SIG=RN(IDP,JDP)
c     I  *((ZI*ZJ+UI*UJ)*(SIGPP(ICM)*(1.-DX)+SIGPP(ICM+1)*DX)
c     S  +(ZI*UJ+UI*ZJ)*(SIGPN(ICM)*(1.-DX)+SIGPN(ICM+1)*DX))
cC
c      END
c
c
c
      SUBROUTINE THETAN(CT,EPCM,IDP,JDP)
      use global
C  SUBROUTINE SAMPLES RANDOMLY COSINE OF THE SCATTERING ANGLE
C  FOR TWO NUCLEONS ACCORDING TO THE DIFFERENTIAL SCATTERING
C  CROSS SECTIONS
C    EPCM - TOTAL CM ENERGY TIMES CM MOMENTUM OF ONE OF THE
C           PARTICLES
C    IDP,JDP  - PARTICLE IDS, 1 -- PROTON, 2 -- NEUTRON
C  --OUTPUT--
C    CT   - COSINE OF THE SCATTERING ANGLE
C
C  SOURCES: CUGNON ET AL. NIMRB111(96)215, NPA620(97)475, PRC56(97)2431
C           UCRL-20000 NN, UCRL-9043 CLEMENTS AND WINSBERG
C
      PARAMETER (AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER(AM0K=AM0*AM0,AM0Q=AM0K*AM0K)
C
      COMMON/CSEED/ISEED
C
      PARAMETER(TLNC=.3)  ! cutoff energy for neutrons above which
C                           Cugnon used
      PARAMETER(NEPC=33,NKSI=12)
      DIMENSION CNP(0:NEPC,0:NKSI)
      PARAMETER(DKSI=1./NKSI)
      PARAMETER(NSMALL=4)
C
      PARAMETER(NGM=8)
      DIMENSION XGG(NGM),WGG(NGM)
C
      DATA (XGG(I),WGG(I),I=1,NGM)
     D  /0.095013, 0.189451
     A  ,0.281604, 0.182603
     T  ,0.458017, 0.169157
     A  ,0.617876, 0.149596
     D  ,0.755404, 0.124629
     A  ,0.865631, 0.095159
     T  ,0.944575, 0.062254
     A  ,0.989401, 0.027152/
C
      DOUBLE PRECISION BNP,EB,BPP
C
      LOGICAL LID,FIRST
      DATA FIRST/.TRUE./
C
C
      IF(FIRST)THEN
        EICNK=AM0*(AM0+.5*TLNC)
        PCNK=EICNK-AM0K
        EPCN=2.*SQRT(EICNK*PCNK)
        DEPC=EPCN/NEPC
        DO J=0,NEPC
          EPC=J*DEPC
          EPCMK=EPC*EPC
          PK=EPCMK/(2.*(SQRT(AM0Q+EPCMK)+AM0K))
          PLABK=4.*PK*(1.+PK/AM0K)
          EM=SQRT(1.+PLABK/AM0K)-1.
          EM=MAX(EM,0.)
          AA=0.
          DO IG=1,NGM
            X=XGG(IG)
            AA=AA+WGG(IG)*(CSPNEL(EM,X)+CSPNEL(EM,-X))
          ENDDO
          CPN=AA*DKSI/NSMALL
          COPN=1.
          CNP(J,NKSI)=COPN
          DO KS=NKSI*NSMALL-1,NSMALL,-1
            DCOPN=CPN/CSPNEL(EM,COPN)
            COPN=COPN-.5*(DCOPN+CPN/CSPNEL(EM,COPN-DCOPN))
            IF(COPN.LT.0.)GOTO 120
            KZ=KS/NSMALL
            IF(KZ*NSMALL.EQ.KS)CNP(J,KZ)=COPN
          ENDDO
 120      CONTINUE
          COPN=-1.
          CNP(J,0)=COPN
          DO KS=1,(NKSI-1)*NSMALL
            DCOPN=CPN/CSPNEL(EM,COPN)
            COPN=COPN+.5*(DCOPN+CPN/CSPNEL(EM,COPN+DCOPN))
            IF(COPN.GE.0.)GOTO 140
            KD=KS/NSMALL
            IF(KD*NSMALL.EQ.KS)THEN
              IF(KD.EQ.KZ)THEN
                CNP(J,KD)=.5*(COPN+CNP(J,KZ))
                GOTO 140
              ELSE
                CNP(J,KD)=COPN
              ENDIF
            ENDIF
          ENDDO
 140      CONTINUE
        ENDDO
        FIRST=.FALSE.
      ENDIF
C
      LID=IDP.EQ.JDP
      RS=getRan()
      IF(LID.OR.EPCM.GE.EPCN)THEN
        EPCMK=EPCM*EPCM
        PK=EPCMK/(2.*(SQRT(AM0Q+EPCMK)+AM0K))
        PK=MAX(PK,1E-6)
        PLAB=2.*SQRT(PK*(1.+PK/AM0K))
        IF(LID)THEN
          IF(PLAB.LT.2.)THEN
            PLAB8=PLAB**8
            BPP=5.5*PLAB8/(7.7+PLAB8)
          ELSE
**          BPP=5.334+.67*(PLAB-2.)
            BPP=11.6*TANH((5.776+.67*(PLAB-2.))/11.6)
          ENDIF
          BPP=BPP*2.*PK
          IF(BPP.LT.20D0)THEN
            IF(BPP.GT.1D-3)THEN
              EB=EXP(BPP)
              CT=LOG(EB*(1.-RS)+RS/EB)/BPP
            ELSE
              CT=2.*getRan()-1.
            ENDIF
          ELSE
            CT=1.+LOG(1.-RS)/BPP
            CT=MAX(CT,-1.)
          ENDIF
          IF(getRan().GT..5)CT=-CT
        ELSE
          ANP=(.8/PLAB)**2
          IF(PLAB.LT.1.1)THEN
            BNP=9.87-4.88*PLAB
          ELSE
**          BNP=3.68+.76*PLAB
            BNP=11.6*(4.84+PLAB)/(14.2+PLAB)
          ENDIF
          BNP=BNP*2.*PK
          IF(BNP.LT.20D0)THEN
            IF(BNP.GT.1D-3)THEN
              EB=EXP(BNP)
              CT=LOG(EB*(1.-RS)+RS/EB)/BNP
            ELSE
              CT=2.*getRan()-1.
            ENDIF
          ELSE
            CT=1.+LOG(1.-RS)/BNP
            CT=MAX(CT,-1.)
          ENDIF
          IF(getRan().GT.1./(1.+ANP))CT=-CT
        ENDIF
      ELSE
        RE=EPCM/DEPC
        RE=MIN(RE,NEPC-1E-3)
        J=RE
        J1=J+1
        DE=RE-J
        RS=RS/DKSI
        RS=MIN(RS,NKSI-1E-3)
        KS=RS
        KS1=KS+1
        DRS=RS-KS
        CT=(CNP(J,KS)*(1.-DRS)+CNP(J,KS1)*DRS)*(1.-DE)
     C  +(CNP(J1,KS)*(1.-DRS)+CNP(J1,KS1)*DRS)*DE
      ENDIF
C
      END


      FUNCTION CSDIF(EPCM,CT,SIGEL,IDP,JDP)
C  FUNCTION CALCULATES ELASTIC DIFFERENTIAL CROSS SECTION
C  FOR NUCLEONS; FOR PP, NORM IS AS IF THEY WERE DISTINCT
C  LOW-ENERGY PARAMETRIZATION BY CLEMENTS AND WINSBERG
C  AND HIGH-ENERGY CUGNON ET AL ARE COMBINED
C  --INPUT--
C    EPCM - TOTAL CM ENERGY TIMES CM MOMENTUM OF ONE OF THE
C           PARTICLES
C    SIGEL - NET ELASTIC CROSS SECTION IN APPROPRIATE
C            UNITS GIVEN
C    CT   - COSINE OF THE SCATTERING ANGLE
C    IDP,JDP  - PARTICLE IDS, 1 -- PROTON, 2 -- NEUTRON
C  --OUTPUT--
C    CSDIF - CROSS SECTION NORMALIZED SO THAT
C            INT_DOMEGA CSDIF = SIGEL
C
C  SOURCES: CUGNON ET AL. NIMRB111(96)215, NPA620(97)475, PRC56(97)2431
C           UCRL-20000 NN, UCRL-9043 CLEMENTS AND WINSBERG
C
      PARAMETER (AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER(AM0K=AM0*AM0,AM0Q=AM0K*AM0K)
C
      PARAMETER(TLNC=.3)  ! cutoff energy for neutrons above which
C                           Cugnon used
      PARAMETER(NEPC=33,NKSI=12)
      DIMENSION CNP(0:NEPC,0:NKSI)
      PARAMETER(DKSI=2./NKSI)
C
      PARAMETER(NGM=8)
      DIMENSION XGG(NGM),WGG(NGM)
C
      DATA (XGG(I),WGG(I),I=1,NGM)
     D  /0.095013, 0.189451
     A  ,0.281604, 0.182603
     T  ,0.458017, 0.169157
     A  ,0.617876, 0.149596
     D  ,0.755404, 0.124629
     A  ,0.865631, 0.095159
     T  ,0.944575, 0.062254
     A  ,0.989401, 0.027152/
C
      DOUBLE PRECISION BNP,ECT,BPP,AG
C
      LOGICAL LID,FIRST
      DATA FIRST/.TRUE./
C
      IF(FIRST)THEN
        PI=4.*ATAN(1.)
        EICNK=AM0*(AM0+.5*TLNC)
        PCNK=EICNK-AM0K
        EPCN=2.*SQRT(EICNK*PCNK)
        DEPC=EPCN/NEPC
        DO J=0,NEPC
          EPC=J*DEPC
          EPCMK=EPC*EPC
          PK=EPCMK/(2.*(SQRT(AM0Q+EPCMK)+AM0K))
          PLABK=4.*PK*(1.+PK/AM0K)
          EM=SQRT(1.+PLABK/AM0K)-1.
          EM=MAX(EM,0.)
          AA=0.
          DO IG=1,NGM
            X=XGG(IG)
            AA=AA+WGG(IG)*(CSPNEL(EM,X)+CSPNEL(EM,-X))
          ENDDO
          CSPN=AA*2.*PI
          DO K=0,NKSI
            X=-1.+K*DKSI
            CNP(J,K)=CSPNEL(EM,X)/CSPN
          ENDDO
*         write(*,*)cspn,sig(epc,1,2)*10.
        ENDDO
*       stop
*       write(*,*)'start'
*       do j=0,nepc
*         epc=j*depc
*         epcmk=epc*epc
*         pk=epcmk/(2.*(sqrt(am0q+epcmk)+am0k))
*         plabk=4.*pk*(1.+pk/am0k)
*         tlab=sqrt(am0k+plabk)-am0
*         tlab=max(0.,tlab)
*         write(*,*)tlab,cnp(j,nksi-1),cspnel(tlab/am0,1.-dksi)
*       enddo
        FIRST=.FALSE.
      ENDIF
C
      LID=IDP.EQ.JDP
      IF(LID.OR.EPCM.GE.EPCN)THEN
        EPCMK=EPCM*EPCM
        PK=EPCMK/(2.*(SQRT(AM0Q+EPCMK)+AM0K))
        PK=MAX(PK,1E-6)
        PLAB=2.*SQRT(PK*(1.+PK/AM0K))
        IF(LID)THEN
          IF(PLAB.LT.2.)THEN
            PLAB8=PLAB**8
            BPP=5.5*PLAB8/(7.7+PLAB8)
          ELSE
**          BPP=5.334+.67*(PLAB-2.)
            BPP=11.6*TANH((5.776+.67*(PLAB-2.))/11.6)
          ENDIF
          BPP=BPP*2.*PK
          IF(BPP.LT.20D0)THEN
            CSDIF=SIGEL*BPP*COSH(BPP*CT)/(4.*PI*SINH(BPP))
          ELSE
            AG=BPP*(1.-ABS(CT))
            IF(AG.LT.20D0)THEN
              CSDIF=SIGEL*BPP*EXP(-AG)/(4.*PI)
            ELSE
              CSDIF=0.
            ENDIF
          ENDIF
        ELSE
          ANP=(.8/PLAB)**2
          IF(PLAB.LT.1.1)THEN
            BNP=9.87-4.88*PLAB
          ELSE
**          BNP=3.68+.76*PLAB
            BNP=11.6*(4.84+PLAB)/(14.2+PLAB)
          ENDIF
          BNP=BNP*2.*PK
          IF(BNP.LT.20D0)THEN
            ECT=EXP(BNP*CT)
            CSDIF=SIGEL*BNP*(ECT+ANP/ECT)/(4.*PI*(1.+ANP)*SINH(BNP))
          ELSE
            AG=BNP*(1.-ABS(CT))
            IF(AG.LT.20D0)THEN
              IF(CT.GT.0.)THEN
                CSDIF=SIGEL*BNP*EXP(AG)/(4.*PI*(1.+ANP))
              ELSE
                CSDIF=SIGEL*BNP*ANP*EXP(AG)/(4.*PI*(1.+ANP))
              ENDIF
            ELSE
              CSDIF=0.
            ENDIF
          ENDIF
        ENDIF
      ELSE
        RE=EPCM/DEPC
        RE=MIN(RE,NEPC-1E-3)
        J=RE
        J1=J+1
        DR=RE-J
        CE=(CT+1.)/DKSI
        CE=MAX(CE,1E-3)
        CE=MIN(CE,NKSI-1E-3)
        K=CE
        DC=CE-K
        K1=K+1
        CSDIF=SIGEL*((1.-DR)*((1.-DC)*CNP(J,K)+DC*CNP(J,K1))
     C    +DR*((1.-DC)*CNP(J1,K)+DC*CNP(J1,K1)))
      ENDIF
C
      END


      FUNCTION CSPNEL(E,CT)
C
C  FUNCTION EVALUATES ELASTIC DIFFERENTIAL PROTON-NEUTRON CROSS-SECTION
C  WITH THE ANALYTIC FIT OF T.P. CLEMENTS AND L. WINSBERG, POLYNOMIAL
C  FITS OF NUCLEON-NUCLEON SCATTERING DATA, REPORT UCRL-9043.
C
C  P. DANIELEWICZ, DECEMBER 6, 1983
C
C  --INPUT--
C    E  - LABORATORY ENERGY DIVIDED BY NUCLEON MASS 939 MEV
C    CT - COSINE OF THE SCATTERING ANGLE
C
C  --OUTPUT--
C    CSPNEL - DSIGMA/DOMEGA
C
C  THE FIT IS PRESUMABLY REASONABLE ONLY TO ABOUT 3 GEV
C
C
      DOUBLE PRECISION CT2,CT4,CT10,CT14,CT20,CT40,CT100
C
      DIMENSION ED(6)
      DATA ED/2.E-4,.015,.2,.62,1.065,6.55/
C
      CALL LOCATE(K,E,6,ED)
      K=K+1
      GOTO(10,20,30,40,50,60,70),K
C
 10   CSPNEL=1620.+E*(1.89E10*E-79.2E5)
      RETURN
C
 20   CSPNEL=78.93-3400.*E+(.3757+(1.0608E-8/E-9.977E-5)/E)/E
      RETURN
C
 30   IF(CT.GT.0.)GOTO 35
      T=2.1+((.04011-4.E-4/E)/E-.1624)/E
      U=1.57+E*(251.7+(E*(E*(41192.-1.162E5*E)-5021.)))
      U=AMAX1(0.,U)
      V=E*(E*(2978.+E*(108909.*E-33953.))-41.)
      V=AMAX1(0.,V)
      CSPNEL=T+U*CT**2+V*CT**14
      RETURN
C
 35   P=-.9+(.377+(.0184-1.77E-4/E)/E)/E
      Q=E*(182.9+E*(3066.2*E-1408.8))
      CSPNEL=P+Q*CT**4
      RETURN
C
 40   IF(CT.GT.0.)GOTO 45
      W=-.5+E*(18.26+E*(25.08*E-41.64))
      X=37.42+E*(E*(512.62-380.37*E)-227.38)
      Y=-4.42+E*(35.43+E*(59.52*E-78.62))
      Z=4.
      CT2=CT*CT
      CT4=CT2*CT2
      CT10=CT4*CT4*CT2
      CT14=CT10*CT4
      CT20=CT10*CT10
      CT40=CT20*CT20
      CT100=CT40*CT40*CT20
      CSPNEL=W+X*CT4+Y*CT14+Z*CT100
      RETURN
C
 45   R=1.32+E*(3.95-6.71*E)
      S=4.19+E*(3.34+E*(62.18*E-44.86))
      CSPNEL=R+S*CT*CT
      RETURN
C
 50   A=-20.84+E*(88.88+E*(47.08*E-114.82))
      B=149.74+E*(E*(704.13-285.13*E)-562.82)
      CSPNEL=A+B*CT**4
      RETURN
C
 60   C=-11.37+E*(26.603+E*(E*(2.01675-.119009*E)-11.6091))
      AN=-9.68+E*(16.89+E*(.4469*E-4.384))
      N=AN+.5
      CSPNEL=C*(ABS(CT))**N
      RETURN
C
 70   CSPNEL=13.*CT**50
      RETURN
      END


      SUBROUTINE LOCATE(ILOC,XX,IT,X)
C
C  ROUTINE ALLOCATES A BIN ILOC FOR THE VALUE XX, ILOC=0,...,IT
C  LOWER LIMITS OF THE BINS 1,...,IT ARE STORED IN X
C
      DIMENSION X(*)
C
      DATA KK/0/
C
C
      IN=1
      ITP=IT
C
 10   KK=1-KK
      ILOC=IN+(ITP-IN+KK)/2
      IF(ITP.LE.IN)GOTO 30
C
      IF(XX.GE.X(ILOC))GOTO 20
      ITP=ILOC-1
      GOTO 10
C
 20   IN=ILOC
      GOTO 10
C
 30   IF(ILOC.NE.1)RETURN
      IF(XX.LT.X(1))ILOC=0
      RETURN
C
      END
