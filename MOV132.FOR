      SUBROUTINE MOVEX(DT)
      use global
C  MOVES IN SPACE
C  UPDATES DENSITY
      INCLUDE 'PARTIC15'
      COMMON/ENERGY/EKIN,EPOT,EPR,APR,ETOG,PARTI(NIDP),PARTIG(NIDP)
     E  ,ANPAR,ANPARG,ANPR(6),ANPRG(6)
     N  ,EKINT,EKINZ,ECINT,ECINZ
     N  ,EKINTG,EKINZG
     E  ,PPRX,PPRY,PPRZ,PPRZL,PGOX,PGOY,PGOZ,PGOZL
     R  ,GST,BGSTX,BGSTY,BGSTZ
     G  ,PRSKT0,PRSP0,PRSKL0,PRSXZ0
      COMMON/RAP/YBEAM,YCM,PF,RENAN,GG1,BB1,GG2,BB2,dmo
C
      COMMON/FREE/ANFPR(6)
C
      PARAMETER (B=.008,AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER (BD=.002225,AMD=AM0+AM0-BD)
      PARAMETER (RDEM=1.)
      PARAMETER (AMD2=AMD+AMD)
      PARAMETER (AMB=AM0-B)
C
      INCLUDE 'SIZE2'
      INCLUDE 'SIZEC2'
      INCLUDE 'SURF'
C
      PARAMETER (CSUR=.067)  !*(1.-MO*.15))      !coef in surface energy in GeV*fm**5
      PARAMETER (QDWLK=(.5/DWL)**2,QDLLK=(.5/DLL)**2)
      PARAMETER (CSUQW=CSUR*QDWLK,CSUQL=CSUR*QDLLK)
C
*      PARAMETER (DUD=.015)
      PARAMETER (DUD=.006)
*      PARAMETER (DUD=.000)
C
      COMMON/CLOPA/IX,IY,IZ,CALO
      LOGICAL CALO
      COMMON/CCLOPA/IXB,IXF,IYB,IYF,IZB,IZF,XC,YC,ZC,IIVOV
C
      COMMON/CPRI/PRI,TIM,IZCEN
      LOGICAL PRI
C
      COMMON/CPA/RHOPAM,UUP
c
      common/mut/pxpt,pypt,pzpt,empt,nopt
      common/hu/bxpt,bypt,bzpt,gmpt
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
      PARAMETER (RHODU=.160/15.)
*      PARAMETER (RHODU=.160/8.)
*      PARAMETER (RHODU=.160/10.)
      LOGICAL LPART
      COMMON/CENTRO/ENGAIN,ENTROT,EDGAIN,EDTROT,DNCELL,DDCELL
     C  ,EBGAIN,EBTROT,DBCELL
     e  ,eptro,eptrog,anuc,anucg
C
      COMMON/VELOC/B1X,B1Z,B2X,B2Z
C
      COMMON/CHARGE/R1,X1,Z1,R2,X2,Z2,ZCEN
C
      COMMON/COOLC/PFRHO,IPTCO,UPO1,DUPNO1
C
      PARAMETER (RHOCO=.160/8.)
*      LOGICAL MARK,NMARK
C
      COMMON/COMPSU/FUZ
C
      COMMON/RHOPAR/RHOPAR
C
      PARAMETER (ELQP=1.4400E-3)
      PARAMETER (ELQ=ELQP*FCOU)
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
c
      common/cseed/iseed
C
      PARAMETER (AN0=.160)
C
      COMMON/BOXES/EKI(NIDP),EKIT(NIDP)
     B  ,EKIZ(NIDP),EKITC(NIDP),EKIZC(NIDP)
     O  ,ECVZ(NIDP),ECVT(NIDP),EISH(NIDP)
C
      PARAMETER (NET=40,DET=.015,DETC=.005,DNGL=30.)
      DIMENSION DEKT(NET,NIDP),DEKTS(NET,NIDP)
     D  ,DEKTC(NET,NIDP),DEKTCS(NET,NIDP)
      DIMENSION ECOV(3:5),ECON(5)
C
      dimension pan1(3),pan2(3)
** f/Ajit
      dimension panr(3)
** end f/Ajit
c
      parameter(nbbc=40)
      dimension abbc(nbbc),bccd(nbbc),bcdd(nbbc,6)
      dimension epid(6),npid(6)
c  for Roy
*      parameter(nyy=30)
*      dimension anyy(nyy)
c  end f/Roy
Cc  pressure
C      common/pres/ppvc
C
      COMMON/CRPI/RHOPI,REFIN
      DIMENSION NFRA(1:6),ROFRA(1:6)
      DIMENSION NFRE(1:6),ROFRE(1:6)
C
      DIMENSION FRX(-1:1),FRY(-1:1),FRZ(-1:1)
cc
c      dimension rrpt(3,2),npt(2)
C
c  f/Mihai
      parameter(nafi=8)
      dimension navi(nafi),bavi(nafi),ekia(nafi),ecola(nafi)
      dimension ekias(nafi),ecolas(nafi)
      dimension etha(nafi),ethas(nafi)
c  end f/Mihai
      integer*2 idic,iit,iix,iiy,iiz
C
      IF(FIRST)THEN
        PI=4.*ATAN(1.)
        PI4=4.*PI
        FRHO=1./(NQU*DWLK*DLL)
        dmo=(gg1*abs(bb1)+gg2*abs(bb2))*amb+4.*pf
        dmo=dmo/(nl+nl+1)
        frmo=1./(nqu*dmo**3)
        FZHO=PI4*ELQ/(NQU*DWLK*DLL)
        IF(COU)THEN
          FUZ=.5/(PI4*MAX(ELQ,1E10))
          FDO=ELQ/NQU
*          DO IZ=-NL,NL
*            DO IY=-NT,NT
*              DO IX=-NT,NT
*                KINT(IX,IY,IZ)=0
*              ENDDO
*            ENDDO
*          ENDDO
           NSTEP=20
        ENDIF
c  for Roy
*        ymin=-.4-.2*ybeam
*        ymax=1.2*ybeam+.4
*        dyy=(ymax-ymin)/nyy
*        open(46,file='royrp.',status='UNKNOWN')
*        write(46,*)'ymin = ',ymin,'  ymax = ',ymax,'  dyy = ',dyy
c  end f/Roy
*        OPEN(40,FILE='LOLA2.',STATUS='UNKNOWN')
C
        FIRST=.FALSE.
        ANGL=DNGL*PI/180.
        IF(ANGL.LT..5*PI-1E-4)THEN
          FTZK=TAN(ANGL)**2
        ELSE
          FTZK=1E30
        ENDIF
        CANGL=SIN(ANGL)
        FET=1./((PI+PI)*(CANGL+CANGL)*DET*NQU)
        FETC=1./((PI+PI)*(CANGL+CANGL)*DETC*NQU)
      ELSE
*        NSTEP=2
      ENDIF
C
      IF(DT.EQ.0..AND.COU)THEN
        IF(IPTCO.EQ.0)THEN
          DO IZ=-NL,NL
            Z=IZ*DLL
            ZL1K=(Z-Z1)**2
            ZL2K=(Z-Z2)**2
            DO IY=-NT,NT
              Y=IY*DWL
              YK=Y*Y
              YL1K=ZL1K+YK
              YL2K=ZL2K+YK
              DO IX=-NT,NT
                X=IX*DWL
                RL1=SQRT(YL1K+(X-X1)**2)
                RL2=SQRT(YL2K+(X-X2)**2)
                UC(IX,IY,IZ)=UCS(IZ1,R1,RL1)+UCS(IZ2,R2,RL2)
              ENDDO
            ENDDO
          ENDDO
*          DO IZ=-NL,NL
*            WRITE(40,*)UC(0,0,IZ)
*          ENDDO
        ELSE
          IF(IPTCO.EQ.1)THEN
            RC=R1
            IZC=IZ1
          ELSE
            RC=R2
            IZC=IZ2
          ENDIF
          DO IZ=-NL,NL
            Z=IZ*DLL
            ZK=Z*Z
            DO IY=-NT,NT
              Y=IY*DWL
              YK=Y*Y+ZK
              DO IX=-NT,NT
                X=IX*DWL
                RL=SQRT(YK+X*X)
                UC(IX,IY,IZ)=UCS(IZC,RC,RL)
              ENDDO
            ENDDO
          ENDDO
        ENDIF
      ENDIF
c
      IF(DT.EQ.0.)THEN
** f/Ajit
        ener=0.
        depotr=0.
        ekinr=0.
        if(iframe.eq.1)then
          bexr=b1x
          bezr=b1z
          xxcr=x1
          zzcr=z1
          radrk=(r1*1.18)**2
        elseif(iframe.eq.2)then
          bexr=b2x
          bezr=b2z
          xxcr=x2
          zzcr=z2
          radrk=(r2*1.18)**2
        else  !0 implied
          bexr=0.
          bezr=0.
          xxcr=0.
          zzcr=zcen
          radrk=(2.*(r1+r2)*1.5)**2
        endif
        ger=1./sqrt(1.-bexr*bexr-bezr*bezr)
** end f/Ajit
      ENDIF
C
c f/Mihai
      flowan=23./180.*pi
      wid=.23
      cth=cos(flowan)
      sth=sin(flowan)
      barbo=0.
      ekbo=0.
      ekbos=0.
      ek2bo=0.
      ek2bos=0.
      ecbo=0.
      ecbos=0.
      ec2bo=0.
      ec2bos=0.
      etbo=0.
      etbos=0.
      et2bo=0.
      et2bos=0.
      babo=0.
      ba2bo=0.
c end f/Mihai
      IF(DT.EQ.0.)GOTO 52
** f/Ajit
      if(ener.ne.0.)then
        ag=sqrt((ener+am0)**2+panr(1)**2+panr(3)**2)
        if(ag.gt.0.)then
          bexr=panr(1)/ag
          bezr=panr(3)/ag
        else
          bexr=0.
          bezr=0.
        endif
        ger=1./sqrt(1.-bexr*bexr-bezr*bezr)
        fnew=dt/5.
        fold=1.-fnew
        xxcr=(fnew*xxccr+fold*xxcr)
        zzcr=(fnew*zzccr+fold*zzcr)
        radrk=(fnew*raadrk+fold*radrk)          !slowing down the changes
      endif
** end f/Ajit
C  EMITTED/BOUND MANAGEMENT
C  NOTE: AFTER COLLISION STAGE THOSE THAT HAVE COLLIDED ARE MARKED WITH
C        NEGATIVE IPTI; AFTER MOVEX NEGATIVE IPTI INDICATES EMITTED PTCLE
      anbou=0.
      eboul=0.
      ebout=0.
c  patch for equilibrium pion number
      eqpion=0.
      BARIT=0.
      EKICT=0.
      baril=0.
      barin=0.
c  end patch
      IF(PENT)THEN
        do i=1,nbbc
          abbc(i)=0.
          bccd(i)=0.
          do j=1,6
            bcdd(i,j)=0.
          enddo
        enddo
      ENDIF
      do j=1,6
        epid(j)=0.
        npid(j)=0
      enddo
      DO I=1,NIDP
        EKITC(I)=0.
        EKIZC(I)=0.
        ECVT(I)=0.
        ECVZ(I)=0.
        EISH(I)=0.
        DO J=1,NET
          DEKTC(J,I)=0.
        ENDDO
      ENDDO
*      elopro=0.
*      ecopro=0.
*      etopro=0.
      ECINT=0.
      ECINZ=0.
      nfpri=0
      RHOPAR=0.
      PARR=0.
c f/Mihai
      do i=1,nafi
        navi(i)=0
        bavi(i)=0
        ekia(i)=0.
        ekias(i)=0.
        ecola(i)=0.
        ecolas(i)=0.
        etha(i)=0.
        ethas(i)=0.
      enddo
c end f/Mihai
      DO 50 IZ=-NL,NL
      IZB=MAX(-NL,IZ-1)
      IZF=MIN(NL,IZ+1)
      do k=0,nsit
      VZE(IZ,k)=0.
      vxze(iz,k)=0.
      enddo
      DO 40 IY=-NT,NT
      IYB=MAX(-NT,IY-1)
      IYF=MIN(NT,IY+1)
      DO 30 IX=-NT,NT
      IXB=MAX(-NT,IX-1)
      IXF=MIN(NT,IX+1)
C  THESE ARE FOR LOCVE ...
      IMXXYZ=IMX(IX,IY,IZ)
      IF(IMXXYZ.LT.0)GOTO 30
      IMNXYZ=IMN(IX,IY,IZ)
      CALO=.TRUE.
      UCI=0.
      THOI=0.
      UTTI=0.
      UTI=0.
      UTEI=0.
      IF(COU)THEN
        UCI=UC(IX,IY,IZ)
        IF(POT)THEN
          UTTI=UTTU(IX,IY,IZ)
          UTI=UTU(IX,IY,IZ)
          UTEI=UTTE(IX,IY,IZ)
*          UTEI=.5*UTI*THO(IX,IY,IZ)/MAX(RHO(IX,IY,IZ),.16E-4)
        ENDIF
      ENDIF
      IF(POT)THEN
        UUP=UU(IX,IY,IZ)
      ELSE
        UUP=0.
      ENDIF
      ITIM=NINT(20.*TIM)
      IF(LCOR)THEN
*        MARK=RHO(IX,IY,IZ).LT.RHOCO
*        NMARK=.NOT.MARK
        IRHO=NINT(1E4*RHO(IX,IY,IZ))
        DO 200 I=IMNXYZ,IMXXYZ
        IP=IPO(I)
        IDP=ID(IP)
        IF(IDP.LE.0)GOTO 200
*        IF((MARK.AND.ITCR(IP).LT.0).OR.IPTI(IP).LT.0)THEN
        IF(IPTI(IP).LT.0)THEN
          IRCR(1,IP)=NINT(100.*XX(IP))
          IRCR(2,IP)=NINT(100.*YY(IP))
          IRCR(3,IP)=NINT(100.*ZZ(IP))
          ITCR(IP)=ITIM
*          IPCR(1,IP)=PX(IP)*1E3
*          IPCR(2,IP)=PY(IP)*1E3
*          IPCR(3,IP)=PZ(IP)*1E3
*          IECR(IP)=(EE(IP)+ZPA(IDP)*UCI+TIZO(IDP)*THOI)*1E3
          IRHCR(IP)=IRHO
*        ELSEIF(NMARK)THEN
*          ITCR(IP)=-1
        ENDIF
 200    CONTINUE
      ENDIF
      IF(PRI)CALL LOCVE    !note this may need to be corrected if ipt varied ...
      CALL LOCVEt   !note this may need to be corrected if ipt varied ...
      RHOX=RHO(IX,IY,IZ)
      RT=RHOX/AN0
      IF(POT)THEN
        EPP=EPO(RT)
      ELSE
        EPP=0.
      ENDIF
      IF(ABS(IX).LT.NT)
     E  EPP=EPP+CSUQW*(RHO(IX+1,IY,IZ)-RHO(IX-1,IY,IZ))**2
      IF(ABS(IY).LT.NT)
     E  EPP=EPP+CSUQW*(RHO(IX,IY+1,IZ)-RHO(IX,IY-1,IZ))**2
      IF(ABS(IZ).LT.NL)
     E  EPP=EPP+CSUQL*(RHO(IX,IY,IZ+1)-RHO(IX,IY,IZ-1))**2
      IF(RHOx.NE.0.)EPP=EPP/RHOx
      IF(PENT)spaax=spaa(ix,iy,iz)
      IRFOI=NINT(RT*1E3)
      DO 10 I=IMNXYZ,IMXXYZ
      IP=IPO(I)
      IDP=ID(IP)
      IF(IDP.LE.0)GOTO 10
      BARI=BAR(IDP)
      MDE(IP)=ETOM*(ZPA(IDP)*UCI+TIZO(IDP)*UTI+bari*UTTI)
      mdep(ip)=etom*(.5*zpa(idp)*uci+bari*UTEI)
*      mdep(ip)=.5*mde(ip)   !test
      AMVI=AMV(IP)
      if(pot)then
        epsh(ip)=ee(ip)-sqrt(px(ip)**2+py(ip)**2+pz(ip)**2+amvi**2)
        if(lmo)then
          epsh(ip)=epsh(ip)+bari*(epp-uup)
        else
          epsh(ip)=epsh(ip)+bari*(amvi+bari*uup)/ee(ip)*(epp-uup)
        endif
      else
        epsh(ip)=0.
      endif
      IPT=NINT((ABS(IPTI(IP))+.1*getRan())/MAX(BARI,1.))
      IF(NCS(IP).NE.0.OR.IDP.GE.3)THEN
        PARR=PARR+BARI
        RHOPAR=RHOPAR+BARI*RHOX
      ENDIF
      if(ncs(ip).lt.0..or.ipti(ip).lt.0.)then
*      if(((NCS(ip).NE.0.or.idp.ge.3).and.rhox.ge..160*.3)
*     i  .or.NCS(ip).LT.0)then
*      if(((NCS(ip).NE.0.or.idp.ge.3).and.rhox.ge..160*.7)
*     i  .or.ipti(ip).LT.0)then
        IF(PENT)isrx(ip)=100.*spaax
        bxpti=pxpt-px(ip)
        bypti=pypt-py(ip)
        bzpti=pzpt-pz(ip)
        gmpti=empt-ee(ip)
        if(Gmpti.gt.1e-5)then !.and.nopt.gt.4)then
          bxpti=bxpti/gmpti
          bypti=bypti/gmpti
          bzpti=bzpti/gmpti
          ag=1.-(bxpti*bxpti+bypti*bypti+bzpti*bzpti)
        else
          ag=-1.
        endif
        if(ag.gt.0.)then
          gmpti=1./sqrt(ag)
        else
          bxpti=0.
          bypti=0.
          bzpti=0.
          gmpti=1.
        endif
C  NOTE: IT MIGHT BE DONE BETTER FOR MO-DEP, BUT THE FINITE-PTCLE
C    EFFECTS COULD BE MORE IMPORTANT
        CALL LOREN(-BXPti*GMPti,-BYPti*GMPti
     L    ,-BZPti*GMPti,GMPti,PX(IP),PY(IP),PZ(IP),EE(IP)
     O    ,PCX,PCY,PCZ,EC)
        igx(ip)=bxpti*gmpti*1e3
        igy(ip)=bypti*gmpti*1e3
        igz(ip)=bzpti*gmpti*1e3
        BARV=1E3/MAX(BARI,1.)
        IBT(IP)=SQRT(PCX*PCX+PCY*PCY)*BARV
        IBZ(IP)=PCZ*BARV
        IFT(IP)=SQRT(PX(IP)**2+PY(IP)**2)*BARV
        IFZ(IP)=PZ(IP)*BARV
*       ICT(IP)=SQRT(1E3*BARV*(BXPti*bxpti+BYPti*bypti)*.5*amv(ip))
*       ICZ(IP)=SQRT(1E3*BARV*BZPti*bzpti*.5*amvI)
c  collective energy redesigned  (1e3*barv u sqrt f/efficient storage)
        GFA=GMPTI*GMPTI/(1.+GMPTI)    ! about 0.5
**temporary modification for Roy
**        gmptr=1./sqrt(1.-bxpti*bxpti-bypti*bypti)
**        gfa=gmptr*gmptr/(1.+gmptr)
**end of the modification for Roy
        IF(EC.GT.0.)THEN
          ICT(IP)=SQRT(1E3*BARV*(BXPTI*BXPTI+BYPTI*BYPTI)*GFA*EC)
          ICZ(IP)=SQRT(1E3*BARV*BZPTI*BZPTI*GFA*EC)
        ELSE
          ICT(IP)=0
          ICZ(IP)=0
        ENDIF
        EFSH=EC-SQRT(PCX*PCX+PCY*PCY+PCZ*PCZ+AMVI*AMVI)
**        EFSH=EFSH+.5*PETOM*MDE(IP)
        EFSH=EFSH+PETOM*MDEp(IP)
        IF(LMO)THEN
          EFSH=EFSH+BARI*(EPP-UUP)
        ELSE
          EFSH=EFSH+BARI*(AMVI+BARI*UUP)/EE(IP)*(EPP-UUP)
        ENDIF
        IFSH(IP)=SIGN(SQRT(1E3*BARV*ABS(EFSH)),EFSH)
      ENDIF
c  f/Mihai
c  note: it might be done better for mo-dep...
      if(pri.and.bari.gt.0..and.rhox.gt..2*an0)then
        qx=1.+wid*rang(iseed)
        qy=wid*rang(iseed)
        qq=sqrt(qx*qx+qy*qy)
        qx=qx/qq
        qy=qy/qq
        pxq=px(ip)*qx+py(ip)*qy
        pyq=py(ip)*qx-px(ip)*qy
        pxr=pxq*cth-pz(ip)*sth
        pzr=pz(ip)*cth+pxq*sth
        prk=pxr*pxr+pyq*pyq
        pr=sqrt(prk)
        if(pr.gt.0.)then
          thr=atan2(pr,pzr)*180./pi
          if(thr.gt.80..and.thr.lt.110.)then
            phir=atan2(abs(pyq),abs(pxr))
            iafi=2.*phir/pi*nafi+1.
            if(iafi.ge.1.and.iafi.le.nafi)then
              navi(iafi)=navi(iafi)+1.
              bavi(iafi)=bavi(iafi)+bari
              bgx=igx(ip)*1e-3
              bgy=igy(ip)*1e-3
              bgz=igz(ip)*1e-3
              gam=sqrt(1.+bgx*bgx+bgy*bgy+bgz*bgz)
              deco=(gam-1.)*amvi
              ecola(iafi)=ecola(iafi)+deco
              ecolas(iafi)=ecolas(iafi)+deco*deco/bari
              pk=pxr*pxr+pyq*pyq+pzr*pzr
              deki=pk/(amvi+sqrt(pk+amvi*amvi))
              ekia(iafi)=ekia(iafi)+deki
              ekias(iafi)=ekias(iafi)+deki*deki
              CALL LOREN(-bgx,-bgy
     l          ,-bgz,gam,px(ip),py(ip),pz(ip),ee(ip)
     o          ,pccx,pccy,pccz,ecc)
              pcck=pccx*pccx+pccy*pccy+pccz*pccz
              deth=pcck/(amvi+sqrt(pcck+amvi*amvi))
!              deth=deki-deco  !!
              etha(iafi)=etha(iafi)+deth
              ethas(iafi)=ethas(iafi)+deth*deth
            endif
          endif
        endif
      endif
c  end f/Mihai
      IF(IDP.EQ.1.OR.IDP.EQ.2)THEN
        IF(IPTI(IP).LT.0)THEN
          ITFO(IP)=ITIM
          IRFO(IP)=IRFOI
        ENDIF
      ENDIF
      IF(IDP.GE.3.AND.IDP.LE.6)THEN
        IF(NCS(IP).LT.0)THEN
*        IF(ipti(IP).LT.0)THEN
          ITFO(IP)=ITIM
          IRFO(IP)=IRFOI
        ENDIF
      ENDIF
      if(idp.ge.7.and.idp.le.12)then         !delta's
        itfo(ip)=rhox*1e3/an0
      endif
      NCS(IP)=ABS(NCS(IP))
      IIPT=ABS(IPTI(IP))
      IF(IDP.GT.6)THEN
        IPTI(IP)=-IIPT               !deltas are always participants
      ELSE                          !and pions
        IPTI(IP)=IIPT                !tentatively all other bound
      ENDIF
 10   CONTINUE
      if(pri.and.rhox.gt.rhodu)then
        if(calo)call locve
        abou=0.
        do i=imnxyz,imxxyz
          ip=ipo(i)
          idp=id(ip)
          if(idp.gt.0)abou=abou+bar(idp)
        enddo
        anbou=anbou+abou
        if(.not.nopaul1)then
          eboul=eboul+abou*bzpa1**2
          ebout=ebout+abou*(bxpa1**2+bypa1**2)
        endif
      endif
*      IF(PRI.AND.IX.EQ.0.AND.IY.EQ.0)THEN
      IF(PRI.AND.(IX.ge.0.and.ix.le.nsit).AND.IY.EQ.0)THEN
        if(calo)CALL LOCVE
        VZE(IZ,ix)=BZPA1
        vxze(iz,ix)=bxpa1
      ENDIF
      IF(PRI.AND.IZ.EQ.0.AND.IX.EQ.0)THEN
        IF(CALO)CALL LOCVE
        VYE(IY)=BYPA(1)
      ENDIF
      IF(RHOX.GT.RHODU)GOTO 25     !if density too low finish
      IF(CALO)CALL LOCVE
      LPART=NOPAUL(1).AND.NOPAUL(2)
      DO 20 I=IMNXYZ,IMXXYZ
      IP=IPO(I)
      IDP=ID(IP)
      IF(IDP.LE.0)GOTO 20
      IF(IDP.GT.6)GOTO 20             !deltas have been done already
      IIPT=IPTI(IP)                    !positive
      IF(LPART)THEN
        IPTI(IP)=-IIPT                 !empty momentum space
        GOTO 19
      ENDIF
      IPTI(IP)=-IIPT                   !tentatively participant
      DO 18 IPT=1,2
      IF(NOPAUL(IPT))GOTO 18
      BPI=BXPA(IPT)*PX(IP)+BYPA(IPT)*PY(IP)+BZPA(IPT)*PZ(IP)
**      EIC=GMPA(IPT)*(EE(IP)+PETOM*MDE(IP)-BPI)
**      eic=gmpa(ipt)*(ee(ip)-bpi)+tizo(idp)*thoi
**      IF(EIC.LT.AMS(IDP))IPTI(IP)=IIPT
*      eic=gmpa(ipt)*(ee(ip)+tizo(idp)*thoi-bpi)
      eic=gmpa(ipt)*(ee(ip)+tizo(idp)*UTI+bari*UTTI-bpi)
      IF(EIC.LT.AMS(IDP)-BAR(IDP)*DUD)IPTI(IP)=IIPT
** ! change in emitted def of protons f/residue 4/19/99  ?? 7/6/03
 18   CONTINUE
 19   CONTINUE
      IF(IPTI(IP).LT.0)THEN     !this is only statistics
        IF(IDP.EQ.1)THEN
          if(NCS(ip).NE.0)nfpri=nfpri+1
        ENDIF
      ENDIF
 20   CONTINUE
 25   continue
c  patch for equilibrium pion number
      if(pri)then
        barl=0.
        do 26 i=imnxyz,imxxyz
          ip=ipo(i)
          idp=id(ip)
          if(idp.le.0)goto 26
*          barin=barin+bar(idp)
          if(nint(xx(ip)/dwl).ne.ix)goto 26
          if(nint(yy(ip)/dwl).ne.iy)goto 26
          if(nint(zz(ip)/dll).ne.iz)goto 26
          barl=barl+bar(idp)
 26     continue
*        barin=barin+barl
        if(barl.le.0.)goto 30
c
        if(calo)call locve
        if(nopaul1)goto 30
        bgx=-bxpa1*GMP1
        bgy=-bypa1*GMP1
        bgz=-bzpa1*GMP1
        gg=GMP1
c
        bari=0.
        ekic=0.
c
        is=1
 840    continue
        do 900 iiz=izb,izf
        do 890 iiy=iyb*is,iyf*is,is
        do 880 iix=ixb,ixf
        iimx=imx(iix,iiy,iiz)
        if(iimx.lt.0)goto 880
        iimn=imn(iix,iiy,iiz)
        do 870 ii=iimn,iimx
        iip=ipo(ii)
        iidp=id(iip)
        if(iidp.le.0)goto 870
        if(nint(xx(iip)/dwl).ne.iix)goto 870
        if(nint(yy(iip)/dwl).ne.iiy)goto 870
        if(nint(zz(iip)/dll).ne.iiz)goto 870
        call loren(bgx,bgy
     l    ,bgz,gg,px(iip),py(iip)*is,pz(iip),ee(iip)
     o    ,pcix,pciy,pciz,eci)
        if(iix.eq.ix)then
          frix=.5
        else
          frix=.5*(1.5-abs(xx(iip)/dwl-ix))
        endif
        if(iiy.eq.iy*is)then
          friy=.5
        else
          friy=.5*(1.5-abs(yy(iip)/dwl-iy*is))
        endif
        if(iiz.eq.iz)then
          friz=.5
        else
          friz=.5*(1.5-abs(zz(iip)/dll-iz))
        endif
        frxyz=frix*friy*friz
        bari=bari+bar(iidp)*frxyz
        ekic=ekic+sqrt(pcix*pcix+pciy*pciy+pciz*pciz+amv(iip)**2)*frxyz
 870    continue
 880    continue
 890    continue
 900    continue
C
        if(symmy.and.is.eq.1)then
          is=-1
          goto 840
        endif
c
        EKICT=EKICT+EKIC
        BARIT=BARIT+BARI
        ekic=ekic/bari
*       anl=bari*frho/((izf-izb+1)*(iyf-iyb+1)*(ixf-ixb+1)*gg)
        anl=bari*frho/gg
        if(symmy)anl=.5*anl
        baril=baril+anl*gg
*        write(*,*)anl/an0
        rapi=pibar(anl,ekic)
        rapi=min(rapi,2.)
        eqpion=eqpion+rapi*barl
        barin=barin+barl
      endif
c  end patch
 30   CONTINUE
 40   CONTINUE
 50   CONTINUE
      RHOPAR=RHOPAR/MAX(PARR,1.)
c
*      write(*,*)'ec/el ',ecopro,elopro,ecopro+elopro,etopro
C
      IF(PRI)THEN
c  f/Mihai
        nrav=0
        nav=0
        barbo=0.
        ekbo=0.
        ekbos=0.
        ek2bo=0.
        ek2bos=0.
        ecbo=0.
        ecbos=0.
        ec2bo=0.
        ec2bos=0.
        etbo=0.
        etbos=0.
        et2bo=0.
        et2bos=0.
        babo=0.
        ba2bo=0.
        do iafi=1,nafi
          co2fi=cos(pi/nafi*(iafi-.5))
          navii=navi(iafi)
          nav=nav+navii
          if(navii.ne.0)then
            nrav=nrav+1
            ekia(iafi)=ekia(iafi)/navii
            ekias(iafi)=ekias(iafi)/navii-ekia(iafi)**2
            ekias(iafi)=max(ekias(iafi),0.)/navii
            ecola(iafi)=ecola(iafi)/bavi(iafi)
            ecolas(iafi)=ecolas(iafi)/bavi(iafi)-ecola(iafi)**2
            ecolas(iafi)=max(ecolas(iafi),0.)/navii
            etha(iafi)=etha(iafi)/navii
            ethas(iafi)=ethas(iafi)/navii-etha(iafi)**2
            ethas(iafi)=max(ethas(iafi),0.)/navii
            ekbo=ekbo+ekia(iafi)
            ekbos=ekbos+ekias(iafi)
            ek2bo=ek2bo+ekia(iafi)*co2fi
            ek2bos=ek2bos+ekias(iafi)*co2fi*co2fi
            ecbo=ecbo+ecola(iafi)
            ecbos=ecbos+ecolas(iafi)
            ec2bo=ec2bo+ecola(iafi)*co2fi
            ec2bos=ec2bos+ecolas(iafi)*co2fi*co2fi
            etbo=etbo+etha(iafi)
            etbos=etbos+ethas(iafi)
            et2bo=et2bo+etha(iafi)*co2fi
            et2bos=et2bos+ethas(iafi)*co2fi*co2fi
            barbo=barbo+bavi(iafi)
            bavi(iafi)=bavi(iafi)/navii
            babo=babo+bavi(iafi)
            ba2bo=ba2bo+bavi(iafi)*co2fi
          endif
        enddo
        if(nrav.ne.0)then
          barbo=barbo/nav
          ekbo=ekbo/nrav
          ekbos=sqrt(ekbos)/nrav
          ek2bo=2.*ek2bo/nrav
          ek2bos=2.*sqrt(ek2bos)/nrav
          ecbo=ecbo/nrav
          ecbos=sqrt(ecbos)/nrav
          ec2bo=2.*ec2bo/nrav
          ec2bos=2.*sqrt(ec2bos)/nrav
          etbo=etbo/nrav
          etbos=sqrt(etbos)/nrav
          et2bo=2.*et2bo/nrav
          et2bos=2.*sqrt(et2bos)/nrav
          babo=babo/nrav
          ba2bo=2.*ba2bo/nrav
        endif
c  end f/Mihai
c  insert for Roy
*        do i=1,nyy
*          anyy(i)=0.
*        enddo
c  end insert f/Roy
        if(anbou.ne.0.)then
          eboul=eboul/anbou*am0*.5
          ebout=ebout/anbou*am0*.5
          anbou=anbou/nqu
        endif
        write(*,*)'bound a, el, et ',anbou,eboul,ebout
        WRITE(*,'(1X,A,5(1X,F7.3))')'free p/n/d/3He/t',(anfpr(i),i=1,5)
        write(*,*)'int free p''s = ',nfpri/float(nqu)
     W    ,' RHOPAR = ',RHOPAR/AN0
c  patch for equilibrium pion number
        eqpion=eqpion/nqu
        barin=barin/nqu
        write(*,*)'eq. pi #: ',eqpion,EKICT/BARIT
     W    ,BARIl*VOLE,barin
c  end patch
      ENDIF
c
 52   continue


      if(pri)then
        v2=0.
        v2s=0.
        anv=0.
        do 510 ip=1,nq
          idp=abs(id(ip))
          if(idp.eq.0)goto 510
          if(abs(bar(idp)-1.).gt..1)goto 510
          pxi=px(ip)
          pyi=py(ip)
          pkt=pxi*pxi+pyi*pyi
          if(pkt.eq.0.)goto 510
          amvpk=amv(ip)**2
          pzi=pz(ip)
          pk=pkt+pzi*pzi
          ycp=log((sqrt(amvpk+pk)+pzi)/sqrt(amvpk+pkt))
          if(abs(ycp).lt..3*(ybeam-ycm))then
            anv=anv+1.
            dv2=(pxi*pxi-pyi*pyi)/pkt
            v2=v2+dv2
            v2s=v2s+dv2*dv2
          endif
  510   continue
        if(anv.gt.0.)then
          v2=v2/anv
          v2s=sqrt(max(v2s/anv-v2*v2,0.)/anv)
        endif
        write(*,*)'v2 = ',v2,' +- ',v2s
      endif
*      rewind(57)
* 520  continue
*      read(57,*,end=530)ip
*      if(id(ip).gt.0)then
*        iit=nint(20.*tim)
*        iix=nint(100.*xx(ip))
*        iiy=nint(100.*yy(ip))
*        iiz=nint(100.*zz(ip))
*        idic=id(ip)
*        write(58)ip,idic,iit,iix,iiy,iiz
*      endif
*      goto 520
* 530  continue

C
      DO IZ=-NL,NL
        DO IY=-NT,NT
          rhomo(iy,iz)=0.
        ENDDO
      ENDDO
C
      DO I=1,NIDP
        EKI(I)=0.
        EKIT(I)=0.
        EKIZ(I)=0.
        PARTI(I)=0.
        DO J=1,NET
          DEKT(J,I)=0.
        ENDDO
      ENDDO
** f/Ajit
      annur=0.
      zuur=0.
      do i=1,3
        panr(i)=0.
      enddo
      xxccr=0.
      zzccr=0.
      rrccrk=0.
      ainr=0.
      almor=0.
      ener=0.
      ekinr=0.
      depotr=0.
      eporr=0.
** end f/Ajit
      annu1=0.                 !for Bill/Betty
      annu2=0.
      do i=1,3
        pan1(i)=0.             !for Dave B.
        pan2(i)=0.
      enddo
      zuu1=0.
      zuu2=0.
      ene1=0.
      ene2=0.
      epo1=0.
      epo2=0.                  !for Bill/Betty
      eptro=0.
      anuc=0.
      ebtrot=0.
      edtrot=0.
      NPAR=0
      DO I=1,6
        ANPR(I)=0.
      ENDDO
*      z=0.
*      a=0.
      nne=0
      nnpi=0
      rhopi=0.
      DO I=1,6
        NFRA(I)=0
        ROFRA(I)=0.
        NFRE(I)=0
        ROFRE(I)=0.
      ENDDO
      pato=0.
*      quad=0.
*      bquad=0.
*      dipo=0.
*      bdipo=0.
      ancs=0.
c f/Mihai
      do i=1,nafi
        navi(i)=0
        bavi(i)=0
        ekia(i)=0.
        ekias(i)=0.
        ecola(i)=0.
        ecolas(i)=0.
        etha(i)=0.
        ethas(i)=0.
      enddo
c end f/Mihai
c f/Bozek
      fyb=0.
      fydb=0.
c end f/Bozek
*c  for Jorg
*        xtarg=0.
*        ztarg=0.
*        rktarg=0.
*        antarg=0.
*c  end for Jorg
      DO 350 IP=1,NQ        !!start of first big particle loop
      IDP=ID(IP)
      IDPA=ABS(IDP)
      IF(IDP.EQ.0)GOTO 350                 !all taken including gone
      IF(IDPA.EQ.16)GOTO 350
C
      yyn=py(ip)/dmo
      iy=nint(yyn)
      if(abs(iy).gt.1)goto 60
      xxn=px(ip)/dmo
      ix=nint(xxn)
      zzn=pz(ip)/dmo
      iz=nint(zzn)
      ixf=min(ix+1,nt)
      ixb=max(ix-1,-nt)
      frx(1)=.5*(xxn-ix+.5)
      frx(-1)=.5-frx(1)
      iyf=min(iy+1,nt)
      iyb=max(iy-1,-nt)
      fry(1)=.5*(yyn-iy+.5)
      fry(-1)=.5-fry(1)
      izf=min(iz+1,nl)
      izb=max(iz-1,-nl)
      frz(1)=.5*(zzn-iz+.5)
      frz(-1)=.5-frz(1)
      ixf=min(ix+1,nt)
      ixb=max(ix-1,-nt)
      izf=min(iz+1,nl)
      izb=max(iz-1,-nl)
      drhomo=bar(idpA)*frmo
      do iiz=izb,izf
        frzz=frz(iiz-iz)
        iiza=abs(iiz)
        fryy=frzz*fry(-iy)
        do iix=ixb,ixf
          frxx=fryy*frx(iix-ix)
          rhomo(iix,iiz)=rhomo(iix,iiz)+drhomo*frxx     !?density in momentum
        enddo
      enddo
 60   continue
      pato=pato+1.
      if(idpa.ge.13.and.idpa.le.15)then
        nnpi=nnpi+1
        rhopi=rhopi+iRfo(ip)
        ancs=ancs+abs(ncs(ip))
      endif
      IF(IDPA.GE.1.AND.IDPA.LE.6)THEN
        NFRA(IDPA)=NFRA(IDPA)+1
        ROFRA(IDPA)=ROFRA(IDPA)+IRFO(IP)
        IF(IPTI(IP).LT.0)THEN
          NFRE(IDPA)=NFRE(IDPA)+1
          ROFRE(IDPA)=ROFRE(IDPA)+IRFO(IP)
        ENDIF
      ENDIF
C
      AMVI=AMV(IP)
      PKT=PX(IP)**2+PY(IP)**2                    !used below
      PKZ=PZ(IP)**2
      PK=PKT+PKZ                                 !used below
      BARP=BAR(IDPA)
C
      IF(PRI)THEN
c  f/Mihai
c  note: it might be done better for mo-dep...
      if(barp.gt.0.)then
        qx=1.+wid*rang(iseed)
        qy=wid*rang(iseed)
        qq=sqrt(qx*qx+qy*qy)
        qx=qx/qq
        qy=qy/qq
        pxq=px(ip)*qx+py(ip)*qy
        pyq=py(ip)*qx-px(ip)*qy
        pxr=pxq*cth-pz(ip)*sth
        pzr=pz(ip)*cth+pxq*sth
        prk=pxr*pxr+pyq*pyq
        pr=sqrt(prk)
        thr=atan2(pr,pzr)*180./pi
        if(thr.gt.80..and.thr.lt.110.)then
          phir=atan2(abs(pyq),abs(pxr))
          iafi=2.*phir/pi*nafi+1.
          if(iafi.ge.1.and.iafi.le.nafi)then
            navi(iafi)=navi(iafi)+1.
            bavi(iafi)=bavi(iafi)+barp
            bgx=igx(ip)*1e-3
            bgy=igy(ip)*1e-3
            bgz=igz(ip)*1e-3
            gam=sqrt(1.+bgx*bgx+bgy*bgy+bgz*bgz)
            deco=(gam-1.)*amvi
            ecola(iafi)=ecola(iafi)+deco
            ecolas(iafi)=ecolas(iafi)+deco*deco/barp
            deki=pk/(amvi+sqrt(pk+amvi*amvi))
            ekia(iafi)=ekia(iafi)+deki
            ekias(iafi)=ekias(iafi)+deki*deki
            CALL LOREN(-bgx,-bgy
     l        ,-bgz,gam,px(ip),py(ip),pz(ip),ee(ip)
     o        ,pccx,pccy,pccz,ecc)
            pcck=pccx*pccx+pccy*pccy+pccz*pccz
            deth=pcck/(amvi+sqrt(pcck+amvi*amvi))
!            deth=deki-deco  !!
            etha(iafi)=etha(iafi)+deth
            ethas(iafi)=ethas(iafi)+deth*deth
          endif
        endif
      endif
c  end f/Mihai
c f/Bozek
*      if(barp.gt..1)then
*        amvpk=amv(ip)**2
*        ycp=log((sqrt(amvpk+pk)+pz(ip))/sqrt(amvpk+pkt))
*        ycpn=ycp/ycm
*        if(abs(ycpn).lt..25)then
*          if(ycp.gt.0.)then
*            sy=1.
*          else
*            sy=-1.
*          endif
*          fyb=fyb+px(ip)/barp*sy !ycpn
*          fydb=fydb+abs(ycpn) !**2
*        endif
*      endif
c end f/Bozek
c  for Roy
*        amvpk=amv(ip)**2
*        ycp=log((sqrt(amvpk+pk)+pz(ip))/sqrt(amvpk+pkt))
*        ycl=ycm+ycp
*        iyy=(ycl-ymin)/dyy+1.
*        if(iyy.ge.1.and.iyy.le.nyy)anyy(iyy)=anyy(iyy)+bar(idpa)
*     i    /(nqu*dyy)
c  end f/Roy
      ENDIF
      IF(IDPA.GE.3.OR.NCS(IP).NE.0)THEN
        if(pent)then
          if(idpa.ge.3.and.idpa.le.6)edtrot=edtrot+isr(ip)
          if(idpa.le.6)ebtrot=ebtrot+isr(ip)
          entrot=entrot+isr(ip)
        endif
        IF(PRI)THEN
          BARV=1E-3*MAX(BARP,1.)
          PFT=BARV*IFT(IP)
          PFZ=BARV*IFZ(IP)
          PFKT=PFT*PFT
          PFKZ=PFZ*PFZ
          PFK=PFKT+PFKZ
          EPT=PFKT/(AMV(IP)+SQRT(AMV(IP)**2+PFK))
          EKIT(IDPA)=EKIT(IDPA)+EPT
          EPZ=PFKZ/(AMV(IP)+SQRT(AMV(IP)**2+PFK))
          EKIZ(IDPA)=EKIZ(IDPA)+EPZ
          IF(FTZK*PFKT.GE.PFKZ)THEN
*           IEPT=INT((EPT+EPZ)/DET+.5)  !lowest 1/2 bin skipped (saturation)
            IEPT=(EPT+EPZ)/DET+.1
            IF(IEPT.GE.1.AND.IEPT.LE.NET)
     I        DEKT(IEPT,IDPA)=DEKT(IEPT,IDPA)+1.
          ENDIF
          PCT=BARV*IBT(IP)
          PCZ=BARV*IBZ(IP)
          PCKT=PCT*PCT
          PCKZ=PCZ*PCZ
          PCK=PCKT+PCKZ
          ECPT=PCKT/(AMV(IP)+SQRT(AMV(IP)**2+PCK))
          EKITC(IDPA)=EKITC(IDPA)+ECPT
          ECPZ=PCKZ/(AMV(IP)+SQRT(AMV(IP)**2+PCK))
          EKIZC(IDPA)=EKIZC(IDPA)+ECPZ
          IF(FTZK*PCKT.GE.PCKZ)THEN
*           IEPT=INT((ECPT+ECPZ)/DETC+.5) !lowest 1/2 bin skipped (saturation)
            IEPT=(ECPT+ECPZ)/DETC+.1
            IF(IEPT.GE.1.AND.IEPT.LE.NET)
     I        DEKTC(IEPT,IDPA)=DEKTC(IEPT,IDPA)+1.
          ENDIF
          ECT=REAL(ICT(IP))**2
          ECT=BARV*ECT*1E-3
          ECINT=ECINT+ECT
          ECZ=REAL(ICZ(IP))**2
          ECZ=BARV*ECZ*1E-3
          ECINZ=ECINZ+ECZ
          ECVT(IDPA)=ECVT(IDPA)+ECT
          ECVZ(IDPA)=ECVZ(IDPA)+ECZ
*         write(*,*)'ept = ',1e3*ept,' ecpt = ',1e3*ecpt
*    w      ,' ect = ',1e3*ect
          bbc=sqrt(MAX(2.*(ect+ecz)/amv(ip),0.))
          ibbc=1.+bbc*nbbc
*          write(*,*)'bbc = ',bbc,'  ibbc = ',ibbc
          if(3.*ecz.lt.ect.and.ibbc.le.nbbc)
     i      abbc(ibbc)=abbc(ibbc)+1.
          EFSH=IFSH(IP)**2*BARV*1E-3
          IF(IFSH(IP).LT.0)EFSH=-EFSH
          EISH(IDPA)=EISH(IDPA)+EFSH
          IF(PENT)THEN
            spaax=.01*isrx(ip)
            ispaa=1.+spaax*nbbc/9.
            if(ispaa.le.nbbc.and.ispaa.ge.1)then
              bccd(ispaa)=bccd(ispaa)+1.
              if(idpa.le.6)bcdd(ispaa,idpa)=bcdd(ispaa,idpa)+1.
            endif
          ENDIF
        ENDIF
      ENDIF
      ipt=ipti(ip)
      if(barp.gt.0..and.idp.gt.0.)then   !and.ipt.gt.0)then
** f/Ajit
!        write(*,*)'xxcr,zzcr,radrk:',xxcr,zzcr,radrk
        if((xx(ip)-xxcr)**2+yy(ip)**2
     f      +(zz(ip)-zzcr)**2.lt.radrk)then
          almor=almor+pz(ip)*(xx(ip)-xxcr)+px(ip)*(zzcr-zz(ip))
          ainr=ainr+barp*((xx(ip)-xxcr)**2+(zz(ip)-zzcr)**2)
          xxccr=xxccr+barp*xx(ip)
          zzccr=zzccr+barp*zz(ip)
          rrccrk=rrccrk+barp*(xx(ip)**2+yy(ip)**2+zz(ip)**2)
          annur=annur+barp
          if(pent)eporr=eporr+isr(ip)
          panr(1)=panr(1)+px(ip)
          panr(2)=panr(2)+py(ip)
          panr(3)=panr(3)+pz(ip)
!          write(*,*)'panr:',panr
          amvu=sqrt(MAX(ee(ip)**2-pk,0.))
          uub=amvu-amvi
          depotr=depotr+ee(ip)-sqrt(amvi*amvi+pk)-amvu/ee(ip)*uub
          ekinr=ekinr+sqrt(amvi*amvi+pk)
          zuur=zuur+zpa(idpa)
** end f/Ajit
        endif
      endif
      if(barp.gt.0..and.ipt.gt.0)then
        ipt=nint((abs(ipt)+.1*getRan())/barp)
        if(ipt.eq.1)then
          annu1=annu1+barp
          if(pent)epo1=epo1+isr(ip)
          pan1(1)=pan1(1)+px(ip)
          pan1(2)=pan1(2)+py(ip)
          pan1(3)=pan1(3)+pz(ip)
          ene1=ene1+pk/(amv(ip)+sqrt(pk+amv(ip)**2))+epsh(ip)
*     e      +.5*petom*mde(ip)+amv(ip)-barp*am0
     e      +petom*mdep(ip)+amv(ip)-barp*am0
          zuu1=zuu1+zpa(idpa)
        else
          annu2=annu2+barp
          if(pent)epo2=epo2+isr(ip)
          pan2(1)=pan2(1)+px(ip)
          pan2(2)=pan2(2)+py(ip)
          pan2(3)=pan2(3)+pz(ip)
          ene2=ene2+pk/(amv(ip)+sqrt(pk+amv(ip)**2))+epsh(ip)
*     e      +.5*petom*mde(ip)+amv(ip)-barp*am0
     e      +petom*mdep(ip)+amv(ip)-barp*am0
*          epe2=epe2+epsh(ip)+.5*petom*mde(ip)
          epe2=epe2+epsh(ip)+petom*mdep(ip)
          zuu2=zuu2+zpa(idpa)
        endif
      endif
C
      IF(IDP.LE.0)GOTO 350
c
*c  for Jorg
*      if(pri)then
*        if(barp.gt..1.and.abs(ipti(ip)).eq.2)then
*          xtarg=xtarg+barp*xx(ip)
*          ztarg=ztarg+barp*zz(ip)
*          rktarg=rktarg+barp*(xx(ip)**2+yy(ip)**2+zz(ip)**2)
*          antarg=antarg+barp
*        endif
*      endif
*c  end for Jorg
C
      if(abs(idp).eq.2)then
        idpi=1
      else
        idpi=idp
      endif
*     z=z+zpa(idp)
*     a=a+bar(idp)
      IIPT=IPTI(IP)
      CALL TEST(IDPi,PX(IP),PY(IP),PZ(IP),EE(IP),PETOM*MDE(IP)
     C  ,IIPT,*350,IPAR,IOV)
      IF(IDP.GE.3.OR.NCS(IP).NE.0)THEN
        PARTI(IDP)=PARTI(IDP)+1.
        IF(BOX)THEN
          EKI(IDP)=EKI(IDP)+PK/(AMV(IP)+SQRT(AMV(IP)**2+PK))
        ENDIF
        IF(IDP.LE.6)THEN
          ANPR(IDP)=ANPR(IDP)+IOV
        ENDIF
      ENDIF
      if(idp.ne.2.AND.IDP.LE.6)NPAR=NPAR+IPAR
      IF(IDP.GE.1.AND.IDP.LE.6)THEN
        if(pent)then
          if(isr(ip).ne.0.)then
            eptro=eptro+isr(ip)*iov
            anuc=anuc+barp*iov
          endif
          spaax=.01*isrx(ip)
          epid(idp)=epid(idp)+spaax
          npid(idp)=npid(idp)+1
        endif
      ENDIF
      IF(DT.EQ.0.)GOTO 333
C
      EEP=EE(IP)
C
      IF(FRCAS)THEN
        IF(NCS(IP).EQ.0)THEN
          IF(IPTI(IP).EQ.1)THEN
            XX(IP)=XX(IP)+DT*B1X
            ZZ(IP)=ZZ(IP)+DT*B1Z
            GOTO 333
          ELSEIF(IPTI(IP).EQ.2)THEN
            XX(IP)=XX(IP)+DT*B2X
            ZZ(IP)=ZZ(IP)+DT*B2Z
            GOTO 333
          ENDIF
        ENDIF
      ENDIF
C
      IF(BARP.EQ.0.)THEN
        DTE=DT/EEP
      ELSE
        XXN=XX(IP)/DWL
        IX=NINT(XXN)
        IF(ABS(IX).GT.NT)IX=SIGN(NT,IX)
        YYN=YY(IP)/DWL
        IY=NINT(YYN)
        IF(ABS(IY).GT.NT)IY=SIGN(NT,IY)   !precautions on June 10, 2010
        ZZN=ZZ(IP)/DLL
        IZ=NINT(ZZN)
        IF(ABS(IZ).GT.NL)IZ=SIGN(NL,IZ)   ! after 0.5 failure
C
        IXF=IX+1
        IXB=IX-1
        FRX(1)=.5*(XXN-IX+.5)
        FRX(-1)=.5-FRX(1)
        FRX(0)=.5
        IYF=IY+1
        IYB=IY-1
        FRY(1)=.5*(YYN-IY+.5)
        FRY(-1)=.5-FRY(1)
        FRY(0)=.5
        IZF=IZ+1
        IZB=IZ-1
        FRZ(1)=.5*(ZZN-IZ+.5)
        FRZ(-1)=.5-FRZ(1)
        FRZ(0)=.5
C
        IF(ABS(IX).EQ.NT)THEN
          IF(IX.EQ.NT)THEN
            FRX(0)=1.-FRX(-1)
            IXF=NT
          ELSE
            FRX(0)=1.-FRX(1)
            IXB=-NT
          ENDIF
        ENDIF
C
        IF(ABS(IY).EQ.NT)THEN
          IF(IY.EQ.NT)THEN
            FRY(0)=1.-FRY(-1)
            IYF=NT
          ELSE
            FRY(0)=1.-FRY(1)
            IYB=-NT
          ENDIF
        ENDIF
C
        IF(ABS(IZ).EQ.NL)THEN
          IF(IZ.EQ.NL)THEN
            FRZ(0)=1.-FRZ(-1)
            IZF=NL
          ELSE
            FRZ(0)=1.-FRZ(1)
            IZB=-NL
          ENDIF
        ENDIF
C
        IF(LMO)THEN
          DTEX=0.
          DTEY=0.
          DTEZ=0.
        ELSE
          DTE=0.
        ENDIF
        DO IIZ=IZB,IZF
          FRZZ=FRZ(IIZ-IZ)
          DO IIY=IYB,IYF
            FRYY=FRZZ*FRY(IIY-IY)
            DO IIX=IXB,IXF
              FRXX=FRYY*FRX(IIX-IX)
              IF(LMO)THEN
                CALL PTNO(VXNO(IIX,IIY,IIZ),VYNO(IIX,IIY,IIZ)
     C            ,VZNO(IIX,IIY,IIZ),RHO(IIX,IIY,IIZ)/AN0
     A            ,UU(IIX,IIY,IIZ),AMVI,BARP,PX(IP),PY(IP),PZ(IP)
     L            ,EEI,PC,EC,VXI,VYI,VZI,2)  ! Note: recalculates EE!
                DTEX=DTEX+FRXX*VXI
                DTEY=DTEY+FRXX*VYI
                DTEZ=DTEZ+FRXX*VZI
              ELSE
                DTE=DTE+FRXX/SQRT((AMVI+BARP*UU(IIX,IIY,IIZ))**2+PK)
              ENDIF
            ENDDO
          ENDDO
        ENDDO
        DTE=DTE*DT
      ENDIF
C
      IF(BARP.EQ.0..OR.LMI)THEN
        XX(IP)=XX(IP)+DTE*PX(IP)
        YY(IP)=YY(IP)+DTE*PY(IP)
        ZZ(IP)=ZZ(IP)+DTE*PZ(IP)
      ELSE
        XX(IP)=XX(IP)+DTEX*DT
        YY(IP)=YY(IP)+DTEY*DT
        ZZ(IP)=ZZ(IP)+DTEZ*DT
      ENDIF
 333  CONTINUE
      IF(BOX)THEN
        XXN=XX(IP)/DWL
        IX=NINT(XXN)
        YYN=YY(IP)/DWL
        IY=NINT(YYN)
        ZZN=ZZ(IP)/DLL
        IZ=NINT(ZZN)
        IF(IX.GT.NT)THEN
          XX(IP)=XX(IP)-(NT+NT+1)*DWL+1E-3*DWL
          GOTO 333
        ENDIF
        IF(IX.LT.-NT)THEN
          XX(IP)=XX(IP)+(NT+NT+1)*DWL-1E-3*DWL
          GOTO 333
        ENDIF
        IF(IY.GT.NT)THEN
          YY(IP)=YY(IP)-(NT+NT+1)*DWL+1E-3*DWL
          GOTO 333
        ENDIF
        IF(IY.LT.-NT)THEN
          YY(IP)=YY(IP)+(NT+NT+1)*DWL-1E-3*DWL
          GOTO 333
        ENDIF
        IF(IZ.GT.NL)THEN
          ZZ(IP)=ZZ(IP)-(NL+NL+1)*DLL+1E-3*DLL
          GOTO 333
        ENDIF
        IF(IZ.LT.-NL)THEN
          ZZ(IP)=ZZ(IP)+(NL+NL+1)*DLL-1E-3*DLL
          GOTO 333
        ENDIF
      ENDIF
 350  CONTINUE              !!end of the first big ptcle loop
C
C  SORTING FROM WITHIN
      CALL GIVAL                         !all of these moved to MOVEX now
      CALL SORTI(IPO,IVAL,NQ)
      CALL FINDI(IPO,IVAL,NQ)
C
      IF(LMO)THEN
        KRU=0
        KRUF=1
      ENDIF
 360  CONTINUE
      DO IY=-NT,NT
        RHOBY(IY)=0.
        spay(iy)=0.
      ENDDO
      EKI0=0.
      EKI0T=0.
      EKI0Z=0.
      PRSKT0=0.      ! transverse kinetic pressure
      PRSKL0=0.      ! longitudinal kinetic pressure
      PRSXZ0=0.      ! off-diagonal pressure
c
Cc pressure
C      do ix=-nt,nt
C        do iy=-nt,nt
C          ptxy(ix,iy)=0.
C        enddo
C      enddo
      DO IZ=-NL,NL
c       RHOBZ(IZ)=0.
        do k=0,nsit
        spaz(iz,k)=0.
        enddo
        DO IY=-NT,NT
          rhob(iy,iz)=0.
Cc pressure
C          ptxz(iy,iz)=0.
          DO IX=-NT,NT
            IF(PENT)THEN
              spaa(IX,IY,IZ)=0.
              RBaa(IX,IY,IZ)=0.
            ENDIF
            IF(LMI)RHO(IX,IY,IZ)=0.
            ZHO(IX,IY,IZ)=0.
            THO(IX,IY,IZ)=0.
*            UC(IX,IY,IZ)=0.            !insurance
          ENDDO
        ENDDO
      ENDDO
C  MOMENTS FOR COULOMB
      D0=0.
c      npt(1)=0
c      npt(2)=0
      DO I3=1,3
        D1(I3)=0.
c        rrpt(i3,1)=0.
c        rrpt(i3,2)=0.
        DO I2=1,I3
          D2(I2,I3)=0.
          DO I1=1,I2
            D3(I1,I2,I3)=0.
          ENDDO
        ENDDO
      ENDDO
C
      IF(LMO)THEN
        DO IZ=-1,N3MO    ! zeroing buffers
          DO IY=-NT,NT
            DO IX=-NT,NT
              VXNOU(IX,IY,IZ)=0.
              VYNOU(IX,IY,IZ)=0.
              VZNOU(IX,IY,IZ)=0.
              RHOU(IX,IY,IZ)=0.
              DUPNO(IX,IY,IZ)=0.
            ENDDO
          ENDDO
        ENDDO
      ENDIF
      DO I=-1,1
        IZNO(I)=I
      ENDDO
C
      DO 100 IZ=-NL,NL
      IZB=MAX(-NL,IZ-1)
      IZF=MIN(NL,IZ+1)
      DO 90 IY=-NT,NT
      IYB=MAX(-NT,IY-1)
      IYF=MIN(NT,IY+1)
      DO 80 IX=-NT,NT
      IXB=MAX(-NT,IX-1)
      IXF=MIN(NT,IX+1)
      IMXXYZ=IMX(IX,IY,IZ)
      IF(IMXXYZ.LT.0)GOTO 80
      IMNXYZ=IMN(IX,IY,IZ)
      DO 70 I=IMNXYZ,IMXXYZ
      IP=IPO(I)
      IDP=ID(IP)
      IF(IDP.LE.0)GOTO 70
      IF(IDP.EQ.16)GOTO 70
c
*      quad=quad+bar(idp)*(2.*zz(ip)**2-xx(ip)**2-yy(ip)**2)
*      bquad=bquad+bar(idp)
*      ipt=abs(ipti(ip))
*      if(ipt.eq.1.or.ipt.eq.2)then
*        dipo=dipo+2.*(ipt-.5)*zz(ip)*bar(idp)
*        bdipo=bdipo+bar(idp)
*      endif
C
      AMVI=AMV(IP)
      PXI=PX(IP)
      PYI=PY(IP)
      PZI=PZ(IP)
      EEI=EE(IP)
C
      PKT=PXI*PXI+PYI*PYI                        !used below
      PKZ=PZI*PZI
      PK=PKT+PKZ                                 !used below
      BARP=BAR(IDP)
C
      XXN=XX(IP)/DWL
      YYN=YY(IP)/DWL
      ZZN=ZZ(IP)/DLL
C
      FRX(1)=.5*(XXN-IX+.5)
      FRX(-1)=.5-FRX(1)
      FRX(0)=.5
      FRY(1)=.5*(YYN-IY+.5)
      FRY(-1)=.5-FRY(1)
      FRY(0)=.5
      FRZ(1)=.5*(ZZN-IZ+.5)
      FRZ(-1)=.5-FRZ(1)
      FRZ(0)=.5
C
      IF(ABS(IX).EQ.NT)THEN
        IF(IX.EQ.NT)THEN
          FRX(0)=1.-FRX(-1)
        ELSE
          FRX(0)=1.-FRX(1)
        ENDIF
      ENDIF
C
      IF(ABS(IY).EQ.NT)THEN
        IF(IY.EQ.NT)THEN
          FRY(0)=1.-FRY(-1)
        ELSE
          FRY(0)=1.-FRY(1)
        ENDIF
      ENDIF
C
      IF(ABS(IZ).EQ.NL)THEN
        IF(IZ.EQ.NL)THEN
          FRZ(0)=1.-FRZ(-1)
        ELSE
          FRZ(0)=1.-FRZ(1)
        ENDIF
      ENDIF
C
      DRHO=BARP*FRHO
      DTHO=FRHO*TIZO(IDP)
      ZP=ZPA(IDP)
      DZHO=FZHO*ZP
C
      EEPO=SQRT(AMVI*AMVI+PK)
      DO IIZ=IZB,IZF
        FRZZ=FRZ(IIZ-IZ)
        IIZS=IZNO(IIZ-IZ)
        iiza=abs(iiz)
        DO IIY=IYB,IYF
          FRYY=FRZZ*FRY(IIY-IY)
          iiya=abs(iiy)
          DO IIX=IXB,IXF
            FRXX=FRYY*FRX(IIX-IX)
            IF(BARP.NE.0.)THEN
              IF(LMI)THEN
                IF(DT.NE.0.)THEN
                  AMVU=AMVI+BARP*UU(IIX,IIY,IIZ)
                ELSE
                  AMVU=AMVI+BARP*UPO1
                ENDIF
                RHO(IIX,IIY,IIZ)=RHO(IIX,IIY,IIZ)+DRHO*FRXX
     R            *AMVU/SQRT(AMVU*AMVU+PK)
              ELSE
                IF(DT.NE.0.)THEN
                  RHOV=RHO(IIX,IIY,IIZ)/AN0
                  VXN=VXNO(IIX,IIY,IIZ)
                  VYN=VYNO(IIX,IIY,IIZ)
                  VZN=VZNO(IIX,IIY,IIZ)
                  UUN=UU(IIX,IIY,IIZ)
                ELSE
                  RHOV=1.
                  UUN=DUPNO1
                  VYN=0.
                  IF(IPTI(IP).EQ.1)THEN
                    VXN=B1X
                    VZN=B1Z
                  ELSEIF(IPTI(IP).EQ.2)THEN
                    VXN=B2X
                    VZN=B2Z
                  ELSE
                    WRITE(*,*)'Bad in MOVEX'
                    STOP
                  ENDIF
                ENDIF
                CALL PTNO(VXN,VYN,VZN,RHOV
     A            ,UUN,AMVI,BARP,PXI,PYI,PZI
     L            ,EEI,PC,EC,VXI,VYI,VZI,2)  ! Note: recalculates EE!
                DRHOX=DRHO*FRXX
                RHOU(IIX,IIY,IIZS)=RHOU(IIX,IIY,IIZS)+DRHOX  !adding vector density
                VXNOU(IIX,IIY,IIZS)=VXNOU(IIX,IIY,IIZS)+DRHOX*VXI
                VYNOU(IIX,IIY,IIZS)=VYNOU(IIX,IIY,IIZS)+DRHOX*VYI
                VZNOU(IIX,IIY,IIZS)=VZNOU(IIX,IIY,IIZS)+DRHOX*VZI
                rhag=barp*am0/amvi*fxi(rhov)  !arguments f/dupno in local frame
                DUPNO(IIX,IIY,IIZS)=DUPNO(IIX,IIY,IIZS)
     D            +DRHOX*AIVRp(pc/amvi,rhag)*fxip(rhov)
**     D            +DRHOX*AIVDR(PC/AMVI,RHOV*BARP*AM0/AMVI)
              ENDIF
            ENDIF
            IF(PENT)THEN
              IF(BAR(IDP).GT.0.)spaa(IIX,IIY,IIZ)=
     I          spaa(IIX,IIY,IIZ)+DRHO*FRXX*isr(ip)*.01/BAR(IDP)
              RBaa(IIX,IIY,IIZ)=RBaa(IIX,IIY,IIZ)+DRHO*FRXX
            ENDIF
            THO(IIX,IIY,IIZ)=THO(IIX,IIY,IIZ)+DTHO*FRXX
            ZHO(IIX,IIY,IIZ)=ZHO(IIX,IIY,IIZ)+DZHO*FRXX
            IIXA=ABS(IIX)
*            IF(IIXA+IIYA.EQ.0)THEN
            IF(IIYA.EQ.0.and.(iix.ge.0.and.iix.le.nsit))THEN
c             RHOBZ(IIZ)=RHOBZ(IIZ)+DRHO*FRXX
*         if(pent.and.bar(idp).ne.0.)
*    i      spaZ(IIZ,iix)=spaZ(IIZ,iix)+DRHO*FRXX*isr(ip)*.01/bar(idp)
          if(pent.and.bar(idp).ne.0.)
     i      spaZ(IIZ,iix)=spaZ(IIZ,iix)+fRHO*FRXX*isr(ip)*.01/bar(idp)
            ENDIF
c pressure
            IF(IIZA.EQ.0.OR.IIYA.EQ.0)THEN
              IF(LMI)THEN
                IF(IIXA+IIYA+IIZA.EQ.0)THEN
                  PRSKT0=PRSKT0+.5*PKT/SQRT(AMVU*AMVU+PK)*FRHO*FRXX
                  PRSKL0=PRSKL0+PKZ/SQRT(AMVU*AMVU+PK)*FRHO*FRXX
                  PRSXZ0=PRSXZ0+PXI*PZI/SQRT(AMVU*AMVU+PK)*FRHO*FRXX
                ENDIF
              ELSEIF(KRU.EQ.KRUF)THEN
                IF(BARP.EQ.0.)THEN
                  RHOV=RHO(IIX,IIY,IIZ)/AN0
                  VXN=VXNO(IIX,IIY,IIZ)
                  VYN=VYNO(IIX,IIY,IIZ)
                  VZN=VZNO(IIX,IIY,IIZ)
                  UUN=UU(IIX,IIY,IIZ)
                  CALL PTNO(VXN,VYN,VZN,RHOV
     A              ,UUN,AMVI,BARP,PXI,PYI,PZI
     L              ,EEI,PC,EC,VXI,VYI,VZI,2)  ! Note: recalculates EE!
                ENDIF
C                if(iiza.eq.0)then
C                  fg=sqrt(max(0.,1.-vxn*vxn-vyn*vyn-vzn*vzn))
C                  ptxy(iix,iiy)=ptxy(iix,iiy)+.5*ppvc*frho*frxx*fg
C                endif
C                if(iiya.eq.0)then
C                  fg=sqrt(max(0.,1.-vxn*vxn-vyn*vyn-vzn*vzn))
C                  ptxz(iix,iiz)=ptxz(iix,iiz)+.5*ppvc*frho*frxx*fg
C                endif
                IF(IIXA+IIYA+IIZA.EQ.0)THEN
                  PRSKT0=PRSKT0+.5*(VXI*PXI+VYI*PYI)*FRHO*FRXX
                  PRSKL0=PRSKL0+VZI*PZI*FRHO*FRXX
                  PRSXZ0=PRSXZ0+.5*(VXI*PZI+PXI*VZI)*FRHO*FRXX
                ENDIF
              ENDIF
            ENDIF
            IF(IIXA+IIZA.EQ.0)THEN
              RHOBY(IIY)=RHOBY(IIY)+DRHO*FRXX
              if(pent.and.bar(idp).ne.0.)
     i          spaY(IIY)=spaY(IIY)+DRHO*FRXX*isr(ip)*.01/bar(idp)
              IF(IIYA.EQ.0)THEN
                EKI0=EKI0+DRHO*FRXX*(EEPO-AMVI)
                EKI0Z=EKI0Z+DRHO*FRXX*PZ(IP)**2/(AMVI+EEPO)
                EKI0T=EKI0T
     E            +DRHO*FRXX*(PX(IP)**2+PY(IP)**2)/(AMVI+EEPO)
              ENDIF
            ENDIF
            IF(IIyA.EQ.0)then
              RHOB(IIx,iiz)=RHOB(IIx,iiz)+DRHO*FRXX
*              if(idp.ge.7)RHOB(IIx,iiz)=RHOB(IIx,iiz)+FRXX*frho
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
      IF(NOCOU)GOTO 70
      IF(ZP.EQ.0.)GOTO 70     !!charge moments
      ZH=FDO*ZP
      D0=D0+ZH
      RDS(1)=XX(IP)
      RDS(2)=YY(IP)
      RDS(3)=ZZ(IP)
      DO I3=1,3
        ZI3=ZH*RDS(I3)
        D1(I3)=D1(I3)+ZI3
        DO I2=1,I3
          ZI32=ZI3*RDS(I2)
          D2(I2,I3)=D2(I2,I3)+ZI32
          DO I1=1,I2
            D3(I1,I2,I3)=D3(I1,I2,I3)+ZI32*RDS(I1)
          ENDDO
        ENDDO
      ENDDO
   70 CONTINUE
   80 CONTINUE
   90 CONTINUE
C  REWRITING OF THE BUFFER
      IF(LMO)THEN
        IZNB=-1
        IZNF=-1
        IF(IZ.EQ.-NL)THEN
          IZNB=0
        ELSEIF(IZ.EQ.NL)THEN
          IZNF=0
        ENDIF
        DO IZN=IZNB,IZNF
          IIZS=IZNO(IZN)          !source IZ?
          IZT=IZ+IZN
          DO IY=-NT,NT
            DO IX=-NT,NT
              RHOI=RHOU(IX,IY,IIZS)
              IF(RHOI.NE.0.)THEN
                VXNO(IX,IY,IZT)=VXNOU(IX,IY,IIZS)/RHOI
                VYNO(IX,IY,IZT)=VYNOU(IX,IY,IIZS)/RHOI
                VZNO(IX,IY,IZT)=VZNOU(IX,IY,IIZS)/RHOI
                AG=VXNO(IX,IY,IZT)**2+VYNO(IX,IY,IZT)**2
     A            +VZNO(IX,IY,IZT)**2
                IF(AG.LT..99999)THEN
                  GNO=1./SQRT(1.-AG)      !gamma factor
                ELSE
                  GNO=1.
                ENDIF
                RHO(IX,IY,IZT)=RHOI/GNO   !local density is obtained
                UU(IX,IY,IZT)=DUPNO(IX,IY,IIZS)*AM0/(AN0*GNO) !gamma: local frame?
                IF(KRU.LT.KRUF)THEN
                  UU(IX,IY,IZT)=UU(IX,IY,IZT)+UPO(RHO(IX,IY,IZT)/AN0)
                ENDIF
              ELSE
                VXNO(IX,IY,IZT)=0.
                VYNO(IX,IY,IZT)=0.
                VZNO(IX,IY,IZT)=0.
                UU(IX,IY,IZT)=0.
                GNO=1.
                RHO(IX,IY,IZT)=0.
              ENDIF
              RHOU(IX,IY,IIZS)=0.
              VXNOU(IX,IY,IIZS)=0.
              VYNOU(IX,IY,IIZS)=0.
              VZNOU(IX,IY,IIZS)=0.
              DUPNO(IX,IY,IIZS)=0.
            ENDDO
          ENDDO
        ENDDO
        IIB=IZNO(-1)
        IZNO(-1)=IZNO(0)
        IZNO(0)=IZNO(1)
        IZNO(1)=IIB
      ENDIF
  100 CONTINUE     ! previous end of big ptcle loop
C
      IF(LMO)THEN
        KRU=KRU+1
        IF(KRU.LE.KRUF)GOTO 360
      ENDIF
C
      IF(PENT)THEN
        do iy=-nt,nt
          if(rhoby(iy).ne.0.)spay(iy)=spay(iy)/rhoby(iy)
          do iz=-nl,nl
            do ix=-nt,nt
              if(rBAA(ix,iy,iz).ne.0.)spaa(ix,iy,iz)=spaa(ix,iy,iz)
     i          /rBAA(ix,iy,iz)
            enddo
          enddo
        enddo
        do k=0,min(nsit,NT)
          do iz=-nl,nl
            if(rhob(k,iz).ne.0.)spaz(iz,k)=spaz(iz,k)/rhob(k,iz)
          enddo
        enddo
      ENDIF
      IF(RHOBY(0).NE.0.)THEN
        EKI0=EKI0/RHOBY(0)
        EKI0T=EKI0T/RHOBY(0)
        EKI0Z=EKI0Z/RHOBY(0)
      ENDIF
      if(pri)write(*,*)'eki0 = ',eki0,' eki0z = ',eki0z,' eki0t = '
     p  ,eki0t
*      quad=quad/max(bquad,1.)
*      dipo=dipo/max(bdipo,1.)
*      if(pri)write(*,*)'quad = ',quad,'  dipo = ',dipo
      pato=pato/nqu
      IF(PRI.AND.PAto.NE.0.)WRITE(*,*)'COL/PAR/DT = ',REFIN/PAto
      if(nnpi.ne.0)rhopi=rhopi*1e-3/nnpi
      NFRAI=NFRA(1)+NFRA(2)
      IF(NFRAI.NE.0)ROFRA(2)=(ROFRA(1)+ROFRA(2))*1E-3/NFRAI
      NFREI=NFRE(1)+NFRE(2)
      IF(NFREI.NE.0)ROFRE(2)=(ROFRE(1)+ROFRE(2))*1E-3/NFREI
      DO I=3,6
        IF(NFRA(I).NE.0)ROFRA(I)=ROFRA(I)*1E-3/NFRA(I)
        IF(NFRE(I).NE.0)ROFRE(I)=ROFRE(I)*1E-3/NFRE(I)
      ENDDO
      IF(PRI)WRITE(*,'(1X,''prod an/an0 N/d/h/t AL/EM =''
     I  ,4(1X,F5.3),2X,4(1X,F5.3))')
     I  (ROFRA(I),I=2,5),(ROFRE(I),I=2,5)
*      if(pri)write(*,*)'rhopi = ',rhopi
*      write(*,*)'z = ',z/nqu,' a = ',a/nqu
C
      DO IZ=-NL,NL     !?for the surface term
        IZB=IZ-2
        IZF=IZ+2
        DO IY=-NT,NT
          IYB=IY-2
          IYF=IY+2
          DO IX=-NT,NT
            RHOP=RHO(IX,IY,IZ)
            RHS=0.
            IF(IZB.GE.-NL)RHS=RHS+(RHO(IX,IY,IZB)-RHOP)*QDLLK
            IF(IZF.LE.NL)RHS=RHS+(RHO(IX,IY,IZF)-RHOP)*QDLLK
            IF(IYB.GE.-NT)RHS=RHS+(RHO(IX,IYB,IZ)-RHOP)*QDWLK
            IF(IYF.LE.NT)RHS=RHS+(RHO(IX,IYF,IZ)-RHOP)*QDWLK
            IXB=IX-2
            IF(IXB.GE.-NT)RHS=RHS+(RHO(IXB,IY,IZ)-RHOP)*QDWLK
            IXF=IX+2
            IF(IXF.LE.NT)RHS=RHS+(RHO(IXF,IY,IZ)-RHOP)*QDWLK
            RHOS(IX,IY,IZ)=RHS
          ENDDO
        ENDDO
      ENDDO
C
      IF(NOCOU)GOTO 400
      DO I3=1,3
c        if(npt(1).ne.0)then
c          rrpt(i3,1)=rrpt(i3,1)/npt(1)
c        endif
c        if(npt(2).ne.0)then
c          rrpt(i3,2)=rrpt(i3,2)/npt(2)
c        endif
        RDS(I3)=D1(I3)/MAX(D0,1E-10)
        DI3=D0*RDS(I3)
        DO I2=1,I3
          DI32=DI3*RDS(I2)
          D2(I2,I3)=D2(I2,I3)-DI32
          D2(I3,I2)=D2(I2,I3)
          DO I1=1,I2
            D123=D3(I1,I2,I3)-D2(I1,I2)*RDS(I3)
     D        -D2(I1,I3)*RDS(I2)-D2(I2,I3)*RDS(I1)-DI32*RDS(I1)
            D3(I1,I2,I3)=D123
            D3(I3,I1,I2)=D123
            D3(I2,I3,I1)=D123
            D3(I1,I3,I2)=D123
            D3(I2,I1,I3)=D123
            D3(I3,I2,I1)=D123
          ENDDO
        ENDDO
      ENDDO
c      write(*,*)'proj ',(rrpt(ii,1),ii=1,3)
c      write(*,*)'targ ',(rrpt(ii,2),ii=1,3)
C
      IS=-1
 300  CONTINUE
      IZ=NLP*IS
      DO IY=-NT,NT
        DO IX=-NT,NT
          CALL GIVEUD(IX,IY,IZ)
        ENDDO
      ENDDO
C
      IY=NTP*IS
      DO IZ=-NL,NL
        DO IX=-NT,NT
          CALL GIVEUD(IX,IY,IZ)
        ENDDO
      ENDDO
C
      IX=NTP*IS
      DO IZ=-NL,NL
        DO IY=-NT,NT
          CALL GIVEUD(IX,IY,IZ)
        ENDDO
      ENDDO
C
      IF(IS.EQ.-1)THEN
        IS=1
        GOTO 300
      ENDIF
C
*      write(40,*)'iptco/dt ',iptco,dt
      DO ISTEP=1,NSTEP+1
        CALL STEPC(4.5)
        CALL STEPC(1.3)
*        CALL STEPC(4.)
*        WRITE(40,*)' '
*        WRITE(40,*)'ISTEP = ',ISTEP
*        DO IZ=-NL,NL
*          WRITE(40,*)UC(0,0,IZ)
*        ENDDO
      ENDDO
      IF(IPTCO.EQ.0)NSTEP=1
C
 400  CONTINUE
      DO I=1,15
        IF(BOX)EKI(I)=EKI(I)/MAX(PARTI(I),1.)
        IF(PRI)THEN
          PARTII=MAX(PARTI(I)+NQU*PARTIG(I),1.)
          EKIT(I)=EKIT(I)/PARTII
          EKIZ(I)=EKIZ(I)/PARTII
*         PARTII=MAX(PARTI(I),1.)
          EKITC(I)=EKITC(I)/PARTII
          EKIZC(I)=EKIZC(I)/PARTII
          ECVT(I)=ECVT(I)/PARTII
          ECVZ(I)=ECVZ(I)/PARTII
          EISH(I)=EISH(I)/PARTII
          AMSI=AMS(I)
          ANORM=0.
          ATIN=0.
          DO J=1,NET
*           TE=J*DET
            TE=(J-.5)*DET
            ETE=AMSI+TE
            P=SQRT(TE*(AMSI+ETE))
            FETP=FET/(P*ETE)
            ANORM=ANORM+DEKT(J,I)
            ATIN=ATIN+DEKT(J,I)*TE
            DEKTS(J,I)=FETP*SQRT(DEKT(J,I))
            DEKT(J,I)=FETP*DEKT(J,I)
*           TE=J*DETC
            TE=(J-.5)*DETC
            ETE=AMSI+TE
            P=SQRT(TE*(AMSI+ETE))
            FETP=FETC/(P*ETE)
            DEKTCS(J,I)=FETP*SQRT(DEKTC(J,I))
            DEKTC(J,I)=FETP*DEKTC(J,I)
          ENDDO
          IF(I.LE.5)THEN
            IF(ANORM.EQ.0.)THEN
              ECON(I)=0.
            ELSE
              ECON(I)=ATIN/ANORM
            ENDIF
          ENDIF
        ENDIF
        PARTI(I)=PARTI(I)/NQU
      ENDDO
      eptro=eptro*.01/nqu
      anuc=anuc/nqu
      edtrot=edtrot*.01/nqu
      ebtrot=ebtrot*.01/nqu
      entrot=entrot*.01/nqu
      epo1=epo1*.01/nqu
      epo2=epo2*.01/nqu
      annu1=annu1/nqu
      anuu1=max(annu1,1.)
      zuu1=zuu1/nqu
      fa1=1./(anuu1*nqu)
      pan1k=0.
      do i=1,3
        pan1(i)=pan1(i)*fa1
        pan1k=pan1k+pan1(i)**2
      enddo
      ene1=ene1*fa1
      AG=MAX((am0+ene1)**2-pan1k,0.)
      ene1=sqrt(AG)-am0
      bex1=pan1(1)/sqrt((ene1+am0)**2+pan1(1)**2+pan1(3)**2)
      bez1=pan1(3)/sqrt((ene1+am0)**2+pan1(1)**2+pan1(3)**2)
      annu2=annu2/nqu
      anuu2=max(annu2,1.)
      zuu2=zuu2/nqu
      fa2=1./(anuu2*nqu)
      pan2k=0.
      do i=1,3
        pan2(i)=pan2(i)*fa2
        pan2k=pan2k+pan2(i)**2
      enddo
      ene2=ene2*fa2
      epe2=epe2*fa2
      AG=MAX((am0+ene2)**2-pan2k,0.)
      ene2=sqrt(AG)-am0
      bex2=pan2(1)/sqrt((ene2+am0)**2+pan2(1)**2+pan2(3)**2)
      bez2=pan2(3)/sqrt((ene2+am0)**2+pan2(1)**2+pan2(3)**2)
      if(pri)then
c  f/Mihai
        nrav=0
        nav=0
        baral=0.
        ekal=0.
        ekals=0.
        ek2al=0.
        ek2als=0.
        ecal=0.
        ecals=0.
        ec2al=0.
        ec2als=0.
        etal=0.
        etals=0.
        et2al=0.
        et2als=0.
        baal=0.
        ba2al=0.
        do iafi=1,nafi
          co2fi=cos(pi/nafi*(iafi-.5))
          navii=navi(iafi)
          nav=nav+navii
          if(navii.ne.0)then
            nrav=nrav+1
            ekia(iafi)=ekia(iafi)/navii
            ekias(iafi)=ekias(iafi)/navii-ekia(iafi)**2
            ekias(iafi)=max(ekias(iafi),0.)/navii
            ecola(iafi)=ecola(iafi)/bavi(iafi)
            ecolas(iafi)=ecolas(iafi)/bavi(iafi)-ecola(iafi)**2
            ecolas(iafi)=max(ecolas(iafi),0.)/navii
            etha(iafi)=etha(iafi)/navii
            ethas(iafi)=ethas(iafi)/navii-etha(iafi)**2
            ethas(iafi)=max(ethas(iafi),0.)/navii
            ekal=ekal+ekia(iafi)
            ekals=ekals+ekias(iafi)
            ek2al=ek2al+ekia(iafi)*co2fi
            ek2als=ek2als+ekias(iafi)*co2fi*co2fi
            ecal=ecal+ecola(iafi)
            ecals=ecals+ecolas(iafi)
            ec2al=ec2al+ecola(iafi)*co2fi
            ec2als=ec2als+ecolas(iafi)*co2fi*co2fi
            etal=etal+etha(iafi)
            etals=etals+ethas(iafi)
            et2al=et2al+etha(iafi)*co2fi
            et2als=et2als+ethas(iafi)*co2fi*co2fi
            baral=baral+bavi(iafi)
            bavi(iafi)=bavi(iafi)/navii
            baal=baal+bavi(iafi)
            ba2al=ba2al+bavi(iafi)*co2fi
          endif
        enddo
        if(nrav.ne.0)then
          baral=baral/nav
          ekal=ekal/nrav
          ekals=sqrt(ekals)/nrav
          ek2al=2.*ek2al/nrav
          ek2als=2.*sqrt(ek2als)/nrav
          ecal=ecal/nrav
          ecals=sqrt(ecals)/nrav
          ec2al=2.*ec2al/nrav
          ec2als=2.*sqrt(ec2als)/nrav
          etal=etal/nrav
          etals=sqrt(etals)/nrav
          et2al=2.*et2al/nrav
          et2als=2.*sqrt(et2als)/nrav
          baal=baal/nrav
          ba2al=2.*ba2al/nrav
          write(*,'(1x,a,3(1x,f4.2),6(1x,f6.2,a,f5.2))')
     w      'all:bar/b/2/ec/2/ek/2/et/2 ',baral,baal,ba2al
     w      ,ecal*1e3,'+-'
     w      ,ecals*1e3,ec2al*1e3,'+-',ec2als*1e3
     r      ,ekal*1e3,'+-',ekals*1e3
     r      ,ek2al*1e3,'+-',ek2als*1e3
     r      ,etal*1e3,'+-',etals*1e3
     r      ,et2al*1e3,'+-',et2als*1e3
          if(barbo.ne.0.)then
            write(*,'(1x,a,3(1x,f4.2),6(1x,f6.2,a,f5.2))')
     w        'bnd:bar/b/2/ec/2/ek/2/et/2 ',barbo,babo,ba2bo
     w        ,ecbo*1e3,'+-'
     w        ,ecbos*1e3,ec2bo*1e3,'+-',ec2bos*1e3
     r        ,ekbo*1e3,'+-',ekbos*1e3
     r        ,ek2bo*1e3,'+-',ek2bos*1e3
     r        ,etbo*1e3,'+-',etbos*1e3
     r        ,et2bo*1e3,'+-',et2bos*1e3
          endif
        endif
c  end f/Mihai
        write(*,*)'panr,annur:',panr,annur
        write(*,*)'pz:r,12,1,2',panr(3)*annur,pan1(3)*annu1
     w    +pan2(3)*annu2,pan1(3)*annu1,pan2(3)*annu2
        write(*,*)'px:r,12,1,2',panr(1)*annur,pan1(1)*annu1
     w    +pan2(1)*annu2,pan1(1)*annu1,pan2(1)*annu2
      endif
      ANPAR=FLOAT(NPAR)/NQU
      DO I=1,6
        ANPR(I)=ANPR(I)/NQU
      ENDDO
      ECINT=ECINT/NQU
      ECINZ=ECINZ/NQU
C
      IF(SYMMY)THEN    !this looks like straight enforcing of y<-->-y symmetry
        DO IZ=-NL,NL
          DO IX=-NT,NT
            DO IY=1,NT
              RHOH=.5*(RHO(IX,IY,IZ)+RHO(IX,-IY,IZ))
              RHO(IX,IY,IZ)=RHOH
              RHO(IX,-IY,IZ)=RHOH
            ! e-thing below in this loop done only on 1/31/98
              RHOH=.5*(RHOS(IX,IY,IZ)+RHOS(IX,-IY,IZ))
              RHOS(IX,IY,IZ)=RHOH
              RHOS(IX,-IY,IZ)=RHOH
              RHOH=.5*(THO(IX,IY,IZ)+THO(IX,-IY,IZ))
              THO(IX,IY,IZ)=RHOH
              THO(IX,-IY,IZ)=RHOH
              RHOH=.5*(UC(IX,IY,IZ)+UC(IX,-IY,IZ))
              UC(IX,IY,IZ)=RHOH
              UC(IX,-IY,IZ)=RHOH
              IF(LMO)THEN
                RHOH=.5*(VXNO(IX,IY,IZ)+VXNO(IX,-IY,IZ))
                VXNO(IX,IY,IZ)=RHOH
                VXNO(IX,-IY,IZ)=RHOH
                RHOH=.5*(VYNO(IX,IY,IZ)-VYNO(IX,-IY,IZ))
                VYNO(IX,IY,IZ)=RHOH
                VYNO(IX,-IY,IZ)=-RHOH
                RHOH=.5*(VZNO(IX,IY,IZ)+VZNO(IX,-IY,IZ))
                VZNO(IX,IY,IZ)=RHOH
                VZNO(IX,-IY,IZ)=RHOH
                RHOH=.5*(DUPNO(IX,IY,IZ)+DUPNO(IX,-IY,IZ))
                DUPNO(IX,IY,IZ)=RHOH
                DUPNO(IX,-IY,IZ)=RHOH
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF
C
** f/Ajit
      eporr=eporr*.01/nqu
      annur=annur/nqu
      anuur=max(annur,1./nqu)
      zuur=zuur/nqu
      far=1./(anuur*nqu)
      panrk=0.
      do i=1,3
        panr(i)=panr(i)*far
        panrk=panrk+panr(i)**2
      enddo
      ekinr=ekinr*far
      depotr=depotr*far
      epotr=0.
      do iz=-nl,nl
        z=iz*dll
        zk=(z-zzcr)**2
        do iy=-nt,nt
          y=iy*dwl
          zyk=y*y+zk
          do ix=-nt,nt
            x=ix*dwl
            if((x-xxcr)**2+zyk.lt.radrk)then
              RHOX=RHO(IX,IY,IZ)
              RT=RHOX/AN0
              EPOTr=EPOTr+EPO(RT)
              IF(COU)THEN
                EPOTr=EPOTr+FUZ*ZHO(IX,IY,IZ)*UC(IX,IY,IZ)
                IF(POT)EPOTr=EPOTr+UTTE(IX,IY,IZ)*rhox
*                IF(POT)EPOTr=EPOTr+.5*UTU(IX,IY,IZ)*THO(IX,IY,IZ)
              ENDIF
              IF(POT)THEN
              IF(ABS(IX).LT.NT)
     E          EPOTr=EPOTr
     P            +CSUQW*(RHO(IX+1,IY,IZ)-RHO(IX-1,IY,IZ))**2
              IF(ABS(IY).LT.NT)
     E          EPOTr=EPOTr
     P            +CSUQW*(RHO(IX,IY+1,IZ)-RHO(IX,IY-1,IZ))**2
              IF(ABS(IZ).LT.NL)
     E          EPOTr=EPOTr
     P            +CSUQL*(RHO(IX,IY,IZ+1)-RHO(IX,IY,IZ-1))**2
              ENDIF
            endif
          enddo
        enddo
      enddo
      EPOTr=EPOTr*VOLE
      epotr=epotr/anuur
      epotr=epotr+depotr
      ener=ekinr+epotr
      if(ener.ne.0.)THEN
        AG=ENER*ENER-PANRK
        if(ag.le.0.)then
          ener=0.
        else
          ener=sqrt(AG)-am0
        endif
      ENDIF
      erotr=.5*almor*almor/max(ainr*nqu*am0,1.)
      almor=almor/nqu/.19733
      xxccr=xxccr*far
      zzccr=zzccr*far
      rrccrk=rrccrk*far
      rrccrk=max(rrccrk-xxccr**2-zzccr**2,0.)
      raadrk=(1.5+1.2*(anuur*3.)**(1./3.))**2
      err=sqrt((ener+am0)**2+panr(1)**2+panr(3)**2)
      if(err.gt.0.)then
        bexr=panr(1)/err
        bezr=panr(3)/err
      else
        bexr=0.
        bezr=0.
      endif
** end f/Ajit
      IF(PRI)THEN
*c  for Jorg
*        antarg=max(antarg,1.)
*        xtarg=xtarg/antarg
*        ztarg=ztarg/antarg
*        rktarg=rktarg/antarg
*        rktarg=rktarg-(xtarg**2+ztarg**2)
*        rktarg=sqrt(max(rktarg,0.))
*        antarg=antarg/nqu
*        write(*,*)'Jorg, rmstgt, atgt: ',rktarg,antarg
*c  end for Jorg
c f/Bozek
        fyb=fyb/max(1E-10,fydb)
** f/Ajit
        write(*,*)'tim/ar/zr/bzr/bxr',tim,annur,zuur,bezr,bexr
**        write(*,*)'ekinr/epotr',ekinr,epotr
        write(*,*)'ener/tot_erotr/almor',ener*1e3,erotr*1e3,almor
        write(*,*)'xr,zr,rr',xxcr,zzcr,sqrt(radrk),' ! fyb= ',fyb
        if(pent)then
          if(annur.ne.0.)write(*,*)'eporr = ',eporr/annur
        endif
** end f/Ajit
        write(*,'(1x,a,2(1x,f6.2),1(1x,f6.2),2(1x,f8.5))')
     w    'a1/z1/e1/vz1/vx1',annu1,zuu1,ene1*1e3
     r    ,bez1,bex1
        write(*,'(1x,a,2(1x,f6.2),1(1x,f6.2),2(1x,f8.5))')
     w    'a2/z2/e2/vz2/vx2',annu2,zuu2,ene2*1e3
     r    ,bez2,bex2
c  for Roy
*        if(dt.ne.0.)then
*          write(46,*)'time = ',tim
**          write(*,*)'time = ',tim
*          do iyy=1,nyy
*            write(46,*)ymin+(iyy-.5)*dyy,anyy(iyy)
**            write(*,*)ymin+(iyy-.5)*dyy,anyy(iyy)
*          enddo
*        endif
c  end f/Roy
        if(nnpi.ne.0)then
          ancs=ancs/nnpi
          write(*,*)'ancs = ',ancs
        endif
        bbcnm=0.
        do ii=1,nbbc
          bbcnm=bbcnm+abbc(ii)
        enddo
        if(abs(bbcnm).gt.1e-10)bbcnm=1./bbcnm
        do ii=1,nbbc
*          abbc(ii)=abbc(ii)*bbcnm
*     d      /(4.*pi/3.*((ii/real(nbbc))**3-((ii-1)/real(nbbc))**3))
        enddo
*        write(*,*)'cv dstr'
        do j=1,nbbc
*          write(*,*)(j-.5)/nbbc,abbc(j)
        enddo
c
        do jj=1,6
          if(npid(jj).ne.0)epid(jj)=epid(jj)/npid(jj)
        enddo
        IF(PENT)THEN
          bbcnm=0.
          do ii=1,nbbc
            bbcnm=bbcnm+bccd(ii)
          enddo
          if(bbcnm.ne.0.)bbcnm=nbbc/9./bbcnm
          do ii=1,nbbc
            bccd(ii)=bccd(ii)*bbcnm
            do jj=1,6
              bcdd(ii,jj)=bcdd(ii,jj)*bbcnm
            enddo
          enddo
*          write(*,'(a,5(1x,f6.3))')'etro p/n/d/h/t',(epid(jj),jj=1,5)
*          write(*,*)'entro dstr'
*          do j=1,nbbc
*            write(*,*)(j-.5)*9./nbbc,bccd(j),bcdd(j,1)
*     d        ,bcdd(j,3),bcdd(j,5)
*          enddo
        ENDIF
        DO IN=3,5
          AMSI=AMS(IN)
          BARI=BAR(IN)
          NPRO=ZPA(IN)
          NNEU=NINT(BARI)-NPRO
          ABNORM=0.
          ABTIN=0.
          DO IP=1,NET
            TE=(IP-.5)*DET
            E=AM0+TE
            P=SQRT(TE*(AM0+AM0+TE))
            PE=P*E
            DDSCO=DEKT(IP,1)**NPRO*DEKT(IP,2)**NNEU
            DDSCOP=DDSCO*PE
            ABNORM=ABNORM+DDSCOP
            PP=P*BARI
            EN=SQRT(PP*PP+AMSI*AMSI)
            TTE=PP*PP/(AMSI+EN)
            ABTIN=ABTIN+TTE*DDSCOP
          ENDDO
          ECOV(IN)=ABTIN
          IF(ABNORM.GT.0.)ECOV(IN)=ABTIN/ABNORM
        ENDDO
*        WRITE(*,*)'Final'
        DO J=1,NET
*         TE=J*DET
          TE=(J-.5)*DET
*          WRITE(*,'(1X,F5.3,2(1X,F7.2,''+-'',F6.2)
*     E      ,3(1X,F6.2,''+-'',F5.2))')
*     W      TE,(DEKT(J,I),DEKTS(J,I),I=1,5)
        ENDDO
*        WRITE(*,*)'Collective'
        DO J=1,NET
*         TE=J*DETC
          TE=(J-.5)*DETC
*          WRITE(*,'(1X,F5.3,2(1X,F7.2,''+-'',F6.2)
*     E      ,3(1X,F6.2,''+-'',F5.2))')
*     W      TE,(DEKTC(J,I),DEKTCS(J,I),I=1,5)
        ENDDO
*       WRITE(*,*)'ECON  ',ECON
*       WRITE(*,*)'ECOV  ',ECOV
      ENDIF
c
      IF(.NOT.PENT.or.dt.eq.0.)return
      ENGAIN=ENGAIN/NQU
      EDGAIN=EDGAIN/NQU
      EBGAIN=EBGAIN/NQU
*     EBTROT=EBTROT+EBGAIN
*     EDTROT=EDTROT+EDGAIN
*     ENTROT=ENTROT+ENGAIN
      IF(PRI)THEN
        WRITE(*,*)'ENTROPY = ',ENTROT,ENGAIN/DT
        IF(DEPRO)WRITE(*,*)'EDTROPY = ',EDTROT,EDGAIN/DT
        write(*,*)'a1/ep1&a2/ep2',annu1,epo1/annu1
     w    ,annu2,epo2/annu2
        IF(ANNU1.ne.0)write(*,*)'a1 & ep1/a1',annu1,epo1/annu1
        if(annu2.ne.0)write(*,*)'a2 & ep2/a2',annu2,epo2/annu2
        if(anuc+anucg.eq.0.)then
          WRITE(*,*)'EBTROPY = ',EBTROT,EBGAIN/DT
        else
          write(*,*)'ebtropy = ',ebtrot,ebgain/dt
     w      ,(eptro+eptrog)/(anuc+anucg)
          write(*,*)'nu&nuct',anuc,anucg,'de&deg'
     w      ,anpr(3),anprg(3)
          write(*,*)'eptro&eptrog',eptro,eptrog
        endif
      ENDIF
C
      END


      SUBROUTINE GIVEUD(IX,IY,IZ)
      INCLUDE 'NUCLS'
      INCLUDE 'NQUA1'
      INCLUDE 'SIZE2'
      INCLUDE 'SIZEC2'
      DIMENSION RLS(3)
C
      RLS(1)=IX*DWL-RDS(1)
      RLS(2)=IY*DWL-RDS(2)
      RLS(3)=IZ*DLL-RDS(3)
      RL=SQRT(RLS(1)**2+RLS(2)**2+RLS(3)**2)
      U=D0/RL
      FA=-.5/RL**3
      FB=1.5/RL**5
      FC=2.5/RL**7
      DO I3=1,3
        U=U+FA*D2(I3,I3)
        DO I2=1,3
          U=U+FB*RLS(I3)*(D2(I2,I3)*RLS(I2)-D3(I2,I2,I3))
          DO I1=1,3
            U=U+FC*D3(I1,I2,I3)*RLS(I1)*RLS(I2)*RLS(I3)
          ENDDO
        ENDDO
      ENDDO
      UC(IX,IY,IZ)=U
C
      END


      SUBROUTINE STEPC(RAC)
      INCLUDE 'NUCLS'
      INCLUDE 'NQUA1'
      INCLUDE 'SIZE2'
      INCLUDE 'SIZEC2'
      DIMENSION UCB(-NT:NT,-NL:NL)
      DIMENSION WT(-NT:NT),WL(-NL:NL),G(-NL:NL)
C
      RACT=RAC/(DWL*DLL)
      RACL=RAC/DWLK
C
      WT(-NT)=1./(2.+RACT*DWLK)         !Note the reversed order...
      DO I=-NT,NT1
        WT(I+1)=1./(2.+RACT*DWLK-WT(I))   !eqs. (2.8)
      ENDDO
C
      WL(-NL)=1./(2.+RACL*DLLK)
      DO I=-NL,NL1
        WL(I+1)=1./(2.+RACL*DLLK-WL(I))   !eqs. (2.8)
      ENDDO
C
c  x sweep
      DO IY=-NT,NT                        !? s-thing to do w/packing??
        DO IX=-NT,NT
          UCB(IX,IY)=UC(IX,IY,-NLP)       !boundary values
        ENDDO
      ENDDO
      DO IZ=-NL,NL
        DO IY=-NT,NT
          DO IX=-NT,NT   !r'_i put into g; eq. (3.10)
            G(IX)=ZHO(IX,IY,IZ)+(RACT-2./DWLK-2./DLLK)*UC(IX,IY,IZ)
     U        +(UC(IX,IY+1,IZ)+UC(IX,IY-1,IZ))/DWLK
     C        +(UC(IX,IY,IZ+1)+UCB(IX,IY))/DLLK
            UCB(IX,IY)=UC(IX,IY,IZ)
          ENDDO
          G(NT)=G(NT)+UC(NTP,IY,IZ)/DWLK
          G(-NT)=WT(-NT)*(G(-NT)+UC(-NTP,IY,IZ)/DWLK)  !eq. (2.9) w/bnd cnd
          DO IX=-NT1,NT
            G(IX)=WT(IX)*(G(IX)+G(IX-1))      !eq. (2.9); g(ix) stores r'_i
          ENDDO
          UC(NT,IY,IZ)=DWLK*G(NT)                           !eq. (2.10)
          DO IX=NT1,-NT,-1
            UC(IX,IY,IZ)=DWLK*G(IX)+WT(IX)*UC(IX+1,IY,IZ)   !eq. (2.10)
          ENDDO
        ENDDO
      ENDDO
C  Y SWEEP
      DO IY=-NT,NT
        DO IX=-NT,NT
          UCB(IX,IY)=UC(IX,IY,-NLP)
        ENDDO
      ENDDO
      DO IZ=-NL,NL
        DO IX=-NT,NT
          DO IY=-NT,NT
            G(IY)=ZHO(IX,IY,IZ)+(RACT-2./DWLK-2./DLLK)*UC(IX,IY,IZ)
     U        +(UC(IX+1,IY,IZ)+UC(IX-1,IY,IZ))/DWLK
     C        +(UC(IX,IY,IZ+1)+UCB(IX,IY))/DLLK
            UCB(IX,IY)=UC(IX,IY,IZ)
          ENDDO
          G(NT)=G(NT)+UC(IX,NTP,IZ)/DWLK
          G(-NT)=WT(-NT)*(G(-NT)+UC(IX,-NTP,IZ)/DWLK)
          DO IY=-NT1,NT
            G(IY)=WT(IY)*(G(IY)+G(IY-1))
          ENDDO
          UC(IX,NT,IZ)=DWLK*G(NT)
          DO IY=NT1,-NT,-1
            UC(IX,IY,IZ)=DWLK*G(IY)+WT(IY)*UC(IX,IY+1,IZ)
          ENDDO
        ENDDO
      ENDDO
C  Z SWEEP
      DO IZ=-NL,NL
        DO IX=-NT,NT
          UCB(IX,IZ)=UC(IX,-NTP,IZ)
        ENDDO
      ENDDO
      DO IY=-NT,NT
        DO IX=-NT,NT
          DO IZ=-NL,NL
            G(IZ)=ZHO(IX,IY,IZ)+(RACL-4./DWLK)*UC(IX,IY,IZ)
     U        +(UC(IX+1,IY,IZ)+UC(IX-1,IY,IZ)+UC(IX,IY+1,IZ)
     C        +UCB(IX,IZ))/DWLK
            UCB(IX,IZ)=UC(IX,IY,IZ)
          ENDDO
          G(NL)=G(NL)+UC(IX,IY,NLP)/DLLK
          G(-NL)=WL(-NL)*(G(-NL)+UC(IX,IY,-NLP)/DLLK)
          DO IZ=-NL1,NL
            G(IZ)=WL(IZ)*(G(IZ)+G(IZ-1))
          ENDDO
          UC(IX,IY,NL)=DLLK*G(NL)
          DO IZ=NL1,-NL,-1
            UC(IX,IY,IZ)=DLLK*G(IZ)+WL(IZ)*UC(IX,IY,IZ+1)
          ENDDO
        ENDDO
      ENDDO
C
      END


      FUNCTION UCS(IZ,R,RL)
C  COULOMB POTENTIAL FOR A SPHERE
C  IZ NO. OF E'S, R - SPHERE RADIUS
C  RL - DISTANCE FROM THE CENTER
C
      INCLUDE 'NUCLS'
      PARAMETER (ELQP=1.4400E-3)
      PARAMETER (ELQ=ELQP*FCOU)
C
      UCS=IZ*ELQ
      IF(RL.GE.R)THEN
        UCS=UCS/RL
      ELSE
        UCS=UCS/R*.5*(3.-(RL/R)**2)
      ENDIF
C
      END


      SUBROUTINE MOVEP(DT)
C  MOVES MOMENTA AND CALCULATES NEW ENERGIES
      INCLUDE 'PARTIC15'
      INCLUDE 'SIZE2'
      INCLUDE 'SIZEC2'
      COMMON/ENERGY/EKIN,EPOT,EPR,APR,ETOG,PARTI(NIDP),PARTIG(NIDP)
     E  ,ANPAR,ANPARG,ANPR(6),ANPRG(6)
     N  ,EKINT,EKINZ,ECINT,ECINZ
     N  ,EKINTG,EKINZG
     E  ,PPRX,PPRY,PPRZ,PPRZL,PGOX,PGOY,PGOZ,PGOZL
     R  ,GST,BGSTX,BGSTY,BGSTZ
     G  ,PRSKT0,PRSP0,PRSKL0,PRSXZ0
      COMMON/COULP/PXCOU,PYCOU,PZCOU
C
      COMMON/FREE/ANFPR(6)
      DIMENSION NFPR(6)
C
      COMMON/CPRI/PRI,TIM,IZCEN
      LOGICAL PRI
C
      PARAMETER (AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER (BD=.002225,AMD=AM0+AM0-BD)
      PARAMETER (RDEM=1.)
      PARAMETER (AMD2=AMD+AMD,AMD4=AMD2+AMD2)
      PARAMETER (AMDK=AMD*AMD)
      PARAMETER (AM02=AM0+AM0,AM0K=AM0*AM0)
C
      PARAMETER (AN0=.160)
C
      DIMENSION FRX(-1:1),FRY(-1:1),FRZ(-1:1)
      DIMENSION FDVUX(-1:1)
      DIMENSION FDVUY(-1:1)
      DIMENSION FDVUZ(-1:1)
      DATA FDVUX(-1)/-.5/FDVUX(1)/.5/
      DATA FDVUY(-1)/-.5/FDVUY(1)/.5/
      DATA FDVUZ(-1)/-.5/FDVUZ(1)/.5/
C
      DATA DUCX,DUCY,DUCZ,DUTX,DUTY,DUTZ/6*0./
c
*     common/ctemp/temp
C
      DTDLL=-DT/DLL
      DTDWL=-DT/DWL
C
      IF(COU)THEN
        VXCOU=-PXCOU/(NQU*EPR)
        VYCOU=-PYCOU/(NQU*EPR)
        VZCOU=-PZCOU/(NQU*EPR)
      ELSE
        VXCOU=0.
        VYCOU=0.
        VZCOU=0.
      ENDIF
C
      DO I=1,6
        NFPR(I)=0
      ENDDO
C
      PPRX=0.
      PPRY=0.
      PPRZ=0.
      PPRZL=0.
      EKIN=0.
      DEPOT=0.
      ekint=0.
      ekinz=0.
*      ekinp=0.
*      nkinp=0
C  THIS WILL BE FOR CALCULATION OF TOTAL ENERGY
      DO 100 IP=1,NQ     ! loop over ptcles
      IDP=ID(IP)
      IF(IDP.EQ.0)GOTO 100
      IIDP=ABS(IDP)
      IF(IIDP.LE.6)THEN   ! only for statistics
        IF(IPTI(IP).LT.0)THEN
          NFPR(IIDP)=NFPR(IIDP)+1
        ENDIF
      ENDIF
      IF(IDP.LT.0)GOTO 100
C
      AMVI=AMV(IP)
      PXI=PX(IP)
      PYI=PY(IP)
      PZI=PZ(IP)
      PK=PXI*PXI+PYI*PYI+PZI*PZI
      BARP=BAR(IDP)
C
      XXN=XX(IP)/DWL
      IX=NINT(XXN)
      IF(ABS(IX).GT.NT)IX=SIGN(NT,IX)
      YYN=YY(IP)/DWL
      IY=NINT(YYN)
      IF(ABS(IY).GT.NT)IY=SIGN(NT,IY)   !precautions on June 10, 2010
      ZZN=ZZ(IP)/DLL
      IZ=NINT(ZZN)
      IF(ABS(IZ).GT.NL)IZ=SIGN(NL,IZ)   ! after 0.5 failure
C
      IXF=IX+1
      IXB=IX-1
      FRX(1)=.5*(XXN-IX+.5)
      FRX(-1)=.5-FRX(1)
      FRX(0)=.5
      FDVUX(0)=0.
      IYF=IY+1
      IYB=IY-1
      FRY(1)=.5*(YYN-IY+.5)
      FRY(-1)=.5-FRY(1)
      FRY(0)=.5
      FDVUY(0)=0.
      IZF=IZ+1
      IZB=IZ-1
      FRZ(1)=.5*(ZZN-IZ+.5)
      FRZ(-1)=.5-FRZ(1)
      FRZ(0)=.5
      FDVUZ(0)=0.
C
      IF(ABS(IX).EQ.NT)THEN
        IF(IX.EQ.NT)THEN
          FRX(0)=1.-FRX(-1)
          FDVUX(0)=.5
          IXF=NT
        ELSE
          FRX(0)=1.-FRX(1)
          FDVUX(0)=-.5
          IXB=-NT
        ENDIF
      ENDIF
C
      IF(ABS(IY).EQ.NT)THEN
        IF(IY.EQ.NT)THEN
          FRY(0)=1.-FRY(-1)
          FDVUY(0)=.5
          IYF=NT
        ELSE
          FRY(0)=1.-FRY(1)
          FDVUY(0)=-.5
          IYB=-NT
        ENDIF
      ENDIF
C
      IF(ABS(IZ).EQ.NL)THEN
        IF(IZ.EQ.NL)THEN
          FRZ(0)=1.-FRZ(-1)
          FDVUZ(0)=.5
          IZF=NL
        ELSE
          FRZ(0)=1.-FRZ(1)
          FDVUZ(0)=-.5
          IZB=-NL
        ENDIF
      ENDIF
C
      DUX=0.
      DUCX=0.
      DUTX=0.
      DUY=0.
      DUCY=0.
      DUTY=0.
      DUZ=0.
      DUCZ=0.
      DUTZ=0.
C
      IF(POT)THEN
        DO IIZ=IZB,IZF
          FRZU=FDVUZ(IIZ-IZ)
          FRZZ=FRZ(IIZ-IZ)
          DO IIY=IYB,IYF
            IB=IIY-IY
            FRYY=FRZZ*FRY(IB)
            FRUYY=FRZZ*FDVUY(IB)
            FRUZZ=FRZU*FRY(IB)
            DO IIX=IXB,IXF
              IB=IIX-IX
              FRUX=FRYY*FDVUX(IB)
              IF(BARP.NE.0.)THEN
                IF(LMI)THEN
                  UUI=SQRT((AMVI+BARP*UU(IIX,IIY,IIZ))**2+PK)
                ELSE
                  CALL PTNO(VXNO(IIX,IIY,IIZ),VYNO(IIX,IIY,IIZ)
     C              ,VZNO(IIX,IIY,IIZ),RHO(IIX,IIY,IIZ)/AN0
     A              ,UU(IIX,IIY,IIZ),AMVI,BARP,PXI,PYI,PZI
     L              ,UUI,PC,EC,VXI,VYI,VZI,2)  ! Note: recalculates EE!
                ENDIF
                UUI=UUI-AMVI
              ELSE
                UUI=0.
              ENDIF
              IF(COU)THEN
                UCI=UC(IIX,IIY,IIZ)
                THOI=UTU(IIX,IIY,IIZ)
                UUI=UUI+BARP*UTTU(IIX,IIY,IIZ)
              ELSE
                UCI=0.
                THOI=0.
              ENDIF
              DUX=DUX+FRUX*UUI
              DUCX=DUCX+FRUX*UCI
              DUTX=DUTX+FRUX*THOI
              FRXI=FRX(IB)
              FRUY=FRUYY*FRXI
              DUY=DUY+FRUY*UUI
              DUCY=DUCY+FRUY*UCI
              DUTY=DUTY+FRUY*THOI
              FRUZ=FRUZZ*FRXI
              DUZ=DUZ+FRUZ*UUI
              DUCZ=DUCZ+FRUZ*UCI
              DUTZ=DUTZ+FRUZ*THOI
            ENDDO
          ENDDO
        ENDDO
C
        UUI=UU(IX,IY,IZ)
      ELSE
        UUI=0.
      ENDIF
      AMVU=AMVI+BARP*UUI
C
      EEPO=SQRT(AMVI*AMVI+PK)
      IF(POT.AND.LMO)THEN
        IF(BARP.NE.0.)THEN
          RHOV=RHO(IX,IY,IZ)/AN0
          VXN=VXNO(IX,IY,IZ)
          VYN=VYNO(IX,IY,IZ)
          VZN=VZNO(IX,IY,IZ)
          CALL PTNO(VXN,VYN,VZN,RHOV
     A      ,UUI,AMVI,BARP,PXI,PYI,PZI
     L      ,EE(IP),PC,EC,VXI,VYI,VZI,2)  ! Note: recalculates EE!
          DEPO=EE(IP)-EEPO-BARP*UUI
        ELSE
          EE(IP)=EEPO
          DEPO=0.
        ENDIF
      ELSE
        EE(IP)=SQRT(AMVU*AMVU+PK)
        DEPO=EE(IP)-EEPO-AMVU/EE(IP)*UUI*BARP
      ENDIF
C
      ZP=ZPA(IDP)
      TIZ=TIZO(IDP)
      PX(IP)=PX(IP)+DTDWL*(DUX+ZP*DUCX+TIZ*DUTX)+VXCOU*EE(IP)
      PY(IP)=PY(IP)+DTDWL*(DUY+ZP*DUCY+TIZ*DUTY)+VYCOU*EE(IP)
      PZ(IP)=PZ(IP)+DTDLL*(DUZ+ZP*DUCZ+TIZ*DUTZ)+VZCOU*EE(IP)
C  COULOMB RECOIL GIVEN TO ALL PARTICLES
      PTK=PX(IP)**2+PY(IP)**2
      PZK=PZ(IP)**2
      PK=PTK+PZK
      EEVI=SQRT(PK+AMVI*AMVI)
      IF(POT.AND.LMO)THEN
        IF(BARP.NE.0.)THEN
          CALL PTNO(VXN,VYN,VZN,RHOV
     A      ,UUI,AMVI,BARP,PX(IP),PY(IP),PZ(IP)
     L      ,EE(IP),PC,EC,VXI,VYI,VZI,2)  ! Note: recalculates EE!
          DEPI=EE(IP)-EEVI-BARP*UUI
        ELSE
          EE(IP)=EEVI
          DEPI=0.
        ENDIF
      ELSE
        EE(IP)=SQRT(PK+AMVU*AMVU)
        DEPI=EE(IP)-EEVI-AMVU/EE(IP)*UUI*BARP
      ENDIF
      DEPOT=DEPOT+.5*(DEPO+DEPI)
      EKIN=EKIN+.5*(EEVI+EEPO)
      ekint=ekint+ptk/(amvI+eeVI)
      ekinz=ekinz+pzk/(amvI+eeVI)
      PPRX=PPRX+PX(IP)
      PPRY=PPRY+PY(IP)
      PPRZ=PPRZ+PZ(IP)
      CALL LAB(EE(IP),PZ(IP),EEL,PZL,AMM,YL)
      PPRZL=PPRZL+PZL
 80   CONTINUE
C
 90   CONTINUE
 100  CONTINUE
C
      PPRX=PPRX/NQU
      PPRY=PPRY/NQU
      PPRZ=PPRZ/NQU
      PPRZL=PPRZL/NQU
      ekint=ekint/nqu
      ekinz=ekinz/nqu
      EKIN=EKIN/NQU
      DEPOT=DEPOT/NQU
      EPOT=EPOT+DEPOT
*      ekinp=ekinp/(2.*am0*nkinp)
*      temp=2.*ekinp/3.
C
      DO I=1,6
        ANFPR(I)=NFPR(I)/REAL(NQU)
      ENDDO
C
      END


      SUBROUTINE FINDU
      INCLUDE 'PARTIC15'
      INCLUDE 'SIZE2'
      INCLUDE 'SIZEC2'
      INCLUDE 'SURF'
      COMMON/ENERGY/EKIN,EPOT,EPR,APR,ETOG,PARTI(NIDP),PARTIG(NIDP)
     E  ,ANPAR,ANPARG,ANPR(6),ANPRG(6)
     N  ,EKINT,EKINZ,ECINT,ECINZ
     N  ,EKINTG,EKINZG
     E  ,PPRX,PPRY,PPRZ,PPRZL,PGOX,PGOY,PGOZ,PGOZL
     R  ,GST,BGSTX,BGSTY,BGSTZ
     G  ,PRSKT0,PRSP0,PRSKL0,PRSXZ0
C
      PARAMETER (B=.008,AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER (BD=.002225,AMD=AM0+AM0-BD)
      PARAMETER (AMD4=4.*AMD)
C
      COMMON/CPRI/PRI,TIM,IZCEN
      LOGICAL PRI
C
      COMMON/COMPSU/FUZ
C
      COMMON/CRPI/RHOPI,REFIN
C
      PARAMETER (AN0=.160)
      PARAMETER(H=.19733)
C
      PARAMETER (CSUR=.067)   !*(1.-MO*.15))      !coef in surface energy in GeV*fm**5
      PARAMETER (QDWLK=(.5/DWL)**2,QDLLK=(.5/DLL)**2)
      PARAMETER (CSUQW=CSUR*QDWLK,CSUQL=CSUR*QDLLK)
C
      LOGICAL LCEN,LYCEN,LZCEN
C
C  COMPLETING THE TOTAL ENERGY
      EPOT=0.
      RTMX=0.
      izmx=0
      iymx=0
      ixmx=0
      pi=4.*atan(1.)
      ski0=h*h/(6.*am0)*(1.5*pi*pi*an0)**(2./3.)
      stox=0.
      ttx=0.
      FERT=AN0*125./(27.*64)/(NQU*VOLE)
      rtnx=0.
      rtpx=0.
      RHOK=0.
      RHS=0.
      PRSP0=0.
*     DO IZ=-NLP,NLP
*       DO IY=-NTP,NTP
*         DO IX=-NTP,NTP
      DO IZ=-NL,NL
        LZCEN=IZ.EQ.0
        DO IY=-NT,NT
          LYCEN=LZCEN.AND.IY.EQ.0
          DO IX=-NT,NT
            LCEN=LYCEN.AND.IX.EQ.0
            RHOX=RHO(IX,IY,IZ)
            RHS=RHS+RHOX
            RHOK=RHOK+RHOX*RHOX
            RT=RHOX/AN0
            THOX=THO(IX,IY,IZ)
            TT=THOX/AN0
            IF(POT)THEN
              UPOR=UPO(RT)
              IF(LMI)THEN
                IF(LCEN)PRSP0=RHOX*UPOR
                UU(IX,IY,IZ)=UPOR-2.*CSUR*RHOS(IX,IY,IZ)
              ELSE
                IF(LCEN)PRSP0=RHOX*(UPOR+UU(IX,IY,IZ))
Cc pressure
C                if(iy.eq.0)then
C                  ptxz(ix,iz)=ptxz(ix,iz)+rhox*(UPOR+UU(IX,IY,IZ))
C                endif
C                if(iz.eq.0)then
C                  ptxy(ix,iy)=ptxy(ix,iy)+rhox*(UPOR+UU(IX,IY,IZ))
C*                  ptpxy(ix,iy)=rhox*(UPOR+UU(IX,IY,IZ))
C                endif
C*                if(lcen)write(*,*)ptpxy(0,0)*1e3,prsp0*1e3,'!!!!'
                UU(IX,IY,IZ)=UU(IX,IY,IZ)
     U            +UPOR-2.*CSUR*RHOS(IX,IY,IZ)
              ENDIF
              IF(COU)THEN
                UTTU(IX,IY,IZ)=4.*DSIX(RT)*TT*TT
                UTU(IX,IY,IZ)=8.*SINTX(RT)*TT
                UTTE(IX,IY,IZ)=4.*SINTXX(RT)*TT*TT
              ELSE
                UTTU(IX,IY,IZ)=0.
                UTU(IX,IY,IZ)=0.
                UTTE(IX,IY,IZ)=0.
              ENDIF
            ELSE
              UU(IX,IY,IZ)=0.
              UTTU(IX,IY,IZ)=0.
              UTU(IX,IY,IZ)=0.
              UTTE(IX,IY,IZ)=0.
            ENDIF
            if(ix.eq.0.and.iy.eq.0.and.abs(iz).le.22)then
*              if(pri)write(*,*)'iz rt ',iz,rt
*              if(abs(tt).gt.1e-5)then
*                write(*,*)ix,iy,iz,utte(ix,iy,iz)*.25*(rt/tt)**2
*              else
*                write(*,*)ix,iy,iz,0.
*              endif
            endif
            RTN=-TT+.5*RT
            RTP=RT-RTNX
            rtnx=max(rtnx,rtn)
            rtpx=max(rtpx,rtp)
            IF(RT.GT.RTMX)THEN
              RTMX=RT
              ixmx=ix
              iymx=iy
              izmx=iz
*              RTNX=-TT+.5*RT
*              RTPX=RT-RTNX
            ENDIF
*            RTMX=MAX(RTMX,RT)
*           if(rt.gt..4)then
*             sto=(ski0*rt**(2./3.)+sint(rt))*4.*(tt/rt)**2
*           else
*             sto=0.
*           endif
*           if(sto.gt.stox)then
*             stox=sto
*             ttx=abs(tt)
*             rtnx=-tt+.5*rt
*             rtpx=rt-rtnx
*           endif
            EPOR=EPO(RT)
            EPOT=EPOT+EPOR
            IF(LCEN)PRSP0=PRSP0-EPOR
Cc pressure
C            if(lmo)then
C              if(iy.eq.0)then
C                ptxz(ix,iz)=ptxz(ix,iz)-epor
C              endif
C              if(iz.eq.0)then
C                ptxy(ix,iy)=ptxy(ix,iy)-epor
C*                ptpxy(ix,iy)=ptpxy(ix,iy)-epor
C              endif
C            endif
            IF(COU)THEN
              EPOT=EPOT+FUZ*ZHO(IX,IY,IZ)*UC(IX,IY,IZ)
              IF(POT)EPOT=EPOT+UTTE(IX,IY,IZ)*RHOX
*              IF(POT)EPOT=EPOT+.5*UTU(IX,IY,IZ)*THOX
            ENDIF
            IF(POT)THEN
            IF(ABS(IX).LT.NT)
     E        EPOT=EPOT+CSUQW*(RHO(IX+1,IY,IZ)-RHO(IX-1,IY,IZ))**2
            IF(ABS(IY).LT.NT)
     E        EPOT=EPOT+CSUQW*(RHO(IX,IY+1,IZ)-RHO(IX,IY-1,IZ))**2
            IF(ABS(IZ).LT.NL)
     E        EPOT=EPOT+CSUQL*(RHO(IX,IY,IZ+1)-RHO(IX,IY,IZ-1))**2
            ENDIF
          ENDDO
        ENDDO
      ENDDO
      IF(RHS.NE.0.)RHOK=RHOK/RHS
      RHOK=RHOK*APR/(IA1+IA2)
      RHOK=RHOK/AN0
      EPOT=EPOT*VOLE
      IF(PRI)THEN
        rhopro=tho(0,0,izcen)+.5*rho(0,0,izcen)
        rhoneu=-tho(0,0,izcen)+.5*rho(0,0,izcen)
        if(rhopro.gt.0.)then
          ranp=rhoneu/rhopro
        else
          ranp=0.
        endif
        rt0=rho(0,0,izcen)/an0
        tt0=tho(0,0,izcen)/an0
        if(rt0.gt.1e-4)then
          sto0=(ski0*rt0**(2./3.)+sint(rt0))*4.*(tt0/rt0)**2
        else
          sto0=0.
        endif
        RTNXE=SQRT(RTNX*FERT)
        RTPXE=SQRT(RTPX*FERT)
        WRITE(*,'(1X,a,f7.2,a,f7.4,a,f7.4,9(A,F7.4))')
     w    'tim = ',tim
     I    ,' RTMX = ',RTMX,' RTNX = ',RTNX,' +- ',rtnxe
     T    ,' RTPX = ',RTPX,' +- ',rtpxe
     w    ,' RT0 = ',Rt0,' RHOV = ',RHOK
     e    ,' rp0 = ',rhopro/an0,' rn0 = ',rhoneu/an0
*        write(*,*)'ix/iy/iz (max) ',ixmx,iymx,izmx
        IF(PENT)WRITE(*,*)'sp0 = ',SPAZ(0,izcen)
      ENDIF
C
      END


      SUBROUTINE PTNO(VNOX,VNOY,VNOZ,RVNO,DUPNO
     S  ,AMVI,BARP,PXI,PYI,PZI,EI,PC,EC,VXI,VYI,VZI,NIT)
C  CALCULATES PTCLE ENERGY/VELOCITY, GIVEN NODE INFO
C  FOR MO-DEP CASE; NIT - NO. OF ITERATIONS
C  NOTE: RVNO IS REDUCED **REST-FRAME** DENSITY
      PARAMETER(AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
Cc  pressure
C      common/pres/ppvc
C
C
      ppvc=0.
      AG=VNOX*VNOX+VNOY*VNOY+VNOZ*VNOZ
      IF(AG.LT..99999)THEN
        GNO=1./SQRT(1.-AG)
        BGNOX=GNO*VNOX
        BGNOY=GNO*VNOY
        BGNOZ=GNO*VNOZ
      ELSE
        GNO=1.
        BGNOX=0.
        BGNOY=0.
        BGNOZ=0.
      ENDIF
      RHAG=BARP*AM0/AMVI*FXI(RVNO) ! density-dependent argument for velocity
      PK=PXI*PXI+PYI*PYI+PZI*PZI
      EI=SQRT(AMVI*AMVI+PK)
      IT=0
 10   CONTINUE
      IT=IT+1
      CALL LOREN(-BGNOX,-BGNOY,-BGNOZ,GNO,PXI,PYI,PZI,EI
     L  ,PCX,PCY,PCZ,EC)
      PCK=PCX*PCX+PCY*PCY+PCZ*PCZ
      PC=SQRT(PCK)
*      write(*,*)'pc = ',pc
      IF(BARP.LT..5)GOTO 60
      EC=AMVI+AMVI*EKIM(PC/AMVI,RHAG)+BARP*DUPNO
*      write(*,*)'ec = ',ec-amvi
      AMK=EC*EC-PCK
      IF(AMK.GT.0.)THEN
        EI=SQRT(AMK+PK)
      ELSE
        CALL LOREN(BGNOX,BGNOY,BGNOZ,GNO,PCX,PCY,PCZ,EC
     L    ,AA,BB,CC,EI)
        IF(EI.EQ.0.)EI=1E-3
        EI=SIGN(SQRT(AMK+PK),EI)
      ENDIF
      IF(IT.LT.NIT)GOTO 10
C
      VC=VEL(PC/AMVI,RHAG)
      GC=1./SQRT(MAX(1.-VC*VC,1E-6))
      GVF=GC*VC/MAX(PC,1E-5)
      CALL LOREN(BGNOX,BGNOY,BGNOZ,GNO,GVF*PCX,GVF*PCY,GVF*PCZ
     L  ,GC,GVXI,GVYI,GVZI,GV)
      VXI=GVXI/GV
      VYI=GVYI/GV
      VZI=GVZI/GV
Cc  pressure
C      ppvc=(pcx*pcx+pcy*pcy)*vc/max(pc,1e-5)
      RETURN
C
 60   CONTINUE
      VXI=PXI/EI
      VYI=PYI/EI
      VZI=PZI/EI
Cc  pressure
C      ppvc=(pcx*pcx+pcy*pcy)/ec
C
      END


      FUNCTION EPO(RT)
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


      FUNCTION UPO(RT)  !t)   ! for the phase transition
      INCLUDE 'NUCLS'
C  OPTICAL POTENTIAL IN NONRELATIVISTIC MATTER IN GEV'S
C  RT IS THE DENSITY RATIO
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
**      rt=min(rtt,2.2)      ! for the phase transition
*      rt=min(rtt,1.)    ! when limiting the mo-dep
*      rt=min(rtt,3.5)    ! PT w/lim mo-dep
      IF(RT.GT.1E-5.AND.POT)THEN
        RTG=RT**GES1
        UPO=RT*(AES+BES*RTG)/(1.+F1*RTG)
**        avv=.876636
**        bvv=.791396
**        cvv1=1.04061-1.
**        dvv=.390701e-3
**        upo=rt*(-avv+bvv*(rt**cvv1+dvv*rt**(cvv1+5.)))
**     u    /(1.+(rt/3.)**(cvv1+5.))
      ELSE
        UPO=0.
      ENDIF
*      if(rtt.gt.1.001)upo=upo+(rtt-1.)**.35*.025  !when limiting mo-dep
*      if(rt.gt.1.7.and.pot)then
*        rt2=(rt-1.7)**2
*        upo=upo+.16*rt2/(.7+rt2)
*      endif
C
      END


      INCLUDE 'EOSET'


      FUNCTION VEL(PM,RHAG)
C  CALCULATES VELOCITY IN THE MO-DEP CASE
C  1ST ARG: MOMENTUM/MASS_X
C  2ND ARG: COMBINATION DEPENDENT ON DENSITY
C
      COMMON/CEOS/AES,BES,GES,GES1,APES,BPES
      PARAMETER(AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
C
      PMK=PM*PM
      VEL=PM/SQRT(PMK+1./(1.+RHAG/(1.+BPES*PMK)**2)**2)
C
      END


      FUNCTION VDRP(PM,RHAG)
C  MO-DEP CASE:
C  CALCULATES FACTOR WITHIN DERIVATIVE D_VELOCITY/D_RHO THAT DEPENDS ON P/M
C  1ST ARG: MOMENTUM/MASS_X
C  2ND ARG: COMBINATION DEPENDENT ON DENSITY
      COMMON/CEOS/AES,BES,GES,GES1,APES,BPES
C
      PMK=PM*PM
      PB=1./(1.+BPES*PMK)**2
      PA=1./(1.+RHAG*PB)
      VDRP=PM*PB*(PA/SQRT(PMK+PA*PA))**3
C
      END


      FUNCTION DUPNOF(RVP)
C  CALCULATES ADDITION TO 1-PTCLE ENERGY FOR MO-DEP POT
      PARAMETER(NRV=20,DXR=1./NRV,NRV1=NRV-1)
      DIMENSION DUPNI(0:NRV)
C
      PARAMETER(RVC=1.)
      PARAMETER(NI=20)
C
      PARAMETER(AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER(AN0=0.160)
      PARAMETER(H=.19733)
      common/ceos/aes,bes,ges,ges1,apes,bpes
C
      SAVE DUPNI
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C
      IF(FIRST)THEN
        PI=4.*ATAN(1.)
        FF=(3.*AN0/(4.*PI)/4.)**(1./3.)*2.*PI*H
        DO I=0,NRV
          IF(I.LT.NRV)THEN
            XR=I*DXR
            RVV=RVC*XR/(1.-XR)
          ELSE
            RVV=1E8
          ENDIF
          PF=FF*RVV**(1./3.)
          DU=0.
          RHAG=FXI(RVV)
          DO II=1,NI
            P=PF*(II-.5)/NI
            DU=DU+P*P*AIVRP(P/AM0,RHAG)
          ENDDO
          DU=DU*PF/NI
          IF(PF.NE.0.)DU=DU*3./PF**3
          DU=DU*RVV*AM0*FXIP(RVV)   !adding missing factors
          DUPNI(I)=DU
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
      DUPNOF=XI1*DUPNI(I)+XI*DUPNI(I1)
C
      END


      FUNCTION AIVRP(PMP,RHAGP)
C  MO-DEP CASE:
C  CALCULATES PM-INTEGRAL OVER FACTOR WITHIN DERIVATIVE
C    D_VELOCITY/D_RHO THAT DEPENDS ON P/M
C  1ST ARG: MOMENTUM/MASS_X
C  2ND ARG: COMBINATION DEPENDENT ON DENSITY
      PARAMETER(NRV=20,DXR=1./NRV)
      PARAMETER(NPM=20,DYP=1./NPM)
      PARAMETER(NPP=15)
      DIMENSION REK(0:NRV,0:NPM)
      PARAMETER(NRV1=NRV-1,NPM1=NPM-1)
C
      PARAMETER(RVC=2.,PMC=.4)
C
      COMMON/CEOS/AES,BES,GES,GES1,APES,BPES
      PARAMETER(AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
C
      SAVE REK,RHAGC
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C
      IF(FIRST)THEN
        RHAGC=FXI(RVC)      !density-dep argument in mass
        DO I=0,NRV
          IF(I.LT.NRV)THEN
            XR=I*DXR
            RHAV=RHAGC*XR/(1.-XR)
          ELSE
            RHAV=1E8
          ENDIF
C
          PMB=0.
          AIN=0.
          REK(I,NPM)=0.
          DO J=1,NPM1
            YP=J*DYP
            PMM=PMC*YP/(1.-YP)
            DPP=(PMM-PMB)/NPP
            DO JJ=1,NPP
              AIN=AIN+DPP*VDRP((PMB+(JJ-.5)*DPP),RHAV)
            ENDDO
            PMB=PMM
            PMK=PMM*PMM
            PMKS=SQRT(1.+PMK)
            REK(I,J)=AIN*PMKS*(1.+PMKS)/PMK
C  DIVIDED BY INTEGRAL EXPECTED IF MO-DEP WEAK
          ENDDO
          REK(I,0)=1.  ! p->0 limit
        ENDDO
*        do i=0,nrv
*          do j=0,npm
*            write(*,*)i,j,rek(i,j)
*          enddo
*        enddo
*        stop
        FIRST=.FALSE.
      ENDIF
C
      RHAG=MAX(RHAGP,0.)       !cleaning up arguments
      PM=MAX(PMP,0.)
C
      XR=NRV*RHAG/(RHAG+RHAGC)
      I=XR
      I=MIN(I,NRV1)
      I1=I+1
      XI=XR-I
      XI1=1.-XI
C
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
      PMKS=SQRT(1.+PMK)
      AIVRP=RE*PMK/(PMKS*(1.+PMKS))  !restored factor f/smoothness of interp
C
      END


      FUNCTION EKIM(PMP,RHAGP)
C  CALCULATES RATIO EKIN/MASS IN THE MO-DEP CASE
C  RHAGP IS THE DENSITY-DEPENDENT ARGUMENT IN VELOCITY
      PARAMETER(NRV=20,DXR=1./NRV)
      PARAMETER(NPM=20,DYP=1./NPM)
      PARAMETER(NPP=8)
      DIMENSION REK(0:NRV,0:NPM)
      PARAMETER(NRV1=NRV-1,NPM1=NPM-1)
C
      PARAMETER(RVC=1.,PMCC=.263)
C
      COMMON/CEOS/AES,BES,GES,GES1,APES,BPES
      PARAMETER(AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
C
      SAVE REK,RHAGC
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C
      IF(FIRST)THEN
        RHAGC=FXI(RVC)      !density-dep argument in mass
        DO I=0,NRV
          IF(I.LT.NRV)THEN
            XR=I*DXR
            RHAV=RHAGC*XR/(1.-XR)       !mesh in the argument
          ELSE
            RHAV=1E8
          ENDIF
          AMF=1./(1.+RHAV)  !mass/vacuum mass for p=0 at given argument
          AMFK=AMF*AMF
          PMC=PMCC/AMF      !chcracteristic momentum
C                 !kin en/m from integrating velocity over mom/m
          PMB=0.
          AIN=0.
          REK(I,0)=1.     !rek: ratio of real to naive kinetic energy
          REK(I,NPM)=1.   !  =1 at edges
          DO J=1,NPM1
            YP=J*DYP
            PMM=PMC*YP/(1.-YP)
            DPP=(PMM-PMB)/NPP
            DO JJ=1,NPP       !submesh in mo for integration
              AIN=AIN+DPP*VEL((PMB+(JJ-.5)*DPP),RHAV) !density-dep argument fixed
            ENDDO
            PMB=PMM
            PMK=PMM*PMM
            REK(I,J)=AIN*(AMF+SQRT(AMFK+PMK))/PMK  !kin en from int/naive
          ENDDO
        ENDDO
*        do i=0,nrv
*          do j=0,npm
*            write(*,*)i,j,rek(i,j)
*          enddo
*        enddo
*        stop
        FIRST=.FALSE.
      ENDIF
C
      RHAG=MAX(RHAGP,0.)       !cleaning up arguments
      PM=MAX(PMP,0.)
C
      XR=NRV*RHAG/(RHAG+RHAGC)
      I=XR
      I=MIN(I,NRV1)
      I1=I+1
      XI=XR-I
      XI1=1.-XI
C
      AMF=1./(1.+RHAG)
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


      SUBROUTINE GIVEE
C  CALCULATES CURRENT ENERGIES
      INCLUDE 'PARTIC15'
      INCLUDE 'SIZE2'
      INCLUDE 'SIZEC2'
C
      PARAMETER (B=.008,AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER (AM0K=AM0*AM0,AM02=AM0+AM0)
      PARAMETER (BD=.002225,AMD=AM0+AM0-BD)
      PARAMETER (AMDK=AMD*AMD,AMD4=AMD*4.)
C
      PARAMETER (AN0=.160)
C
      COMMON/ENERGY/EKIN,EPOT,EPR,APR,ETOG,PARTI(NIDP),PARTIG(NIDP)
     E  ,ANPAR,ANPARG,ANPR(6),ANPRG(6)
     N  ,EKINT,EKINZ,ECINT,ECINZ
     N  ,EKINTG,EKINZG
     E  ,PPRX,PPRY,PPRZ,PPRZL,PGOX,PGOY,PGOZ,PGOZL
     R  ,GST,BGSTX,BGSTY,BGSTZ
     G  ,PRSKT0,PRSP0,PRSKL0,PRSXZ0
C
      EKIN=0.
      DO 100 IZ=-NL,NL
      DO 100 IY=-NT,NT
      DO 100 IX=-NT,NT
      IMXXYZ=IMX(IX,IY,IZ)
      IF(IMXXYZ.LT.0)GOTO 100
      UUP=UU(IX,IY,IZ)
      UCI=UC(IX,IY,IZ)
      rhox=rho(ix,iy,iz)
      rt=rhox/an0
      UTTI=UTTU(IX,IY,IZ)
      UTI=UTU(IX,IY,IZ)
      UTEI=UTTE(IX,IY,IZ)
*      UTEI=.5*UTI*THO(IX,IY,IZ)/MAX(RHOX,.16E-4)
      IMNXYZ=IMN(IX,IY,IZ)
      DO 90 I=IMNXYZ,IMXXYZ
      IP=IPO(I)
      IDP=ID(IP)
      IF(IDP.LE.0.OR.IDP.GE.13)GOTO 90
      PK=PX(IP)**2+PY(IP)**2+PZ(IP)**2
      AMVI=AMV(IP)
      bari=bar(idp)
      IF(POT)THEN
        IF(LMI)THEN
          EE(IP)=SQRT(PK+(AMVI+BAR(IDP)*UUP)**2)
        ELSE
          CALL PTNO(VXNO(IX,IY,IZ),VYNO(IX,IY,IZ)
     C      ,VZNO(IX,IY,IZ),RHO(IX,IY,IZ)/AN0
     A      ,UUP,AMVI,BAR(IDP),PX(IP),PY(IP),PZ(IP)
     L      ,EE(IP),PC,EC,VXI,VYI,VZI,2)  ! Note: recalculates EE!
        ENDIF
      ELSE
        EE(IP)=SQRT(PK+AMVI*AMVI)
      ENDIF
      EKIN=EKIN+SQRT(PK+AMVI*AMVI)
      mde(ip)=etom*(zpa(idp)*uci+tizo(idp)*UTI+bari*UTTI)
      mdep(ip)=etom*(.5*zpa(idp)*uci+bari*UTEI)
*      mdep(ip)=.5*mde(ip)   !test
 90   CONTINUE
 100  CONTINUE
      EKIN=EKIN/NQU
C
      END


      SUBROUTINE COUL(I)
      INCLUDE 'PARTIC15'
      INCLUDE 'SIZE2'
      INCLUDE 'SIZEC2'
C
      COMMON/COULP/PXCOU,PYCOU,PZCOU
C
      COMMON/ENERGY/EKIN,EPOT,EPR,APR,ETOG,PARTI(NIDP),PARTIG(NIDP)
     E  ,ANPAR,ANPARG,ANPR(6),ANPRG(6)
     N  ,EKINT,EKINZ,ECINT,ECINZ
     N  ,EKINTG,EKINZG
     E  ,PPRX,PPRY,PPRZ,PPRZL,PGOX,PGOY,PGOZ,PGOZL
     R  ,GST,BGSTX,BGSTY,BGSTZ
     G  ,PRSKT0,PRSP0,PRSKL0,PRSXZ0
C
C  PARTICIPANTS ONLY
      IF(ID(I).LE.0)RETURN
      IF(IPTI(I).GT.0)RETURN
      IF(MDE(I).EQ.0)RETURN
C   VECTOR TO THE CENTER OF CHARGE
      RDX=XX(I)-RDS(1)
      RDY=YY(I)-RDS(2)
      RDZ=ZZ(I)-RDS(3)
C
      RD=SQRT(RDX*RDX+RDY*RDY+RDZ*RDZ)
      IF(RD.EQ.0.)RETURN
C   UNIT VECTOR IN THE RD DIRECTION
      RDNX=RDX/RD
      RDNY=RDY/RD
      RDNZ=RDZ/RD
C  COULOMB ENERGY
      ECO=PETOM*MDE(I)
      ALF=ECO*RD
C  FRAME OF THE CHARGE
      CALL LOREN(-BGSTX,-BGSTY,-BGSTZ,GST
     L  ,PX(I),PY(I),PZ(I),EE(I),PCX,PCY,PCZ,EC)
C   LONGITUDINAL PROJECTION
      PL=PCX*RDNX+PCY*RDNY+PCZ*RDNZ
C   TRANVERSE PART
      PTX=PCX-PL*RDNX
      PTY=PCY-PL*RDNY
      PTZ=PCZ-PL*RDNZ
C
      PT=SQRT(PTX*PTX+PTY*PTY+PTZ*PTZ)
      IF(PT.EQ.0.)RETURN
C  ANGULAR MOMENTUM
      ANGM=PT*RD
      ANGMK=ANGM*ANGM
C  MASS
      PK=PCX*PCX+PCY*PCY+PCZ*PCZ
      AMK=EC*EC-PK
      AM=SQRT(AMK)
      TE=PK/(AM+AM)
      TEF=TE+ECO
C  PARAMETERS
      PHO=ANGMK/(AM*ALF)              !can be negative
      ECC=SQRT(MAX(1.+2.*TEF*ANGMK/(AM*ALF*ALF),0.))
C
      AG=(PHO/RD+1.)/ECC
      IF(ABS(AG).GT.1.)RETURN
      PHICC=ACOS(AG)
      AG=1./ECC
      IF(ABS(AG).GT.1.)RETURN
      PHICA=ACOS(AG)
      IF(PL.GT.0.)THEN
        PHIRO=PHICA-PHICC
      ELSE
        PHIRO=PHICA+PHICC
      ENDIF
      IF(ECO.LT.0.)PHIRO=-PHIRO
C
      PTNX=PTX/PT
      PTNY=PTY/PT
      PTNZ=PTZ/PT
C
      EEA=EC+ECO
      PAK=EEA*EEA-AMK
      IF(PAK.LE.0.)RETURN
      PA=SQRT(PAK)
C
      PAT=PA*SIN(PHIRO)
      PAL=PA*COS(PHIRO)
      PXA=PAT*PTNX+PAL*RDNX
      PYA=PAT*PTNY+PAL*RDNY
      PZA=PAT*PTNZ+PAL*RDNZ
C  LORENTZ BACK TO OVERALL CM
      CALL LOREN(BGSTX,BGSTY,BGSTZ
     L  ,GST,PXA,PYA,PZA,EEA,PXA,PYA,PZA,EEA)
C
      PXCOU=PXCOU+PXA-PX(I)
      PYCOU=PYCOU+PYA-PY(I)
      PZCOU=PZCOU+PZA-PZ(I)
C
      EE(I)=EEA
      PX(I)=PXA
      PY(I)=PYA
      PZ(I)=PZA
      MDE(I)=0
      mdep(i)=0
C
      END


      SUBROUTINE LOREN(BGX,BGY,BGZ,G,PCX,PCY,PCZ,EC,PX,PY,PZ,E)
C  LORENTZ TRANSF.  (PC,EC) BOOSTED WITH (BG,G) TO (P,E)
C  NOTE: BG IS GAMMA*BETA
C
      BGPC=BGX*PCX+BGY*PCY+BGZ*PCZ
      EG=EC+BGPC/(G+1.)
      PX=PCX+EG*BGX
      PY=PCY+EG*BGY
      PZ=PCZ+EG*BGZ
C
      E=G*EC+BGPC
C
      END


      SUBROUTINE COOL
      INCLUDE 'PARTIC15'
      INCLUDE 'SIZE2'
      INCLUDE 'SIZEC2'
C
      COMMON/RCHA/P1X,P1Z,G1,SG1,P2X,P2Z,G2,SG2
      COMMON/VELOC/B1X,B1Z,B2X,B2Z
C
      PARAMETER (AN0=.160)
C
      COMMON/COOLC/PFRHO,IPTCO,UPO1,DUPNO1
      PARAMETER(T23=2./3.)
C
      DIMENSION FRX(-1:1),FRY(-1:1),FRZ(-1:1)
      DATA FRX(0),FRY(0),FRZ(0)/3*.5/
C
      IF(IPTCO.EQ.1)THEN
        PXC=P1X
        PZC=P1Z
        GG=G1
        BX=B1X
        BZ=B1Z
      ELSE
        PXC=P2X
        PZC=P2Z
        GG=G2
        BX=B2X
        BZ=B2Z
      ENDIF
C
      PFROK=PFRHO*PFRHO
C
      EKIN=0.
      AMVI=AMS(1)
      AMVIK=AMVI*AMVI
      DO 120 IP=1,NQ
      IDP=ID(IP)
      IF(IDP.LE.0.OR.IDP.GE.3)GOTO 120
C
      XXN=XX(IP)/DWL
      IX=NINT(XXN)
      YYN=YY(IP)/DWL
      IY=NINT(YYN)
      ZZN=ZZ(IP)/DLL
      IZ=NINT(ZZN)
C
      IXF=MIN(IX+1,NT)
      IXB=MAX(IX-1,-NT)
      FRX(1)=.5*(XXN-IX+.5)
      FRX(-1)=.5-FRX(1)
      IYF=MIN(IY+1,NT)
      IYB=MAX(IY-1,-NT)
      FRY(1)=.5*(YYN-IY+.5)
      FRY(-1)=.5-FRY(1)
      IZF=MIN(IZ+1,NL)
      IZB=MAX(IZ-1,-NL)
      FRZ(1)=.5*(ZZN-IZ+.5)
      FRZ(-1)=.5-FRZ(1)
C
      IF(IDP.EQ.1)THEN
        SF=2.
      ELSE
        SF=-2.
      ENDIF
C
      S=0.
      PFK=0.
      DO IIZ=IZB,IZF
        FRZZ=FRZ(IIZ-IZ)
        DO IIY=IYB,IYF
          FRYY=FRZZ*FRY(IIY-IY)
          DO IIX=IXB,IXF
            RHOP=RHO(IIX,IIY,IIZ)+SF*THO(IIX,IIY,IIZ)
            FRXX=FRYY*FRY(IIX-IX)
            IF(RHOP.GT.0.)PFK=PFK+FRXX*RHOP**T23
            S=S+FRXX
          ENDDO
        ENDDO
      ENDDO
      RHOP=RHO(IX,IY,IZ)+SF*THO(IX,IY,IZ)
      IF(RHOP.GT.0.)PFK=PFK+(1.-S)*RHOP**T23
C
      PFK=PFK*PFROK
      IF(LMI)PFK=PFK*(1.+.2*PFK/AMVIK)      ! ??? scalar
      PF=SQRT(PFK)
C
      CALL GIVE(PX(IP),PY(IP),PZ(IP),PF,PXC,PZC,GG)
      PK=PX(IP)**2+PY(IP)**2+PZ(IP)**2
      IF(POT)THEN
        IF(LMI)THEN
          AMVU=AMVI+UU(IX,IY,IZ)
          AMVUK=AMVU*AMVU
          EE(IP)=SQRT(PK+AMVUK)
        ELSE
          CALL PTNO(BX,0.,BZ,RHO(IX,IY,IZ)/AN0
     A      ,UU(IX,IY,IZ),AMVI,1.,PX(IP),PY(IP),PZ(IP)
     L      ,EE(IP),PC,EC,VXI,VYI,VZI,2)  ! Note: recalculates EE!
        ENDIF
      ELSE
        EE(IP)=SQRT(PK+AMVIK)
      ENDIF
      EKIN=EKIN+SQRT(PK+AMVIK)
C
 120  CONTINUE
      EKIN=EKIN/NQU
C
      END


      SUBROUTINE MIX
      use global
      INCLUDE 'PARTIC15'
      COMMON/CSEED/ISEED
C
      PARAMETER (NTRY=100,FRC=.4)
C
      WRITE(*,*)'MIX!'
C
      DO 100 IP=1,NQ
      IDP=ID(IP)
      IF(IDP.LE.0)GOTO 100
      NCSI=ABS(NCS(IP))
      IF(NCSI.EQ.0)GOTO 100
      KTRY=0
 20   CONTINUE
      JP=1.+NQ*getRan()
      IF(JP.GT.NQ)GOTO 20
      IF(ID(JP).NE.IDP)GOTO 30
      NCSJ=ABS(NCS(JP))
      IF(NCSJ.EQ.0)GOTO 30
      IF(ABS(NCSI-NCSJ).LE.NINT(FRC*MAX(NCSI,NCSJ))
     i  )THEN
*     i  .and.abs(zz(ip)-zz(jp)).lt.3.)THEN
        A=XX(IP)
        XX(IP)=XX(JP)
        XX(JP)=A
        A=YY(IP)
        YY(IP)=YY(JP)
        YY(JP)=A
        A=ZZ(IP)
        ZZ(IP)=ZZ(JP)
        ZZ(JP)=A
        GOTO 100
      ENDIF
 30   CONTINUE
      KTRY=KTRY+1
      IF(KTRY.LT.NTRY)GOTO 20
 100  CONTINUE
C
      END


      SUBROUTINE ENBKON
C  RESTORES ENERGIES AFTER COLLIDE
C  IN MO-DEPENDENT CASE
C
      INCLUDE 'PARTIC15'
      INCLUDE 'SIZE2'
      INCLUDE 'TSTEP'
C
      PARAMETER (AN0=.160)
C
      COMMON/CFREE/EFREE
C
      COMMON/CPRI/PRI,TIM,IZCEN
      LOGICAL PRI
C
      EFRN=0.
      DO 350 IP=1,NQ        !!start of first big particle loop
      IDP=ID(IP)
      IF(IDP.LE.0)GOTO 350
C
      BARP=BAR(IDP)
      IF(BARP.NE.0.)THEN
C
        XXN=XX(IP)/DWL
        IX=NINT(XXN)
        IX=MIN(IX,NT)
        IX=MAX(IX,-NT)
        YYN=YY(IP)/DWL
        IY=NINT(YYN)
        IY=MIN(IY,NT)
        IY=MAX(IY,-NT)
        ZZN=ZZ(IP)/DLL
        IZ=NINT(ZZN)
        IZ=MIN(IZ,NL)
        IZ=MAX(IZ,-NL)
C
        CALL PTNO(VXNO(IX,IY,IZ),VYNO(IX,IY,IZ)
     C    ,VZNO(IX,IY,IZ),RHO(IX,IY,IZ)/AN0
     A    ,UU(IX,IY,IZ),AMV(IP),BARP,PX(IP),PY(IP),PZ(IP)
     L    ,EE(IP),PC,EC,VXI,VYI,VZI,2)  ! Note: recalculates EE!
C
      ENDIF
      EFRN=EFRN+EE(IP)
 350  CONTINUE
C
      DEF=EFRN-EFREE
      DEF=DEF/NQI
      DEFV=DEF/DT
      if(pri)write(*,*)'def/defv',def,defv
C
      END


C------------ Jun 5-25-11 (copied from thomas)

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
**      DX=MIN(DX,.03)
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


      FUNCTION SINTXX(X)
C
C  INTERACTION SYMMETRY ENERGY DIVIDED BY NORMALIZED DENSITY
C
      SINTXX=SINT(X)/MAX(X,1E-4)**2
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
