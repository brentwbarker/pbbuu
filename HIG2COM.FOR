cC Pol-Bernard, now DSTME is only used for inelastic processes.
cC SIGHO and SIGEH give, respectively, total and tot elastic
cC cross sections.  I've added RAN for completeness.
c
c
c      INCLUDE 'PARTID.'
c      COMMON/CHMAX/SIGHO(NIDP,NIDP),VOLCH(NIDP,NIDP),SIGEH(NIDP,NIDP)
cc
c      parameter(nbmax=40)
c      dimension pfin(nbmax,0:3),amsf(nbmax),iddf(nbmax)
cC
c      PARAMETER(DY=.2,NYM=-5,NYX=25)
c      DIMENSION DNY(NYM:NYX,NIDP)
c      PARAMETER(DPT=.1,NPT=15)
c      DIMENSION DNT(NPT,NIDP)
c      DIMENSION DNTc(NPT,NIDP)
cc
c      logical success,lhigh,star
cc
c      PARAMETER(AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
c      parameter(am0k=am0*am0)
c      PARAMETER(AMV0=1.232)
c      parameter(amv0k=amv0*amv0)
c      PARAMETER (AMEC=.13957,AMEN=.13496,AME=(AMEC+AMEC+AMEN)/3.)
c      PARAMETER (AMEK=AME*AME)
cc
c      integer*4 iseed
c      common/cseed/iseed
cc
cc
c      iseed=123401671
c      uup=0.
cC
c      PI=4.*ATAN(1.)
cc
c      id1=1
c      ppz1=4.
c      ppx1=0.
c      ppy1=0.
c      ee1=sqrt(am0k+ppx1**2+ppy1**2+ppz1**2)
cc
c      id2=1
c      ppz2=0.
c      ppx2=0.
c      ppy2=0.
c      ee2=sqrt(am0k+ppx2**2+ppy2**2+ppz2**2)
cc
c      call dstme(id1,ee1,ppx1,ppy1,ppz1,id2,ee2
c     s  ,ppx2,ppy2,ppz2,uup,success,nfi,iddf,pfin,amsf)
cc
c      write(*,*)'nfi=',nfi
c      do ifi=1,nfi
c        write(*,*)iddf(ifi)
c        write(*,*)(pfin(ifi,i),i=0,3)
c      enddo
cC
c      DO I=1,NIDP
c        DO IY=NYM,NYX
c          DNY(IY,I)=0.
c        ENDDO
c        DO IPT=1,NPT
c          DNT(IPT,I)=0.
c          DNTc(IPT,I)=0.
c        ENDDO
c      ENDDO
cc
c      id1=1
c      id2=1
c      ntr=20000
c      anp=0.
c      ann=0.
c      anm=0.
c      plab=3.7
c      elab=sqrt(plab*plab+am0*am0)
c      ybeam=.5*log((elab+plab)/(elab-plab))
c      ycm=.5*ybeam
c      ptot=plab
c      etot=elab+am0
c      s=etot*etot-ptot*ptot
c      ss=sqrt(s)
c      write(*,*)'ybeam/ss ',ybeam,ss
c      e1=.5*ss
c      p1=sqrt(e1*e1-am0k)
c      decm=ss-am0-am0
c      lhigh=DECM.GT.13.*AME
c      lhigh=.false.
c      if(lhigh)then
c        write(*,*)'high'
c        sigi=sigho(id1,id2)-sigeh(id1,id2)
c      else
c        write(*,*)'low'
c        CALL CIBAR(ID1,AM0,iD2,AM0,DECM,SNN,SND,SNT,SDD,SIGI)
c        ECMK=ss*ss
c        pxi=0.
c        pyi=0.
c        pzi=plab
c        eei=elab
c        eij=elab+am0
c        pijx=0.
c        pijy=0.
c        pijz=plab
c        BX=PIJX/EIJ
c        BY=PIJY/EIJ
c        BZ=PIJZ/EIJ
c        GM=1./SQRT(1.-(BX*BX+BY*BY+BZ*BZ))
c        BPI=BX*PXI+BY*PYI+BZ*PZI
c        EIC=GM*(EEI-BPI)
c        BCPI=GM*(EEI-GM/(GM+1.)*BPI)
c        PICX=PXI-BX*BCPI
c        PICY=PYI-BY*BCPI
c        PICZ=PZI-BZ*BCPI
c        PICK=PICX*PICX+PICY*PICY+PICZ*PICZ
c        PIC=SQRT(PICK)    !2-body cm parameters
c      endif
c      ntrr=0
c      do itr=1,ntr
c        if(lhigh)THEN      !high en cross s.
c          call dstme(id1,elab,0.,0.,plab,id2,am0
c     s      ,0.,0.,0.,uup,success,nfi,iddf,pfin,amsf)
c        else
c          success=.true.
c          nfi=2
c          RSS=getRan()*SIGi
c          if(rss.lt.sdd)then
c            DMI=DECM-AME-AME
cC  KINEMATICS
c            CALL DDMS(AMVIF,AMVJF,DMI)
c            aMVIFK=AMVIF**2
c            aMVJFK=AMVJF**2
c            PIFCK=((ECMK-aMVIFK-aMVJFK)**2-4.*aMVIFK*aMVJFK)/(4.*ECMK)
c            pifck=max(pifck,0.)
c            PIFC=SQRT(PIFCK)
cC  NOW THE ID'S; INITIAL-STATE THRESHOLD
c            CALL IBAR(ID1,iD2,IDI,IDJ,3,DECM)
cC  SCATTERING ANGLE
c            CALL THBARN(CTHS,DECM,ID1,iD2,AM0,AM0,IDI,IDJ,AMVIF,AMVJF)
c            EIFC=SQRT(PIFCK+aMVIFK)
c            CALL NEWS(PICX,PICY,PICZ,PIC,CTHS,PIFC,EIFC,PIFX,PIFY,PIFZ
c     N        ,BX,BY,BZ,GM)                !new lab momentum of the i'th delta
cC  FINAL ENERGY
c            EIF=SQRT(aMVIFK+PIFX*PIFX+PIFY*PIFY+PIFZ*PIFZ)  ! first delta energy
c            amsf(1)=amvif
c          else
c            STAR=RSS.GT.(SDD+SND)
c            DMI=ss-aM0-am0-ame
c            IF(STAR)THEN
c              CALL XMS(AMVJF,DMI)
c            ELSE
c              CALL DMS(AMVJF,DMI)
c            ENDIF
c            aMVJFK=AMVJF**2
c            PIFCK=((ECMK-aM0K-aMVJFK)**2-4.*aM0K*aMVJFK)/(4.*ECMK)
c            pifck=max(pifck,0.)
c            PIFC=SQRT(PIFCK)
cC  NOW THE ID'S; INITIAL-STATE THRESHOLD
c            IF(STAR)THEN
c              CALL IBAR(ID1,iD2,IDI,IDJ,2,DECM)
c            ELSE
c              CALL IBAR(ID1,iD2,IDI,IDJ,1,DECM)
c            ENDIF
cC  THE RESONANCE ALWAYS 2ND PTCLE
c            IF(IDI.GT.6)THEN
c              III=IDI
c              IDI=IDJ
c              IDJ=III
c            ENDIF
cC  SCATTERING ANGLE
c            CALL THBARN(CTHS,DECM,ID1,iD2,AM0,AM0,IDI,IDJ,AM0,AMVJF)
c            EIFC=SQRT(PIFCK+aM0K)
c            CALL NEWS(PICX,PICY,PICZ,PIC,CTHS,PIFC,EIFC,PIFX,PIFY,PIFZ
c     N        ,BX,BY,BZ,GM)                   !new lab momentum of a nucleon
c            EIF=SQRT(aM0K+PIFX*PIFX+PIFY*PIFY+PIFZ*PIFZ)
c            amsf(1)=am0
c          endif
c          amsf(2)=amvjf
c          iddf(1)=idi
c          iddf(2)=idj
c          PJFX=PIJX-PIFX
c          PJFY=PIJY-PIFY
c          PJFZ=PIJZ-PIFZ
c          EJF=SQRT(aMVJFK+PJFX*PJFX+PJFY*PJFY+PJFZ*PJFZ)  !delta energy
c          pfin(1,0)=eif
c          pfin(1,1)=pifx
c          pfin(1,2)=pify
c          pfin(1,3)=pifz
c          pfin(2,0)=ejf
c          pfin(2,1)=pjfx
c          pfin(2,2)=pjfy
c          pfin(2,3)=pjfz
c        endif
c        if(success)then
c          ntrr=ntrr+1
c          nfii=nfi
c*         GOTO 100
c          do ifi=1,nfi
c            idfi=iddf(ifi)
c            if(idfi.ge.7.and.idfi.le.12)then  !breakup
c              eei=pfin(ifi,0)
c              pxi=pfin(ifi,1)
c              pyi=pfin(ifi,2)
c              pzi=pfin(ifi,3)
c              am1=amsf(ifi)
c              am1k=am1*am1
c              PCK=((aM1K-aM0K-AMEK)**2-4.*aM0K*AMEK)/(4.*aM1K)
c              If(pck.lt.0.)then
c                write(*,*)'negative mo'
c                pck=0.
c              endif
c              PC=SQRT(PCK)
c              EIC=SQRT(aM0K+PCK)
c              CTHS=2.*getRan()-1.
c              CALL NEWS(0.,PC,0.,PC,CTHS,PC,EIC,PIFX,PIFY,PIFZ
c     S          ,PXI/EEI,PYI/EEI,PZI/EEI,EEI/aM1)
c              EIF=SQRT(aM0K+PIFX*PIFX+PIFY*PIFY+PIFZ*PIFZ)
cC
c              IF(idfi.EQ.7)THEN        !delta ++
c                IDI=1
c                IDK=13
c              ELSEIF(idfi.EQ.10)THEN             !delta -
c                IDI=2
c                IDK=15
c              ELSEIF(getRan().lt.1./3.)THEN
c                IF(idfi.EQ.8)THEN               !delta +
c                  IDI=2
c                  IDK=13
c                ELSEIF(idfi.EQ.9)THEN           !delta 0
c                  IDI=1
c                  IDK=15
c                ELSE                           !stars
c                  IDI=idfi-10
c                  IDK=14
c                ENDIF
c              ELSE
c                IF(idfi.LE.10)THEN               !delta +/0
c                  IDI=idfi-7
c                  IDK=14                           !pi zero
c                ELSEIF(idfi.EQ.11)THEN           !star +
c                  IDI=2
c                  IDK=13                           !pi plus
c                ELSE                           !star 0
c                  IDI=1
c                  IDK=15                           !pi minus
c                ENDIF
c              ENDIF
cC
c              iddf(ifi)=idi
c              pfin(ifi,0)=eif
c              pfin(ifi,1)=pifx
c              pfin(ifi,2)=pify
c              pfin(ifi,3)=pifz
cc
c              nfii=nfii+1
c              iddf(nfii)=idk
c              pfin(nfii,0)=eei-eif
c              pfin(nfii,1)=pxi-pifx
c              pfin(nfii,2)=pyi-pify
c              pfin(nfii,3)=pzi-pifz
c            endif
c          enddo
c 100      CONTINUE
cc
c          ptoz=0.
c          ptox=0.
c          ptoy=0.
c          eto=0.
c          do ifi=1,nfii
c            idfi=iddf(ifi)
c            if(idfi.eq.11)then
c              anp=anp+2./3.
c              ann=ann+1./3.
c            elseif(idfi.eq.12)then
c              anm=anm+2./3.
c              ann=ann+1./3.
c            elseif(idfi.eq.7)then
c              anp=anp+1.
c            elseif(idfi.eq.10)then
c              anm=anm+1.
c            elseif(idfi.eq.8)then
c              anp=anp+1./3.
c              ann=ann+2./3.
c            elseif(idfi.eq.9)then
c              anm=anm+1./3.
c              ann=ann+2./3.
c            elseif(iddf(ifi).eq.13)then
c              anp=anp+1.
c            elseif(iddf(ifi).eq.14)then
c              ann=ann+1.
c            elseif(iddf(ifi).eq.15)then
c              anm=anm+1.
c            endif
c            EE=PFIN(IFI,0)
c            PZ=PFIN(IFI,3)
c            PX=PFIN(IFI,1)
c            PY=PFIN(IFI,2)
c            PTK=PX*PX+PY*PY
c            PT=SQRT(PTK)
c            Y=.5*ALOG((EE+PZ)/(EE-PZ))
c            IY=NINT(Y/DY)
c            IF(IY.GE.NYM.AND.IY.LE.NYX)DNY(IY,IDFI)=DNY(IY,IDFI)+1.
c            IPT=NINT(.501+PT/DPT)
c            IF(IPT.LE.NPT)then
c              DNT(IPT,IDFI)=DNT(IPT,IDFI)+1.
c              if(abs(y-ycm).lt..15*ycm)then
c                DNTc(IPT,IDFI)=DNTc(IPT,IDFI)+1.
c              endif
c            endif
c            eto=eto+ee
c            ptoz=ptoz+pz
c            ptox=ptox+pX
c            ptoy=ptoy+pY
c            eei=sqrt(amsf(ifi)**2+pz*pz+ptk)
c*           write(*,*)ee,eei
c          enddo
c*         write(*,*)eto,ptoz,ptox,ptoy
c        endif
c      enddo
cc
c      if(ntrr.ne.0)then
c        anp=anp/ntrr
c        ann=ann/ntrr
c        anm=anm/ntrr
c        DO K=1,NIDP
c          DO I=NYM,NYX
c            DNY(I,K)=DNY(I,K)/(DY*NTRR)*SIGI*10.
c          ENDDO
c          DO I=1,NPT
c            PT=(I-.5)*DPT
c            DNT(I,K)=DNT(I,K)/(2.*PT*DPT*NTRR)*SIGI*10.
c            DNTc(I,K)=DNTc(I,K)/(2.*PI*PT*DPT*NTRR*.3*ycm)*SIGI*10.
c          ENDDO
c        ENDDO
c      endif
c      write(*,*)' '
c      write(*,*)'ss/p/n/m/t ',ss,anp,ann,anm,anp+ann+anm
c      write(*,*)'cs p/n/m ',anp*sigi*10.,ann*sigi*10.,anm*sigi*10.
cC
c      WRITE(*,*)
c      WRITE(*,*)'Y      P     PI+     PI-'
c      DO I=NYM,NYX
c        Y=I*DY
c        WRITE(*,*)Y,DNY(I,1),DNY(I,13),DNY(I,15)
c      ENDDO
c      WRITE(*,*)
c      WRITE(*,*)'PT      P     PI+     PI-'
c      SIGP=0.
c      DO I=1,NPT
c        PT=(I-.5)*DPT
c        WRITE(*,*)PT,DNT(I,1),DNT(I,13),DNT(I,15)
c        SIGP=SIGP+DNT(I,13)*2.*PT*DPT
c      ENDDO
c      WRITE(*,*)'sigp = ',sigp
c      WRITE(*,*)
c      WRITE(*,*)'PT      P     PI+     PI-'
c      DO I=1,NPT
c        PT=(I-.5)*DPT
c        WRITE(*,*)PT,DNTc(I,1),DNTc(I,13),DNTc(I,15)
c      ENDDO
cc
c      end
c
c
c      FUNCTION RAN(ISEED)
cC RANDOM NUMBER GENERATOR
cC  ISEED SHOULD BE SET ODD POSITIVE IN THE RANGE 0, 2 147 483 647
cC  AT LEAST 5 DECIMAL DIGITS
c      INTEGER*4 ISEED
c      REAL*8 A,M,RM
c      PARAMETER(A= 69 069. D0, M= 2 147 483 648. D0 , RM = 1.D0/M)
cC
c      ISEED=DMOD(ISEED*A,M)
c      RAN=ISEED*RM
cC
c      END
c
c
c      SUBROUTINE NEWS(PICX,PICY,PICZ,PIC,CTHS,PICC,EICC
c     S  ,PIFX,PIFY,PIFZ,BX,BY,BZ,GM)
cC  BX,BY,BZ,GM ARE BETA (REGULAR) COMPONENTS OF CM IN THE LAB
cC  PICX,PICY,PICZ,PIC   ARE COMPONENTS OF THE VECTOR IN THE CM THAT SETS
cC  REFERENCE DIRECTION, PICC & EICC ARE CM VALUES FOR DEFLECTED PARTICLE,
cC  CTHS IS RANDOM COSINE FROM THE OUTSIDE, PIF ARE FINAL COMPONENTS IN THE LAB
c      COMMON/CSEED/ISEED
c      LOGICAL FIRST
cC
c      DATA FIRST/.TRUE./
cC
c      IF(FIRST)THEN
c        PI=4.*ATAN(1.)
c        PIPI=PI+PI
c        FIRST=.FALSE.
c      ENDIF
cC
c      IF(PIC.GT.0.)THEN
c        CTH=PICZ/PIC
c        AG=1.-CTH*CTH
c      ELSE
c        CTH=1.
c        AG=0.
c      ENDIF
c      IF(AG.GT.0.)THEN
c        STH=SQRT(AG)
c        COFI=PICX/(STH*PIC)
c        SIFI=PICY/(STH*PIC)
c      ELSE
c        STH=0.
c        COFI=1.
c        SIFI=0.
c      ENDIF
cC
c      AG=MAX(0.,1.-CTHS*CTHS)
c      STHS=SQRT(AG)
cC
c      FIS=PIPI*getRan()
cC
c      PICXS=PICC*STHS*COS(FIS)
c      PICYS=PICC*STHS*SIN(FIS)
c      PICZS=PICC*CTHS
cC
c      PICZF=PICZS*CTH-PICXS*STH
c      PICXF=PICXS*CTH+PICZS*STH
cC
c      PICYF=PICYS*COFI+PICXF*SIFI
c      PICXF=PICXF*COFI-PICYS*SIFI
cC
cC  GETTING BACK TO THE LAB
c      BPIC=BX*PICXF+BY*PICYF+BZ*PICZF
c      BFPI=GM*(EICC+GM/(GM+1.)*BPIC)
c      PIFX=PICXF+BX*BFPI
c      PIFY=PICYF+BY*BFPI
c      PIFZ=PICZF+BZ*BFPI
cC
c      END


      BLOCK DATA HIEE
      INCLUDE 'PARTID'
      COMMON/CHMAX/SIGHO(NIDP,NIDP),VOLCH(NIDP,NIDP),SIGEH(NIDP,NIDP)
      PARAMETER(SIGBB=4.,SIGBM=2.7,SIGMM=1.8)
      PARAMETER(SIGEBB=1.,SIGEBM=.50,SIGEMM=.30)
C
      DATA ((SIGHO(I,J),I=1,2),J=1,2)/4*SIGBB/
      DATA ((SIGHO(I,J),I=3,6),J=3,6)/16*0./
      DATA ((SIGHO(I,J),SIGHO(J,I),I=1,2),J=7,12)/24*SIGBB/
      DATA ((SIGHO(I,J),I=7,12),J=7,12)/36*SIGBB/
      DATA ((SIGHO(I,J),SIGHO(J,I),I=1,2),J=13,15)/12*SIGBM/
      DATA ((SIGHO(I,J),SIGHO(J,I),I=7,12),J=13,15)/36*SIGBM/
      DATA ((SIGHO(I,J),I=13,15),J=13,15)/9*SIGMM/
C
      DATA ((SIGEH(I,J),I=1,2),J=1,2)/4*SIGEBB/
      DATA ((SIGEH(I,J),I=3,6),J=3,6)/16*0./
      DATA ((SIGEH(I,J),SIGEH(J,I),I=1,2),J=7,12)/24*SIGEBB/
      DATA ((SIGEH(I,J),I=7,12),J=7,12)/36*SIGEBB/
      DATA ((SIGEH(I,J),SIGEH(J,I),I=1,2),J=13,15)/12*SIGEBM/
      DATA ((SIGEH(I,J),SIGEH(J,I),I=7,12),J=13,15)/36*SIGEBM/
      DATA ((SIGEH(I,J),I=13,15),J=13,15)/9*SIGEMM/
C
      END


      SUBROUTINE SETHI
      include 'PARTID'
      COMMON/CHMAX/SIGHO(NIDP,NIDP),VOLCH(NIDP,NIDP),SIGEH(NIDP,NIDP)
      COMMON/COQU/FV,FD,FT,FG,FTD
C
      DO J=1,NIDP
        DO I=1,NIDP
          VOLCH(I,J)=2.*FV*SIGHO(I,J)
        ENDDO
      ENDDO
C
      END


      subroutine dstme(idd1,ee1,ppx1,ppy1,ppz1
     s  ,idd2,ee2,ppx2,ppy2,ppz2,uup,success
     s  ,nfi,iddf,pfin,amsf)
c
      parameter(nbmax=40)
      dimension pfin(nbmax,0:3),amsf(nbmax),iddf(nbmax)
      dimension ptr(0:3)
c
      logical leading,success,test
      integer idf(nbmax)
      real mass(nbmax),
     & pe1(0:3),pe2(0:3)
c
      parameter(nit=15)
c
      parameter(nidp=16)
      dimension idn(nidp)
      data idn/1,2,0,0,0,0,3,4,5,6,7,8,13,14,15,0/
c
      include 'PARTIT1'
      dimension ido(nidpt)
      data ido/1,2,7,8,9,10,11,12,4*0,13,14,15,3*0/
      data kk/0/
c
c
      idn1=idn(idd1)
      idn2=idn(idd2)
      if(idn1*idn2.eq.0)goto 100
c
      call setma(uup)
      leading=.true.
      test=.false.
c
      pe1(0)=ee1
      pe1(1)=ppx1
      pe1(2)=ppy1
      pe1(3)=ppz1
c
      pe2(0)=ee2
      pe2(1)=ppx2
      pe2(2)=ppy2
      pe2(3)=ppz2
c
      call dstall(idn1,pe1,idn2,pe2,leading,success,nfi,idf,
     &  pfin,mass,nit,test,weightlong,weighttrans)
c
*     write(*,*)success
      if(.not.success)return
c
      do i=0,3
        ptr(i)=-(pe1(i)+pe2(i))
      enddo
c
      do ifi=1,nfi
        idfi=idf(ifi)
        iddf(ifi)=ido(idfi)
        if(iddf(ifi).eq.0)goto 100
        amsf(ifi)=mass(ifi)-bar(idfi)*uup
        if(idfi.le.2)then
          amsf(ifi)=am0
        elseif(idfi.ge.13.and.idfi.le.15)then
          amsf(ifi)=ame
        elseif(idfi.ge.3.and.idfi.le.8)then
          amsf(ifi)=max(amsf(ifi),thrdelt)
        endif
        ag=pfin(ifi,0)**2-(pfin(ifi,1)**2+pfin(ifi,2)**2
     e    +pfin(ifi,3)**2)
        em1=sign(sqrt(abs(ag)),ag)-bar(idfi)*uup
        if(abs(amsf(ifi)-em1).gt..003)then
          kk=kk+1
          if(kk.eq.100)then
            write(*,*)'nf/id/am/em',nfi,idf(ifi),amsf(ifi),em1
            kk=0
          endif
          goto 100
        endif
        do i=0,3
          ptr(i)=ptr(i)+pfin(ifi,i)
        enddo
      enddo
      de=abs(ptr(0))
      dp=0.
      do i=1,3
        dp=dp+ptr(i)**2
      enddo
      dp=sqrt(dp)
      if(de.gt..003.or.dp.gt..008)then
        do i=0,3
          write(*,*)'i/p/dp',i,pe1(i)+pe2(i),ptr(i)
        enddo
        goto 100
      endif
c
      return
c
 100  success=.false.
c
      end


      subroutine setma(uup)
      include 'PARTIT1'
c
      do i=1,nidpt
        ams(i)=amstr(i)+bar(i)*uup
        thr(i)=thrtr(i)+bar(i)*uup
      enddo
c
      end


C23456789012345678901234567890123456789012345678901234567890123456789012
C this package contains all and only the necessary routines to perform the
c high energy generation +  the routine testallD, some crude testing routine
c (if this routine is used, compile with the cern library)
c much more erratic tries should be found in enventgen.dir directory
c      implicit none
c      integer iseed
c      iseed=-346178925
c      call testdistallD
c      end


      subroutine genmassphys(massmax,nfi,idf,first,nbtrymax,
     &  mass,masstot2,nbtry)
      use global
c here, we connect to the "big" model: all physical parameters are
c read from some common block.
c After mature discussion, we have chosen to scale the thresholds
c and mean mass but to let the width unchanged.
c nfi represents the number of final particles and idf their identity.
c For the rest, this is basically genmass rewritten
c input: massmax,stild,nfi,idf,first,nbtrymax
c output: mass,masstot2,nbtry
      implicit none
c the physical types:
      include 'PARTIT1'
c the actual realization:
      integer nbmax
      parameter(nbmax=40)
      real mass(nbmax),gamthresh,massmax,masstot,masstot2,
     & scal(nbmax),pi
      data pi/3.141592653589793/
c etot is the highest mass
      parameter(gamthresh=0.0001)
      integer nfi,idf(nbmax),i,nbtry,nbtrymax
      integer*4 iseed
      common/cseed/iseed
      real ran
      logical first
*     external ran
      save masstot,scal
c
      if(first) then
        if(nfi.gt.nbmax) then
           write(6,*) 'genmass: increase nbmax until ',nfi
           stop
        endif
c first, we generate all the particle having a 0 width for practical
c purposes.
        masstot=0.
        do i=1,nfi
          if(gpa(idf(i)).lt.gamthresh) then
            masstot=masstot+ams(idf(i))
          endif
        enddo
        masstot2=masstot
        do i=1,nfi
          if(gpa(idf(i)).ge.gamthresh) then
            scal(i)=2.*(ams(idf(i))-thr(idf(i)))/gpa(idf(i))
            scal(i)=atan(scal(i))
            masstot2=masstot2+thr(idf(i))
          endif
        enddo
        if(masstot2.gt.massmax) then
c there is obviously not enough energy available
          write(6,*) 'genmassphys: enlarge your phase space:'
          do i=1,nfi
            write(6,*) i,idf(i),thr(idf(i)),gpa(idf(i)),ams(idf(i))
          enddo
          write(6,*) masstot2,massmax
          stop
        endif
      endif
c now, it is possible to evaluate the mass(i):
 10   continue
      nbtry=0
      masstot2=masstot
      do i=1,nfi
        if(gpa(idf(i)).ge.gamthresh) then
          mass(i)=ams(idf(i))+gpa(idf(i))/2.*tan(
     &            -scal(i)+getRan()*(pi/2 + scal(i)))
c to repeat ourselves: between threshold and infinity
          masstot2=masstot2+mass(i)
        else
          mass(i)=ams(idf(i))
        endif
      enddo
      nbtry=nbtry+1
      if(masstot2.gt.massmax) then
        if(nbtry.lt.nbtrymax) goto 10
        write(6,*) 'did not converge in ',nbtrymax,' steps'
        stop
      else
        return
      endif
      end


      subroutine genmasstransone(mass,ptrans2,pt,temp)
      use global
c generates a distribution of transverse momenta according to
c exp(-sqrt(mass^2+pt^2)/temp)
      implicit none
      integer*4 iseed
      common/cseed/iseed
      real ran
*     external ran
      real pt(2),pi,mass,ptrans,ptrans2,phi,mlongkin,temp
      data pi/3.141592654/
      if(getRan().gt.(temp/(temp+mass))) then
c I1 was selected
            mlongkin=-log(max(getRan(),1e-20))*temp
          else
c I2 was selected
            mlongkin=-log(max(getRan()*getRan(),1e-20))*temp
      endif
      ptrans2=mlongkin*(mlongkin+2*mass)
      ptrans=sqrt(ptrans2)
      phi=2*pi*getRan()
      pt(1)=ptrans*cos(phi)
      pt(2)=ptrans*sin(phi)
      return
      end


      subroutine genmasstransbis(nb,mass,etot,drift,success,masslong,
     &  pt,weight,modxhi)
      use global
c Now, suppose we have to generate Exp[E_t/Temp], with
c E_t =p^2/(2*m) and all masses a priori different.
c The trick is to proceed recursively with biassed distributions
c (with drift term. 1: normal gaussian drift/ 2: conformal drift)
c temp=1/bigA.
c
      implicit none
      integer nbmax,nb,profile,i,drift,j
      parameter (nbmax=40)
      integer*4 iseed
      common/cseed/iseed
      real ran
*     external ran
      real pt(nbmax,2),pi,mass(nbmax),r1,etot,masslong(nbmax),
     & weight,ptrans,ptrans2,ptrans2shift,phi,modxhi,alphap,pcum(2),
     & masscum,masscumnext,cf0,cf1,cf2,temp,ptone(2),etottrans,
     & weightb,ag
      parameter (temp=.25)
      data pi/3.141592654/
      logical success
c this is required if there is something on the top of genmasstransbis
c in the montecarlo chain (for instance, a mass generator)
      if(nb.gt.nbmax) then
         write(6,*) 'gentransb: increase nbmax until ',nb
         stop
      endif
c generate the cumulative mass:
 10   continue
      masscum=0.
      etottrans=0.
      do i=1,nb
        masscum=masscum+mass(i)
      enddo
      pcum(1)=0.
      pcum(2)=0.
      weight=0.
c generates the primary gaussian distribution (in other words,
c the non relativistic approximation)
      do i=1,nb-1
          masscumnext=max(masscum-mass(i),0.)
          ag=masscumnext/masscum
          if(ag.lt.0.)then
            write(*,*)'masscum/i ',masscum,i
            write(*,*)'masscumnext ',masscumnext
            do j=1,nb
              write(*,*)'j/mass ',j,mass(j)
            enddo
          endif
          cf1=sqrt(masscumnext/masscum)
          cf2=mass(i)/masscum
          if(drift.eq.1) then
c this generates the i^th momentum according to
c exp[-((pt_i+cf2*pcum)/cf1)^2/(2*mass_i*temp)]
c in the case of gaussian distribution, this is exact way to bias the
c distributions
            ptrans2shift=-2*temp*mass(i)*log(max(getRan(),1e-20))
            ptrans=sqrt(ptrans2shift)
            phi=2*pi*getRan()
            pt(i,1)=cf1*ptrans*cos(phi)-cf2*pcum(1)
            pt(i,2)=cf1*ptrans*sin(phi)-cf2*pcum(2)
c the weight (a):
            weight=weight+mass(i)+ptrans2shift/(2*mass(i))
          else
c this generates the i^th momentum according to
c   exp[-sqrt[mass_i^2+(pt_i/cf1+cf2*pcum)^2]/temp]
c  =exp[-sqrt[mass_i^2+ptrans2shift]/temp]
            call genmasstransone(mass(i),ptrans2shift,ptone,temp)
            pt(i,1)=cf1*ptone(1)-cf2*pcum(1)
            pt(i,2)=cf1*ptone(2)-cf2*pcum(2)
c the weight (a):
            weight=weight+sqrt(mass(i)**2+ptrans2shift)
          endif
c the weight (b):
          ptrans2=pt(i,1)**2+pt(i,2)**2
          masslong(i)=sqrt(mass(i)**2+ptrans2)
          etottrans=etottrans+masslong(i)
          weight=weight-masslong(i)
c now, we accumulate/decumulate
          masscum=masscumnext
          pcum(1)=pcum(1)+pt(i,1)
          pcum(2)=pcum(2)+pt(i,2)
      enddo
c now, we impose the delta condition.
      pt(nb,1)=-pcum(1)
      pt(nb,2)=-pcum(2)
      ptrans2=pt(nb,1)**2+pt(nb,2)**2
      masslong(nb)=sqrt(mass(nb)**2+ptrans2)
      etottrans=etottrans+masslong(nb)
      if(etottrans.ge.etot) then
c        write(6,*) etot,'<',etottrans
        success=.false.
        return
      else
        success=.true.
      endif
c and now, the global weigth: an exp(-Et/temp)
c      weight=(weight-masslong(nb))/temp
c      if (weight.gt.50) goto 10
c      weight=exp(weight)
c up to now, we have neglected the mass dependent factor coming
c from the importance sampling. This is important as variable mass
c resonances are produced:
      weightb=1.
      do i=1,nb-1
        weight=weight-mass(i)
        weightb=weightb*(mass(i)+temp)
      enddo
      weight=weightb*exp((weight-masslong(nb))/temp)
      modxhi=sqrt(ptrans2)
c      write(6,*) drift,modxhi,weight
c      read(5,*)
      return
      end


      subroutine insphere2(ndim,x)
      use global
c generate a point (x) in a 2*ndim dimensional sphere of unit radius.
c we take the positive quadrant,octant,.. 2^(2*ndim)ant
c We now implement a much more refined method, providing an efficiency
c independent of the dimension of the space:
c we rewritte dx1,...dx2n= r1 dr1 dtheta1 ... rn drn dthetan
c so that the phase space becomes:
c int_{0}^{R^2} dr1^2 int_{0}^{R^2-r1^2} dr2^2 ...
c  int_{0}^{R^2-r1^2-..r(n-1)^2} drn^2 1/2^n *
c                          int_0^{Pi/2} dth1 ... int_0^{Pi/2} dthn
c the angles are uncorrelated.
c integrating over  drn^2     -> R^2-r1^2-..r(n-1)^2/1!
c integrating over  dr(n-1)^2 -> (R^2-r1^2-..r(n-2)^2)^2/2!
c integrating over  dr(n-2)^2 -> (R^2-r1^2-..r(n-3)^2)^2/3!
c ....
c integrating over  dr2^2 ->  (R^2-r_{n-(n-1)}^2)^(n-1)/(n-1)!
c                           = (R^2-r1^2)^(n-1)/(n-1)!
c integrating over  dr1^2 -> (pi/4)^(2n) R^(2n)/n!, which gives the ratio
c of a sphere volume incorporated in some hypercube R^(2n):
c (pi/4)^(2n)/n!, which decreses very fast.
c Now, we proceed the other way: introducing s1=R^2-r1^2, we have
c PS \propto int_{0}^{R^2} s1^(n-1) ds1. Next, y1= (s1/R^2)^n and
c PS \propto int_{0}^{1} dy1. Coming to Montecarlo:
c  y1=rand(1), s1=R^2*rand(1)^(1/n), r1=sqrt(R^2-s1)
c and now with the recursion:
c              s2=(R^2-r1^2)*rand(2)^(1/(n-1)), r2=sqrt(R^2-r1^2-s2)
c                =s1*rand(2)^(1/(n-1)),           =sqrt(s1-s2)
c and, generally:
c              s_i=s_{i-1}*rand(i)^(1/(n+1-i)),   r_i=sqrt(s_(i-1)-s_(i))
c up to i=n
      integer ndim,dimxmax,i
      parameter(dimxmax=40)
      real x(dimxmax),s(dimxmax/2),r(dimxmax/2),th(dimxmax/2)
      integer*4 iseed
      common/cseed/iseed
      real pi
      data pi/3.141592653589793/
*     external ran
      if((2*ndim).gt.dimxmax) then
        write(6,*) 'insphere: increase the dimxmax'
        stop
      endif
c initiate the recursion
      s(1)=1.*getRan()**(1./real(ndim))
      r(1)=sqrt(1.-s(1))
      th(1)=pi/2.*getRan()
      do i=2,ndim
        s(i)=s(i-1)*getRan()**(1./real(ndim+1-i))
        r(i)=sqrt(s(i-1)-s(i))
        th(i)=pi/2.*getRan()
      enddo
      do i=1,ndim
        x(2*i-1)=r(i)*cos(th(i))
        x(2*i)=r(i)*sin(th(i))
      enddo
      return
      end


      subroutine distangle(nangle,sinth2)
c generate nangle sin^2(th_i), such that th_i is distributed according to
c sin(th_i)^(j-1), j ranging from 1 to nangle.
c we take benefit of the hyper-spherical coordinates.
c However, one must also recognize that if th is distributed according
c to sin(th)^(j-1), then tau=sin(th)^2 is distributed according to ...
c int sin(th)^(j-1) dth = int sin(th)^(j-2) sin(th) cos(th)/cos(th) dth=
c int_{0}^{1} tau^((j-2)/2) / sqrt(1-tau) dtau, which is precisely the
c distribution we need for the mass fragmentation...
      integer ndim,ndim2,nangle,dimxmax,i
      parameter(dimxmax=40)
      real x(dimxmax),sinth2(dimxmax),rad1,rad2
      integer*4 iseed
      common/cseed/iseed
      real pi
      data pi/3.141592653589793/
*     external ran
      ndim=(nangle+2)/2
      ndim2=2*ndim
      if((ndim2).gt.dimxmax) then
        write(6,*) 'distangle: increase the dimxmax'
        stop
      endif
 10   call insphere2(ndim,x)
      rad1=x(1)**2
      rad2=rad1+x(2)**2
      if(rad2.eq.0.) goto 10
c this should happen extremely rarely -> no real loss of time
      sinth2(1)=rad1/rad2
      rad1=rad2
      do i=2,nangle
        rad2=rad1+x(i+1)**2
        sinth2(i)=rad1/rad2
        rad1=rad2
      enddo
      return
      end


      subroutine genflatlongter(masslong,mtot,nbpart,leading,weight,
     &  masstot)
      use global
c in this subroutine, we cure the problem encountered in genflatlongbis,
c namely the large fluctuation of the weight.
c first, we notice that the leading part effect+ phase space of n-2
c reminding particles is
c [1+(Mdeff/mlong1+Mdeff/mlong2)*tau1+(Mdeff^2/(2*mlong1*mlong2)*tau1^2)]
c  *(1-tau1)^((nbpart-4)/2)*tau1^(-1/2),
c where tau1 is the available mass %age going to the first two particle.
c In the second line, we easily recognize the phase space of the
c reminding part, while the first line tends to privilege values of
c tau1 close to 1 (this is especially true for large Mdeff). We want to
c generate this distribution by importance sampling...
c First, we note that the overall integral is
c I=I0+(Mdeff/mlong1+Mdeff/mlong2)*I1+(Mdeff^2/(2*mlong1*mlong2)*I2
c with I0=Gamma[1/2]*Gamma[nbpart/2-1]/Gamma[nbpart/2-1/2]
c      I1=Gamma[3/2]*Gamma[nbpart/2-1]/Gamma[nbpart/2+1/2]
c        =1/2*(nbpart/2-1/2)^(-1)*I0
c        =1/(nbpart-1) *I0
c      I2=Gamma[5/2]*Gamma[nbpart/2-1]/Gamma[nbpart/2+3/2]
c        =3/2(nbpart/2+1/2)^(-1)*I1
c        =3/(nbpart+1)*I1
c        =3/(nbpart^2-1)*I0
c Thus, introductin the 3 weights w0,w1,w2
c w0=(nbpart^2-1)
c w1=(nbpart+1)*(Mdeff/mlong1+Mdeff/mlong2)
c w2=3*(Mdeff^2/(2*mlong1*mlong2),
c the i^th "branch" should be selected with a probability
c     w_i/(w0+w1+w2)
c Once the branch has been selected, the problem is just to generate
c a distribution
c   (1-tau1)^((nbpart-4)/2)*tau1^(i-1/2),
c or, introducing tau1'=1-tau1,
c   (tau1')^((nbpart-4)/2)*(1-tau1')^(i-1/2)...
c For low values of i, this can be done efficiently starting
c from a (tau1')^(i+(nbpart-4)/2)*(1-tau1')^(-1/2) distribution,
c what we can do
      implicit none
      integer dimxmax,nbpart,ntau,i,ibranch
      parameter(dimxmax=40)
      real mtot,tau(dimxmax),pi,masslong(dimxmax),masstot(dimxmax),
     &  s(dimxmax),massdef(dimxmax),weight,remn,chrapidmin,
     & w(0:2),wtot,wcum(0:2),wrand,randp,tau1,pow
      data pi/3.141592653589793/
      logical leading
      integer*4 iseed
      common/cseed/iseed
      real ran
*     external ran
      real alpha,chymin,ymin
      parameter(alpha=1.6)
c first, we generate the mass fractions. There is n-2 independent variables
c      write(6,*) mtot,nbpart,leading
      if(nbpart.lt.3) then
        if(nbpart.eq.2) then
c the simple 2 body longitudinal phase space. the weight is
c 1/(prel*E_tot)
          masstot(nbpart-1)=mtot
          remn=(
     &     (masstot(nbpart-1)**2-(masslong(1)+masslong(2))**2)*
     &     (masstot(nbpart-1)**2-(masslong(1)-masslong(2))**2))
          if(remn.le.0.) then
c it should never be...
           write(6,*) 'genflatlongter lead : remn<0: ',remn
           stop
          else
            weight=1./sqrt(remn)
            return
          endif
        else
         write(6,*) 'genflatlongter is at least a 2 body output channel'
         stop
        endif
      endif
c now generate the minimum rest masses
      s(1)=masslong(nbpart)
      do i=2,nbpart-2
        s(i)=s(i-1)+masslong(nbpart+1-i)
      enddo
      s(nbpart-1)=masslong(1)+masslong(2)
      s(nbpart)=s(nbpart-1)+s(nbpart-2)
c      do i=1,nbpart
c        write(6,*) i,s(i)
c      enddo
c now evaluates the first mass default, which will help in
c determining the branch to select
      masstot(nbpart)=mtot
      massdef(nbpart)=masstot(nbpart)-s(nbpart)
      if(massdef(nbpart).le.0.) then
         write(6,*) 'genflatlongter:massdef problem'
         write(6,*) massdef(nbpart)
*        stop
         massdef(nbpart)=0.
      endif
c now, decide the branch...
      if(.not.(leading)) then
        ibranch=0
c of course !!
      else
        w(0)=(nbpart**2-1)*masslong(1)*masslong(2)
        w(1)=(nbpart+1)*massdef(nbpart)*(masslong(1)+masslong(2))
        w(2)=1.5*massdef(nbpart)**2
        wtot=w(0)+w(1)+w(2)
        wrand=getRan()*wtot
        do i=0,2
          if(wrand.le.w(i)) then
            ibranch=i
            goto 10
          endif
          wrand=wrand-w(i)
        enddo
**        write(6,*) 'genflatlongter: branch unselected'
**        write(6,*) wrand
**        stop
        IBRANCH=2
        GOTO 10
      endif
      write(6,*) ibranch,' th branch selected'
      write(6,*) nbpart,w(0),w(1),w(2)
      read(5,*)
 10   continue
c now generates all the random numbers
      ntau=nbpart-2+2*ibranch
      call distangle(ntau,tau)
c as explained , we have to devote some special care to
c the first fragmentation:
      randp=1.
      do i=1,ibranch
c generates a p-random number according to x^(i-1+(nbpart-4)*0.5)
          pow=1./(i+(nbpart-4)*0.5)
          randp=randp*getRan()**pow
      enddo
c this is the tau(1) in my notes...
      tau1=1.-randp*tau(ntau)
c now evaluates the remaining mass defaults and masstot, down to
c  masstot(2). From hereafter, everything is similar to the ...bis
c  version, except the final weight.
      massdef(nbpart-1)=tau1*massdef(nbpart)
      masstot(nbpart-1)=s(nbpart-1)+massdef(nbpart-1)
      if(nbpart.gt.3) then
        massdef(nbpart-2)=tau(nbpart-3)*(1-tau1)*
     &                     massdef(nbpart)
        masstot(nbpart-2)=s(nbpart-2)+massdef(nbpart-2)
        do i=3,nbpart-2
c        write(6,*) nbpart-1-i,tau(nbpart-1-i)
          massdef(nbpart-i)=tau(nbpart-1-i)*massdef(nbpart-i+1)
          masstot(nbpart-i)=massdef(nbpart-i)+s(nbpart-i)
        enddo
      endif
      masstot(1)=masslong(nbpart)
c We have just generated the importance sampling... Now, we have to
c produce the weight. (one could try bring more structure)
c-------------------------------------------------------
c First, the weight associated with the "mother" vertex
c-------------------------------------------------------
      remn=
     &  (masstot(nbpart)+masstot(nbpart-1)+masstot(nbpart-2))*
     &  (masstot(nbpart)**2-(masstot(nbpart-1)-masstot(nbpart-2))**2)
      if(remn.le.0.) then
c it should never be...
          write(6,*) 'genflatlongter mother: remn<0: ',remn
          stop
      endif
c dim=M^(-2). OK
      weight=1./sqrt(remn)
c ------------------------------------------------------------
c the "nbpart-2" branch; no contribution if nbpart=3 (single particle)
c ------------------------------------------------------------
      do i=3,nbpart-1
        remn=(
     &   (masstot(nbpart-i+1)+masstot(nbpart-i)+masslong(i))*
     &   (masstot(nbpart-i+1)**2-(masstot(nbpart-i)-masslong(i))**2))
        if(remn.le.0.) then
c it should never be...
          write(6,*) 'genflatlongter rem: remn<0: ',remn
          stop
        endif
        weight=weight*masstot(nbpart-i+1)*sqrt(massdef(nbpart)/remn)
      enddo
c ---------------------------------------------------------
c now, the contribution of the masstot(nbpart-1)->1+2 vertex
c ----------------------------------------------------------
      remn=(
     &  (masstot(nbpart-1)+masslong(1)+masslong(2))*
     &  (masstot(nbpart-1)**2-(masslong(1)-masslong(2))**2))
      if(remn.le.0.) then
c it should never be...
         write(6,*) 'genflatlongter lead : remn<0: ',remn
         stop
      endif
      weight=weight*masstot(nbpart-1)/sqrt(remn)
c a weight to generate an exp(alpha(y1-y2)) distribution:
      if(leading) then
        chymin=(masstot(nbpart-1)**2-masslong(1)**2-masslong(2)**2)
     &          /(2*masslong(1)*masslong(2))
**        ymin=acosh(chymin)
**        write(*,*)'chymin in genflat ',chymin
        ymin=log(chymin+sqrt(max(0.,chymin*chymin-1.)))
        weight=weight*cosh(alpha*ymin)/chymin
      endif
      return
      end


      subroutine genpz(masslong,nbpart,masstot,leading,pzpart,epart,
     & test)
      use global
c a version adapted to genflatlongbis/ter. The particles 1 and 2 are
c leading. We generate the final PZ momentum of particles (not rapidity)
c TEST once and for all and then suppress all those lines
      implicit none
      integer dimxmax,nbpart,i
      parameter(dimxmax=40)
      integer*4 iseed
      common/cseed/iseed
      real ran
*     external ran
      real pi,masslong(dimxmax),masstot(dimxmax),etot,prel,etottest,
     &  pztottest,chymin,probsup,en1,en2,pz1,pz2,invmass,
     &  pzpart(dimxmax),epart(dimxmax),pzres(dimxmax),eres(dimxmax)
      data pi/3.141592653589793/
      logical test,leading
      real alpha,ymin
      parameter(alpha=1.6)
c test is switched to true whe on wants to check the energy-momentum
c conservation
c generates the nbpart-1 branching ratios
c      write(6,*) 'entering genpz'
      if(nbpart.eq.2) then
        etot=masstot(nbpart-1)
        eres(nbpart-1)=etot
        pzres(nbpart-1)=0.
      else
        etot=masstot(nbpart)
        eres(nbpart)=etot
        pzres(nbpart)=0.
c-------------------------------------
c the mother vertex
c-------------------------------------
        eres(nbpart-1)=(masstot(nbpart)**2+masstot(nbpart-1)**2-
     &     masstot(nbpart-2)**2)/(2*masstot(nbpart))
        eres(nbpart-2)=eres(nbpart)-eres(nbpart-1)
        prel=eres(nbpart-1)**2-masstot(nbpart-1)**2
        if(prel.le.0) then
          if(prel.le.-5e-3) then
c real problem
             write(6,*) 'genpz: prel^2<0 !!'
             write(6,*) 'branching number: ',1
             write(6,*) 'mother mass (sorry mom): ',masstot(nbpart)
             write(6,*) 'daughter 1 mass: ',masstot(nbpart-1)
             write(6,*) 'daughter 2 mass: ',masstot(nbpart-2)
             write(6,*) 'prel^2: ',prel
             write(6,*) 'real problem, we stop'
             stop
          else
c just numerical
             prel=0.
          endif
        endif
        prel=sqrt(prel)
c one must now decide about the sign. We can see that yres1 is
c nothing but Y+ of my notes. There is no leading particle effect
c at this level.
        if(getRan().lt.0.5) then
          pzres(nbpart-1)=prel
          pzres(nbpart-2)=-prel
        else
          pzres(nbpart-1)=-prel
          pzres(nbpart-2)=prel
        endif
c and we let so because the mother was at rest.
c-------------------------------------------
c the masstot(n-2) branch
c-------------------------------------------
        do i=3,nbpart-1
c only for nbpart>4
         invmass=1./masstot(nbpart-i+1)
         en1=(masstot(nbpart-i+1)**2-masstot(nbpart-i)**2+
     &        masslong(i)**2)*0.5*invmass
         en2=masstot(nbpart-i+1)-en1
         prel=en1**2-masslong(i)**2
         if(prel.le.0) then
           if(prel.le.-5e-3) then
c real problem
             write(6,*) 'genpz: prel^2<0 !!'
             write(6,*) 'branching number: ',i
             write(6,*) 'mother mass (sorry mom): ',masstot(nbpart-i+1)
             write(6,*) 'daughter 1 mass: ',masstot(nbpart-i)
             write(6,*) 'daughter 2 mass: ',masslong(i)
             write(6,*) 'prel^2: ',prel
             write(6,*) 'real problem, we stop'
             stop
           else
c just numerical
             prel=0.
           endif
         endif
         prel=sqrt(prel)
c one must now decide about the sign (again, there is no leading
c particle effect in this branch)
         if(getRan().lt.0.5) then
            pz1=prel
            pz2=-prel
         else
            pz1=-prel
            pz2=prel
         endif
c now, we boost the new particle
         epart(i)=invmass*(eres(nbpart-i+1)*en1+pzres(nbpart-i+1)*pz1)
         pzpart(i)=invmass*(eres(nbpart-i+1)*pz1+pzres(nbpart-i+1)*en1)
c ... and the new resonance:
         eres(nbpart-i)=invmass*
     &            (eres(nbpart-i+1)*en2+pzres(nbpart-i+1)*pz2)
         pzres(nbpart-i)=invmass*
     &            (eres(nbpart-i+1)*pz2+pzres(nbpart-i+1)*en2)
        enddo
c this is the only relevant line for nbpart=3
        epart(nbpart)=eres(1)
        pzpart(nbpart)=pzres(1)
      endif
c------------------------------------------
c the masstot(n-1) branch (this one contains the leading
c  particle effect)
c------------------------------------------
      invmass=1./masstot(nbpart-1)
      en1=(masstot(nbpart-1)**2-masslong(2)**2+masslong(1)**2)*
     &     0.5*invmass
      en2=masstot(nbpart-1)-en1
      prel=en1**2-masslong(1)**2
      if(prel.le.0) then
         if(prel.le.-5e-3) then
c real problem
             write(6,*) 'genpz: prel^2<0 !!'
             write(6,*) 'branching number: ',1
             write(6,*) 'mother mass (sorry mom): ',masstot(nbpart-2)
             write(6,*) 'daughter 1 mass: ',masslong(1)
             write(6,*) 'daughter 2 mass: ',masslong(2)
             write(6,*) 'prel^2: ',prel
             write(6,*) 'real problem, we stop'
             stop
         else
c just numerical
             prel=0.
         endif
      endif
      prel=sqrt(prel)
c up to now, lead part effect not implemented
c --------------------------------------
c here comes the leading particle effect (second part)
c --------------------------------------
c one must now decide about the sign:
      if(.not.(leading)) then
        probsup=0.5
      else
         chymin=(masstot(nbpart-1)**2-masslong(1)**2-masslong(2)**2)
     &          /(2*masslong(1)*masslong(2))
c We can see that ypart1-ypart2 is nothing but Y- of my notes.
c prob: exp(y_)/2*ch(y_)
         if(chymin.lt.1.) then
           if(chymin.lt.0.999980) then
             write(6,*) 'genpz (leading), chymin<1'
             write(6,*) chymin
             stop
           else
             probsup=0.5
           endif
         else
**           probsup=(1.+sqrt(1.-1./chymin**2))/2.
**           ymin=acosh(chymin)
**           write(*,*)'chymin in genpz ',chymin
           ymin=log(chymin+sqrt(max(0.,chymin*chymin-1.)))
           probsup=(1.+sqrt(1.-1./cosh(alpha*ymin)**2))/2.
         endif
      endif
      if(getRan().gt.probsup) then
c as chymin -> infinity, the chances of flipping the part 1 to negative
c values are vanishing
         pz1=-prel
         pz2=prel
      else
         pz1=prel
         pz2=-prel
      endif
c now, we boost the new particles
      epart(1)=invmass*(eres(nbpart-1)*en1+pzres(nbpart-1)*pz1)
      pzpart(1)=invmass*(eres(nbpart-1)*pz1+pzres(nbpart-1)*en1)
      epart(2)=invmass*(eres(nbpart-1)*en2+pzres(nbpart-1)*pz2)
      pzpart(2)=invmass*(eres(nbpart-1)*pz2+pzres(nbpart-1)*en2)
c----------------------------
c and now some test
c---------------------------
      if(test) then
        etottest=0.
        pztottest=0.
        do i=1,nbpart
          etottest=etottest+epart(i)
          pztottest=pztottest+pzpart(i)
        enddo
        if(abs(1-etot/etottest).gt.1E-4) then
          write(6,*) 'genrapid: energy not properly distributed'
          write(6,*) etot,etottest
          stop
        endif
        if(abs(pztottest/etot).gt.1E-4) then
          write(6,*) 'genrapid: momentum not properly distributed'
          do i=1,nbpart
           write(6,*) i,pzpart(i)
          enddo
          write(6,*) pztottest
          stop
        endif
      endif
      return
      end


      subroutine gen4D(massmax,nfi,idf,leading,nit,test,
     &  success2,mass,epart,ptpart,pzpart,weightlong,weighttrans)
      use global
c here is the "full" 3D+mass montecarlo, once the identity of
c particles have been specified.
c input: massmax,stild,nfi,idf,leading,test
c output: mass,pt,rapid,weightlong,weighttrans
c PLEASE, ALLOW massmax > stild*sum(thresholds)
c evaluated in the cm
      implicit none
c the physical types:
      integer nidpt
      parameter (nidpt=18)
      character*20 panam(nidpt)
      real zpa(nidpt),tizo(nidpt),tiza(nidpt),bar(nidpt)
     r  ,ams(nidpt),thr(nidpt),gpa(nidpt)
      logical ldec(nidpt)
      common/zzct/zpa,tizo,tiza,
     &  bar,ams,thr,gpa,ldec,panam
c actual realization
      integer nbmax,nfi,i,nbtrymax,nbtrymass,nbtrymasstr,drift
      integer*4 iseed
      common/cseed/iseed
      parameter (nbmax=40)
      integer idf(nbmax)
      real mass(nbmax),masstry(nbmax),ptpart(nbmax,2),
     & ptparttry(nbmax,2),masslong(nbmax),masslongtry(nbmax),
     & massint(nbmax),massinttry(nbmax),pzpart(nbmax),epart(nbmax),
c     & rapid(nbmax),
     & weightlong,weighttrans,weight,weighttry,
     & ran,modxhi,massmax,masstot
*     external ran
      logical test,leading,first,success,success2
      integer NIT,it
c nit: the number of iterations for montecarlo. DO NOT CHANGE THIS
c NUMBER TO GAIN TIME.
c For better efficiency, the number of iterations could be
c taken to depend on the number of hadrons
c      parameter(NIT=15)
c no more maximal mass, maximal mass, that will support the transverse
c weight, because we improved the transverse mass generation.
c      write(6,*) 'entrance gen4D:',nfi,' particles'
      integer ktry,ktrym
      data ktry/0/
      parameter(ktrym=1000)
      save ktry
c
      drift=2
      first=.true.
      success2=.false.
**    nbtrymax=50
      nbtrymax=180
      nbtrymasstr=0
 10   continue
      call genmassphys(massmax,nfi,idf,first,nbtrymax,
     &  mass,masstot,nbtrymass)
      first=.false.
c      write(6,*) 'step 1'
      call genmasstransbis(nfi,mass,massmax,drift,
     &  success,masslong,ptpart,weighttrans,modxhi)
      nbtrymasstr=nbtrymasstr+1
      if(.not.(success)) then
        if(nbtrymasstr.le.nbtrymax) goto 10
        ktry=ktry+1
        if(ktry.eq.ktrym)then
        write(6,*) 'gen4D: unable to generate trans dist in less than',
     &   nbtrymax,' steps'
        ktry=0
        masstot=massmax
        do i=1,nfi
          masstot=masstot-mass(i)
        enddo
        write(6,*) 'available default mass:',masstot,'/',massmax
        write(6,*) 'for',nfi,' particles'
c        read(5,*)
        endif
        return
      endif
c      write(6,*) 'step 2'
      call genflatlongter(masslong,massmax,nfi,leading,weightlong,
     &            massint)
      weight=weighttrans*weightlong
      do 30 it=1,nit
c        write(6,*) 'it=',it
c now implement the metropolis
        nbtrymasstr=0
 15     continue
        call genmassphys(massmax,nfi,idf,first,nbtrymax,
     &   masstry,masstot,nbtrymass)
c        write(6,*) 'step 3'
        call genmasstransbis(nfi,masstry,massmax,drift,
     &   success,masslongtry,ptparttry,weighttrans,modxhi)
        nbtrymasstr=nbtrymasstr+1
c        write(6,*) 'step 4'
        if(.not.(success)) then
          if(nbtrymasstr.le.nbtrymax) goto 15
          write(6,*)'gen4D: unable to generate trans dist in less than',
     &      nbtrymax,' steps'
          masstot=massmax
          do i=1,nfi
            masstot=masstot-masstry(i)
          enddo
          write(6,*) 'available default mass:',masstot,'/',massmax
          write(6,*) 'for',nfi,' particles'
c          read(5,*)
          return
        endif
c        write(6,*) 'step 5'
        call genflatlongter(masslongtry,massmax,nfi,leading,
     &         weightlong,massinttry)
c        write(6,*) 'step 6'
        weighttry=weighttrans*weightlong
        if(weighttry.lt.weight)then
          if(weight*getRan().gt.weighttry) goto 30
        endif
        do i=1,nfi
          mass(i)=masstry(i)
          ptpart(i,1)=ptparttry(i,1)
          ptpart(i,2)=ptparttry(i,2)
          masslong(i)=masslongtry(i)
          massint(i)=massinttry(i)
        enddo
        weight=weighttry
 30   continue
c      write(6,*) 'step 7'
c now generate the rapidity...
c      call genrapidbis(masslong,nfi,massint,leading,rapid,test)
c ... or the long momentum
      call genpz(masslong,nfi,massint,leading,pzpart,epart,test)
      success2=.true.
c      write(6,*) 'exit gen4D'
      return
      end


      SUBROUTINE DST(ID1,AM1,ID2,AM2,SS,NFI,IDF)
      use global
C  GIVES NO. NFI AND IDS IDF OF PTCLES PRODUCED
C  IN INELASTIC HADRON-HADRONS COLLISIONS
      PARAMETER(NPMX=40,NPMX2=NPMX-2)
      DIMENSION IDF(NPMX)
C
      INCLUDE 'PARTIT1'
      DIMENSION IDMN(NIDPT),IDMX(NIDPT)
      DATA IDMN/2*1,4*3,2*7,4*0,3*13,3*0/
      DATA IDMX/2*2,4*6,2*8,4*0,3*15,3*0/
C
      integer*4 iseed
      COMMON/cseed/ISEED
*     external ran
C
      SAVE PI4
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      IF(FIRST)THEN
        PI4=ATAN(1.)
        FIRST=.FALSE.
      ENDIF
C
*     write(6,*) 'entrance dst',id1,id2
*     ag=ss-(am1+am2+ame)
*     if(ag.le.0.)then
*       write(*,*)'s12e',ss,am1,am2,ame
*       write(*,*)id1,id2
*     endif
C
      ZPI=ZPA(ID1)+ZPA(ID2)
C
      S=SS*SS
      AM1K=AM1*AM1
      AM2K=AM2*AM2
      PCK=((S-AM1K-AM2K)**2-4.*AM1K*AM2K)/(4.*S)
      PCK=MAX(PCK,0.)
      PC=SQRT(PCK)
      E1=SQRT(AM1K+PCK)
      E2=SQRT(AM2K+PCK)
      YTO=LOG((E1+PC)*(E2+PC)/(AM1*AM2))
*     write(*,*)'yto=',yto,' ss/e1+e2 ',ss,e1+e2
*     stop
C
      DCM=SS-(AM1+AM2+AME+AME)
      IF(DCM.GT.0.)THEN
        ANPIV=1.+.90*DCM*SQRT(DCM)/(1.+.35*DCM)
      ELSE
        ANPIV=1.
      ENDIF
      NPX=(SS-(AM1+AM2))/AME
      IF(NPX.LT.1)GOTO 100
      NPX=MIN(NPX,NPMX2)
      A=0.
      DO IPX=1,NPX
        AG=PI4*(IPX/ANPIV)**2
        IF(AG.GT.20.)GOTO 20
        A=A+IPX*EXP(-AG)
      ENDDO
 20   CONTINUE
      RPI=A*getRan()
      A=0.
      DO IPX=1,NPX
        AG=PI4*(IPX/ANPIV)**2
        IF(AG.GT.20.)THEN
          NPI=IPX
          GOTO 40
        ENDIF
        A=A+IPX*EXP(-AG)
        IF(A.GT.RPI.OR.IPX.EQ.NPX)THEN
          NPI=IPX
          GOTO 40
        ENDIF
      ENDDO
 40   CONTINUE
C
      TIZO1=TIZO(ID1)
      TIZO2=TIZO(ID2)
      TIZOT=TIZO1+TIZO2
      TIZAM=ABS(TIZOT)
      TIZA1=TIZA(ID1)
      TIZA2=TIZA(ID2)
      TIZAX=TIZA1+TIZA2
C
      RS=getRan()
      TIZAT=TIZAM-1.
      A=0.
      DO I=NINT(TIZAM*2.),NINT(TIZAX*2.),2
        TIZAT=TIZAT+1.
        DA=CLEBGN(TIZA1,TIZO1,TIZA2,TIZO2,TIZAT,TIZOT)**2
        A=A+DA
*       write(*,*)tiza1,tizo1,tiza2,tizo2,tizat,tizot,DA
        IF(A.GT.RS)GOTO 50
      ENDDO
 50   CONTINUE
C  DECIDED ON THE TOT ISOSPIN
*     write(*,*)'tizat = ',tizat,a
*     stop
C  NOW CONSIDERING WHAT TO DO W/RESONANCES
      PNORES=EXP(-NPI/MAX(YTO,.1))
*     pnores=0.
      NN=0
      IF(ID1.LE.2)NN=NN+1
      IF(ID2.LE.2)NN=NN+1
      IF(NN.EQ.2.AND.NPI.GE.2)THEN
        NTR=2
      ELSEIF(NN.GE.1)THEN
        NTR=1
      ELSE
        NTR=0
      ENDIF
      KCS=0
      DO I=1,NTR
        IF(getRan().GT.PNORES)KCS=KCS+1
      ENDDO
      NFI=NPI+2-KCS
      IF(KCS.EQ.0)THEN
        IDD1=ID1
        IDD2=ID2
      ELSEIF(KCS.EQ.2)THEN
        IDD1=3                         ! to be consistent w/low en par
        IDD2=3
      ELSE
        IF(getRan().GT..5)THEN
          I1=1
        ELSE
          I1=-1
        ENDIF
        I2=-I1
        DO I=I1,I2,I2-I1
          IF(I.EQ.-1)THEN
            IF(ID1.LE.2)THEN
              IDD1=7
              IDD2=ID2
              GOTO 60
            ENDIF
          ELSE
            IF(ID2.LE.2)THEN
              IDD2=7
              IDD1=ID1
              GOTO 60
            ENDIF
          ENDIF
        ENDDO
 60     CONTINUE
      ENDIF
      IDF(1)=IDD1
      IDF(2)=IDD2
      DO I=3,NFI
        IDF(I)=13
      ENDDO
C
 70   CONTINUE
C OLD START
      ZPF=0.
      DO IFI=1,NFI
        IDFI=IDF(IFI)
        IDF(IFI)=IDMN(IDFI)+(IDMX(IDFI)-IDMN(IDFI)+.99999)*getRan()
        ZPF=ZPF+ZPA(IDF(IFI))
      ENDDO
*     WRITE(*,*)'o',(IDF(I),I=1,NFI)
      IF(ABS(ZPF-ZPI).GT.1E-2)GOTO 70
C NOW TOT ISOSPIN
      TIZOB=TIZO(IDF(1))
      TIZAB=TIZA(IDF(1))
      DO IFI=2,NFI
        IDFI=IDF(IFI)
        TIZAI=TIZA(IDFI)
        TIZOI=TIZO(IDFI)
        TIZOC=TIZOI+TIZOB
        TIZAM=ABS(TIZOC)
        TIZAX=TIZAI+TIZAB
        RS=getRan()
        A=0.
        TIZAC=TIZAM-1.
        DO I=NINT(TIZAM*2.),NINT(TIZAX*2.),2
          TIZAC=TIZAC+1.
          DA=CLEBGN(TIZAI,TIZOI,TIZAB,TIZOB,TIZAC,TIZOC)**2
          A=A+DA
          IF(A.GT.RS)GOTO 90
        ENDDO
 90     CONTINUE
        TIZOB=TIZOC
        TIZAB=TIZAC
      ENDDO
*     WRITE(*,*)'o',(IDF(I),I=1,NFI)
*     WRITE(*,*)'TIZAC = ',TIZAC
      IF(ABS(TIZAC-TIZAT).GT..1)GOTO 70
C
      IF(ABS(ZPA(IDF(2))-ZPA(ID1))+ABS(ZPA(IDF(1))-ZPA(ID2)).LT.
     I  ABS(ZPA(IDF(2))-ZPA(ID2))+ABS(ZPA(IDF(1))-ZPA(ID1)))THEN
        II=IDF(1)
        IDF(1)=IDF(2)
        IDF(2)=II
      ENDIF
*     WRITE(*,*)'o',(IDF(I),I=1,NFI)
      RETURN
C
 100  CONTINUE
      NFI=2
      IDF(1)=ID1
      IDF(2)=ID2
C
      END


      subroutine news2(u,pic,nbpart,picctr,piccz,picc0,pif)
c some of Pawell's routines extended: implements the poincarre rotation-
c boost.
c  u is the quadri velocity of cm in the lab
c  picx,picy,picz,pic are components of the vector in the cm that sets
c  reference direction, picc are cm values for deflected particle,
c  pif are final components in the lab
c Honnestly, we should proceed everyrthing in double precision...
      implicit none
      integer dimxmax,nbpart,i
      parameter(dimxmax=40)
      real pi,pipi,pic(0:3),pica,picctr(dimxmax,2),
     & piccz(dimxmax),picc0(dimxmax),pif(dimxmax,0:3)
      double precision u(0:3),bfpi,bpic
      real cth,ag,sth,cofi,sifi,picxf,picyf,piczf
      logical first
      data first/.true./
      if(first)then
        pi=4.*atan(1.)
        pipi=pi+pi
        first=.false.
      endif
c first, deduce the direction of the leading vector in the cm
c (once and for all)
      pica=sqrt(pic(1)**2+pic(2)**2+pic(3)**2)
      if(pica.gt.0.)then
        cth=pic(3)/pica
        ag=1.-cth*cth
      else
        cth=1.
        ag=0.
      endif
      if(ag.gt.0.)then
        sth=sqrt(ag)
        cofi=pic(1)/(sth*pica)
        sifi=pic(2)/(sth*pica)
      else
        sth=0.
        cofi=1.
        sifi=0.
      endif
      do i=1,nbpart
c we first suppose phi=0
        piczf=piccz(i)*cth-picctr(i,1)*sth
        picxf=picctr(i,1)*cth+piccz(i)*sth
c we now perform the phi rotation
        picyf=picctr(i,2)*cofi+picxf*sifi
        picxf=picxf*cofi-picctr(i,2)*sifi
c getting back to the lab
c        bpic=bx*picxf+by*picyf+bz*piczf
c        pif(i,0)=gm*(bpic+picc0(i))
c        bfpi=gm*(picc(0)+gm/(gm+1.)*bpic)
c        pif(i,1)=picxf+bx*bfpi
c        pif(i,2)=picyf+by*bfpi
c        pif(i,3)=piczf+bz*bfpi
        bpic=u(1)*picxf+u(2)*picyf+u(3)*piczf
        bfpi=picc0(i)+bpic/(u(0)+1.)
        pif(i,0)=real(bpic+u(0)*picc0(i))
        pif(i,1)=real(picxf+u(1)*bfpi)
        pif(i,2)=real(picyf+u(2)*bfpi)
        pif(i,3)=real(piczf+u(3)*bfpi)
      enddo
      return
      end


      subroutine dstall(id1,pe1,id2,pe2,leading,success,nfi,idf,
     &  pef,massf,nit,test,weightlong,weighttrans)
c the ultimate high-energy collision routine
c don't know if we should remember massf separately
c the final information is nfi, idf, pef and massf
c Typically, nit>=15 should be chosen.
c there should be also some "formation time, not implemented yet"
      implicit none
      INCLUDE 'PARTIT1'
      real ssut
c      PARAMETER(SSUT=AMV0+AMV0+GMV+GMV)
c with a value of 2.704. However, looking more closely at the dst subroutine,
c it appears, from the ANAPV definition that SS/ST must be at least
c 4*AM0>AMV0+AMV0+GMV+GMV
      PARAMETER(SSUT=4.*AM0)
      integer id1,id2,nbmax,i,j,nit
      parameter (nbmax=40)
      real plab,PE1(0:3),PE2(0:3),PEF(nbmax,0:3),
     &  massf(nbmax),am1k,am2k,s,am1,am2,
     &  epart(nbmax),ptpart(nbmax,2),pzpart(nbmax),weightlong,
     &  weighttrans,bpic,bfpi,pic(0:3),ptot(0:3),mtest
      double precision PET(0:3),sqrts,u(0:3)
      integer nfi,idf(nbmax)
      logical test,leading,success
c first evaluate the available energy.
      do i=0,3
        pet(i)=pe1(i)+pe2(i)
      enddo
      sqrts=dsqrt(pet(0)**2-(pet(1)**2+pet(2)**2+pet(3)**2))
c the quadri velocity of c.m.
c     if(sqrts.le.ssut) goto ...
c  the low energy case. The decision between the high and low energy
c regime is now performed at a higher level
      am1k=pe1(0)**2-(pe1(1)**2+pe1(2)**2+pe1(3)**2)
      am2k=pe2(0)**2-(pe2(1)**2+pe2(2)**2+pe2(3)**2)
      am1=sqrt(am1k)
      am2=sqrt(am2k)
c now, we generate the particles identities
c (the first 2 are the leading ones)
      call DST(ID1,AM1,ID2,AM2,real(sqrts),NFI,IDF)
c now, generate the mass-momenta in the rotated-cm
c      write(6,*) nfi, ' final particles'
      call gen4D(real(sqrts),nfi,idf,leading,nit,test,
     &  success,massf,epart,ptpart,pzpart,weightlong,weighttrans)
      if(.not.(success)) return
*     do i=1,nfi
*       mtest=epart(i)**2-ptpart(i,1)**2-ptpart(i,2)**2-pzpart(i)**2
*       write(*,*)sqrt(mtest),ams(idf(i))
*     enddo
c and finally come back in the lab. The quadrivelocity of
c the cm in the lab is:
      do i=0,3
        u(i)=pet(i)/sqrts
      enddo
c      write(6,*) u(0)**2-u(1)**2-u(2)**2-u(3)**2
c      write(6,*) sqrt(pe1(0)**2-pe1(1)**2-pe1(2)**2-pe1(3)**2)
c and the energy momentum of the leading particle (1) in the cm is
c given by:
      bpic=-(u(1)*pe1(1)+u(2)*pe1(2)+u(3)*pe1(3))
      bfpi=pe1(0)+bpic/(u(0)+1.)
      pic(0)=u(0)*pe1(0)+bpic
      pic(1)=pe1(1)-u(1)*bfpi
      pic(2)=pe1(2)-u(2)*bfpi
      pic(3)=pe1(3)-u(3)*bfpi
c      write(6,*) sqrt(pic(0)**2-pic(1)**2-pic(2)**2-pic(3)**2)
c we are now ready to recover the lab values:
      call news2(u,pic,nfi,ptpart,pzpart,epart,pef)
      if(test) then
        do j=0,3
          ptot(j)=0.
        enddo
        do i=1,nfi
          mtest=pef(i,0)**2-pef(i,1)**2-pef(i,2)**2-pef(i,3)**2
          if(abs(mtest-massf(i)**2).gt.5e-2*mtest) then
            write(6,*) i,' mass prob2:',sqrt(mtest),' vs ',massf(i)
            read(5,*)
          endif
          do j=0,3
            ptot(j)=ptot(j)+pef(i,j)
          enddo
        enddo
        mtest=(ptot(0)-pet(0))**2-(ptot(1)-pet(1))**2-
     &        (ptot(2)-pet(2))**2-(ptot(3)-pet(3))**2
        if(abs(mtest).gt.1e-5) then
           write(6,*) 'tot energy pro:',mtest
           do j=0,3
             write(6,*) pet(j),ptot(j)
           enddo
           stop
        endif
      endif
      return
      end


      integer function typ(id)
      implicit none
c 1: nucl, 2: delt, 3: Anucl, 4: Adelt, 5:pi, 6:rho
      integer id
      typ=0
      if(id.le.0) return
      if(id.le.2) then
        typ=1
      elseif (id.le.6) then
        typ=2
      elseif (id.le.8) then
        typ=3
      elseif (id.le.12) then
        typ=4
      elseif (id.le.15) then
        typ=5
      elseif (id.le.18) then
        typ=6
      endif
      return
      end




      block data zeesh    !!! needs to be modified if id's changed !!!
c the physical data for the particles
      implicit none
      include 'PARTIT1'
c  ptcle id's are:
c  Baryons:
c  1-proton, 2-neutron, 3-delta++, 4-delta+, 5-delta0, 6-delta-
c  Anti Baryons:
c  7-Aneutron, 8-Aproton, 9-Adelta-, 10-Adelta0, 11-Adelta+, 12-Adelta++
c  mesons:
c  13-pi+, 14-pi0, 15-pi-, 16-rho+, 17-rho0, 18-rho-

c a truncated model:
c-------------------
c                p     n /
c      data   zpa/1.0,  0.0/  !charge
c      data   bar/1.0,  1.0/  !baryon number
c the isospin: ()
c      data  tizo/0.5, -0.5/  !isospin

c the full (spring-summer 97) model:
c-----------------------------------
c the charge:
      data   zpa/1.,0.,2.,1.,0.,-1,1.,0.,1.,0.,-1.,-2.
C                p  n D++ D+ D0 D-,*+ *- aD- aD0 aD+ aD++
     &         ,1.,0.,-1.,1.,0.,-1./
C              pi+ pi0 pi- r+ r0 r-
c the baryon number:
      data  bar/8*1.,4*-1.,6*0./
c net izospin
      data tiza/2*.5,4*1.5,2*.5,4*1.5,6*1./
c z-component of isospin
      data tizo/.5,-.5,1.5,.5,-.5,-1.5,.5,-.5,1.5,.5,-.5,-1.5
C                p  n  D++ D+  D0 D-,  *+ *-  aD- aD0 aD+ aD++
     d  ,1.,0.,-1.,1.,0.,-1./
C       pi+ pi0 pi- r+ r0 r-
c the mean masses:
      data  amstr/2*am0,4*amv0,2*amx,4*amv0,3*ame,3*amrh/
c the thresholds:
      data thrtr/2*0.,10*thrdelt,3*0.,3*thrrho/
c does it decay ? ultimately
c      data ldec/2*.false.,4*.true.,2*.false.,4*.true.,
c     &  3*.false.,3*.true./
c for the time:
      data ldec/2*.false.,4*.false.,2*.false.,4*.false.,
     &  3*.false.,3*.false./
c the full width:
      data  gpa/2*0.,4*gmv,2*gmx,4*gmv,3*0.,3*gmr/
      data panam/'proton','neutron','delta++','delta+','delta0',
     & 'delta-','N*+', 'N*0', 'anti delta-',
     & 'anti delta0', 'anti delta+', 'anti delta++','pi+','pi0','pi-',
     & 'rho+', 'rho0', 'rho-'/
      end


C*from Keio Lewenkopf  calculates <a a_z b b_z| c c_z> in Brink-Satchler
C*convention  6/27/93
C      write(*,*)clebgn(1.5,.5,.5,-.5,2.,0.),1./sqrt(2.)
C      write(*,*)clebgn(1.5,-1.5,1.,0.,2.5,-1.5),sqrt(2./5.)
C      write(*,*)threej(3.,4.,5.,-2.,-2.,4.),2.*sqrt(2./(3.*11.*13.))
C      write(*,*)threej(2.,3.,3.,1.,2.,-3.),-sqrt(5./(3.*7.))/2.
C      aj=3.
C      ajj=2.
C      g=2.
C      ex=(1.+g+aj-ajj)*(1.+g-aj+ajj)/((aj+aj+1.)*(aj+aj+2.)
C     e  *(ajj+ajj+1.)*(ajj+ajj+2.))
C      ex=sqrt(ex)
C      if(mod(nint(1.+g+aj+ajj),2).eq.1)ex=-ex
C      write(*,*)sixj(aj,aj+.5,.5,ajj,ajj+.5,g+.5),ex
C      aj=1.
C      ajj=2.
C      g=2.
C      ex=(1.+g+aj-ajj)*(1.+g-aj+ajj)/((aj+aj+1.)*(aj+aj+2.)
C     e  *(ajj+ajj+1.)*(ajj+ajj+2.))
C      ex=sqrt(ex)
C      if(mod(nint(1.+g+aj+ajj),2).eq.1)ex=-ex
C      write(*,*)sixj(aj,aj+.5,.5,ajj,ajj+.5,g+.5),ex
C      aj=2.
C      ajj=2.
C      g=0.
C      ex=(1.+g+aj-ajj)*(1.+g-aj+ajj)/((aj+aj+1.)*(aj+aj+2.)
C     e  *(ajj+ajj+1.)*(ajj+ajj+2.))
C      ex=sqrt(ex)
C      if(mod(nint(1.+g+aj+ajj),2).eq.1)ex=-ex
C      write(*,*)sixj(aj,aj+.5,.5,ajj,ajj+.5,g+.5),ex
C      write(*,*)sixj(1.,1.,0.,2.,2.,2.),-1./sqrt(3.*5)
Cc
C      end
C
C
      FUNCTION SIXJ(U,V,W,X,Y,Z)
C
C           U V W
C  RETURNS {     }
C           X Y Z
C
      SIXJ=RACAH(U,V,Y,X,W,Z)
      IF(MOD(NINT(U+V+Y+X),2).EQ.1)SIXJ=-SIXJ
C
      END


      FUNCTION THREEJ(U,V,W,X,Y,Z)
C
C           U V W
C  RETURNS (     )
C           X Y Z
C
      THREEJ=CLEBGN(U,X,V,Y,W,-Z)/SQRT(W+W+1.)
      ICO=ABS(NINT(U-V-Z))
      IF(MOD(ICO,2).EQ.1)THREEJ=-THREEJ
C
      END


      BLOCK DATA BLOCK10
      COMMON/BLOC10 /FACT(200),FIRST
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      END


C...............................................................................
C
      FUNCTION CLEBGN(A,AZ,B,BZ,C,CZ)
C
C -  PROPOSITO:  CALCULAR OS COEFICIENTES DE CLEBSCH-GORDAN
C
C...............................................................................
C
      COMMON/BLOC10 /FACT(200),FIRST
      LOGICAL FIRST
      DIMENSION D(6)
C
      IF(FIRST)THEN
        CALL LOGFAC
        FIRST=.FALSE.
      ENDIF
C
      IF (ABS(AZ+BZ-CZ)-0.1) 1,1,4
    1 IF (C-ABS(A+B)-0.1) 10,10,4
   10 IF (ABS(A-B)-C-0.1) 11,11,4
   11 IF (AMAX1(ABS(AZ)-A-0.1,ABS(BZ)-B-0.1,ABS(CZ)-C-0.1)) 111,111,4
  111 IF (AMIN1(A+0.1,B+0.1,C+0.1)) 4,4,12
   12 R=A+B+C
      S=R-INT(R+1.1)+1.0
      IF (S-0.1) 13,13,4
   13 IF (ABS(AZ)-0.1) 2,5,5
    2 IF (ABS(BZ)-0.1) 3,5,5
    3 IF (SIGN(1.0,2.0*INT(0.5*R+0.1)+0.5-R)) 4,41,41
    4 CLEBGN=0.0
      RETURN
C
   41 S=INT(0.5*R+0.1)
      P=SIGN(1.0,2.0*INT(0.5*(S+C)+0.1)+0.5-S-C)
      K1=INT(S-A+1.1)
      K2=INT(S-B+1.1)
      K3=INT(S-C+1.1)
      K4=INT(S  +1.1)
      T=0.5*ALOG(C+C+1.0)+DELT(A,B,C)+FACT(K4)
     .            -FACT(K1)-FACT(K2)-FACT(K3)
      CLEBGN=P*EXP(T)
      RETURN
C
    5 D(1)=0.0
      D(2)=B-C-AZ
      D(3)=A-C+BZ
      D(4)=A+B-C
      D(5)=A-AZ
      D(6)=B+BZ
      R=AMAX1(D(1),D(2),D(3))
      S=AMIN1(D(4),D(5),D(6))
      IF (S-R+0.1) 4,6,6
    6 MN=INT(R+0.1)
      MX=INT(S+0.1)
      K1=INT(A+AZ+1.1)
      K2=INT(D(5)+1.1)
      K3=INT(D(6)+1.1)
      K4=INT(B-BZ+1.1)
      K5=INT(C+CZ+1.1)
      K6=INT(C-CZ+1.1)
      S=0.5*(FACT(K1)+FACT(K2)+FACT(K3)+FACT(K4)+FACT(K5)+FACT(K6))
     .          +DELT(A,B,C)
      R=0.0
      P=SIGN(1.0,2.0*INT(0.5*MN+0.1)+0.5-MN)
      DO 8 M=MN,MX
      T=S
      DO 7 I=1,3
      J=INT(M-D(I)+1.1)
      K=INT(D(I+3)-M+1.1)
      T=T-FACT(J)-FACT(K)
    7 CONTINUE
      R=R+P*EXP(T)
      P=-P
    8 CONTINUE
      CLEBGN=R*SQRT(C+C+1.0)
C
      END


      FUNCTION DELT(A,B,C)
      COMMON/BLOC10 /FACT(200),FIRST
      LOGICAL FIRST
C
      I1=INT(A+B+C+2.1)
      I2=INT(A+B-C+1.1)
      I3=INT(A+C-B+1.1)
      I4=INT(B+C-A+1.1)
      DELT=0.5*(FACT(I2)+FACT(I3)+FACT(I4)-FACT(I1))
C
      END


      SUBROUTINE LOGFAC
      COMMON/BLOC10 /FACT(200),FIRST
      LOGICAL FIRST
C
      FACT(1)=0.0
      DO 1 I=2,200
      FACT(I)=FACT(I-1)+ALOG(FLOAT(I-1))
    1 CONTINUE
C
      END


C...............................................................................
C
      FUNCTION RACAH(A,B,C,D,E,F)
C
C PROPOSITO :  CALCULAR OS COEFICIENTES DE RACAH.
C
C...............................................................................
C
      COMMON/BLOC10 /FACT(200),FIRST
      LOGICAL FIRST
      DIMENSION G(8)
C
      IF(FIRST)THEN
        CALL LOGFAC
        FIRST=.FALSE.
      ENDIF
C
      G(1)=0.0
      G(2)=A+D-E-F
      G(3)=B+C-E-F
      G(4)=A+B-E
      G(5)=C+D-E
      G(6)=A+C-F
      G(7)=B+D-F
      DO 100 M=4,6
      S=G(M)-AINT(G(M)+1.1)+1.0
      IF (S-0.1) 100,100,1
  100 CONTINUE
      R=AMAX1(G(1),G(2),G(3))
      S=AMIN1(G(4),G(5),G(6),G(7))
      IF (S-R+0.1) 1,2,2
    1 RACAH=0.0
      RETURN
C
    2 IF (E-0.1) 4,3,3
    3 IF (F-0.1) 4,5,5
    4 S=ABS(A+D-E-F)
      RACAH=SIGN(1.0,2.0*AINT(0.5*S+0.1)+0.5-S)/
     .      SQRT((2.0*A+1.0)*(2.0*D+1.0))
      RETURN
C
    5 IF (A-0.1) 9,6,6
    6 IF (B-0.1) 9,7,7
    7 IF (C-0.1) 9,8,8
    8 IF (D-0.1) 9,10,10
    9 RACAH=1.0/SQRT((2.0*E+1.0)*(2.0*F+1.0))
      RETURN
C
   10 MN=INT(R+0.1)
      MX=INT(S+0.1)
      S=DELT(A,B,E)+DELT(A,C,F)+DELT(B,D,F)+DELT(C,D,E)
      G(8)=A+B+C+D+1.0
      R=0.0
      P=SIGN(1.0,2.0*AINT(0.5*MN+0.1)+0.5-MN)
      DO 12 M=MN,MX
      T=S
      DO 11 I=1,3
      J=INT(M-G(I)+1.1)
      K=INT(G(I+3)-M+1.1)
      T=T-FACT(J)-FACT(K)
   11 CONTINUE
      J=INT(G(8)-M+1.1)
      K=INT(G(7)-M+1.1)
      R=R+P*EXP(T+FACT(J)-FACT(K))
      P=-P
   12 CONTINUE
      RACAH=R
C
      END
