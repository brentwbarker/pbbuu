*      PARAMETER (IA1=84,IA2=197)
      PARAMETER (IA1=197,IA2=197)
      PARAMETER (TLAB=.10)
      PARAMETER (NFI=1)
*      PARAMETER (NFI=15)
*      PARAMETER (NFI=21)
      CHARACTER*7 FNAME(NFI)
      DIMENSION NQU(NFI)
*      DATA NQU/200/
      DATA NQU/NFI*200/
      PARAMETER (PHOTR=0.)  !photon nrm. factor (0-20)
      data fname/'AAfvzdz'/
**    data fname/'aakzzn2','aaz65e8','aarfr3w','aazq2n5','aan0xb8'
**   d  ,'aasc2cw'/
*      DATA FNAME/'SCYUZ4F','SCYGAIO','SCYQ4YL','SCZDE80','SCXIKHF'/
*      DATA FNAME/'NCYUQW6','NCYL9PV','NCYGMYR','NCYARS8'/
*      data nqu/170,600,8*500,2*600/
*     a  fname/'aaxxz14','aax69rt','aamp04h'
*     d  ,'aamsly1','aarq89c','aazi4q8','aaxp2ux','aaymkx2'
*     a  ,'aa08put','aazmkwu'
*     t  ,'aay8rbd','aay7cip'/
*      DATA NQU/170,500/FNAME/'AAYPYWH','AAXLEID'/
*      DATA NQU/170,500/FNAME/'AAX0T9U','AAZ1WF5'/
*      DATA FNAME/'AAykzbj','aa-xpiy','AAYIE8V','AARINSX','AAMN4VO'
*     D  ,'AAURU57'/
*      DATA NQU/2*170,500/FNAME/'AA_SWYA','AA_U8C7','AAZBV3Q'/
*      DATA NQU/170,500/FNAME/'AA_S3NO','AAXRCBG'/
*      DATA NQU/170,500/FNAME/'AA-TUU4','AAYW3TR'/
*      DATA NQU/170,500/FNAME/'AA$TV3P','AAXSHT8'/
*      DATA NQU/170,500/FNAME/'AA-sij1','AAxv6jd'/
*      DATA NQU/170,500/FNAME/'AA$9ne0','AAzf2ux'/
*      DATA NQU/170,500/FNAME/'AA_9lqq','AAxc166'/
*      DATA NQU/170,500/FNAME/'AA$ylpw','AAy0eua'/
*      DATA NQU/170,500/FNAME/'AA-2uow','AAyoh0z'/
*      data nqu/2*600,170,2*600/fname/'aay8rbd','aay7cip','aaxxz14'
*     d  ,'aaydjh7','aax69rt'/
*      DATA NQU/1000,8000/FNAME/'AK$CASC','AK$CASK'/
*      DATA NQU/1000,8000/FNAME/'AK$CASI','AK$CAS1'/
*      DATA NQU/2*170,600,500,600,400/
*      DATA FNAME/'AAX0FGO','AAU80UM','AAXWPI2','AAR1V0D','AAXVW0B'
*     D  ,'AAR8SLH'/
*      DATA nqu/170,600,5*500/FNAME/'aazvl5p','aay1ctv','aa0zlqu'
*     d  ,'aa$jo2o','aazxbjy','aaxgshc','aay2nsd'/
*      data fname/'np-c9a2','np-v46y'/nqu/170,600/
      logical lold(nfi)
      data lold/nfi*.true./
*      data lold/2*.true./
*      data fname/'aky8r9m','akybgns','akuh8ag','akumpgq'/
*      data nqu/600,3*1000/
*      DATA NQU/7*170,8*600/
*      data fname/'laXE53L','LAYNMIR','lau7dpt','la06oql'
*     f  ,'layzgj6','laudyre','laywvge'
*     N  ,'LLYZQEM','LLX5YVT','LLZEMMT','LLZ9GVU','LLX136P'
*     A  ,'LLYZKI4','LLY8VOJ','LLXIVZC'/
*      data NQU/2*700,16*600,3*170/
*      data fname/'laxvj3j','laxseo8','lly9zu6','llz7wrb','llxa7sg'
*     d  ,'llx2bay','llzf2w5','llyk8g6'
*     a  ,'llykg53','llxie6w','llzpey9','llztlw4','llx3d5h','llywuur'
*     T  ,'LLXY9B9','LLY4FVT','LLZODKT','LLY52LS'
*     a  ,'llxg8fr','lly9yz9','llyk8g6'/
*      data fname/'aay8rbd','aay7cip','AAZU59T'/NQU/3*600/
*      data fname/'aax0sxr','aa0hw05'/NQU/600,170/
*      data fname/'lay3pyh','lay5817'/NQU/2*700/
*      data fname/'aa08aev','aa0w9ry'/
*      data fname/'aa0ib8q','aaxnpyt','AAXUCEM','AA-BIFF','AAYW1YZ'
*     F  ,'AAY50JS','AAYGMQO','AAZ7QPE','AAZ96VO'/
*      data fname/'aaxi3d8','aax0t9u'/
*      data fname/'ap$gt23','ap0wopv','apx2kj2'/
*      DATA FNAME/'aaxws9y'/
*      data NQU/170,1000/fname/'aaU9hxm','AAqn2qs'/
*      data NQU/170,1000/fname/'aaUZD6I','AAUELPO'/
*      data nqu/nfi*400/fname/'kn-qy40','kn_9zpd','KNx5ruk','KN_mfz2'/
*      data nqu/nfi*400/fname/'kn_pkzr','kn-didt','KNxr5te','KNxondi'/
*      data nqu/nfi*400/fname/'kn-6bsp','kn_2w0i','KNxxv1c','KN0nreo'/
*      data nqu/nfi*400/fname/'kn_jb1o','kn-n0ck','KN-DX18','KN_VX1B'/
*      data nqu/nfi*400/fname/'KN_5XP9','kn_ype6','kn_q0bb'
*     d  ,'kn-rojk'/
*       data nqu/2*350/fname/'np-6vyd','np_eyed'/
*      DATA FNAME/'nB4B45P_1F'/
*      data nqu/3*170,350,170/
*      data fname/'la_7d4i','la$7j4u','la07pwa','la09hwo','la$si5p'/
      PARAMETER (NPX=1)         !set to 1 for px printout, 0 otherwise
*      DATA NQU/NFI*600/
*      DATA FNAME/'ak_48yj','ak$7iqg','AK_jljj','ak$3ciz','ak$xc0u'/
*      DATA FNAME/'la0ijmy','la_cnol'/
*      data fname/'la_bjw5','la0u8ag','LA0G6O5','la0ijmy','la_cnol'
*     d  ,'la-q96d','la$isl2','la_ectn'/
*      DATA FNAME/'LA0sjx9','la_wzlz'/
*      DATA FNAME/'AU-PCKR','AU_DXRZ'/
*      data fname/'au_kn8n','AU_46VQ'/
*      data fname/'au0j1go','au_rqct','au_mff1'/
*      DATA FNAME/'AA0LW05','AA0WJ2O','AA-PYBW'/
*       DATA FNAME/'NB_HNC4','NB_901K','NB_XNWV','NB_SYAI'/
*      DATA FNAME/'AA_L6UV','AA_1OVY','AAXCKWA','AAXHJ5L'/
*      DATA FNAME/'AAXKOKO','AA_XYI1','AA_R4RP','AA06QN7'
*     D  ,'AA-KSDT','AA04UHG'/
*      DATA FNAME/'AAXBX9K','AAX9SJ6','AAXWS9Y','AA0PO1J','AAXHHMC'/
*      DATA FNAME/'AA0JMLA','AA_5EOT','AA0WY7T','AA04ICR','AA_F2VC'/
*      DATA FNAME/'nb0vp8n','nbxnep4','nbo4qhh','nb0uomh','nb_sgre'/
*      DATA FNAME/'NB-31X1','NB02610','NB_8RUG','NB-62WD'/
*      DATA FNAME/'NBxjc31','NB$8dq2','NBxbxyh','NB$yb2p','nb_edo2'/
*      DATA FNAME/'AA-H8VC'/
*      DATA FNAME/'NB-BPKH'/
*      DATA FNAME/'AA-Ws78','AA052ub','AAX9KQK'/
*      DATA FNAME/'AA-W45X','AA0WALG'/
*      DATA FNAME/'AAxde5s','AA-t39j','AA0K47C','AAXAEQQ'/
*      DATA FNAME/'AA_pv95','AA-fiac'/
*      DATA FNAME/'AA_06cv','AA_42xf','AAXLXBR','AA0YXGF'/
*      DATA FNAME/'CAXAOXY'/
*      DATA FNAME/'AA_P514','AA_3LKU','AA_CAY0','AA_B0NZ','AA-EUM5'/
*      DATA FNAME/'AA_CAY0','AA_B0NZ'/
*      DATA FNAME/'CC_S2JY'/NQU/1700/
*      DATA FNAME/'NB_5V7D','NB$IODU','NBXFM9Z','NB_EX0X'
*     F  ,'NB_OQYQ','NB_HRTM','NB_J4ZR'/
*      DATA NQU/5*170,220,170/
C
      INCLUDE 'PARTID.'
C
      PARAMETER (NII=9)
      PARAMETER (NIIP=8)
*      PARAMETER (EMAX=.45)
      PARAMETER (EMAX=.20)
*      PARAMETER (DET=.006,DNGL=10.)
*      PARAMETER (DET=.015,DNGL=10.)
      PARAMETER (DET=.010,DNGL=90.)
      PARAMETER (NET=EMAX/DET+1)
      PARAMETER (NETS=4)
      DIMENSION DSE(NET,NII),DSES(NET,NII)
      PARAMETER (NAA=7)
      DIMENSION DSA(NAA,NII),DSAS(NAA,NII)
      PARAMETER (DY=.4)
      PARAMETER (NPHI=7)
      DIMENSION DSH(NPHI,NII),DSHS(NPHI,NII)
      DIMENSION DSR(NPHI,NII),DSRS(NPHI,NII)
      DIMENSION SUH(NII),SUR(NII)
      DATA SUH/NII*0./SUR/NII*0./
c
      DIMENSION APT(NII),EPT(NII),EPTS(NII)
C
      DIMENSION EPCT(NII),EPCTS(NII),EPCZ(NII),EPCZS(NII),APC(NII)
      DIMENSION EPCX(NII),EPCXS(NII),EPCY(NII),EPCYS(NII)
      DIMENSION APCC(NII)
      DIMENSION EPrX(NII),EPrXS(NII),EPrY(NII),EPrYS(NII),apr(nii)
C
      PARAMETER (YMIN=-.50,YMMAX=1.5)
      PARAMETER (DDY=.10,NY=(YMMAX-YMIN)/DDY*NPX+1.)
      DIMENSION ANY(NY,0:NII),PXY(NY,0:NII),PXYS(NY,0:NII)
      PARAMETER (NYI=NY*NII,NYI1=NY*(NII+1))
      DATA ANY/NYI1*0./PXY/NYI1*0./PXYS/NYI1*0./
      LOGICAL GETPX
      PARAMETER (GETPX=NPX.NE.0)
C
      PARAMETER (PTMAX=1.)
      PARAMETER (DPT=.025,NPT=PTMAX/DPT+1.)
      DIMENSION ANPT(NPT,NII)
      PARAMETER (NPTI=NPT*NII)
      DATA ANPT/NPTI*0./
c
      parameter (detc=.08)
      parameter (emaxc=.6)
      parameter (netc=emaxc/detc+1.)
      dimension ani(netc),anico(netc),anicos(netc)
      data ani,anico,anicos/netc*0.,netc*0.,netc*0./
C
      DIMENSION VYY(NII),VXX(NII),VZZ(NII),VXZ(NII)
      DIMENSION VTH(NII),CVTH(NII),SVTH(NII)
C
      PARAMETER (AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER (AM0K=AM0*AM0)
      PARAMETER (BD=.002225,AMD=AM0+AM0-BD)
      PARAMETER (AMDK=AMD*AMD)
      PARAMETER (AMEC=.13957,AMEN=.13496,AME=(AMEC+AMEC+AMEN)/3.)
      PARAMETER (AMEK=AME*AME)
      PARAMETER (BT=.0086,AMT=AMP+AMN+AMN-BT)
      PARAMETER (B3E=.0080,AM3E=AMP+AMP+AMN-B3E)
      PARAMETER (AMTE=.5*(AMT+AM3E))
      PARAMETER (AMTEK=AMTE*AMTE)
C
      PARAMETER (B=.008)
      PARAMETER (AMB=AM0-B)
      PARAMETER (EBEAM=TLAB+AMB)
      PARAMETER (AMBK=AMB*AMB)
C
      COMMON/RAP/YBEAM,YCM,PF,RENAN,GG1,BB1,GG2,BB2
C
      INTEGER*2 IXI,IYI,IZI
      INTEGER*2 IDIC
      CHARACTER FILNAM*14
C
      PARAMETER (AN0=.160)
C
      PI=4.*ATAN(1.)
c
      NAB=NAA+NAA-1
      DAA=PI/NAB
      FEA=.5/((PI+PI)*DAA)
C
      DPHI=.5*PI/(NPHI-1)
C
      ANGL=DNGL*PI/180.
      IF(ANGL.LT..5*PI-1E-4)THEN
        FTZK=TAN(ANGL)**2
      ELSE
        FTZK=1E30
      ENDIF
      CANGL=SIN(ANGL)
      FET=1./((PI+PI)*(CANGL+CANGL)*DET)
C
      PBEAM=SQRT(EBEAM*EBEAM-AMBK)
      YBEAM=ALOG((EBEAM+PBEAM)/AMB)
C
      ECM=IA1*EBEAM+IA2*AMB
      PCM=IA1*PBEAM
      YCM=.5*ALOG((ECM+PCM)/(ECM-PCM))
C  NEXT FOR LORENTZ TRANSFORMATIONS
      YPR=YBEAM-YCM
      GG1=COSH(YPR)
      BB1=TANH(YPR)
      GG2=COSH(YCM)
      BB2=-TANH(YCM)                          !negative...
C
      YMAX=YBEAM+.5
      NYMX=(YMAX-YMIN)/DDY+1.
      NYMX=MIN(NYMX,NY)
C
      DO IN=1,NII
        APC(IN)=0.
        APCC(IN)=0.
        EPCT(IN)=0.
        EPCTS(IN)=0.
        EPCX(IN)=0.
        EPCXS(IN)=0.
        EPCY(IN)=0.
        EPCYS(IN)=0.
        EPCZ(IN)=0.
        EPCZS(IN)=0.
        APR(IN)=0.
        EPRX(IN)=0.
        EPRXS(IN)=0.
        EPRY(IN)=0.
        EPRYS(IN)=0.
        DO IP=1,NET
          DSE(IP,IN)=0.
          DSES(IP,IN)=0.
        ENDDO
        DO IA=1,NAA
          DSA(IA,IN)=0.
          DSAS(IA,IN)=0.
        ENDDO
        DO IH=1,NPHI
          DSH(IH,IN)=0.
          DSHS(IH,IN)=0.
          DSR(IH,IN)=0.
          DSRS(IH,IN)=0.
        ENDDO
      ENDDO
      c4=0.
      c4s=0.
      C2=0.
      C2S=0.
      NC2=0
C
      DO IN=1,NII
        VXX(IN)=0.
        VYY(IN)=0.
        VZZ(IN)=0.
        VXZ(IN)=0.
      ENDDO
C
      SXX=0.
      syy=0.
      SZZ=0.
      SXZ=0.
C
      PST=0.
      PSZ=0.
      zcha=0.
      zpart=0.
C
      KF=1
 41   CONTINUE
      FILNAM=FNAME(KF)//'.DAt'
      OPEN(11,FILE=FILNAM
     O  ,FORM='UNFORMATTED',STATUS='OLD')
C
 51   CONTINUE
      READ(11,END=251)IDIC,IXI,IYI,IZI
      if(lold(kf).and.idic.ge.4)idic=idic+3
      IDC=IDIC
      IF(IDC.LT.1)GOTO 51
      IF(IDC.GT.5.AND.IDC.LT.13)GOTO 51
*      IF(IDC.GT.16)GOTO 51
      IF(IDC.Ge.16)GOTO 51
      IF(IDIC.LT.1.OR.IDIC.GT.5)GOTO 51
      AM=AMS(IDC)
      AMK=AM*AM
      PXI=IXI*1E-3
      PYI=IYI*1E-3
      PZI=IZI*1E-3
      PTK=PXI*PXI+PYI*PYI
      PT=SQRT(PTK)
      PK=PTK+PZI*PZI
      E=SQRT(AMK+PK)
      CALL CMS(E,PZI,EEC,PZC,AMM,YC)
C
      IF(ZPA(IDC).NE.0..AND.BAR(IDC).NE.0.)THEN
        PST=PST+PT
        PSZ=PSZ+ABS(PZC)
        zbaf=zpa(idc)/bar(idc)
        SXX=SXX+PXI*PXI/AM*zbaf
        Syy=Syy+PyI*PyI/AM*zbaf
        SZZ=SZZ+PZC*PZC/AM*zbaf
        SXZ=SXZ+PXI*PZC/AM*zbaf
        zcha=zcha+zpa(idc)
      ENDIF
      IF(BAR(IDC).NE.0.)THEN
        VXX(IDC)=VXX(IDC)+PXI*PXI/(AM+EEC)
        VZZ(IDC)=VZZ(IDC)+PZC*PZC/(AM+EEC)
        VXZ(IDC)=VXZ(IDC)+PXI*PZC/(AM+EEC)
      ENDIF
C
      GOTO 51
C
 251  CONTINUE
      CLOSE(11)
      KF=KF+1
      IF(KF.LE.NFI)GOTO 41
C
      TH2=ATAN2(SXZ+SXZ,SZZ-SXX)
      TH=.5*TH2     !0./180.*pi
*      th=25./180.*pi
      CTH=COS(TH)
      STH=SIN(TH)
c
*      cof=.82
*      co2f=.53
      cof=1.
      co2f=1.
      ssu=sxx+syy
      IF(SZZ.NE.0.)THEN
        ERAT=(SXX+SYY)/SZZ
      ELSE
        ERAT=0.
      ENDIF
      sdi=(sxx-syy)*co2f
      sxzi=sxz*cof
      syyi=.5*(ssu-sdi)
      sxxi=.5*(ssu+sdi)
      f1i=.5*(szz+sxxi-sqrt((szz-sxxi)**2+4.*sxzi*sxzi))
      f3i=szz+sxxi-f1i
      r21i=syyi
      if(f1i.ne.0.)r21i=r21i/f1i
      r31i=f3i
      if(f1i.ne.0.)r31i=r31i/f1i
C
      DO IN=1,NII
        IF(VZZ(IN).NE.0.)THEN
          VTH2=ATAN2(VXZ(IN)*2.,VZZ(IN)-VXX(IN))
          VTH(IN)=.5*VTH2
        ELSE
          VTH(IN)=TH
        ENDIF
        CVTH(IN)=COS(VTH(IN))
        SVTH(IN)=SIN(VTH(IN))
        VZZ(IN)=0.
        VXX(IN)=0.
      ENDDO
C
      OPEN(13,FILE=FNAME(1)//'.ANS',STATUS='UNKNOWN')
C
      KF=1
      NTO=0
 40   CONTINUE
      FILNAM=FNAME(KF)//'.DAt'
      OPEN(11,FILE=FILNAM,FORM='UNFORMATTED'
     O  ,STATUS='OLD')
C
      NTO=NTO+NQU(KF)
C
      DO I=1,NII
        APT(I)=0.
        EPT(I)=0.
        EPTS(I)=0.
      ENDDO
 50   CONTINUE
      READ(11,END=250)IDIC,IXI,IYI,IZI
      if(lold(kf).and.idic.ge.4)idic=idic+3
      IF(IDIC.LT.1)GOTO 50
      IF(IDIC.GT.5.AND.IDIC.LT.13)GOTO 50
*      IF(IDIC.GT.16)GOTO 50
      IF(IDIC.Ge.16)GOTO 50
      AM=AMS(IDIC)
      AMK=AM*AM
      PXI=IXI*1E-3
      PYI=IYI*1E-3
      PZI=IZI*1E-3
      PTK=PXI*PXI+PYI*PYi
      PT=SQRT(PTK)
      PK=PTK+PZI*PZI
      E=SQRT(AMK+PK)
      CALL CMS(E,PZI,EEC,PZC,AMM,YC)
      IDC=IDIC       !horror below
      iidc=idc
      pxii=pxi
      pyii=pyi
      pzci=pzc
      eeci=eec
*      if(iidc.ge.13.and.eeci-ams(13).lt..1)goto 50   !!!!!!!!!!!!!!!!
      CALL TEST(iIDC,PXIi,PYIi,PZCi,EECi,*50,IPART,IOV)
*      if(ipart.le.0)goto 50
      if(ipart.ne.0.and.ZPA(iIDC).NE.0..AND.BAR(iIDC).NE.0.)then
        zpart=zpart+zpa(iidc)
      endif
*      if(idc.ge.6.or.idc.le.0)goto 50
*      pttk=pxi*pxi+pyi*pyi
*      ptt=sqrt(pttk)
*      if(ptt.Agt.6.*abs(pzc))then
      if(pt.gt.6.*abs(pzc))then
        ipt=1
      else
        ipt=0
      endif
*      if(ipart.eq.0)ipt=0
      ipt=1
*      et=sqrt(pttk+amk)-sqrt(amk)
*      et=pttk/(sqrt(amk)+eec)
*      write(*,*)idic,et,ipt,ipart
*      write(*,*)idic,pzi,pzc
      IF(IDC.LE.5)THEN
        IDCI=IDC
      ELSE
        IDCI=IDC-7
      ENDIF
      IF(IDCI.LE.NII)THEN
        APC(IDCI)=APC(IDCI)+1.
        ET=PTK/(AM+EEC)
        EPCT(IDCI)=EPCT(IDCI)+ET
        EPCTS(IDCI)=EPCTS(IDCI)+ET*ET
        EZ=PZC*PZC/(AM+EEC)
        EPCZ(IDCI)=EPCZ(IDCI)+EZ
        EPCZS(IDCI)=EPCZS(IDCI)+EZ*EZ
C
        PXV=PXI*CVTH(IDCI)-PZC*SVTH(IDCI)
        PZV=PZC*CVTH(IDCI)+PXI*SVTH(IDCI)
        VXX(IDCI)=VXX(IDCI)+PXV*PXV/(AM+EEC)
        VYY(IDCI)=VYY(IDCI)+PYI*PYI/(AM+EEC)
        VZZ(IDCI)=VZZ(IDCI)+PZV*PZV/(AM+EEC)
C
        IT=PT/DPT+1.
        IF(IT.LE.NPT)THEN
          ANPT(IT,IDCI)=ANPT(IT,IDCI)+1.
        ENDIF
C
        IF(GETPX)THEN
          YL=YC+YCM
          IY=(YL-YMIN)/DDY+1.
          IF(IY.GE.1.AND.IY.LE.NYMX)THEN
            ANY(IY,IDCI)=ANY(IY,IDCI)+1.
            PXY(IY,IDCI)=PXY(IY,IDCI)+PXI
            PXYS(IY,IDCI)=PXYS(IY,IDCI)+(PXI)**2
*            PXY(IY,IDCI)=PXY(IY,IDCI)+PXI/eec
*            PXYS(IY,IDCI)=PXYS(IY,IDCI)+(PXI/eec)**2
**            IF(IDCI.LE.5.AND.IDCI.NE.2)THEN
            IF(IDCI.ge.1.AND.IDCI.le.2)THEN
              ANY(IY,0)=ANY(IY,0)+1.
              PXY(IY,0)=PXY(IY,0)+PXI/BAR(IDCI)
              PXYS(IY,0)=PXYS(IY,0)+(PXI/BAR(IDCI))**2
*              PXY(IY,0)=PXY(IY,0)+PXI/eec
*              PXYS(IY,0)=PXYS(IY,0)+(PXI/eec)**2
            ENDIF
          ENDIF
        ENDIF
C
*        ET=EEC-AM
        DEPT=ipt*et
*       IP=INT((EEC-AM)/DET+.5)
        IP=(EEC-AM)/DET+1.
        if(idci.eq.8)then
          ipc=(eec-am)/detc+1.
          if(ipc.ge.1.and.ipc.le.netc.and.pzc.ne.0.)then
            ani(ipc)=ani(ipc)+1.
            cosk=pzc*pzc/(ptk+pzc*pzc)
            coss=sqrt(cosk)
            anico(ipc)=anico(ipc)+coss
            anicos(ipc)=anicos(ipc)+coss*coss
*           anico(ipc)=anico(ipc)+cosk
*           anicos(ipc)=anicos(ipc)+cosk*cosk
          endif
        endif
*       IF(FTZK*PTTK.GE.PZC*PZC)THEN
        IF(FTZK*PTK.GE.PZC*PZC)THEN
          APT(IDCI)=APT(IDCI)+IPT
          EPT(IDCI)=EPT(IDCI)+DEPT
          EPTS(IDCI)=EPT(IDCI)+DEPT*DEPT
          IF(IP.GE.1.AND.IP.LE.NET)THEN
            DSE(IP,IDCI)=DSE(IP,IDCI)+1.
          ENDIF
        ENDIF
        if(pzc.ne.0.)then
*         ang=atan(ptt/abs(pzc))
          ang=atan2(pt,pzc)
          ia=ang/daa+1.
          IF(IA.GT.NAA)IA=NAA+NAA-IA
          if(ia.ge.1.and.ia.le.naa)THEN
            dsa(ia,idcI)=dsa(ia,idcI)+1.
          ENDIF
        endIF
*       IF(ABS(pzC)/AMS(IDC).LT.DY)THEN
        IF(ABS(YC).LT.DY)THEN
          APCC(IDCI)=APCC(IDCI)+1.
          EX=PXI*PXI/(AM+EEC)
          EPCX(IDCI)=EPCX(IDCI)+EX
          EPCXS(IDCI)=EPCXS(IDCI)+EX*EX
          EY=PYI*PYI/(AM+EEC)
          EPCY(IDCI)=EPCY(IDCI)+EY
          EPCYS(IDCI)=EPCYS(IDCI)+EY*EY
          IF(PXI.NE.0.)THEN
            PHI=ATAN(ABS(PYI/PXI))
            IH=PHI/DPHI+1.5
            IF(IH.GE.1.AND.IH.LE.NPHI)THEN
              DSH(IH,IDCI)=DSH(IH,IDCI)+1.
            ENDIF
            IF(IDCI.LE.6)THEN
              co4=cos(4.*phi)
              c4=c4+co4
              c4s=c4s+co4*co4
              CO2=COS(PHI+PHI)
              C2=C2+CO2
              C2S=C2S+CO2*CO2
              NC2=NC2+1
            ENDIF
          ENDIF
        ENDIF
C
        PXR=PXI*CTH-PZC*STH
        PZR=PZC*CTH+PXI*STH
*       YR=.5*LOG((EEC+PZR)/(EEC-PZR))
        IF(AMS(IDC).GT.0.)THEN
          IF(ABS(pzR)/AMS(IDC).LT.DY)THEN
            APR(IDCI)=APR(IDCI)+1.
            EX=PXR*PXR/(AM+EEC)
            EPRX(IDCI)=EPRX(IDCI)+EX
            EPRXS(IDCI)=EPRXS(IDCI)+EX*EX
            EPRY(IDCI)=EPRY(IDCI)+EY
            EPRYS(IDCI)=EPRYS(IDCI)+EY*EY
            IF(PXR.NE.0.)THEN
              PHI=ATAN(ABS(PYI/PXR))
              IH=PHI/DPHI+1.5
              IF(IH.GE.1.AND.IH.LE.NPHI)THEN
                DSR(IH,IDCI)=DSR(IH,IDCI)+1.
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      endif
      GOTO 50
C
 250  CONTINUE
      KF=KF+1
      IF(KF.LE.NFI)GOTO 40
      CLOSE(11)
C
      do ip=1,netc
        anii=ani(ip)
        if(anii.gt.0.)then
          aco=anico(ip)/anii
          acs=anicos(ip)/anii-aco*aco
*          write(*,*)acs,anii
          acs=acs/anii
          acs=sqrt(acs)
          aso=6.*(2.*aco-1.)/(3.-4*aco)
          asos=acs*12./(3.-4.*aco)**2
*         aso=(15.*aco-5.)/(3.-5*aco)
*         asos=acs*20./(3.-5.*aco)**2
          anico(ip)=aso
          anicos(ip)=asos
        endif
      enddo
c
      zcha=zcha/nto
      zpart=zpart/nto
      write(*,*)'zcha = ',zcha,' nto = ',nto,' zpart = ',zpart
      write(*,*)' '
      write(13,*)'zcha = ',zcha,' nto = ',nto,' zpart = ',zpart
      write(13,*)' '
      FETN=FET/NTO
      FEAN=FEA/NTO
      DO IN=1,NII
        IF(IN.LE.5)THEN
          AMSI=AMS(IN)
        ELSE
          AMSI=AMS(IN+7)
        ENDIF
        DO IP=1,NET
*          te=ip*DET
          TE=(IP-.5)*DET
          E=AMSI+TE
          P=SQRT(TE*(AMSI+AMSI+TE))
          PE=P*E
          FETP=FETN/PE
          IF(IN.EQ.16)THEN
            IF(PHOTR.NE.0.)FETP=FETP/PHOTR
          ENDIF
          DSES(IP,IN)=SQRT(DSE(IP,IN))*FETP
          DSE(IP,IN)=DSE(IP,IN)*FETP
        ENDDO
        aton=0.
        do ia=1,NAA
          AN=DAA*(IA-.5)
          SINA=SIN(AN)
          FENS=FEAN/SINA
          dsas(ia,in)=sqrt(dsa(ia,in))
          aton=aton+dsa(ia,in)
          DSA(IA,IN)=DSA(IA,IN)*FENS
          DSAS(IA,IN)=DSAS(IA,IN)*FENS
          IF(IA.EQ.NAA)THEN
            DSA(IA,IN)=2.*DSA(IA,IN)
            DSAS(IA,IN)=2.*DSAS(IA,IN)
          ENDIF
        enddo
        if(aton.ne.0.)then
          aton=aton/nto/(4.*pi)
          do ia=1,NAA
            dsa(ia,in)=dsa(ia,in)/aton
            dsas(ia,in)=dsas(ia,in)/aton
          enddo
        endif
        DO IH=1,NPHI
          DSHS(IH,IN)=SQRT(DSH(IH,IN))
          FSH=.25/((DY+DY)*DPHI*NTO)
          SUH(IN)=SUH(IN)+FSH*DSH(IH,IN)
          SUR(IN)=SUR(IN)+FSH*DSR(IH,IN)
          IF(IH.EQ.1.OR.IH.EQ.NPHI)FSH=FSH+FSH
          DSH(IH,IN)=DSH(IH,IN)*FSH
          DSHS(IH,IN)=DSHS(IH,IN)*FSH
          DSRS(IH,IN)=SQRT(DSR(IH,IN))
          DSR(IH,IN)=DSR(IH,IN)*FSH
          DSRS(IH,IN)=DSRS(IH,IN)*FSH
        ENDDO
      enddo
      WRITE(13,*)' '
      WRITE(13,*)'pt dn/p_T*dp_T'
      ANPTO=0.
      DO IT=1,NPT
        PT=DPT*(IT-.5)
        FPT=1E-6/(PT*DPT*NTO)
        WRITE(13,'(1X,F6.1,9(1X,E8.3,''_'',E8.3))')
     W    PT*1E3,(FPT*ANPT(IT,IN),FPT*SQRT(ANPT(IT,IN)),IN=1,NIIP)
        ANPTO=ANPTO+FPT*ANPT(IT,1)*PT*DPT*1E6
      ENDDO
      WRITE(13,*)'NET P ',ANPTO
      WRITE(13,*)' '
      IF(GETPX)THEN
        DO IN=0,NIIp
          DO IY=1,NYMX
            ANYI=ANY(IY,IN)
            IF(ANYI.NE.0.)THEN
              PXY(IY,IN)=PXY(IY,IN)/ANYI
              PXYS(IY,IN)=SQRT(PXYS(IY,IN)/ANYI)
              IF(ANYI.GE.2.)THEN
                PXYS(IY,IN)=SQRT((PXYS(IY,IN)**2-PXY(IY,IN)**2)
     P            /(ANYI-1.))
              ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDIF
      write(*,*)' '
      do ip=1,NET
*        te=ip*DET
        te=(ip-.5)*DET
        WRITE(*,'(1X,F3.2,2(1X,F4.0,''_'',F4.0)
     E    ,3(1X,F4.1,''_'',F3.1),3(1X,F4.0,''_'',F3.0))')
     w    te,(dse(ip,in),dses(ip,in),in=1,niip)
        WRITE(13,'(1x,F6.4,2(1X,F9.4,''_'',F8.4)
     E    ,3(1X,F8.4,''_'',F7.4))')
     w    te,(dse(ip,in),dses(ip,in),in=1,nii-4)
      enddo
      write(13,*)' '
      do ip=1,NET
*        te=ip*DET
        te=(ip-.5)*DET
        WRITE(13,'(1x,F6.3,3(1X,F8.4,''_'',F7.4)
     w    ,(1X,F8.3,''_'',F7.2))')
     w    te,(dse(ip,in),dses(ip,in),in=nii-3,nii)
      enddo
      write(13,*)' '
      write(13,*)'asys'
      do ip=1,NETc
        te=(ip-.5)*DETc
        WRITE(13,'(F5.3,1X,F6.2,''_'',F5.2)')
     r    te,anico(ip),anicos(ip)
      enddo
      IF(GETPX)THEN
        WRITE(13,*)' '
        WRITE(13,*)'y px'
*        WRITE(13,*)'y dN/dy'
        DO IY=1,NYMX
          YL=YMIN+(IY-.5)*DDY
          WRITE(13,'(1x,F5.3,9(1X,F6.1,''_'',F6.1))')
     W      YL,(1e3*PXY(IY,IN),1e3*PXYS(IY,IN),IN=0,NIIp)
*     W      YL,(ANY(IY,IN)/(NTO*DDY),SQRT(ANY(IY,IN))/(NTO*DDY),
*     R      IN=0,NIIP)
*          WRITE(13,'(1x,F5.3,9(1X,F6.4,''_'',F6.4))')
*     W      YL,(PXY(IY,IN),PXYS(IY,IN),IN=0,NIIp)
        ENDDO
      ENDIF
      write(*,*)' '
      write(13,*)' '
      WRITE(*,*)'polar angle'
      WRITE(13,*)'polar angle'
      write(*,*)' '
      write(13,*)' '
      do ia=1,naa
        an=daa*(ia-.5)*180./pi
        write(*,'(1x,f4.1,8(1x,f4.1,''_'',f3.1))')
     w    AN,(dsa(ia,in),dsas(ia,in),in=1,niip)
        write(13,'(1x,f5.1,8(1x,f6.3,''_'',f6.3))')
     w    AN,(dsa(ia,in),dsas(ia,in),in=1,niip)
      enddo
      write(*,*)' '
      write(13,*)' '
      WRITE(*,*)'lab azimuth'
      WRITE(13,*)'lab azimuth'
      DO IN=1,NII
        SUH(IN)=SUH(IN)/(NPHI-1)
        SUR(IN)=SUR(IN)/(NPHI-1)
      ENDDO
      do iH=1,nPHI
        PHI=dPHI*(iH-1)*180./pi
        DO IN=1,NIIp
          IF(SUH(IN).NE.0.)THEN
            DSH(IH,IN)=DSH(IH,IN)/SUH(IN)
            DSHS(IH,IN)=DSHS(IH,IN)/SUH(IN)
          ENDIF
        ENDDO
        write(*,'(1x,f4.1,8(1x,f3.1,''_'',f3.1))')
     w    PHI,(dsH(iH,in),dsHs(iH,in),in=1,niip)
        write(13,'(1x,f5.1,8(1x,f6.2,''-'',f6.2))')
     w    PHI,(dsH(iH,in),dsHs(iH,in),in=1,niip)
      enddo
      write(*,*)' '
      write(13,*)' '
      IF(PSZ.NE.0.)THEN
        RAT=2./PI*PST/PSZ
      ELSE
        RAT=-1.
      ENDIF
      WRITE(*,*)'rot azimuth  TH = ',TH*180./PI,'  RAT = ',RAT
      WRITE(13,*)'rot azimuth  TH = ',TH*180./PI,'  RAT = ',RAT
      do iH=1,nPHI
        PHI=dPHI*(iH-1)*180./pi
        DO IN=1,NIIp
          IF(SUR(IN).NE.0.)THEN
            DSR(IH,IN)=DSR(IH,IN)/SUR(IN)
            DSRS(IH,IN)=DSRS(IH,IN)/SUR(IN)
          ENDIF
        ENDDO
        write(*,'(1x,f4.1,8(1x,f3.1,''_'',f3.1))')
     w    PHI,(dsr(iH,in),dsrs(iH,in),in=1,niip)
        write(13,'(1x,f5.1,8(1x,f6.2,''_'',f6.2))')
     w    PHI,(dsr(iH,in),dsrs(iH,in),in=1,niip)
      enddo
      write(*,*)' '
      write(13,*)' '
      vxxi=0.
      vyyi=0.
      do i=1,NIIp
        IF(APCC(I).NE.0.)THEN
          EPCX(I)=EPCX(I)/APCC(I)
          EPCXS(I)=EPCXS(I)/APCC(I)-EPCX(I)**2
          EPCXS(I)=SQRT(EPCXS(I)/APCC(I))
          EPCY(I)=EPCY(I)/APCC(I)
          EPCYS(I)=EPCYS(I)/APCC(I)-EPCY(I)**2
          EPCYS(I)=SQRT(EPCYS(I)/APCC(I))
        ENDIF
        IF(APC(I).NE.0.)THEN
          EPCT(I)=EPCT(I)/APC(I)
          EPCTS(I)=EPCTS(I)/APC(I)-EPCT(I)**2
          EPCTS(I)=SQRT(EPCTS(I)/APC(I))
          EPCZ(I)=EPCZ(I)/APC(I)
          EPCZS(I)=EPCZS(I)/APC(I)-EPCZ(I)**2
          EPCZS(I)=SQRT(EPCZS(I)/APC(I))
          if(i.le.5.and.i.ne.2)then
            vxxi=vxxi+vxx(i)
            vyyi=vyyi+vyy(i)
          endif
          VXX(I)=VXX(I)/APC(I)
          VYY(I)=VYY(I)/APC(I)
          VZZ(I)=VZZ(I)/APC(I)
        ENDIF
        IF(APR(I).NE.0.)THEN
          EPRX(I)=EPRX(I)/APR(I)
          EPRXS(I)=EPRXS(I)/APR(I)-EPRX(I)**2
          EPRXS(I)=SQRT(EPRXS(I)/APR(I))
          EPRY(I)=EPRY(I)/APR(I)
          EPRYS(I)=EPRYS(I)/APR(I)-EPRY(I)**2
          EPRYS(I)=SQRT(EPRYS(I)/APR(I))
        ENDIF
        if(apt(i).ne.0.)then
          EPT(I)=EPT(I)/APT(I)
          EPTS(I)=EPTS(I)/APT(I)-EPT(I)**2
          EPTS(I)=SQRT(EPTS(I)/APT(I))
        endif
*        IF(I.LE.2)THEN
*          write(*,'(i3,3(2x,f6.4,a4,f6.4),2x,f6.4)')
*     w      i,epcz(i),' +- ',epczs(i)
*     r      ,epct(i),' +- ',epcts(i)
*        ELSE
*        ENDIF
      enddo
      DO I=1,NIIp
        write(*,'(i3,3(2x,f6.4),2x,f6.1)')
     w    i,VXX(I),VYY(I),VZZ(I),VTH(I)*180./PI
        write(13,'(1x,i3,3(2x,f6.4),2x,f6.1)')
     w    i,VXX(I),VYY(I),VZZ(I),VTH(I)*180./PI
      ENDDO
      write(*,*)'ryxi = ',vyyi/vxxi,' r21i = ',r21i
      write(*,*)'r31i = ',r31i,' th = ',th*180./pi
      write(*,*)' '
      write(13,*)'ryxi = ',vyyi/vxxi,' r21i = ',r21i
      write(13,*)'r31i = ',r31i,' th = ',th*180./pi
      write(13,*)' '
      DO I=1,NIIp
        write(*,'(i3,3(2x,f6.4,a4,f6.4),2(2x,f6.4))')
     w    i,epcz(i),' +- ',epczs(i)
     r    ,epct(i),' +- ',epcts(i)
     i    ,ept(i),' +- ',epts(i),EPCX(I),EPCY(I)   !,EPRX(I),EPRY(I)
        write(13,'(1x,i3,3(2x,f6.4,a4,f6.4),4(2x,f6.4))')
     w    i,epcz(i),' +- ',epczs(i)
     r    ,epct(i),' +- ',epcts(i)
     i    ,ept(i),' +- ',epts(i)  !,EPRX(I),EPRY(I)
     i    ,EPCX(I),EPCY(I)
      ENDDO
      WRITE(*,*)'D/P = ',APT(3)/APT(1)
      WRITE(*,*)'3HE/P = ',APT(4)/APT(1),'T/P = ',APT(5)/APT(1)
      ACH=0.
      DO I=1,NII
        APC(I)=APC(I)/NTO
        IF(ZPA(I).NE.0.)ACH=ACH+APC(I)
      ENDDO
      WRITE(*,*)'ERAT = ',ERAT
      WRITE(13,*)'ERAT = ',ERAT
      WRITE(*,*)' NMUL = ',APC(2),' CHMUL = ',ACH
      WRITE(13,*)' NMUL = ',APC(2),' CHMUL = ',ACH
C
      NC2=MAX(NC2,1)
      c4=c4/nc2
      c4s=max(c4s/nc2-c4*c4,0.)
      c4s=sqrt(c4s/nc2)
      C2=C2/NC2
      C2S=MAX(C2S/NC2-C2*C2,0.)
      C2S=SQRT(C2S/NC2)
      WRITE(*,*)'cos 2phi = ',C2,' +- ',C2S,'  cos 4phi = ',c4
     w  ,' +- ',c4s
C
      END


      SUBROUTINE CMS(EEL,PZL,EEC,PZC,AMM,YC)
      COMMON/RAP/YBEAM,YCM,PF,RENAN,GG1,BB1,GG2,BB2
C
      DF=EEL-PZL
      IF(DF.GT.0.)THEN
        YL=.5*LOG((EEL+PZL)/(EEL-PZL))
        YC=YL-YCM
        AMM=SQRT(EEL*EEL-PZL*PZL)
      ELSE
        YC=1E6
        AMM=0.
      ENDIF
      EEC=GG2*(EEL+BB2*PZL)
      PZC=GG2*(PZL+BB2*EEL)
C
      END


      SUBROUTINE TEST(IDP,PXP,PYP,PZP,EEP,*,IPART,IOV)
C  TESTS A QUASIPARTICLE AGAINST
C  A PLASTIC BALL DEFINITION OF PARTICIPANT
C  AND ACCEPTED PROTON/DEUTERON
      INCLUDE 'PARTID.'
      COMMON/RAP/YBEAM,YCM,PF,RENAN,GG1,BB1,GG2,BB2
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
        AG=(PF+DPF)/PCMR
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
        AG=(PF+DPF)/PCMR
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
C
      IIDP=ABS(IDP)
      IF(IIDP.GT.6.AND.IIDP.LT.13.or.idp.gt.15)RETURN
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
      TL=GG2*(EEP-BB2*PZP)-AM       !DEP term: Coulomb + isospin
      IF(TL.LE.0.)RETURN
C  NEGATIVE KINETIC ENERGY IN THE LAB
      TR=GG1*(EEP-BB1*PZP)-AM       !same
      IF(TR.LT.0.)RETURN
C  NEGATIVE KINETIC ENERGY IN THE ANTILAB
C
      IIZP=MAX(IZP,1)                    !neutrons treated as protons
      PZL=GG2*(PZP-BB2*EEP)
      PL=SQRT(PTK+PZL*PZL)
      VL=PL/(AM+TL)             !DEP to make kinematic velocity...
      THL=ATAN2(PT,PZL)
      IAPP=IAP
      TLL=TL*1E3
      TLLP=TLL
      CALL SIMDAT_CLUSTERS(IAP,IIZP,TLLP,VL,THL,TAKE)
      IF(.NOT.TAKE)RETURN
C
      jdp=idp
      IF(TL.LT.IAP*TLMIN)RETURN
      IF(IAPP.NE.IAP.OR.TLLP.NE.TLL)THEN
        TL=1E-3*TLLP
        IF(IAP.EQ.1)THEN
          AM=AM0
          IDP=SIGN(2-IiZP,IDP)
        ELSEIF(IAP.EQ.2)THEN
          AM=AMD
          IDP=SIGN(3,IDP)
        ELSEIF(IAP.EQ.3)THEN
          AM=AMTE
          IDP=SIGN(6-IiZP,IDP)
        ELSEIF(IAP.EQ.4)THEN
          AM=AM4E
          IDP=SIGN(6,IDP)
        ENDIF
        if(idp.le.0)write(*,*)'jdp,idp,iap,izp ',jdp,idp,iap,iizp
        EL=TL+AM
        PL=SQRT(EL*EL-AM*AM)
        PZL=PL*COS(THL)
        PT=PL*SIN(THL)
        YL=.5*LOG((EL+PZL)/(EL-PZL))
        CALL CMS(EL,PZL,EEP,PZP,AM,YC)
        PTT=SQRT(PXP*PXP+PYP*PYP)
        IF(PTT.NE.0.)THEN
          PXP=PT*PXP/PTT
          PYP=PT*PYP/PTT
        ENDIF
      ENDIF
      IF(ABS(YL-YPPR).LT.DYR.AND.PT.LT.IAP*DPT)RETURN
C
      IPART=IIZP
C
      END


      BLOCK DATA ZEES            !!! NEEDS TO BE MODIFIED IF ID'S CHANGED !!!
      INCLUDE 'PARTID.'
      DATA   ZPA/1.,0.,1.,2.,1.,2.               !charge
C                p  n  d 3He t 4He
     Z  ,2.,1.,0.,-1.,1. ,0., 1., 0., -1., 0./
C       D++ D+ D0 D- N*+ N*- pi+ pi0  pi-  gm
C
      DATA TIZO/.5,-.5,0.,.5,-.5,0.              !isospin
C                p  n  d 3He  t 4He
     T  ,1.5,.5,-.5,-1.5,.5,-.5,1.,0.,-1., 0./
C       D++ D+ D0 D- N*+ N*- pi+ pi0  pi-  gm
C
      DATA   BAR/1.,1.,2.,3.,3.,4.               !baryon number
C                p  n  d 3He t 4He
     Z  ,1.,1.,1.,1.,1. ,1., 0., 0., 0., 0./
C       D++ D+ D0 D- N*+ N*- pi+ pi0 pi- gm
C
      PARAMETER (B=.008,AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER (AMB=AM0-B)
      PARAMETER (BD=.002225,AMD=AM0+AM0-BD)
      PARAMETER (AMEC=.13957,AMEN=.13496,AME=(AMEC+AMEC+AMEN)/3.)
      PARAMETER (BT=.0086,AMT=AMP+AMN+AMN-BT)
      PARAMETER (B3E=.0080,AM3E=AMP+AMP+AMN-B3E)
      PARAMETER (AMTE=.5*(AMT+AM3E))
      PARAMETER (B4E=.0286,AM4E=4.*AM0-B4E)
      PARAMETER (AMV0=1.232)
      PARAMETER (AMX=1.440)
C
      DATA AMS/2*AM0,AMD,2*AMTE,AM4E,4*AMV0,2*AMX,3*AME,0./       !fixed mass
C              N's    D  T&3HE  4HE  DELTA   N*    PI's gm
C
      END

      subroutine bubu
      end
