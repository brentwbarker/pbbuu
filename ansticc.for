c     updated June 2009: added 1- and 2-particle RMS calculations as
c     alternative method of quantifying source size - BWB

      use class_ArrayList

      PARAMETER(TLAB=0.05)
C
      PARAMETER(BIM1=2.65) ! 2.59) !3.61) 2.65
      PARAMETER(BMAX=bim1*1.4142,NBI=1)
      PARAMETER(BMAXK=BMAX*BMAX,DBK=BMAXK/NBI)
C
      PARAMETER(NFI=1)
      CHARACTER(len=7) FNAME(NFI)
      DIMENSION NQU(NFI)
c      DATA NQU/NFI*800/  ! total number of test particles
c      DATA fname/'x11G1GV'/nqu/4000/ !40ar45sc dep soft no-comp-pro free
c      DATA fname/'x11L2FB'/nqu/4000/ !40ar45sc dep soft no-comp-pro !!overlapping initial state
c      DATA fname/'x118O18'/nqu/4000/ !40ar45sc dep soft no-comp-pro w gnuplot .SPA format !!overlapping
c      DATA fname/'x11WBVX'/nqu/4000/ !40ar45sc dep soft no-comp-pro
c      DATA fname/'x119BOK'/nqu/4000/ !40ar45sc dep soft comp-pro
c      DATA fname/'x11UH1U'/nqu/4000/ !40ar45sc dep soft no-comp-pro, NN CS 5x
C      DATA fname/'x11M2F6'/nqu/4000/ !40ar45sc dep soft no-comp-pro, NN CS 10x
c      DATA fname/'x11094F'/nqu/3200/ !86kr93nb 70 MeV
c      DATA fname/'x1185G2'/nqu/800/  !84kr93nb70
c      DATA fname/'x11AUPP'/nqu/3200/
c      DATA fname/'x119SHZ'/nqu/800/  !84kr93nb100
c      DATA fname/'x11DQRP'/nqu/3200/
c      DATA fname/'x126JCF'/nqu/800/  !84kr93nb120
c      DATA fname/'x11YPK3'/nqu/3200/
c      DATA fname/'x1199ZT'/nqu/3200/ !112sn112sn
c      DATA fname/'x11D0N9'/nqu/4000/ !112sn112sn dep soft no-comp-pro
c      DATA fname/'x11U21Y'/nqu/4000/ !112sn112sn dep soft comp-pro
c      DATA fname/'x11M67K'/nqu/4000/ !112sn112sn dep soft no-comp-pro, NN CS x/5
c      DATA fname/'x11YV7I'/nqu/4000/ !112sn112sn dep soft no-comp-pro, NN CS x/25
c      DATA fname/'x11LF76'/nqu/3200/ !112sn124sn
c      data fname/'x117XAH'/nqu/4000/
c      DATA fname/'x115W71'/nqu/3200/ !124sn112sn
c      DATA fname/'x11MVPJ'/nqu/3200/ !124sn124sn
      DATA fname/'x12OWMX'/nqu/3200/ !129xe197au dep soft no-comp-pro, 50 MeV
c      DATA fname/'x11RZ67'/nqu/3200/ !129xe197au dep soft comp-pro
c      data fname/'x11ML70'/nqu/800/  !arsc50 eta=0.6
c      DATA fname/'x1171KN'/nqu/3200/
c      data fname/'x11SEC4'/nqu/800/  !arsc150 free CS
c      data fname/'x11E7B3'/nqu/800/  !arsc100 free CS
c      data fname/'x12OVD9'/nqu/800/  !arsc150
c      DATA fname/'x11NDC4'/nqu/3200/
c      data fname/'x113AKB'/nqu/200/  !arsc100
c      data fname/'x11LVZ3'/nqu/400/
c      data fname/'x117ESB'/nqu/800/
c      data fname/'x11OPTN'/nqu/800/
c      data fname/'x11E1TZ'/nqu/1600/
c      data fname/'x11OBGA'/nqu/2400/
C
      INCLUDE 'PAR.INC'
C
      INCLUDE 'MOS.INC'
c
c     generic histogram
      logical ghist
      parameter(ghist=.false.) ! create histogram
      parameter(gmin1=-5.,gmin2=-5)   ! value of min bin
      parameter(gmax1=5.,gmax2=5.) ! value of max bin
      parameter(dgbin1=0.05,dgbin2=0.05) ! step size
      parameter(ngbin1=int((gmax1-gmin1)/dgbin1+0.5)) ! number of steps
      parameter(ngbin2=int((gmax2-gmin2)/dgbin2+0.5)) ! number of steps
      dimension gbin(ngbin1,ngbin2),gval1(ngbin1)
!      real :: gval2(ngbin2)

c     initialize emitted particle yield over space
      logical spachist
      parameter(spachist=.true.)
      parameter(shzmx=50.) ! absolute max of space
      parameter(dsh=1) ! step size
      parameter(nsh=int(shzmx/dsh+0.5)) ! half number of steps
      dimension shz(-nsh:nsh),sha(-nsh:nsh)
c
c     initialize emitted particle yield over time
      logical emithist
      parameter(emithist=.true.)
      parameter(ehtimx=280.) ! max of histogram
      parameter(deh=7) ! delta step
      parameter(neh=int(ehtimx/deh+0.5))       ! number of steps
      dimension ehtim(neh),ehy(neh)
      integer emit_tot   ! total number of emitted particles (within pt/m gate)
C
c     produce emitted particle in timesteps files
      logical emit
      parameter(emit=.false.)
      parameter(ntimo=26,ntimo1=ntimo+1)
      dimension timo(0:ntimo)
      dimension ktimo(0:ntimo)
      data timo/1E30,0.,5.,10.,15.,20.
     2     ,25.,30.,35.,40.,45.,50.,55.,60.,65.,70.,75.,80.,85.,90.,95.
     3     ,100.,120.,140.,170.,200.
     4     ,230./
      data ktimo/ntimo1*1/
      parameter(dtim=2.5) ! width of times to use for snapshots
c
c     variables to calculate standard deviation
      real x_mean
      real y_mean
      real z_mean
      real x_rms
      real y_rms
      real z_rms
      real x_sig, y_sig, z_sig !, sigg  !sigg is average of x,y,z sig

c     variables to calculate 2-particle rms - BWB 2009-06-08
      logical rc_rms  ! if true, write to file
      parameter(rc_rms=.false.)
      real rc_cnt     ! count num of pairs used
      real rc_x_rms   ! RMS of relative x
      real rc_y_rms
      real rc_z_rms
      real rc_r_rms   ! RMS of relative r
      parameter(rc_cut=15.0) ! RCIJ must be less than this
c
c     variables for emitted particle snapshots
      integer, parameter :: rct=2 ! how many source size intervals
c      real rcmn(rct),rcmx(rct) !source size to cut emitted particles
c      data rcmn/0./rcmx/6./
      real pttmn(rct) !total p_t/m gate
      data pttmn/0.0,0.4/
      real iwt(nsiz,rct)
      parameter(wtm=2.) ! minimum weight to plot emitted
      parameter(wwt=0.02) ! size of dots
c
      integer, parameter :: PCT=1 ! how many intervals to cut momentum/energy in
      PARAMETER(NSR=100) ! number of steps
      PARAMETER(DSR=50./NSR) ! delta of each step in fm
      DIMENSION S(NSR,PCT),SS(NSR,PCT),NEN(PCT)
      DIMENSION TIM(NSR,PCT),TIMS(NSR,PCT),td(nsr,PCT),zem(nsr,PCT)
     &     ,rem(nsr,PCT)
      dimension rhxij(nsr,PCT)
      DIMENSION BR(0:NSR)
      parameter(broff=0.0)  ! amount to shift binning to the right
                            ! (first bin is from BROFF to BROFF+DSR)

c     variables for time distributions per source size bin - BWB 2009-08-07
      LOGICAL bthist
      PARAMETER(bthist=.true.)
      PARAMETER(dbt=11) !width of each bin in fm/c
      PARAMETER(nbt=25) !num of bins
      DIMENSION btt(nbt) !center of each bin in fm/c
      PARAMETER(btx=dbt*nbt) !max time in fm/c
      REAL bav(0:nsr)           ! average time of particle in time bin
      REAL ber(0:nsr)           ! error of above (standard deviation)
      REAL btot(0:nsr)          ! total particles in bin
      REAL btim(0:Nsr,0:nbt)  ! actual histogram
C

      ! 2D rectangular scalar grid
      type :: grid2d
       real :: dx,dy      ! resolution
       integer :: nx,ny
       real :: xmin,ymin
       real, allocatable, dimension(:,:) :: val
      end type grid2d

      ! variables for transverse momentum/mass vs. (longitudinal) rapidity 2d histogram - BWB 2012-10-31
      logical,parameter  :: calcHistPtmy = .true.
      integer            :: fhistptmy_u  ! file unit
      real, parameter    :: histPtmy_ymin = -0.5
      real, parameter    :: histPtmy_ymax = 0.5
      real, parameter    :: histPtmy_pmin = 0.0
      real, parameter    :: histPtmy_pmax = 0.5
      integer, parameter :: histPtmy_ny = 40
      integer, parameter :: histPtmy_np = 40
      real               :: histPtmy_dy
     &                     ,histPtmy_dp
      integer, dimension(histPtmy_ny,histPtmy_np) :: histPtmy
      integer :: hist_iy,hist_ip

      ! variables for transverse momentum/mass vs. time 2d histogram - BWB 2012-12-09
      logical,parameter  :: calcHistPtmt = .true.
      integer            :: fhistptmt_u  ! file unit
      real, parameter    :: histPtmt_tmin = 0.d0
      real, parameter    :: histPtmt_tmax = 280.d0
      real, parameter    :: histPtmt_pmin = 0.d0
      real, parameter    :: histPtmt_pmax = 0.5d0
      integer, parameter :: histPtmt_nt = 40
      integer, parameter :: histPtmt_np = 40
      real               :: histPtmt_dt
     &                     ,histPtmt_dp
      integer, dimension(histPtmt_nt,histPtmt_np) :: histPtmt
      integer :: hist_it

      ! variables for r_cms vs. time 2d histogram - BWB 2012-12-09
      logical,parameter  :: calcHistRcmt = .true.
      integer            :: fhistRcmt_u  ! file unit
      real, parameter    :: histRcmt_tmin = 0.d0
      real, parameter    :: histRcmt_tmax = 280.d0
      real, parameter    :: histRcmt_rmin = -40.d0
      real, parameter    :: histRcmt_rmax = 40.d0
      integer, parameter :: histRcmt_nt = 40
      integer, parameter :: histRcmt_nr = 40
      real               :: histRcmt_dt
     &                     ,histRcmt_dr
      integer, dimension(histRcmt_nt,histRcmt_nr) :: histRcmt
      integer :: hist_ir

      ! variables for dN/d(ptm) vs. pT/m 1D histogram - BWB 2012-12-15
      logical,parameter  :: calcHistNptm = .true.
      integer            :: fhistNptm_u  ! file unit
      real, parameter    :: histNptm_pmin = 0.d0
      real, parameter    :: histNptm_pmax = 0.5d0
      integer, parameter :: histNptm_np = 40
      real               :: histNptm_dp
      integer, dimension(histNptm_np) :: histNptm
      integer :: hist_iNptm

      ! variables for sorting particles by time and tagging them - BWB 2013-01-10
      integer            :: sorttime_u ! file unit
      integer, parameter :: sorttime_nt = 40
      real, parameter    :: sorttime_tmin = 0.d0
      real, parameter    :: sorttime_tmax = 280.d0
      real               :: sorttime_dt
      integer, dimension(sorttime_nt,nsiz) :: sorttime_tag !tagged particle indices
      integer, dimension(sorttime_nt) :: sorttime_tot !total particles in each t bin
      integer :: sorttime_it
      ! also gate on region of source function they contribute to
      integer, allocatable, dimension(:) :: sorttime_ptag !< # times particle contributes
                                                          !! to source region
      type(iArrayList), dimension(sorttime_nt)
     & :: sorttime_srtid !< list of particle indices in each time bin [srtid = "S(r) t ID"]
      real, parameter :: sorttime_srn = 15.0  !< minimum of S(r) range to tag
      real, parameter :: sorttime_srx = 20.0 !< maximum of S(r) range to tag


      logical ulab ! true if mo/e gate is in lab frame
      logical ucms ! true if mo/e gate is in cms frame
      logical umom ! true if using mo gate
      logical uken ! true if using ke gate
      parameter(umom=.false.)
      parameter(ulab=.true.)
c
c     One can cut in momentum xor energy, in the lab xor CMS frame
      real PTOM(PCT),PTOMX(PCT) ! twice lab mo cut min, max in GeV/c
      real KMN(PCT),KMX(PCT)    ! KE cut min, max in GeV
c      DATA PTOM/0.0/PTOMX/10.0/ ! cut to twice detector resolution
c     cut in kinetic energy instead
c      DATA KMN/0.013/,KMX/0.140/ !sn+sn
c      DATA KMN/0.020/,KMX/0.140/ !ar+sc,kr+nb
      DATA KMN/0.01/,KMX/0.145/  !xe+au.
c      DATA KMN/0.00/,KMX/10.0/

c     Gate in "Ptot_cms" for G. Verde's exp. analysis limitations. Added BWB 2013-02-08.
c     Ptot_cms = \sqrt{p_1^2+p_2^2}, where p_1 and p_2 are in CMS.
c      real, parameter :: ptotcmsn = 0.150  ! min
      real, parameter :: ptotcmsn = 0.0  ! min
      real, parameter :: ptotcmsx = 1.e6   ! max. for no limit, set to 1.e6
      real :: ptotcms

      ! Gate in lab theta of ptot, \vec{ptot} = \vec{p_1}+\vec{p_2}, for G. Verde.
      ! (in degrees)
      real :: ptot_theta_lab_min = 0.0 !12.0, 32.0
      real :: ptot_theta_lab_max = 180.0 !27.0, 62.0
c
c     gate in transverse momentum over mass
      parameter(ptmin=0.2)   ! minimum p_t/m (single particle)
      real, parameter :: ptmax=100000.0
      parameter(npt=1)      ! number of pttmin gates(only last one makes
                             !+normal source size, emittance
      real pttmin(npt)       ! minimum total (pti+ptj)/2m
      data pttmin/0.0/
      real, parameter :: pttmax=10.d0
c      data pttmin/0.0,0.0266,0.0533,0.0799,
c     a            0.107,0.133,0.160,0.187,
c     b            0.213,0.240,0.266,0.293/
c
c     theta angular cut in degrees. One can gate in both CMS
c     +and lab frames differently
c      PARAMETER(angmn=0,angmx=180) ! gate in lab theta
      PARAMETER(angmn=12,angmx=62) ! xe129au197
c      PARAMETER(ANGMN=28.25,ANGMX=45.5) ! krnb70
c      PARAMETER(ANG=38.5,DANG=8.5) ! krnb100,120
c      PARAMETER(ANGMN=28.45,ANGMX=45.55) ! old 40ar45sc
c      parameter(angmn=27.0,angmx=74.0) ! 40ar45sc 50 MeV
c      parameter(angmn=23.0,angmx=67.5) ! 40ar45sc 100 MeV
c      parameter(ANGMN=21.0,angmx=65.5) ! 40ar45sc 150 MeV
c      parameter(angmn=7,angmx=58) ! sn124sn124
c      parameter(angmn=20,angmx=60) ! 4micha
c      parameter(angmn=30,angmx=47) !old krnb100,120
c       parameter(angmn=29,angmx=73) !krnb70
c       parameter(angmn=21,angmx=65) !krnb100,120
      real ang, dang
c      parameter(angcmn=0,angcmx=180) ! gate in CMS theta
      parameter(angcmn=70,angcmx=110) ! gate in CMS theta
      
      ! gate in single-particle time for source function [fm/c]
      real, parameter :: tmin = 0.0
      real, parameter :: tmax = 15000000.0
 
C
      DIMENSION RHOX(PCT),ANOX(PCT),ANIB(PCT)
C
      PARAMETER(AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN)) ! proton, neutron, average masses in GeV/c^2
      PARAMETER(B=.008) ! binding energy per nucleon in GeV/c^2
      PARAMETER(AMB=AM0-B)
      PARAMETER(EBEAM=TLAB+AMB) ! total lab frame energy per particle
C
      PARAMETER(AM0K=AM0*AM0)
C
      INTEGER(2) IDIC
      INTEGER(2) IRX(NSIZ),IRY(NSIZ),IRZ(NSIZ),ITC(NSIZ),IRHR(NSIZ)
C
      CHARACTER(len=11) FILNAM
      character(len=12) fcor2
      character(len=11)fver
      character(len=13) ftest
      character(len=19) femit
      character(len=5) fiinam
      character(len=2) fbinam
      character(len=11) fafo  !< info file for ansticc parameters
      character(len=11) feh   !< emitted particle yield over time histogram
      character(len=11) fbt   !< time distribution per source bin histograms
      character(len=11) fbo   !< average time per source bin, average delta-t
      character(len=18) fsh   !< emitted particle yield over space histogram
      character(len=11) frms  !< 2-particle RMS over different dependencies
                              !! like pttmin or rc_cut
      integer :: frms_u
      integer :: fbo_u
      integer :: fbt_u

      !loop variables
      integer :: cnt
C
      PI=4.*ATAN(1.)
C
      dang=(angmx-angmn)/2.
      ang=angmn+dang

c     convert to radians
      ANGR=ANG*PI/180.
      DANGR=DANG*PI/180.
      angcmnr=angcmn*PI/180.
      angcmxr=angcmx*PI/180.
      ptot_theta_lab_min = ptot_theta_lab_min*pi/180.
      ptot_theta_lab_max = ptot_theta_lab_max*pi/180.
C
      PBEAM=SQRT((EBEAM-AMB)*(EBEAM+AMB)) ! lab momentum per nucleon of projectile
      PTO=IA1*PBEAM                       ! total lab momentum
      ETO=IA1*EBEAM+IA2*AMB               ! total lab energy
      YCM=.5*ALOG((ETO+PTO)/(ETO-PTO))    ! CM rapidity
      BCM=PTO/ETO                         ! beta of center of mass
      GCM=1./SQRT(1.-BCM*BCM)             ! gamma
      BGCM=GCM*BCM
C
      BBM=PBEAM/EBEAM
      GBM=1./SQRT(1.-BBM*BBM)
      BGBM=GBM*BBM
C
      BR(0)=broff
      IF(DSR.LE.0.)THEN
        OPEN(15,FILE='INTS.',STATUS='UNKNOWN')
        DO IR=1,NSR
          READ(15,*)A
          BR(IR)=BR(IR-1)+A
        ENDDO
        CLOSE(15)
      ELSE
        DO IR=1,NSR
          BR(IR)=IR*DSR+broff
        ENDDO
      ENDIF
      RSMX=BR(NSR)
C
      DO IS=1,PCT
        NEN(IS)=0
        ANOX(IS)=0.
        ANIB(IS)=0.
        DO IR=1,NSR
          S(IR,IS)=0.
          tim(ir,is)=0.
          tims(ir,is)=0.
          td(ir,is)=0.
          zem(ir,is)=0.
          rem(ir,is)=0.
          rhxij(ir,is)=0.
        ENDDO
      ENDDO
C
c     zero out snapshot weights
      do j=1,RCT
         do i=1,NSIZ
            iwt(i,j)=0
         enddo
      enddo
c     these bins are centered around ehtim values
      do i=1,neh
         ehtim(i)=ehtimx/neh*i*1.0-0.5*ehtimx/neh
         ehy(i)=0
      enddo

c     zero out bthist variables
      DO ibt=1,nbt
         btt(ibt)=btx/nbt*ibt*1.0-0.5*btx/nbt
         DO isr=1,nsr
            btim(isr,ibt)=0.0
            bav(isr)=0
            ber(isr)=0
            btot(isr)=0
         ENDDO
      ENDDO
C
      uken=.true.
      if(umom)uken=.false.
      ucms=.true.
      if(ulab)ucms=.false.
c
c     convert kinetic energies to 2*momentum
      if(uken)then
         do i=1,pct
            ptom(i)=2.*sqrt(kmn(i)*kmn(i)+2.*amp*kmn(i))
            ptomx(i)=2.*sqrt(kmx(i)*kmx(i)+2.*amp*kmx(i))
         enddo
      endif
c
      KF=1
      NTO=0
 40   CONTINUE
      FILNAM=FNAME(KF)//'.COR'
      fcor2=fname(kf)//'.COR2'
      FTEST=FNAME(KF)//'.TEST1' ! source function
      FVER=FNAME(KF)//'.VER'
      fafo=fname(kf)//'.AFO'    ! file of inputs used
      feh=fname(kf)//'.EHH'     ! emitted particles histogram
      fbt=fname(kf)//'.BTH'     ! time dist per source bin histograms
      fbt_u=2
      fbo=fname(kf)//'.BBO'
      fbo_u=3
      frms=fname(kf)//'.RMS'    ! 2-particle RMS
      frms_u=1
      OPEN(11,FILE=FILNAM,FORM='UNFORMATTED',STATUS='OLD')

      IF(bthist)THEN
      ENDIF

      IF(rc_rms)THEN
         open(unit=frms_u,file=frms)
         write(frms_u,*)'# 2-particle statistics:'
         write(frms_u,*)'# p_T/m   x_rms   y_rms   z_rms'
      ENDIF
C
      KB=1
 45   CONTINUE
C
      NTO=NTO+NQU(KF)
C
      BIMK=DBK*(KB-.5)
      BIM=BIM1  !hard-coded for 1 impact parameter
C

      ! read in particles

      IEN=1  !IEN is number of particles
 50   CONTINUE
      READ(11,END=250)IDIC,IXXI(IEN),IYYI(IEN),IZZI(IEN)
     R  ,ITC(IEN),IRX(IEN),IRY(IEN),IRZ(IEN),IRHR(IEN)
c      READ(11,'(9(I7))')IDIC,IXXI(IEN),IYYI(IEN),IZZI(IEN)
c     R  ,ITC(IEN),IRX(IEN),IRY(IEN),IRZ(IEN),IRHR(IEN)
      IF(IDIC.EQ.-100)THEN
c         goto 250
         IF((KB/2)*2.NE.KB)THEN
            KB=KB+1
            GOTO 50
         ELSE
            GOTO 250
         ENDIF
      ENDIF
      IF(IDIC.NE.1)GOTO 50
      IPO(IEN)=IEN
      IEN=IEN+1
      GOTO 50
c
 250  CONTINUE

      !ien is incremented once too many above
      ien=ien-1



c     initialize generic histogram
      if(ghist)then
       open(37,file='x11LHF7_hi.PTMY',status='unknown')
!       ien1=ien-1
       gtot=0
       do i=1,ngbin1
        gbin(i,1)=(gmax1-gmin1)/ngbin1*i+gmin1*1.0
        gval1(i)=0
       enddo
      endif

      ! initialize histPtmy
      if(calcHistPtmy) then
       histPtmy=0
       histPtmy_dy = (histPtmy_ymax - histPtmy_ymin)/histPtmy_ny
       histPtmy_dp = (histPtmy_pmax - histPtmy_pmin)/histPtmy_np
       open(newunit=fhistPtmy_u,file=fname(kf)//'-histPtmy.ansout')
      endif

      ! initialize histPtmt
      if(calcHistPtmt) then
       histPtmt=0
       histPtmt_dt = (histPtmt_tmax - histPtmt_tmin)/histPtmt_nt
       histPtmt_dp = (histPtmt_pmax - histPtmt_pmin)/histPtmt_np
       open(newunit=fhistPtmt_u,file=fname(kf)//'-histPtmt.ansout')
      endif

      ! initialize histRcmt
      if(calcHistRcmt) then
       histRcmt=0
       histRcmt_dt = (histRcmt_tmax - histRcmt_tmin)/histRcmt_nt
       histRcmt_dr = (histRcmt_rmax - histRcmt_rmin)/histRcmt_nr
       open(newunit=fhistRcmt_u,file=fname(kf)//'-histRcmt.ansout')
      endif

      ! initialize histNptm
      if(calcHistNptm) then
       histNptm=0
       histNptm_dp = (histNptm_pmax - histNptm_pmin)/histNptm_np
       open(newunit=fhistNptm_u,file=fname(kf)//'-histNptm.ansout')
      endif

      ! initialize sorttime
      sorttime_tag=0
      sorttime_tot=0
      sorttime_dt=(sorttime_tmax - sorttime_tmin) / sorttime_nt
      open(newunit=sorttime_u,file=fname(kf)//'-sorttime.ansout')

      allocate(sorttime_ptag(ien))
      sorttime_ptag=0
      do ii=1,sorttime_nt
       sorttime_srtid(ii)=new_iArrayList()
      enddo

c     initialize emithist arrays
      emit_tot=0
 
      !loop over particles to get single-particle statistics
      do ip=1,ien
!         Ip=IPO(i)
         xi=irx(ip)*1e-2 !position in cms
         yi=iry(ip)*1e-2
         zi=irz(ip)*1e-2
         PXI=IXXI(Ip)*1E-3   !momentum in lab
         PYI=IYYI(IP)*1E-3
         PZI=IZZI(IP)*1E-3
         timi=itc(ip)*0.05 ! time of emission in fm/c
         PITK=PXI*PXI+PYI*PYI
         PIT=SQRT(PITK) ! transverse momentum magnitude
         ANGT=ATAN2(PIT,PZI)  ! azimuthal angle theta (lab)
         cangt=cos(angt)
         angtd=angt*180./PI
         PITOK=PITK+PZI*PZI
         pito=sqrt(pitok)
         EI=SQRT(AM0K+PITOK) ! total energy in lab
         ylab=atanh(pzi/ei)
         ptm=pit/amp
         CALL LOREN(0.,0.,-BGCM,GCM,PXI,PYI,PZI,EI
     L        ,PXCI,PYCI,PZCI,ECI)
         ycms=atanh(pzci/eci)
         PCI2=2.*SQRT(PITK+PZCI*PZCI)
         angtc=atan2(pit,pzci) ! azimuthal angle theta (cms)
         cangtc=cos(angtc)
         angtcd=angtc*180./PI
         if((angtc<angcmnr).or.(angtc>angcmxr)) cycle ! theta_cms filter
         if((angtd<angmn).or.(angtd>angmx)) cycle  ! theta_lab filter

         if(ulab) then ! if using e/mo gate in lab
          egate=ei-am0
          pgate=pito
         else          ! if using e/mo gate in cms
          egate=eci-am0
          pgate=0.5*pci2
         endif
         if (uken) then ! if gating on kinetic energy
          if ((egate<kmn(1)).or.(egate>kmx(1))) cycle
         else ! if gating on momentum
          if ((pgate<ptom(1)).or.(pgate>ptomx(1))) cycle
         endif

         if ((ptm<ptmin).or.(ptm>ptmax)) cycle

         ! histRcmt in-loop calc
         hist_it=ceiling((itc(ip)/20.-histRcmt_tmin)/histRcmt_dt)
         hist_ir=ceiling((irz(ip)*0.01 -histRcmt_rmin)/histRcmt_dr)
         if(     (1<=hist_it).and.(hist_it<=histRcmt_nt)
     &    .and.(1<=hist_ir).and.(hist_ir<=histRcmt_nr)) then
           histRcmt(hist_it,hist_ir)=histRcmt(hist_it,hist_ir)+1
         endif

         ! histPtmt in-loop calc
         hist_it=ceiling((itc(ip)/20.-histPtmt_tmin)/histPtmt_dt)
         hist_ip=ceiling((ptm -histPtmt_pmin)/histPtmt_dp)
         if(     (1<=hist_it).and.(hist_it<=histPtmt_nt)
     &    .and.(1<=hist_ip).and.(hist_ip<=histPtmt_np)) then
           histPtmt(hist_it,hist_ip)=histPtmt(hist_it,hist_ip)+1
         endif

         ! histNptm in-loop calc
         hist_iNptm=ceiling((ptm -histNptm_pmin)/histNptm_dp)
         if((1<=hist_iNptm).and.(hist_iNptm<=histNptm_np)) then
           histNptm(hist_iNptm)=histNptm(hist_iNptm)+1
         endif

         ! emithist in-loop calc
         if(emithist) then
          ibin=int(itc(ip)/20./ehtimx*neh+1.0)
          if(ibin.le.neh)then
           ehy(ibin)=ehy(ibin)+1
           emit_tot=emit_tot+1
          endif
         endif

!         if(itc(ip)/20..gt.100.)cycle
         if(ghist) write(37,*)ptm,ycms
c
         val=cangt
         ibin=int((val-gmin1)/(gmax1-gmin1)*ngbin1+1.0)
         if(ibin.le.ngbin1)then
            gval1(ibin)=gval1(ibin)+1
            gtot=gtot+1
         endif

       ! histPtmy in-loop calc
       hist_iy=ceiling((ycms-histPtmy_ymin)/histPtmy_dy)
       hist_ip=ceiling((ptm -histPtmy_pmin)/histPtmy_dp)
       if(     (1<=hist_iy).and.(hist_iy<=histPtmy_ny)
     &    .and.(1<=hist_ip).and.(hist_ip<=histPtmy_np)) then
        histPtmy(hist_iy,hist_ip)=histPtmy(hist_iy,hist_ip)+1
       endif

       ! sorttime in-loop calc
       sorttime_it = ceiling((timi-sorttime_tmin)/sorttime_dt)
       if((1<=sorttime_it).and.(sorttime_it<=sorttime_nt)) then
        sorttime_tot(sorttime_it)=sorttime_tot(sorttime_it)+1
        sorttime_tag(sorttime_it,sorttime_tot(sorttime_it))=ip
       endif

      enddo !i=1,ien

      close(37)
      if(ghist) then
       open(36,file='testhi.hist',status='unknown')
       do i=1,ngbin
        write(36,*)gbin1(i),gval1(i)/gtot
       enddo
       close(36)
      endif !if(ghist)

      ! histPtmy after-loop
      write(fhistPtmy_u,*)'# 2D histogram, pTransvers vs. y for emitted'
     & //' particles'
      write(fhistPtmy_u,*)'# y_cms   pT/m'
      do hist_iy=1,histPtmy_ny
       do hist_ip=1,histPtmy_np
        write(fhistPtmy_u,*)histPtmy_ymin+histPtmy_dy*(hist_iy-0.5)
     &                     ,histPtmy_pmin+histPtmy_dp*(hist_ip-0.5)
     &                     ,histPtmy(hist_iy,hist_ip)
!     &                      /nqu/histPtmy_dy/histPtmy_dp
       enddo
       write(fhistPtmy_u,*)
      enddo

c      ! histPtmt after-loop
      write(fhistPtmt_u,*)'# 2D histogram, pTransvers vs. t for emitted'
     & //' particles. x,y coords are the left edge of bin'
      write(fhistPtmt_u,*)'# time (fm/c)   pT/m (c)   N'
      do hist_it=1,histPtmt_nt
       do hist_ip=1,histPtmt_np
        write(fhistPtmt_u,*)histPtmt_tmin+histPtmt_dt*(hist_it-1)
     &                     ,histPtmt_pmin+histPtmt_dp*(hist_ip-1)
     &     ,histPtmt(hist_it,hist_ip)/(nqu*1.d0)/histPtmt_dt/histPtmt_dp
       enddo
       write(fhistPtmt_u,*)
      enddo
c
c     ! histRcmt after-loop
      write(fhistRcmt_u,*)'# 2D histogram, r_{cms} vs. t for emitted'
     & //' particles'
      write(fhistRcmt_u,*)'# time (fm/c)   r_{cms} (fm)'
      do hist_it=1,histRcmt_nt
       do hist_ir=1,histRcmt_nr
        write(fhistRcmt_u,*)histRcmt_tmin+histRcmt_dt*(hist_it-0.5)
     &                     ,histRcmt_rmin+histRcmt_dr*(hist_ir-0.5)
     &                ,histRcmt(hist_it,hist_ir)/histRcmt_dr/histRcmt_dt
       enddo
       write(fhistRcmt_u,*)
      enddo

c      ! histNptm after-loop
      write(fhistNptm_u,*)
     & '# 1D histogram, dN/(dpT/m) vs. pT/m for emitted particles'
      write(fhistNptm_u,*)'# pT/m (c)   N'
      do hist_iNptm=1,histNptm_np
       write(fhistNptm_u,*)histNptm_pmin+histNptm_dp*(hist_iNptm-0.5)
     &                    ,histNptm(hist_iNptm)/(nqu*1.d0)/histNptm_dp
      enddo

      ! sorttime after-loop
      write(sorttime_u,*)'# tagged particles, sorted by time'
      write(sorttime_u,*)'# coordinate system = cms'
      write(sorttime_u,*)'# time [fm/c]  x [fm]  y [fm]  z [fm]'
     & //'  px [GeV/c]  py [GeV/c]  pz [GeV/c]'
      write(sorttime_u,*)
      write(sorttime_u,*)

      ! loop over time bins
      do it=1,sorttime_nt
       write(sorttime_u,'(A,I3,A,F4.1,A,F4.1)')'# i=',it,' ,time ='
     &  ,sorttime_dt*(it-0.5d0)
     &                    +sorttime_tmin,' fm/c +/- ',0.5d0*sorttime_dt

       if(sorttime_tot(it)==0) then
        write(sorttime_u,'(3F7.2,5F8.3)')0.0
       endif
       ! loop over particles in each time bin
       do ip=1,sorttime_tot(it)
        ipp=sorttime_tag(it,ip)
        xi=irx(ipp)*1e-2 !position in cms
        yi=iry(ipp)*1e-2
        zi=irz(ipp)*1e-2
        PXI=IXXI(Ipp)*1E-3   !momentum in lab
        PYI=IYYI(IPp)*1E-3
        PZI=IZZI(IPp)*1E-3
        timi=itc(ipp)*0.05
        PITK=PXI*PXI+PYI*PYI
        PITOK=PITK+PZI*PZI
        EI=SQRT(AM0K+PITOK) ! total energy in lab
        CALL LOREN(0.,0.,-BGCM,GCM,PXI,PYI,PZI,EI
     L            ,PXCI,PYCI,PZCI,ECI)
        write(sorttime_u,'(3F7.2,5F8.3)')xi,yi,zi,pxci,pyci,pzci
     &                                  ,sqrt(pxci**2+pyci**2),timi
       enddo

       write(sorttime_u,*)
       write(sorttime_u,*)
      enddo  ! sorttime after-loop
c
c
!      if(emithist)then
c     make histogram of emitted particles over time
!         ien1=ien-1
!         do i=1,ien1
!            Ip=IPO(i)
!            PXI=IXXI(Ip)*1E-3
!            PYI=IYYI(IP)*1E-3
!            PZI=IZZI(IP)*1E-3
!            PITK=PXI*PXI+PYI*PYI
!            PIT=SQRT(PITK)
!            ANGT=ATAN2(PIT,PZI)
!            cangt=cos(angt)         
!            angtd=angt*180./PI
!            PITOK=PITK+PZI*PZI
!            EI=SQRT(AM0K+PITOK)
!            ylab=atanh(pzi/ei)
!            ptm=pit/amp
!            CALL LOREN(0.,0.,-BGCM,GCM,PXI,PYI,PZI,EI
!     L                 ,PXCI,PYCI,PZCI,ECI)
!            angtc=atan2(pit,pzci) ! azimuthal angle theta (cms)
!            if((angtc<angcmnr).or.(angtc>angcmxr)) cycle ! theta_cms filter
!            if((angtd<angmn).or.(angtd>angmx)) cycle  ! theta_lab filter
!
!         if(ulab) then ! if using e/mo gate in lab
!          egate=ei-am0
!          pgate=pito
!         else          ! if using e/mo gate in cms
!          egate=eci-am0
!          pgate=0.5*pci2
!         endif
!         if (uken) then ! if gating on kinetic energy
!          if ((egate<kmn(1)).or.(egate>kmx(1))) cycle
!         else ! if gating on momentum
!          if ((pgate<ptom(1)).or.(pgate>ptomx(1))) cycle
!         endif
!
!            if(ptm.gt.ptmin.AND.ptm.GT.pttmin(npt))then
!                          endif
!         enddo

      ! emithist after-loop
      if(emithist) then
         if(kf.eq.nfi)then
            open(34,file=feh,status='unknown')
            do i=1,neh
               write(34,*)ehtim(i),ehy(i)/(nqu*1.d0)/deh
     &                    ,sqrt(ehy(i))/(nqu*1.d0)/deh
c     write(*,*)ehtim(i),ehy(i)
            enddo
            write(34,*)
            write(34,*)
            write(34,*)'# total of above particles'
            write(34,*)emit_tot
            close(34)
         endif
      endif
c
c     spatial distribution of emitted particles, on z axis
      if(spachist)then
         do nti=1,ntimo
c     make filename
            write(fiinam,'(i5.5)')nint(timo(nti))
            k=1
            do ik=1,2
               if(fiinam(ik:ik).ne.'0')goto 378
               k=k+1
            enddo
 378        continue
            write(fbinam,'(i2.2)')nint(bim)
            kb=1
            if(fbinam(1:1).eq.'0')kb=2
            fsh=fname(1)//FBINAM(KB:2)//FIINAM(K:5)//'.SHH'

            open(35,file=fsh,status='unknown')
            do i=-nsh,nsh
               shz(i)=shzmx/nsh*i*1.0
               sha(i)=0
            enddo
            ien1=ien-1
            do i=1,ien1         ! for each particle
               if(abs(itc(i)/20.-timo(nti)).le.DTIM)then ! if in timeslice
                  ibin=int(irz(i)*0.01/shzmx*nsh+0.5)
                  if(abs(ibin).le.nsh)then
                     sha(ibin)=sha(ibin)+1
                  endif
               endif
            enddo
            do i=-nsh,nsh
               write(35,*)shz(i),sha(i)
            enddo
            close(35)
         enddo
      endif
C
c     calculate single-particle statistics
      x_mean=0
      y_mean=0
      z_mean=0
      x_rms=0
      y_rms=0
      z_rms=0
      do i=1,ien
         xi=irx(i)*1e-2
         yi=iry(i)*1e-2
         zi=irz(i)*1e-2
         x_mean=x_mean+xi
         y_mean=y_mean+yi
         z_mean=z_mean+zi
         x_rms=x_rms+xi*xi
         y_rms=y_rms+yi*yi
         z_rms=z_rms+zi*zi
      enddo
c     constant for calculating hwhm (half-width-half-max)
      cln4k = log(4.0)
      cln4 = sqrt(cln4k)

      x_mean=x_mean/ien
      x_mean2=x_mean*x_mean
      x_rms2=x_rms/ien
      x_rms=sqrt(x_rms2)
      x_sig2=x_rms2-x_mean2
      x_sig=sqrt(x_sig2)
      x_hwhm=cln4*x_sig
      y_mean=y_mean/ien
      y_mean2=y_mean*y_mean
      y_rms2=y_rms/ien
      y_rms=sqrt(y_rms2)
      y_sig2=y_rms2-y_mean2
      y_sig=sqrt(y_sig2)
      y_hwhm=cln4*y_sig
      z_mean=z_mean/ien
      z_mean2=z_mean*z_mean
      z_rms2=z_rms/ien
      z_rms=sqrt(z_rms2)
      z_sig2=z_rms2-z_mean2
      z_sig=sqrt(z_sig2)
      z_hwhm=cln4*z_sig

      write(*,*)'single particle statistics:'
      write(*,*)'x_mean,x_hwhm:',x_mean,x_hwhm
      write(*,*)'y_mean,y_hwhm:',y_mean,y_hwhm
      write(*,*)'z_mean,z_hwhm:',z_mean,z_hwhm

c     loop over multiple pttmin gates
      DO ipt=1,npt
      
c     initialize 2-particle statistics variables
      rc_cnt=0
      rc_x_rms=0
      rc_y_rms=0
      rc_z_rms=0

c     begin source calculation
      IEN=IEN-1
      WRITE(*,*)IEN
      IF(IEN.NE.0)THEN
       ! sort particles into 3D momentum bins
       CALL GIVAL
       CALL SORTI(IPO,IVAL,IEN)
       CALL FINDI(IPO,IVAL,IEN)
       ! for each momentum bin, see if it contributes to source function
       DO IIZ=-NLM,NLX
        DO IIY=-NT,NT
         DO IIX=-NT,NT
          IIMX=IMX(IIX,IIY,IIZ)
          IF(IIMX.GT.0)THEN
           IIMN=IMN(IIX,IIY,IIZ)
           DO 270 II=IIMN,IIMX
            IIP=IPO(II)
            PXI=IXXI(IIP)*1E-3
            PYI=IYYI(IIP)*1E-3
            PZI=IZZI(IIP)*1E-3
            PITK=PXI*PXI+PYI*PYI
            PIT=SQRT(PITK)
            ANGT=ATAN2(PIT,PZI)
            ptmi=pit/amp
            if((ptmi.le.ptmin).or.(ptmi.ge.ptmax))goto 270 ! cut on p_t/m
            IF(ABS(ANGR-ANGT).GT.DANGR)GOTO 270 ! gate on lab theta
            PITOK=PITK+PZI*PZI
            pito2=2*sqrt(pitok)
            EI=SQRT(AM0K+PITOK)
            CALL LOREN(0.,0.,-BGCM,GCM,PXI,PYI,PZI,EI
     L           ,PXCI,PYCI,PZCI,ECI)
            PCI2=2.*SQRT(PITK+PZCI*PZCI)
            pci=pci2*0.5
            angtc=atan2(pit,pzci)
            if(angtc.lt.angcmnr.or.angtc.gt.angcmxr)goto 270 ! gate on CMS theta
            ISB=0
            DO IS=1,PCT
             if(ulab)then
              IF(Pito2.GE.PTOM(IS).AND. !pito2 was PCI2
     I               Pito2.LE.PTOMX(IS))ISB=IS
             elseif(ucms)then
              if(pci2.ge.ptom(is).and.
     i               pci2.le.ptomx(is))isb=is
             endif
            ENDDO
            IF(ISB.NE.0)THEN
             ANOX(ISB)=ANOX(ISB)+1.
             RHOX(ISB)=RHOX(ISB)+IRHR(IIP)
            ENDIF
            TI=ITC(IIP)/20.
            XI=IRX(IIP)*1E-2
            YI=IRY(IIP)*1E-2
            ZI=IRZ(IIP)*1E-2
            rhxi=irhr(iip)*1e-3
            if((ti<tmin).or.(ti>tmax)) cycle
            DO 260 JJ=II+1,IIMX
             JJP=IPO(JJ)
             PXJ=IXXI(JJP)*1E-3
             PYJ=IYYI(JJP)*1E-3
             PZJ=IZZI(JJP)*1E-3
             PJTK=PXJ*PXJ+PYJ*PYJ
             PJT=SQRT(PJTK)
             pjtok=pjtk+pzj*pzj
             pjto2=2*sqrt(pjtok)
             ANGT=ATAN2(PJT,PZJ)
             ptmj=pjt/amp
             ptmij=sqrt((pxi+pxj)**2+(pyi+pyj)**2)/(2.0*amp)
             if(ptmij.le.pttmin(ipt).or.(ptmij.gt.pttmax))goto 260
             if((ptmj.le.ptmin).or.(ptmj.ge.ptmax))goto 260
             IF(ABS(ANGR-ANGT).GT.DANGR)GOTO 260 ! gate on lab theta
             TJ=ITC(JJP)/20.
             if((tj<tmin).or.(tj>tmax)) cycle
             PJTOK=PJTK+PZJ*PZJ
             pjto=sqrt(pjtok)
             EJ=SQRT(AM0K+PJTOK)
c            boost momentum from lab to reaction cms
             CALL LOREN(0.,0.,-BGCM,GCM,PXJ,PYJ,PZJ,EJ
     L            ,PXCJ,PYCJ,PZCJ,ECJ)
             angtc=atan2(pjt,pzcj)
             if(angtc.lt.angcmnr.or.angtc.gt.angcmxr)goto 260 ! gate on CMS theta
             PXIJ=PXJ+PXI
             PYIJ=PYJ+PYI
             pzij=pzj+pzi
             PZCIJ=PZCJ+PZCI
             PIJTK=PXIJ*PXIJ+PYIJ*PYIJ
             pijt=sqrt(pijtk)
             PCIJK=PIJTK+PZCIJ*PZCIJ
             PCIJ=SQRT(PCIJK)
             pcjtok=pxcj*pxcj+pycj*pycj+pzcj*pzcj
             pcjto2=2*sqrt(pcjtok)  ! 2*total CMS momentum of jth particle
             pij=sqrt(pijtk+pzij*pzij)

             pcj = sqrt(pxcj**2 + pycj**2 + pzcj**2)
             ptotcms=sqrt(pci**2 + pcj**2)
             ptot_theta_lab = atan2(pijt,pzij)
             if((ptotcms<ptotcmsn).or.(ptotcms>ptotcmsx))cycle
             if((ptot_theta_lab<ptot_theta_lab_min)
     &          .or.(ptot_theta_lab>ptot_theta_lab_max))cycle

             ISB=0
             DO IS=1,PCT
              if(ulab)then
               IF(pjto2.GE.PTOM(IS).AND.pjto2.LE.PTOMX(IS))ISB=IS !pjto2 was PCIJ
              elseif(ucms)then
               IF(pcjto2.GE.PTOM(IS).AND.pcjto2.LE.PTOMX(IS))ISB=IS
              endif
             ENDDO
             IF(ISB.NE.0)THEN
              ECIJ=ECI+ECJ
              AMIJ=SQRT(ECIJ*ECIJ-PCIJK)
              TJ=ITC(JJP)/20.
              TIJ=TJ-TI
              xj=irx(jjp)*1e-2
              yj=iry(jjp)*1e-2
              zj=irz(jjp)*1e-2
              rhxj=irhr(jjp)*1e-3
              XIJ=IRX(JJP)*1E-2-XI
              YIJ=IRY(JJP)*1E-2-YI
              ZIJ=IRZ(JJP)*1E-2-ZI
c             boost rel position from reaction CMS to 2-particle CMS
              CALL LOREN(-PXIJ/AMIJ,-PYIJ/AMIJ
     C             ,-PZCIJ/AMIJ,ECIJ/AMIJ,XIJ,YIJ
     L             ,ZIJ,TIJ,XCIJ,YCIJ,ZCIJ,TCIJ)
c
c             boost i,j momentum from reaction CMS to 2-particle CMS
              CALL LOREN(-PXIJ/AMIJ,-PYIJ/AMIJ
     C             ,-PZCIJ/AMIJ,ECIJ/AMIJ,PXCI,PYCI
     L             ,PZCI,ECI,PX2I,PY2I,PZ2I,E2I)
              CALL LOREN(-PXIJ/AMIJ,-PYIJ/AMIJ
     C             ,-PZCIJ/AMIJ,ECIJ/AMIJ,PXCJ,PYCJ
     L             ,PZCJ,ECJ,PX2J,PY2J,PZ2J,E2J)

!       if(0.5d0*(TI+TJ)/20.>100.d0)cycle 
c
c             relative momentum in 2-particle CMS
              p2ij=0.5*sqrt((px2i-px2j)**2+(py2i-py2j)**2
     +             +(pz2i-pz2j)**2)
              RCIJ=SQRT(XCIJ*XCIJ+YCIJ*YCIJ+ZCIJ*ZCIJ)
              IF(RCIJ.LT.RSMX)THEN !if pairing is within bin scope
c              output cor2 file
c               write(38,'(2(6(E13.4),F7.2),F8.4,E13.4,E13.4)')xi,yi,zi
c     w                ,pxi,pyi,pzci,ti
c     w                ,xj,yj,zj,pxj,pyj,pzcj,tj,rcij,p2ij,pcij

c              loop to determine weights for xz snapshot plot
               do cnt=1,rct
c                if(rcij.ge.rcmn(cnt)
c     i             .and.rcij.lt.rcmx(cnt))then !if within source selection
                if(ptmij.gt.pttmn(cnt))then ! total p_t/m gate
                 iwt(iip,cnt)=iwt(iip,cnt)+1 !increment weight
                 iwt(jjp,cnt)=iwt(jjp,cnt)+1 ! for both particles
                endif
               enddo

c     add to rms determination - BWB 2009-06-08
               IF(rcij.lt.rc_cut)THEN
                  rc_cnt=rc_cnt+1
                  rc_x_rms=rc_x_rms+xcij*xcij
                  rc_y_rms=rc_y_rms+ycij*ycij
                  rc_z_rms=rc_z_rms+zcij*zcij
               ENDIF

               ! tag particle if it contributes to desired source range TODO
               if((rcij>=sorttime_srn).and.(rcij<=sorttime_srx)) then
                sorttime_ptag(iip)=sorttime_ptag(iip)+1
                sorttime_ptag(jjp)=sorttime_ptag(jjp)+1
               endif
c     
               DO ISR=1,NSR
                IF(br(isr-1).LE.rcij.AND.rcij.LE.br(isr))THEN  ! changed to allow for BROFF
c                IF(BR(ISR).GT.RCIJ.AND.)THEN
                 S(ISR,ISB)=S(ISR,ISB)+1.
                 TIM(ISR,ISB)=TIM(ISR,ISB)+TI+TJ
                 TIMS(ISR,ISB)=TIMS(ISR,ISB)+TI*TI+TJ*TJ
                 td(isr,isb)=td(isr,isb)+abs(ti-tj)
                 zem(isr,isb)=zem(isr,isb)+zi+zj
                 rem(isr,isb)=rem(isr,isb)
     r                +sqrt((xi+xj)**2+(yi+yj)**2)
                 rhxij(isr,isb)=rhxij(isr,isb)+rhxi+rhxj

c     add to time dist/bin
                 itim=int((ti+tj)/2.0/btx*nbt+1.0)
                 btim(isr,itim)=btim(isr,itim)+1
                 bav(isr)=bav(isr)+(ti+tj)/2.0
                 ber(isr)=ber(isr)+(ti+tj)/2*(ti+tj)/2
                 btot(isr)=btot(isr)+1

                 GOTO 320
                ENDIF !br.le.rcij...
               ENDDO !isr
 320           CONTINUE
              ENDIF !RCIJ.LT.RSMX
              NEN(ISB)=NEN(ISB)+1
              ANIB(ISB)=ANIB(ISB)+BIM
             ENDIF !isb.ne.0
 260        CONTINUE
 270       CONTINUE
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDIF
C     
      KB=KB+1
      IF(KB.LE.NBI)GOTO 45
C
c      close(38)
      KF=KF+1
      IF(KF.LE.NFI)GOTO 40
      CLOSE(11)
C
      DO ISB=1,PCT
        RHOX(ISB)=RHOX(ISB)*1E-4/MAX(ANOX(ISB),1.)/.160
        NE=MAX(NEN(ISB),1)
        ANIB(ISB)=ANIB(ISB)/NE
        DO ISR=1,NSR
          SI=S(ISR,ISB)
          SIM=MAX(1.,SI+SI)
          TIM(ISR,ISB)=TIM(ISR,ISB)/SIM
          TIMS(ISR,ISB)=TIMS(ISR,ISB)/SIM
          TIMS(ISR,ISB)=SQRT(MAX(0.,TIMS(ISR,ISB)-TIM(ISR,ISB)**2)
     T      /SIM)
          td(isr,isb)=td(isr,isb)/max(1.,si)
          zem(isr,isb)=zem(isr,isb)/sim
          rem(isr,isb)=rem(isr,isb)/sim
          rhxij(isr,isb)=rhxij(isr,isb)/sim
          SS(ISR,ISB)=SQRT(S(ISR,ISB))
          FACC=4./3.*PI*(BR(ISR)**3-BR(ISR-1)**3)*NE
          SS(ISR,ISB)=SS(ISR,ISB)/FACC
          S(ISR,ISB)=S(ISR,ISB)/FACC
        ENDDO
      ENDDO
C
c     pair emittance BWB 2008-07-01 -- only good for 1 fname
      if(emit)then
        do nti=1,ntimo
c     make filename
          write(fiinam,'(i5.5)')nint(timo(nti))
          k=1
          do ik=1,2
            if(fiinam(ik:ik).ne.'0')goto 372
              k=k+1
          enddo
 372      continue
          write(fbinam,'(i2.2)')nint(bim)
          kb=1
          if(fbinam(1:1).eq.'0')kb=2
          femit=fname(1)//FBINAM(KB:2)//FIINAM(K:5)//'.EMIT'
          open(32,file=femit,status='unknown')
c     
          do 380 j=1,rct ! for each source bin
            if (j.eq.1)WRITE(32,*)'SET COLOR red'
            IF (j.EQ.2)WRITE(32,*)'SET COLOR blue'
            IF (j.EQ.3)WRITE(32,*)'SET COLOR magenta'
            do 375 i=1,ien ! for each particle
              if(iwt(i,j).ge.wtm)then ! if above weight threshold
                if(abs(itc(i)/20.-timo(nti)).le.DTIM)then ! if in timeslice
                 dotwt=(iwt(i,j)-wtm+1.)*wwt  ! size of dot
c                 if(j.eq.1)dotwt=dotwt*.5
                 if(dotwt.ge.0.1)then ! if within size resolution
                  write(32,*)'SET SYMBOL 9o SIZE',dotwt
                  write(32,*)IRX(i)*1E-2,' ',IRY(i)*1E-2,' '
     w              ,IRZ(i)*1E-2,' ',iwt(i,j)
                  write(32,*)'PLOT FILL'
                 endif
                endif
              endif
 375          continue
 380       continue
          close(32)
        enddo                    !times
      endif                     !if emit
c     
      open(30,file=ftest,status='unknown')
      DO ISR=1,nsr
c        WRITE(*,'(1X,F5.2,3(2X,E10.5,1X,''( '',E10.5,'' )''))')
c     W    .5*(BR(ISR)+BR(ISR-1)),(S(ISR,ISB),SS(ISR,ISB),ISB=1,PCT)
        WRITE(30,'(1X,F5.2,3(2X,E12.5,1X,''( '',E12.5,'' )''))')
     W    .5*(BR(ISR)+BR(ISR-1)),(S(ISR,ISB),SS(ISR,ISB),ISB=1,PCT)
      ENDDO
      write(30,*)
      write(30,*)
      WRITE(*,*)RHOX
      WRITE(30,*)RHOX
      WRITE(*,*)ANIB
      WRITE(30,*)ANIB
      open(31,file=fver,status='unknown')
      do isb=1,PCT
        do isr=1,nsr
          write(31,*)dble(br(isr)),dble(s(isr,isb)),dble(ss(isr,isb))
        enddo
        write(31,*)' '
      enddo
C
      DO ISB=1,PCT
        AA=0.
        BB=0.
        DO ISR=1,NSR
          FACC=BR(ISR)**3-BR(ISR-1)**3
          AA=AA+FACC*S(ISR,ISB)
          BB=BB+(FACC*SS(ISR,ISB))**2
        ENDDO
        AA=AA*4.*PI/3.
        BB=SQRT(BB)*4.*PI/3.
        WRITE(*,*)AA,BB
        WRITE(30,*)AA,BB
      ENDDO

      write(*,*)'3D integral, error up to 30 steps:'
      write(30,*)'3D integral, error up to 30 steps:'
      DO ISB=1,PCT
        AA=0.
        BB=0.
        DO ISR=1,30
          FACC=BR(ISR)**3-BR(ISR-1)**3
          AA=AA+FACC*S(ISR,ISB)
          BB=BB+(FACC*SS(ISR,ISB))**2
        ENDDO
        AA=AA*4.*PI/3.
        BB=SQRT(BB)*4.*PI/3.
        WRITE(*,*)'3dint,err',AA,BB
        WRITE(30,*)'3dint,err',AA,BB
      ENDDO
c
      DO ISR=1,nsr
c        WRITE(*,'(1X,F5.2,3(2X,F5.1,1X,''('',F4.1,'')'',1x,f4.0
c     r    ,1x,f5.0))')
c     W    .5*(BR(ISR)+BR(ISR-1)),(TIM(ISR,ISB),TIMS(ISR,ISB)
c     r    ,td(isr,isb),rem(isr,isb),ISB=1,PCT)
        WRITE(30,'(1X,F5.2,3(2X,F5.1,1X,''('',F4.1,'')'',1x,f4.0
     r    ,1x,f5.0))')
     W    .5*(BR(ISR)+BR(ISR-1)),(TIM(ISR,ISB),TIMS(ISR,ISB)
     r    ,td(isr,isb),rhxij(isr,isb)*100.,isb=1,PCT)  !rem(isr,isb),ISB=1,3)
      ENDDO

c     out-of-loop calc for time dist/source bin
      if(bthist)THEN
         OPEN(unit=fbo_u,file=fbo)
         WRITE(fbo_u,*)'# average time of pairs contributing to a source
     &        size bin'
         WRITE(fbo_u,*)'# source [fm]   t_av   t_av_err   tij_av   
     &        tij_av_err'
      DO isr=1,nsr
         bav(isr)=bav(isr)/btot(isr)
         ber(isr)=ber(isr)/btot(isr)-bav(isr)*bav(isr)
         ber(isr)=sqrt(ber(isr))
         WRITE(fbo_u,*)0.5*(br(isr)+br(isr-1)),bav(isr),ber(isr),
     &         1./(sqrt(1d0*btot(isr)))*bav(isr)
      ENDDO

c     write out time dist/source bin
           OPEN(unit=fbt_u,file=fbt)
           WRITE(fbt_u,'(a)')'# time distribution per source bin'
           WRITE(fbt_u,'(a)')'# time_bin   number   error'
           DO isr=1,nsr
              WRITE(fbt_u,'(a,f5.3)')'# br: ',.5*(BR(ISR)+BR(ISR-1))
              DO ibt=1,nbt
                 WRITE(fbt_u,*)btt(ibt),btim(isr,ibt)
              ENDDO
              WRITE(fbt_u,*)
              WRITE(fbt_u,*)
           ENDDO
           CLOSE(fbt_u)
        ENDIF
        
c
c     write info file of parameters of ansticc
      open(33,file=fafo,status='unknown')
      WRITE(33,*)'bim:',bim1
      write(33,*)'tlab:',tlab
      WRITE(33,*)'rcmn,rcmx:',rcmn,rcmx
      write(33,*)'ptom,ptomx:',ptom,ptomx
      write(33,*)'ang,dang:',ang,dang
      write(33,*)'dtim:',dtim
      write(33,*)'ptmin,ptmax=',ptmin,ptmax
      close(33)

c     calculate 2-particle statistics - BWB 2009-06-08
      rc_r_rms=rc_x_rms+rc_y_rms+rc_z_rms
      rc_r_rms=rc_r_rms/rc_cnt
      rc_r_rms=sqrt(rc_r_rms)
      rc_x_rms=rc_x_rms/rc_cnt
      rc_x_rms=sqrt(rc_x_rms)
      rc_y_rms=rc_y_rms/rc_cnt
      rc_y_rms=sqrt(rc_y_rms)
      rc_z_rms=rc_z_rms/rc_cnt
      rc_z_rms=sqrt(rc_z_rms)

      if(rc_rms)then
         write(frms_u,*)pttmin(ipt),rc_x_rms,rc_y_rms,rc_z_rms,rc_r_rms
      endif

      ENDDO  ! loop over ipt (p_T/m gates)

      if(rc_rms)close(frms_u)

      !sort tagged particles into time bins (sorttime)
      do ip = 1,ien
       if (sorttime_ptag(ip)>=1) then
        sorttime_it = ceiling((itc(ip)/20.0-sorttime_tmin)/sorttime_dt)
!        write(*,*)'time,index=',itc(ip)/20.0, sorttime_it
        if((sorttime_it>=1).and.(sorttime_it<=sorttime_nt)) then
         call sorttime_srtid(sorttime_it)%add(ip)
        endif
       endif
      enddo

!      ! write out some debugging output
!      do it=1,sorttime_nt
!       write(*,*)'it,particles=',it,sorttime_srtid(it)%size()
!       do ii=1,sorttime_srtid(it)%size()
!        write(*,*)sorttime_srtid(it)%get(ii)
!       enddo
!       write(*,*)
!       write(*,*)
!      enddo

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

c     Makes filename from base name, impact parameter, timesnapshot, 
c      and extension desired.
      subroutine mkfnam(tim,bim,fbase,ext,ffull)
      real tim,bim
      character(len=5)  ext, fiinam
      character(len=2)  fbinam
      character(len=16) ffull
      character(len=7)  fbase
!      character fnam*7
      integer k,ik,kb

      write(fiinam,'(i5.5)')nint(tim)
      k=1
      do ik=1,2
         if(fiinam(ik:ik).ne.'0')goto 393
         k=k+1
      enddo
 393  continue
      write(fbinam,'(i2.2)')nint(bim)
      kb=1
      if(fbinam(1:1).eq.'0')kb=2
      ffull=fbase//FBINAM(KB:2)//FIINAM(K:5)//ext
      end

      SUBROUTINE GIVAL
C
      INCLUDE 'MOS.INC'
C
      INCLUDE 'PAR.INC'
C
      DO I=1,IEN

        ! determine indices of momentum cells
        IX=NINT(IXXI(I)*1E-3/DPTL)
        IY=NINT(IYYI(I)*1E-3/DPTL)
        IZ=NINT(IZZI(I)*1E-3/DPTL)

        ! if indices are within total momentum region considered,
        IF(ABS(IX).LE.NT.AND.ABS(IY).LE.NT.AND.
     I    IZ.GE.-NLM.AND.IZ.LE.NLX)THEN
          IVAL(I)=IY*NTL1+IX*NLL1+IZ
        ELSE
          IVAL(I)=MAXIPO
        ENDIF
      ENDDO
C
      END


      SUBROUTINE SORTI(IPO,IVAL,NQ)
      DIMENSION IPO(*),IVAL(*)
C  IPO -  POINTERS, IVAL - VALUES
C  HEAPSORT ALGORITHM FROM NUMERICAL RECIPES
C  POINTERS ARE ARRANGED IN THE ASCENDING OREDER OF IVAL'S
C  IVAL'S ARE NOT MOVED
C
      L=NQ/2+1
      IR=NQ
 10   CONTINUE
      IF(L.GT.1)THEN
        L=L-1
        IPT=IPO(L)
        IVA=IVAL(IPT)
      ELSE
        IPT=IPO(IR)
        IVA=IVAL(IPT)
        IPO(IR)=IPO(1)
        IR=IR-1
        IF(IR.EQ.1)THEN
          IPO(1)=IPT
          RETURN
        ENDIF
      ENDIF
      I=L
      J=L+L
 20   IF(J.LE.IR)THEN
        IF(J.LT.IR)THEN
          IF(IVAL(IPO(J)).LT.IVAL(IPO(J+1)))J=J+1
        ENDIF
        IF(IVA.LT.IVAL(IPO(J)))THEN
          IPO(I)=IPO(J)
          I=J
          J=J+J
        ELSE
          J=IR+1
        ENDIF
        GOTO 20
      ENDIF
      IPO(I)=IPT
      GOTO 10
C
      END


      SUBROUTINE FINDI(IPO,IVAL,NQ)
C  PUTS VALUES INTO IMN AND IMX
      DIMENSION IPO(*),IVAL(*)
      INCLUDE 'MOS.INC'
C
      IQC=1
      DO 60 IY=-NT,NT
      IYN=IY*NTL1
      DO 60 IX=-NT,NT
      IYXN=IYN+IX*NLL1
      DO 60 IZ=-NLM,NLX
      IMN(IX,IY,IZ)=IQC
      IVXYZ=IYXN+IZ
 20   CONTINUE
      IF(IQC.GT.NQ)THEN
        IF(IMN(IX,IY,IZ).LE.NQ)THEN
          IMX(IX,IY,IZ)=NQ
        ELSE
          IMX(IX,IY,IZ)=-1
        ENDIF
      ELSEIF(IVAL(IPO(IQC)).GT.IVXYZ)THEN
*** EXTRA***
        IF(IQC.GT.IMN(IX,IY,IZ))THEN
          IMX(IX,IY,IZ)=IQC-1
        ELSE
          IMX(IX,IY,IZ)=-1
        ENDIF
      ELSE
        IQC=IQC+1
        GOTO 20
      ENDIF
 60   CONTINUE
C
      END
