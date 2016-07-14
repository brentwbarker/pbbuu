C*  General features of BEM (Boltzmann Equation Model)
C*
C*  This code solves a set of coupled Boltzmann equations for
C*  particles in energetic heavy-ion reactions.  The most
C*  up-to-date description of the code and physics within may be
C*  found in P.D., Nucl. Phys. A673, 375 (2000).  Earlier
C*  publications include P.D., G. F. Bertsch, Nucl. Phys. A533 ,
C*  712 (1991); P.D., Nucl. Phys. A545, 21c (1992); P.D., Qiubao
C*  Pan, Phys. Rev. C 46, 2002 (1992); Qiubao Pan, P.D., Phys. Rev.
C*  Lett. 70, 2062 (1993), P.D., Phys. Rev. C 51, 916 (1995),
C*  P.D., Phys. Rev. Lett. 81 (1998) 2438.
C*




C*  Instructions for BUU
C*
C*  Parameters for running need to be set in several places.
C*
C*  *I start with the discussion of the file NUCLS.
C*
C*  FNAME is up to 10-character parameter that starts output file
C*  names (the rest of the name is usually random).  Currently, I
C*  use isotope designation for projectile and target.
C*
C*  IA1 & IZ1 are mass & charge nos. of the projectile, and IA2 &
C*  IAZ2 those of the target.  It is possible to run the code with
C*  IA1=0 to get an assessment of some parameters for one nucleus
C*  only.
C*
C*  TLAB is lab energy in GeV/nucleon.  Most nos within the code
C*  are in GeV.
C*
C*  IFRAME selects the frame for the calculation.  IFRAME=0, selecting
C*  the system center of mass is generally the safest, reducing Lorentz
C*  effects.  Also various running analyses are tailored to this option.
C*  However, when attention is dedicated to the projectile or traget
C*  regions, then it may be worthwhile to use IFRAME=1 (projectile frame)
C*  or IFRAME=2 (target frame) options.
C*
C*  NBI is no of impact parameters in the run.  More on impact pars
C*  in the discussion of BUU258.FOR.
C*
C*  NI=0 yields soft EOS and NI=1 stiff EOS as discussed in PRC 51
C*  (1995) 716.
C*
C*  ASYI=.TRUE. enforces isospin asymmetry in the calculation of
C*  Pauli blocking.  Personally, I use normally ASYI=.FALSE. which
C*  speeds up the program.
C*
C*  NTRO=1 gives printout of entropy values in the .LOG file.  Use
C*  normally NTRO=0, as otherwise program slows down significantly.
C*  Note that this parameter does not affect the dynamics in any
C*  way but just switches on and off an analysis of that dynamics.
C*
C*  NODEP=.TRUE. switches off composite production.  Put it on or
C*  off depending on physical question.  The production slows down
C*  execution and to some degree enhances thermalization.
C*
C*  NCOR=1 gives data for correlation calculations in the .COR
C*  file.  Normally set NCOR=0.
C*
C*  SYMMY=.TRUE. dampens out statistical fluctuations by exploiting
C*  the symmetry of nuclear systems with rgard to the reflection
C*  within the reaction plane.  Personally, I use most often
C*  SYMMY=.FALSE. in order to speed up calculations.
C*
C*  FRCAS=.TRUE. forces on frozen cascade calculation (frozen
C*  refers to the handling of initial nucleons that move with same
C*  speed as initial nucleus until 1'st collision - this to prevent
C*  dissolution of the nucleus in absence of mean field).
C*  In the cascade calculation there is no Pauli pcple, no
C*  nuclear/Coulomb potential.
C*
C*  BOX=.TRUE. encloses system in a box.  This is for testing
C*  equilibrium properties.  Normally use BOX=.FALSE.
C*
C*  NOPAU=.TRUE. eliminates Pauli principle.  Normally
C*  NOPAU=.FALSE.
C*
C*  NPION=.TRUE. eliminates free pions.  Only deltas are produced
C*  or reabsorbed in interactions with nucleons.  After evolution
C*  has ended, deltas are decayed.  Normally use NPION=.FALSE.
C*
C*  WRPION=.TRUE. forces on wrong detailed balance relation, such
C*  as used in the past in the literature.  Normally use
C*  WRPION=.FALSE.
C*
C*  OLAST=.TRUE. eliminates delta/pion production.  Normally use
C*  OLAST=.FALSE.
C*
C*  NOPOT=.TRUE. eliminates nuclear/Coulomb potentials.  Normally
C*  NOPOT=.FALSE.
C*
C*  NOCOU=.TRUE. eliminates Coulomb/isospin asymmetric potentials.
C*  Normally NOCOU=.FALSE.
C*
C*  LMIX=.TRUE. forces interchange of particle positions w/o
C*  changing their momenta during system expansion.  This has an
C*  effect of killing collective motion.  Normally use LMIX=.FALSE.
C*
C*  FRAT is a parameter that renormalizes rates in the system.
C*  FRAT=2. makes collision cross sections twice as large as
C*  normal, while FRAT=.5 makes them half of normal value.
C*
C*
C*  *file BUU258.FOR
C*
C*  NTIMO & TIMO  Normally NTIMO=0.  Set a value larger than 1 and
C*  values for TIMO if you want TOPDRAWER data files for contour
C*  plots of density in the configuration/momentum.  TIMO are times
C*  in fm/c at which snapshots are taken.
C*
C*  BIM is an impact parameter.  Set a value and possibly loop
C*  control (IB) at the beginning of the main program.  Otherwise,
C*  comment out BIM=value statement.  Given some NBI impact
C*  parameters are spread evenly in (impact par)^2 to have even
C*  c.s. weighting for the parameters.
C*
C*  ISEED is a seed for random number generator.  In case of
C*  several runs with same parameters for larger statistics, you
C*  need to change each time starting values in ISEED=...  in unit
C*  RESET.  (There is only one ISEED= in the segment.)
C*
C*
C*  *file SIZE2
C*
C*  TRAN gives transverse dimension of computational region i.e.
C*  overall region is from -TRAN fm to +TRAN fm.  ALO gives
C*  longitudinal size.  Normally I use a larger ALO than TRAN and
C*  ALO larger than 20.
C*
C*  DLL/DWL give computational cell size.  Normally, I use DLs in the
C*  vicinity of 0.9, but the code was tested in the past to be
C*  stable with respect to variations of DL between 0.4 to 1.5.
C*  Above 2 GeV/u start reducing DLL compared to DWL following
C*  cm gamma factors.
C*
C*  NSIT at the bottom of the file is used in profile density
C*  printouts.  I guess it stands for no. of slices.  The reason
C*  for NSIT=1 set at this moment is that for NSIT=0 the program
C*  might collapse(?).  Patch-work level.
C*
C*
C*  *file TSTEP
C*
C*  DT is time step.  Generally  v*DT/min(DLL,DWL) should be small.
C*  Thus DT should decrease when speeds increase and may grow when
C*  speeds decrease.  Larger DT makes the code run faster.
C*  Remember that there are always Fermi speeds within the system.
C*  Normally, for DL=.9, I never exceed DT=0.5 and say for 2 GeV/u
C*  lab, I use DT=.15-.18.
C*
C*  NSTEP is number of time steps.  It should be chosen such as to
C*  make the changes within the system slow already at the
C*  completion of evolution.  On the other hand it should not be
C*  made too large so as to have large chunks of nuclear matter
C*  crossing the boundary of the computational region.  The are
C*  provisions within the code to make the results insensitive to
C*  the boundary crossing.  These provisions appear to work but
C*  double precautions don't hurt.  Reasonable values of NSTEP
C*  depend on energy and size of the system and somewhat on impact
C*  parameter.
C*
C*  LPRO=.TRUE.  gives density, velocity and possibly entropy
C*  printouts along and perpendicular to beam axis with possible
C*  x-tra slices in a file with extension .PRF  Normally
C*  LPRO=.FALSE.
C*
C*  LFORM=.TRUE.  gives fragment formation times in a file with
C*  extension .FRM   If I were to use it I'd d-check whether these
C*  still continue to be formation times.   Normally LFORM=.FALSE.
C*
C*
C*  *file NQUA1
C*
C*  NQU is no of test-particles per particle.  Normally I run with
C*  NQU=170 or 250.  This is equivalent to the number of parallel
C*  runs.  Too low number may cause excessive fluctuations.
C*  Execution time is slightly more than linear in NQU.
C*
C*
C*
C*  **Instructions for analysing BUU results
C*  An example of analysis program is ANSTI.FOR
C*
C*  Binary output file with extension .DAT contains id's and
C*  laboratory momenta of emitted particles.  There is no header,
C*  all information is in identical records with numbers written as
C*  INTEGER*2.  To read use:
C*
C*        INTEGER*2 IXI,IYI,IZI
C*        INTEGER*2 IDIC
C*   50   CONTINUE
C*        READ(11,END=250)IDIC,IXI,IYI,IZI
C*   ...
C*        PXI=IXI*1E-3
C*        PYI=IYI*1E-3
C*        PZI=IZI*1E-3
C*   ...  Analysis
C*        GOTO 50
C*   250  CONTINUE
C*
C*  In the above momenta P?I are in GeV/c.  Id's are as described
C*  in PARTIC15 or at the start of BUU258.FOR.  Output file with
C*  extension .COR has also times and system-c.m. coordinates of
C*  last collision,
C*
C*        INTEGER*2 IXI,IYI,IZI,ITC,IRX,IRY,IRZ
C*        INTEGER*2 IDIC
C*   50   CONTINUE
C*        READ(11,END=250)IDIC,IXI,IYI,IZI,ITC,IRX,IRY,IRZ
C*   ...
C*        PXI=IXI*1E-3
C*        PYI=IYI*1E-3
C*        PZI=IZI*1E-3
C*        TIM=ITC/20.
C*        XI=IRX*.01
C*        YI=IRY*.01
C*        ZI=IRZ*.01
C*   ...  Analysis
C*        GOTO 50
C*   250  CONTINUE
C*
C*  ***NOTE:  times/positions in system cm while momenta in lab***
C*  In the above TIM is in fm/c and I?I in fm.  As there are no
C*  headers, the analysis program may need to be provided with
C*  information on beam energy, nuclear masses, number of
C*  test-particles per particle, etc.  The present ANSTI.FOR prints
C*  out results to a file with extension .ANS.  The results
C*  include, in sequence, c.m. momentum distributions of different
C*  particles dN/dp^3 in the vicinity of c.m. 90 deg vs. c.m.
C*  energy (Fig. 10 of I, Fig. 22 of II), angular asymmetry
C*  coefficients for pions vs. c.m. energy (insert in Fig. 25 in
C*  II), mean px vs y (~Fig. 13 of II), distribution in spherical
C*  angle vs. polar angle (Fig. 5 of I), distribution at
C*  midrapidity in the azimuthal angle about the beam axis (Fig. 6
C*  of I), distribution at midrapidity in the azimuthal angle about
C*  the flow axis (Fig. 7 of I), eigenvalues (energies) of kinetic
C*  energy tensor (Eq. (30) and Fig. 12 of II) and flow angles for
C*  different particles (Fig. 15a of II), mean longitudinal
C*  energies, transverse energies (Fig. 9 of I), energies at 90 deg
C*  (Fig. 8 of I), mean energy in and out of the reaction plane for
C*  different particles.
C*
C*  I. P.D. & Pan, PRC 46 (92) 2002
C*  II.  P.D., PRC 51 (95) 916
C*
C*
C*  Few comments on output from BUU258.FOR:
C*
C*  Info from time steps includes no of elastic collisions, various
C*  formations and break-up's that actually took place and total
C*  attempted but partially blocked by Pauli pcple.
C*
C*  ***Note this line below may be for a different version than
C*  ***the one you have.
C*  Line with a1/z1/vz1/vx1/a2/z2/e2   gives mass, charge, velocity
C*  components and energy per nucleon in MeV (relative to nucleon
C*  mass) of what is identified at a given moment as projectile 1
C*  and target 2 remnants.  More info can be pulled out that is
C*  printed, and also definitions of remnants could be altered.
C*
C*  Line with tim E/N/P/3He/T gives time, energy per nucleon in GeV
C*  (should be constant), number of participant nucleons, protons,
C*  and produced deuterons, 3He and t.
C*
C*  Subsequent lines give no's of ptcles emitted into wide angles
C*  (i.e. with omission of spectator regions at high energy in a
C*  way as was done by Nagamiya and Plastic Ball), no's of
C*  different delta's and pions, and total baryon number within
C*  computational region.
C*
C*  Many other lines present were being added ad hoc and I usually
C*  only vaguely remember what they were.
C*
C*
C*  ***  Modification around 30-Oct-1996:                       ***
C*  Justification:  Simultaneous inclusion of few-body and two-body
C*  collisions overestimates, most likely, the interaction rates.
C*  Thus, in a complete few-body treatment, production of bound
C*  states should be accompanied by a modification of collisions
C*  populating continuum states.  The continuum states should be
C*  orthogonal to the bound states and, while the population of
C*  bound states enhances the rates, a~simultaneous modification of
C*  continuum states should act to offset the enhancement.
C*  However, the modification is not done and will not be done in
C*  practise and the problem should become more and more serious as
C*  density increases and it becomes more and more likely for a few
C*  particles to meet at one spot.  The practical effect of the
C*  excessive rates appears to be enhanced stopping.  Thus,
C*  Au + Au calculations w/o the few-body collisions, and thus an
C*  enhanced stopping, appear to agree, after cuts, with the FOPI
C*  data on ERAT cross sections, while calculations with the
C*  few-body collisions overestimate the stopping by about 20%
C*  (ERAT for a given reaction cross section).  Note that in either
C*  type of calculations, the matter expands primarly in transverse
C*  directions in b = 0 Au + Au collisions within a broad range of
C*  energies; in that sense the difference is quantitative not
C*  qualitative.  The figures in PRC51(95)716 showing density
C*  distributions, estimates in Sec. VII there, were done with
C*  composite production off and the author not realizing some
C*  sensitivity of the stopping to the composite production.
C*  To remedy the situation, given that composites produced early
C*  anyway get broken, the few-body collisions with the
C*  composite production have been suppressed for average
C*  densities larger than 95% of the normal density, rho > 0.95
C*  rho_0.
C*
      use bwbfunc
      INCLUDE 'PARTIC15'
C  THESES ARE PTCLE ID'S: 0 - EMPTY, 1 - PROTON, 2 - NEUTRON
C  3 - DEUTERON, NEGATIVE - WENT OUT OF THE COMPUTATIONAL AREA
C  4 - HELIUM 3, 5 - TRITON, 6 - HELIUM 4
C  7 - DELTA ++, 8 - DELTA +, 9 - DELTA 0, 10 - DELTA -
C  11 - N* +, 12 - N* 0
C  13 - PI +, 14 - PI 0, 15 - PI -, 16 - GAMMA
C  IEM KEEP TRACK OF EMPTY QUASIPARTICLES, FOR FILL UP WHEN
C  A DEUTERON BREAKS UP
C  ...  USES DATA FILES: GENSGAV.DAT,GENSGAW.DAT,DEB.DAT,GENFIN.DAT,
C                        RHOM.DAT,CALKSI.DAT,CSCALL.DAT,GENFIT.DAT,TEBB.DAT
C                        PHOTPN.DAT
C  ...  FILES FOR LINKING: BUU258.FOR,COLL16.FOR,LOC16.FOR,MYFI.FOR,MOV131.FOR,
c                          HIG2COM.FOR, CSPAK2.FOR
C  ...  INCLUDE FILES: NUCLS.,PARTIC15.,SIZE2.,TSTEP.,SPEC1.,NQUA1.,PARTID.,
C                      SIZEC2.,SURF.,PARTIT.,EOSET.,PARTIT1.
C
C  GENSGAV.DAT AND GENSGAW.DAT ARE PRODUCED BY GENSGAV.FOR
C  TEBB.DAT IS PRODUCED BY TEB.FOR, PHOTPN.DAT - BY PHOPN.FOR
C  CALKSI.DAT BY CALKSI.FOR
C  CSCAL.DAT BY CSCAL.FOR AND HAND MODIFIED TO CSCALL.DAT
C
      INCLUDE 'TSTEP'
      INCLUDE 'SIZE2'
      INCLUDE 'SIZEC2'
C
      COMMON/ENERGY/EKIN,EPOT,EPR,APR,ETOG,PARTI(NIDP),PARTIG(NIDP)
     E  ,ANPAR,ANPARG,ANPR(6),ANPRG(6)
     N  ,EKINT,EKINZ,ECINT,ECINZ
     N  ,EKINTG,EKINZG
     E  ,PPRX,PPRY,PPRZ,PPRZL,PGOX,PGOY,PGOZ,PGOZL
     R  ,GST,BGSTX,BGSTY,BGSTZ
     G  ,PRSKT0,PRSP0,PRSKL0,PRSXZ0
      COMMON/RAP/YBEAM,YCM,PF,RENAN,GG1,BB1,GG2,BB2,dmo
      DIMENSION PARTIT(NIDP)
      DATA PARTIT/NIDP*0./
*c
*c  for jorg
*      parameter(njorg=70)
*      common/cjorg/dejorg,anatem(njorg),anasuc(njorg)
*c
*      parameter(ncomx=50)
*      dimension proba(0:ncomx)
*      dimension probs(0:ncomx)
*c  end for Jorg
C
*      parameter(ntimo=0,ntimo1=ntimo+1)
*      parameter(ntimo=13,ntimo1=ntimo+1)
      parameter(ntimo=20,ntimo1=ntimo+1)
      dimension timo(0:ntimo)
      dimension ktimo(0:ntimo)
*      data timo/1E30,0.,1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.,13.
*     d  ,14.,15.,16.,17.,18.,19.,20.,21.,22.,23.,25.,27.,30.,33.,36./
*      data timo/1E30,0.,2.,4.,6.,8.,10.,12.,14.,16.,18.,20.,22.,25./
      data timo/1E30,0.,5.,10.,15.,20.
     d  ,25.,30.,35.,40.,45.,50.,60.,70.,80.,100.,120.,140.,170.,200.
     a  ,230./
      data ktimo/ntimo1*1/
C
C  DATA FOR INITIALIZATION FOLLOW
C
      PARAMETER (B=.008)
      PARAMETER (AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER (AM02=AM0+AM0)
      PARAMETER (BD=.002225,AMD=AM0+AM0-BD)
      PARAMETER (AMD2=AMD+AMD,AMD4=AMD2+AMD2)
      PARAMETER (AMDK=AMD*AMD)
      PARAMETER (AMB=AM0-B)
*      PARAMETER (EBEAM=TLAB+AMB)
      PARAMETER (AMBK=AMB*AMB)
      PARAMETER (AM0K=AM0*AM0)
C
      PARAMETER (AN0=.160)
      PARAMETER (H=.19733)
C
      PARAMETER (UIZO=0.097*2) !was 0.092, changed to match mov131 2010-06-02 BWB
      PARAMETER (FIZO=UIZO/AN0)
C
C
      PARAMETER (ALPHA=1./137.036)
C
C  FOR THE OUTPUT:
      CHARACTER FCHA,FDATE*9,FTIME*10
      CHARACTER FCHA4*4,FILNAM*20
      character fiinam*5,fbinam*2,fillam*23
      INTEGER*2 IDIC
      INTEGER*2 IXI,IYI,IZI
C
      LOGICAL LINI
      DATA LINI/.TRUE./
C
      COMMON/CLMAIN/KK
      COMMON/CPRI/PRI,TIM,IZCEN
      LOGICAL PRI
C
      COMMON/VELOC/B1X,B1Z,B2X,B2Z
C
      COMMON/CHARGE/R1,X1,Z1,R2,X2,Z2,ZCEN
      COMMON/RCHA/P1X,P1Z,G1,SG1,P2X,P2Z,G2,SG2
C
      COMMON/RMK/RRK(2,2)
C
      COMMON/COOLC/PFRHO,IPTCO,UPO1,DUPNO1
C
      COMMON/RHOPAR/RHOPAR
      DATA RHOPAB/0./
C
      DIMENSION DANDL(7:12),ANE(13:15),ANET(13:15)
      DATA ANET/3*0./
C
      DIMENSION ANPRT(6)
      DATA ANPRT/6*0./,IBF/0/
C
      PARAMETER(THIRD=1./3.)
C
      COMMON/BOXES/EKI(NIDP),EKIT(NIDP)
     B  ,EKIZ(NIDP),EKITC(NIDP),EKIZC(NIDP)
     O  ,ECVZ(NIDP),ECVT(NIDP),EISH(NIDP)
      DIMENSION PARI(NIDP)
c
      common/mixc/mmix
c
*     common/ctemp/temp
      COMMON/CSEED/ISEED
C
      logical isFcha4Set
      isFcha4Set=.false.
C
      INCLUDE 'SPEC1'
C  IMPACT PARAMETER
C
      IF(COU)THEN
        FCOUL=IZ1*IZ2*H*ALPHA
      ELSE
        FCOUL=0.
      ENDIF
C
      IAA1=MAX(IA1,1)
      IAA2=MAX(IA2,1)
      AA1=MAX(REAL(IA1),1E-4)
      AA2=MAX(REAL(IA2),1E-4)
C
      PI=4.*ATAN(1.)
C
      R1=(3.*AA1/(4.*PI*AN0))**(1./3.)
      R2=(3.*AA2/(4.*PI*AN0))**(1./3.)
C
      BMAX=R1+R2+DWL                !!!!!!!!!! 3/11/91
**      bmax=(r1+r2)*.3  !!!!!
      BMAXK=BMAX*BMAX
      DBK=BMAXK/NBI
C
**      DO 500 IB=1,NBI
      DO 500 IB=1,1
**    DO 500 IB=NBI-2,NBI
      IBF=IBF+1
      BIMK=DBK*(IB-.5)
      BIM=SQRT(BIMK)
      bim=0.                   !set hard-wired impact parameter
C
      IF(BOX)THEN
        VOLT=DWL*DWL*DLL*(NT+NT+1)**2*(NL+NL+1)
        WRITE(*,*)'VOLUME = ',VOLT
      ENDIF
C
      CALL RESET
C
*      WRITE(*,*)'GIVE IMPACT PARAMETER: '
*      READ(*,*)BIM
C
      WRITE(*,*)'A1 = ',IA1,' Z1 = ',IZ1
      WRITE(*,*)'A2 = ',IA2,' Z2 = ',IZ2
      WRITE(*,*)'TLAB = ',TLAB,' GEV  ',' B = ',BIM,' FM'
      WRITE(*,'()')
C
      IF(LPRO)THEN
*        OPEN(9,FILE=FNAME//'.PRE',STATUS='UNKNOWN')
        OPEN(10,FILE=FNAME//'.PRF',STATUS='UNKNOWN')
      ENDIF
      OPEN(11,FILE=FNAME//'.TOP',STATUS='UNKNOWN')
*      open(57,file='ipartc',status='old')
*      open(58,file='zipartc.',form='unformatted',status='unknown')
C
      NEM=NQEX
C  NUMBER OF QUASIPARTICLES IN THE STACK FOR PIONS AND N'S FROM D'S
C
      WRITE(11,'(A)')'//SET DEVICE TEKTRONIX SIDEWAYS'
      WRITE(11,'(A)')'//SET DEVICE EXCL SIDEWAYS'
C
      PFRHO=2.*PI*H*(3./(4.*PI*4.))**THIRD
      PF=PFRHO*AN0**THIRD
C
      R2ZA1=(IZ1+IZ1)/FLOAT(IAA1)
      R2ZA2=(IZ2+IZ2)/FLOAT(IAA2)
      R2NA1=2.-R2ZA1
      R2NA2=2.-R2ZA2
      PF1P=PF*R2ZA1**THIRD
      PF1N=PF*R2NA1**THIRD
      PF2P=PF*R2ZA2**THIRD
      PF2N=PF*R2NA2**THIRD
C
      IF(IA1.EQ.0)THEN
        EBEAM=AMB+.05
      ELSE
        EBEAM=AMB+TLAB
      ENDIF                !patch
      PBEAM=SQRT(EBEAM*EBEAM-AMBK)
      YBEAM=ALOG((EBEAM+PBEAM)/AMB)
C
      ECM=AA1*EBEAM+IA2*AMB
      PCM=AA1*PBEAM
      YCM=.5*ALOG((ECM+PCM)/(ECM-PCM))
C
      IF(IFRAME.EQ.1)THEN    !frame choice
        YCM=YBEAM
      ELSEIF(IFRAME.EQ.2)THEN
        YCM=0.
      ENDIF
C
C  NEXT FOR LORENTZ TRANSFORMATIONS
      YPR=YBEAM-YCM
      GG1=COSH(YPR)
      BB1=TANH(YPR)
      GG2=COSH(YCM)
      BB2=-TANH(YCM)                          !negative...
      WRITE(*,*)'ycm,ypr: ',ycm,' ',ypr       !bwb
C
C  COULOMB DONE NONRELATIVISTICALLY IN THE FRAME OF THE CALCULATION,
C  WITH ENERGIES IN THAT FRAME SUBSTITUTED FOR MASSES  (NOV 5, 2009)
C  NOTE: THERE IS NO WAY TO DO IT CLEANLY
      AMN1=AA1*AMB
      AMN2=IAA2*AMB
C
      E1T=AMN1*GG1
      P1T=AMN1*GG1*BB1
      E2T=AMN2*GG2
      P2T=AMN2*GG2*BB2                         !negative...
      AE1=.5*(AMN1+E1T)
      AE2=.5*(AMN2+E2T)
      AMRE=AE1*AE2/(AE1+AE2)  !gives better ultrarelativistic asymptotics in tcm
      PRE=.5*(P1T-P2T)      !decision to alter relative momentum but not total
      TCM=.5*PRE*PRE/AMRE
C
      R=(R1+R2)/MIN(GG1,GG2)+MAX(DWL,DLL)*ANX !not much fiddling with gamma-factors
      IF(FCOUL.EQ.0.)THEN        !no Coulomb
        BIN=BIM               !transverse separation
        DPCOUZ=0.             !shift in longitudinal momentum due to Coulomb
        PCOUX=0.          !transverse momentum developed due to Coulomb
        AG=R*R-BIN*BIN
        IF(AG.LT.0.)GOTO 452   !fast exit: nuclei missing each other
        Z12=SQRT(AG)
        GOTO 91
      ENDIF
C
      RMIN0=FCOUL/TCM             !closest approach in head-on Coulomb
      THSCH=ATAN2(RMIN0,BIM+BIM)
      THSC=THSCH+THSCH      !Coulomb scattering angle relative to straight on
      THMIN=.5*(THSC+PI) !again measured relative to straight on
C
      SQR=SQRT(1.+((BIM+BIM)/RMIN0)**2)
      RMIN=.5*RMIN0*(1.+SQR)  !closest approach at finite impact par
      IF(RMIN.GE.R)GOTO 452   !again, missing each other
C
      COPHI=(1.+2.*BIM*BIM/(R*RMIN0))/SQR
      PHI=ACOS(COPHI)    !angle relative to the bisector of the trajectory
      THCOU=THMIN+PHI    !angle for r-separation on Coulomb trajectory vs beam
      SITCOU=SIN(THCOU)
      COTCOU=COS(THCOU)  !should be greater than pi/2, i.e. cos negative
C
      BIN=SITCOU*R      !new transverse separation
      Z12=ABS(COTCOU*R)    !new separation in longitudinal direction
C
      PCOUK=(AMRE+AMRE)*(TCM-FCOUL/R)
      PCOU=SQRT(MAX(PCOUK,0.))    !local momentum within Coulomb
      THCOH=.5*THCOU
      ANGP=THCOH-ACOS(PRE/PCOU*COS(THCOH))
      PCOUX=PCOU*SIN(ANGP)
      PCOUZ=PCOU*COS(ANGP)
      DPCOUZ=PCOUZ-PRE
C
 91   CONTINUE
      P1TZ=P1T+DPCOUZ
      P1TX=PCOUX
      P2TZ=P2T-DPCOUZ
      P2TX=-PCOUX       !total momenta have been changed due to Coulomb
C
      IF(IA1.NE.0)THEN       !conversion to per nucleon
        P1Z=P1TZ/IAA1
        P1X=P1TX/IAA1
      ELSE
        P1Z=0.
        P1X=0.
      ENDIF
      IF(IA2.NE.0)THEN
        P2Z=P2TZ/IAA2
        P2X=P2TX/IAA2
      ELSE
        P2Z=0.
        P2X=0.
      ENDIF
C
      P1K=P1Z*P1Z+P1X*P1X
      P2K=P2Z*P2Z+P2X*P2X
      EB1=SQRT(AMBK+P1K)
      EB2=SQRT(AMBK+P2K)
C
      B1Z=P1Z/EB1
      B1X=P1X/EB1
      B2Z=P2Z/EB2
      B2X=P2X/EB2
      write(*,*)'b1 ',b1x,0.,b1z
      write(*,*)'b2 ',b2x,0.,b2z
C
      G1=EB1/AMB
      G2=EB2/AMB
C
      SG1=1./G1
      SG2=1./G2
C
      IF(BOX.OR.IA1.EQ.0)THEN
        Z1=0.
        Z2=0.
        X1=0.
        X2=0.
        IZCEN=0
      ELSE
C  ADJUSTMENT SO THAT THEY HIT THE BOUNDARY AT THE SAME TIME
        FRA1=R1/(R1+R2)
        FRA2=1.-FRA1
        ND=4
        ZCEMIN=-NL*DLL+ND*DLL+SG1*R1+FRA1*Z12  !min position of system center
        ZCEMAX=NL*DLL-ND*DLL-SG2*R2-FRA2*Z12   !max position of system center
        IF(ZCEMIN.GT.ZCEMAX)THEN
          ZCEN=.5*(ZCEMIN+ZCEMAX)     !if too little space, squeeze in-between
        ELSE
          Z1F=NL*DLL-ND*DLL-R1     !position where projectile touches front
          Z2F=-(NL*DLL-ND*DLL-R2)    !position where target touches back
          TIMF=(Z1F-Z2F+Z12)/(BB1-BB2)  !time to hit boundaries simultaneously
          ZCEN=.5*(Z1F+Z2F-(BB1+BB2)*TIMF)  !position for proj/targ hit at once
          IF(ZCEN.LT.ZCEMIN)THEN
            ZCEN=ZCEMIN               !if too close to boundaries, reset
          ELSEIF(ZCEN.GT.ZCEMAX)THEN
            ZCEN=ZCEMAX
          ENDIF
        ENDIF
C
**        zcen=0.
        IZCEN=NINT(ZCEN/DLL)
C
        Z1=-Z12*FRA1+ZCEN
        Z2=Z12*FRA2+ZCEN
        X1=BIN*FRA1
        X2=X1-BIN
      ENDIF
      tcross=(z2-z1)/max((b1z-b2z),1e-6)
      write(*,*)'time f/centers to cross: ',tcross
      txtra1=sg1*sqrt(max(0.,r1*r1-(r2-bim)**2))/max(b1z,1e-6)
      txtra2=sg2*sqrt(max(0.,r2*r2-(r1-bim)**2))
     t  /max((-b2z),1e-6)
      txtra=max(txtra1,txtra2)
      txtra1l=sg1*sqrt(max(0.,r1*r1-max(r2-bim,0.)**2))/max(b1z,1e-6)
      txtra2l=sg2*sqrt(max(0.,r2*r2-max(r1-bim,0.)**2))
     t  /max((-b2z),1e-6)
      txtral=max(txtra1l,txtra2l)
      write(*,*)'time f/spectators to clear: ',tcross+txtra
      write(*,*)'longer time f/spectators to clear: ',tcross+txtral
      write(*,*)'centers z1,z2 = ',z1,z2
C
      DO I=1,2
        DO J=1,2
          RRK(I,J)=0.
        ENDDO
      ENDDO
C
      DO I=1,IZ1*NQU
        CALL GIVE(PX(I),PY(I),PZ(I),PF1P,P1X,P1Z,G1)
        ID(I)=1
        AMV(I)=AM0
      ENDDO
C  PROJECTILE PROTONS
C
      DO I=IZ1*NQU+1,IA1*NQU
        CALL GIVE(PX(I),PY(I),PZ(I),PF1N,P1X,P1Z,G1)
        ID(I)=2
        AMV(I)=AM0
      ENDDO
C  PROJECTILE NEUTRONS
C
      XMA=0.
      ZMI=1E30
      DO 100 I=1,IA1*NQU
*      IF(NOCOU)THEN
*        CALL GIVE(XX(I),YY(I),ZZ(I),R1,X1,Z1,SG1)
*      ELSE
        IDP=ID(I)
        CALL GIVER(XX(I),YY(I),ZZ(I),IDP,1)     !no Lorentz now
*      ENDIF
      NCS(I)=0
      IPTI(I)=1
      XMA=MAX(XMA,XX(I))
      ZMI=MIN(ZMI,ZZ(I))
 100  CONTINUE
      WRITE(*,*)'XMA = ',XMA,'  ZMI = ',ZMI
C  PHASE SPACE COORDINATES OF PROJECTILE NUCLEONS
C
C  FIRST STAGE OF PROJECTILE DONE
C  SOME GENERAL STUFF DONE NEXT
C
      DO I=1,NQ
        IPO(I)=I
        ITFO(I)=-100
        IRFO(I)=-100
      ENDDO
C
      DO I=IA1*NQU+1,NQI
        ID(I)=0            !switched off target
      ENDDO
C
      DO I=NQI+1,NQ
        ID(I)=0
        IEM(I-NQI)=I       !pointer to empty particles
      ENDDO
C
      IF(POT)THEN
        UPO1=UPO(1.)
        IF(LMO)THEN
          DUPNO1=DUPNOF(1.)
          write(*,*)'dupno = ',dupno1
          DUPNO1=DUPNO1+UPO1
          write(*,*)'dupno = ',dupno1
        ENDIF
      ELSE
        UPO1=0.
      ENDIF
      IF(IA1.EQ.0)GOTO 77
C
      APR0=0.
      DO I=1,IA1*NQU
        IDP=ID(I)
        APR0=APR0+BAR(IDP)
        PTK=PX(I)**2+PY(I)**2
        PZK=PZ(I)**2
        PK=PTK+PZK
        IF(POT.AND.LMO)THEN
          CALL PTNO(B1X,0.,B1Z,1.,DUPNO1
     C      ,AMV(I),BAR(IDP),PX(I),PY(I),PZ(I),EE(I)
     A      ,PC,EC,VXI,VYI,VZI,2)
        ELSE
          EE(I)=SQRT(PK+(AMV(I)+UPO1)**2)
        ENDIF
        MDE(I)=0                       !energy shift in Coulomb patching
        mdep(i)=0
        IF(LCOR)ITCR(I)=-1
        if(pent)then
          isr(i)=0
          isrx(i)=0
        endif
        igx(i)=b1x*g1*1e3
        igy(i)=0.
        igz(i)=b1z*g1*1e3
      ENDDO
      APR0=APR0/NQU
      APR01=APR0
      APR=APR0
C
C  COOLING OF THE PROJECTILE
      IPTCO=1    !needed by MOVEX and COOL
      CALL MOVEX(0.)
*      CALL GIVAL                         !all of these moved to MOVEX now
*      CALL SORTI(IPO,IVAL,NQ)
*      CALL FINDI(IPO,IVAL,NQ)
      CALL FINDU
!*Jorg!!
      CALL COOL
c
cc f/deformed collisions
c      isee=iseed
c      th1=.5*pi*getRan()
c      th2=th1*getRan()
c      fi1=pi*getRan()
c      fi2=pi*(-1.+2.*getRan())
c      th1=.5*pi
c      th2=th1
c      fi1=0.
c      fi2=th1  ! 0.
c      write(*,*)'agls iseed/th1/th2 ',isee,th1,th2
c      write(*,*)'fi1/fi2 ',fi1,fi2
cc
c      ct1=cos(th1)
c      st1=sin(th1)
c      ct2=cos(th2)
c      st2=sin(th2)
c      cf1=cos(fi1)
c      sf1=sin(fi1)
c      cf2=cos(fi2)
c      sf2=sin(fi2)
cc
c      xc=0.
c      yc=0.
c      zc=0.
c      do i=1,IA1*NQU+1
c        xc=xc+xx(i)
c        yc=yc+yy(i)
c        zc=zc+zz(i)
c      enddo
c      xc=xc/(nqu*ia1)
c      yc=yc/(nqu*ia1)
c      zc=zc/(nqu*ia1)
c      delta=0.27
c      ala=((1.+delta*4./3.)/(1.-delta*2./3.))**(1./6.)
c      pala=1./ala
c      alak=ala*ala
c      do i=1,IA1*NQU
c        dx=(xx(i)-xc)*pala
c        dy=(yy(i)-yc)*pala
c        dz=(zz(i)-zc)/sg1*alak
c        dxa=dx*ct1+dz*st1
c        dz=-dx*st1+dz*ct1
c        dx=dxa*cf1+dy*sf1
c        dy=-dxa*sf1+dy*cf1
c        xx(i)=xc+dx
c        yy(i)=yc+dy
c        zz(i)=zc+dz*sg1
c*        xx(i)=xc+(xx(i)-xc)*pala
c*        yy(i)=yc+(yy(i)-yc)*pala
c*        zz(i)=zc+(zz(i)-zc)*alak
c      enddo
cc end f/deformed collisions
C
C  SWITCHING OFF
      DO I=1,IA1*NQU
        ID(I)=-ID(I)                    !switched off
      ENDDO
C
 77   CONTINUE
C  NOW TARGET
C
C  PHASE SPACE COORDINATES OF TARGET NUCLEONS
C
      DO I=IA1*NQU+1,(IA1+IZ2)*NQU
        CALL GIVE(PX(I),PY(I),PZ(I),PF2P,P2X,P2Z,G2)
        AMV(I)=AM0
        ID(I)=1
      ENDDO
C  TARGET PROTONS
C
      DO I=(IA1+IZ2)*NQU+1,NQI
        CALL GIVE(PX(I),PY(I),PZ(I),PF2N,P2X,P2Z,G2)
        AMV(I)=AM0
        ID(I)=2
      ENDDO
C  TARGET NEUTRONS
C
      XMI=0.
      ZMA=-1E30
      DO 150 I=IA1*NQU+1,NQI
*      IF(NOCOU)THEN
*        CALL GIVE(XX(I),YY(I),ZZ(I),R2,X2,Z2,SG2)
*      ELSE
        IDP=ID(I)
        CALL GIVER(XX(I),YY(I),ZZ(I),IDP,2)
*      ENDIF
      NCS(I)=0
      IPTI(I)=2
      XMI=MIN(XMI,XX(I))
      ZMA=MAX(ZMA,ZZ(I))
 150  CONTINUE
      WRITE(*,*)'XMI = ',XMI,'  ZMA = ',ZMA
C
      APR0=0.
      DO I=IA1*NQU+1,NQI
        IDP=ID(I)
        APR0=APR0+BAR(IDP)
        PTK=PX(I)**2+PY(I)**2
        PZK=PZ(I)**2
        PK=PTK+PZK
        IF(POT.AND.LMO)THEN
          CALL PTNO(B2X,0.,B2Z,1.,DUPNO1
     C      ,AMV(I),BAR(IDP),PX(I),PY(I),PZ(I),EE(I)
     A      ,PC,EC,VXI,VYI,VZI,2)
        ELSE
          EE(I)=SQRT(PK+(AMV(I)+UPO1)**2)
        ENDIF
        MDE(I)=0                       !energy shift in Coulomb patching
        mdep(i)=0
        IF(LCOR)ITCR(I)=-1
        if(pent)then
          isr(i)=0
          isrx(i)=0
        endif
        igx(i)=b2x*g2*1e3
        igy(i)=0.
        igz(i)=b2z*g2*1e3
      ENDDO
      APR0=APR0/NQU
      APR=APR0
C
C  TARGET NOW
C  COOLING OF THE TARGET
      IPTCO=2               !needed by COOL and MOVEX
      CALL MOVEX(0.)
*      CALL GIVAL
*      CALL SORTI(IPO,IVAL,NQ)
*      CALL FINDI(IPO,IVAL,NQ)
      CALL FINDU
!*Jorg!!!
      CALL COOL
C
      APR0=APR0+APR01
      APR=APR0
      DO I=1,IA1*NQU
        ID(I)=-ID(I)   !switched on again
      ENDDO

cc  expanding f/monopole oscillations
c      xc=0.
c      yc=0.
c      zc=0.
c      do i=IA1*NQU+1,NQI
c        xc=xc+xx(i)
c        yc=yc+yy(i)
c        zc=zc+zz(i)
c      enddo
c      xc=xc/(nqu*ia2)
c      yc=yc/(nqu*ia2)
c      zc=zc/(nqu*ia2)
cc      fa=1.+.02
c      do i=IA1*NQU+1,NQI
cc        xx(i)=xc+(xx(i)-xc)*fa
cc        yy(i)=yc+(yy(i)-yc)*fa
cc        zz(i)=zc+(zz(i)-zc)*fa
cc        if(id(i).eq.1)then
cc          zz(i)=zz(i)+(ia2-iz2)*2./ia2*.0
cc        elseif(id(i).eq.2)then
cc          zz(i)=zz(i)-iz2*2./ia2*.0
cc        endif
cc f/deformed nuclei
c        dx=(xx(i)-xc)*pala
c        dy=(yy(i)-yc)*pala
c        dz=(zz(i)-zc)/sg2*alak
c        dxa=dx*ct2+dz*st2
c        dz=-dx*st2+dz*ct2
c        dx=dxa*cf2+dy*sf2
c        dy=-dxa*sf2+dy*cf2
c        xx(i)=xc+dx
c        yy(i)=yc+dy
c        zz(i)=zc+dz*sg2
c*        xx(i)=xc+(xx(i)-xc)*pala
c*        yy(i)=yc+(yy(i)-yc)*pala
c*        zz(i)=zc+(zz(i)-zc)*alak
cc end f/deformed nuclei
c      enddo
cc  end expanding

cc  expanding for dipole oscillations
c      pxc=0.
c      pyc=0.
c      pzc=0.
c      fa=.015*.2/(.9*float(ia2)**(1./3.))**2
c      do i=ia1*nqu+1,nqi
c        dpx=fa*2.*(xx(i)-xc)*(zz(i)-zc)
c        dpy=fa*2.*(yy(i)-yc)*(zz(i)-zc)
c        dpz=fa*((xx(i)-xc)**2+(yy(i)-yc)**2+3.*(zz(i)-zc)**2)
c        px(i)=px(i)+dpx
c        py(i)=py(i)+dpy
c        pz(i)=pz(i)+dpz
c        pxc=pxc+px(i)
c        pyc=pyc+py(i)
c        pzc=pzc+pz(i)
c      enddo
c      pxc=-pxc/(nqu*ia2)
c      pyc=-pyc/(nqu*ia2)
c      pzc=-pzc/(nqu*ia2)
c      do i=ia1*nqu+1,nqi
c        px(i)=px(i)+pxc
c        py(i)=py(i)+pyc
c        pz(i)=pz(i)+pzc
c      enddo
cc  end expanding for dipole oscillations

c f/Roy removing caps
c      rcut=r1+1.9
c      rcutk=rcut*rcut
c      bim2=.5*bim
c      do i=1,nqi
c        if(id(i).gt.0)then
c          ryyk=rcutk-yy(i)**2
c          xxi=xx(i)
c          if((xxi-bim2)**2.gt.ryyk.or.(xxi+bim2)**2.gt.ryyk)then
c            apr0=apr0-bar(id(i))/nqu
c            apr=apr-bar(id(i))/nqu
c            id(i)=0
c          endif
c      enddo
c  end f/Roy
c

C
      RRK(1,1)=SQRT(RRK(1,1)/(MAX(IZ1,1)*NQU))
      RRK(2,1)=SQRT(RRK(2,1)/(MAX(IA1-IZ1,1)*NQU))
C
      RRK(1,2)=SQRT(RRK(1,2)/(MAX(IZ2,1)*NQU))
      RRK(2,2)=SQRT(RRK(2,2)/(MAX(IA2-IZ2,1)*NQU))
C
**    WRITE(*,*)'radii proj/tgt  ',RRK
**    stop
C
      IPTCO=0
      CALL MOVEX(0.)
C  SPACE
C
*      CALL GIVAL
C  THAT SUBROUTINE ASSIGNS VALUES FOR SORTING TO THE POINTERS
C  SORTING ACCORDING TO THE PARTICIPATION IN DIFFERENT SPATIAL CELLS
C
*      CALL SORTI(IPO,IVAL,NQ)
C  THIS ONE SORTS
C
*      CALL FINDI(IPO,IVAL,NQ)
C  THIS ONE FINDS THE ENDS...
C  ... ALL THESE WILL BE ALWAYS IN A BLOCK
C
      CALL FINDU
C  LOOKS FOR THE OPTICAL POTENTIAL, NOW WITH SURFACE DIFFUSNESS
C
      CALL GIVEE
c
      if(lmo)then
        do iz=-nl/2,0
          write(*,*)iz,rho(0,0,iz)/an0,vzno(0,0,iz),uu(0,0,iz)
*          write(*,*)iz,rho(0,0,iz)/an0
        enddo
*       stop
      endif
C
      EPR=EKIN+EPOT
      CALL MOVEP(.5*DT)
*      RHOPAC=.73*AN0
      RHOPAC=.43*AN0
      facr=.93
      krho=1
C
C  MAIN LOOP STARTS ...
      ISTEP=0
      tim=-dt
 350  CONTINUE
C
cexpanding
*      write(*,*)'before collide, tim = ',tim
      CALL COLLIDE
      IF(LMO)CALL ENBKON
C  COLLIDES PSEUDOPARTICLES IN THE CELLS
      CALL GIVAL
      CALL SORTI(IPO,IVAL,NQ)
      CALL FINDI(IPO,IVAL,NQ)             !too much of luxury
C  WE ADD THAT SOLELY BECAUSE PTCLES MAY BE CREATED FROM D BREAKUP
*      write(*,*)'before movex'
      CALL MOVEX(DT)
C  MOVES COORDINATES
      IF(LMIX)THEN
        IF(mMIX.eq.-1)THEN
          IF(RHOPAR.LT.facr*RHOPAB.AND.RHOPAR.GT.RHOPAC)mmix=0  !alert
          IF(FACR*RHOPAB.LT.RHOPAC.AND.krho.eq.1
     I      .and.rhopar.lt.rhopab.and.rhopar.gt..4*an0)then
            mmix=0   !alert
            krho=0
          endif
          RHOPAB=MAX(RHOPAR,RHOPAB)
*          mmix=-1
        elseif(mmix.eq.0)then
          call mix
          mmix=1
          call gival
          call sorti(ipo,ival,nq)
          call findi(ipo,ival,nq)
        else
          rhopab=rhopar
          mmix=-1
*        mmix=2
*          RHOPAC=.79*AN0
          RHOPAC=.54*AN0
          facr=.85
        endif
      ENDIF
*      CALL GIVAL
*      CALL SORTI(IPO,IVAL,NQ)
*      CALL FINDI(IPO,IVAL,NQ)  !Lijun says redundant 1/22/03
C  PUTS STUFF IN ORDER
*      write(*,*)'before FinDU'
      CALL FINDU
C  FINDS POTENTIAL
*      write(*,*)'before movep'
      CALL MOVEP(DT)
C  MOVES MOMENTA AND CALCULATES NEW ENERGIES
C
      TIM=ISTEP*DT
      EPR=EKIN+EPOT
      AMPRK=EPR*EPR-(PPRX*PPRX+PPRY*PPRY+PPRZ*PPRZ)
      IF(AMPRK.GE.0.)THEN
        AMPR=SQRT(AMPRK)
        GST=EPR/AMPR
        BGSTX=PPRX/AMPR
        BGSTY=PPRY/AMPR
        BGSTZ=PPRZ/AMPR
      ENDIF
*     write(*,*)'gst = ',gst,' bgstx = ',bgstx
      ETO=EPR+ETOG
      IF(PRI)THEN

cc expanded nucleus
c        xcc=xc
c        ycc=yc
c        zcc=zc
c        xc=0.
c        yc=0.
c        zc=0.
c        do 490 ip=1,nq
c          if(id(ip).le.0)goto 490
c          bari=bar(idp)
c          xxi=xx(ip)
c          yyi=yy(ip)
c          zzi=zz(ip)
c          dr2=(xxi-xcc)**2+(yyi-ycc)**2+(zzi-zcc)**2
c          if(dr2.gt.200..or.bari.lt..1)goto 490
c          bart=bart+bari
c          xc=xc+bari*xxi
c          yc=yc+bari*yyi
c          zc=zc+bari*zzi
c 490    continue
c        pbart=1./max(bart,1e-3)
c        xc=xc*pbart
c        yc=yc*pbart
c        zc=zc*pbart
c        x2=0.
c        y2=0.
c        z2=0.
c        bart=0.
c        rt=0.
c        rz3=0.
c        rz=0.
c        do 510 ip=1,nq
c          if(id(ip).le.0)goto 510
c          bari=bar(idp)
c          xxi=xx(ip)
c          yyi=yy(ip)
c          zzi=zz(ip)
c          dr2=(xxi-xc)**2+(yyi-yc)**2+(zzi-zc)**2
c          if(dr2.gt.200..or.bari.lt..1)goto 510
c          bart=bart+bari
c          rt=rt+bari*sqrt(dr2)
c          x2=x2+bari*xxi*xxi
c          y2=y2+bari*yyi*yyi
c          z2=z2+bari*zzi*zzi
c          rz3=rz3+bari*(zzi-zc)*((zzi-zc)**2+(xxi-xc)**2+(yyi-yc)**2)
c          rz=rz+2.*(id(ip)-1.5)*(zzi-zc)
c 510    continue
c        pbart=1./max(bart,1e-3)
c        x2=x2*pbart
c        y2=y2*pbart
c        z2=z2*pbart
c        r2=x2-xc*xc+y2-yc*yc+z2-zc*zc
c        rt=rt*pbart
c        rz3=rz3*pbart
c        rz=rz*pbart
c        write(*,'(1x,a,5(1x,f10.5))')
c     w    'ti/r2/rt/rz3/rz ',tim,r2,rt,rz3,rz
cc end expanded nucleus

        IF(LPRO)THEN
          NADD=MIN(NDLS,MAX(1,NINT(1./DT)))
          FADD=1./(NADD*DT*DLL*NQU)
*          FADE=1./(NADD*DT*DES*NQU)
*          WRITE(9,*)' '
*          WRITE(9,*)'t = ',tim
*          DO IE=1,NES
*            DDES=0.
*            DO IDLS=1,NADD
*              DDES=DDES+NDES(IE,IDLS)
*            ENDDO
*            DDES=DDES*FADE
*            WRITE(9,'(F7.3,1X,F9.3)')(IE-.5)*DES,DDES
*          ENDDO
          do k=0,nsit
          WRITE(10,*)' '
          WRITE(10,*)'t = ',tim,' k = ',k
          if(lmo)then
            write(10,*)'x direction'
            do ix=0,nt
              x=ix*dwl
              rhox=.5*(rho(ix,0,0)+rho(-ix,0,0))/an0
              vx=.5*(vxno(ix,0,0)-vxno(-ix,0,0))
              WRITE(10,
     O          '(F6.2,1X,F8.4,1X,F7.4,1X,F7.4,1X,F8.4,1X,F9.6)')
     1          x,rhox,vx
            enddo
            write(10,*)'y direction'
            do iy=0,nt
              y=iy*dwl
              rhoy=.5*(rho(0,iy,0)+rho(0,-iy,0))/an0
              vy=.5*(vyno(0,iy,0)-vyno(0,-iy,0))
              WRITE(10,
     O          '(F6.2,1X,F8.4,1X,F7.4,1X,F7.4,1X,F8.4,1X,F9.6)')
     1          y,rhoy,vy
            enddo
*            goto 1112
          endif
          WRITE(10,*)'z direction'
*          goto 1111
          IF(IA1.EQ.IA2.or.ia1*ia2.eq.0)THEN
            DO IZ=0,NL
              DDLZ=0.
              DO IDLS=1,NADD
                DDLZ=DDLZ+NDLZ(IZ,IDLS)+NDLZ(-IZ,IDLS)
              ENDDO
              DDLZ=DDLZ*FADD*.5
              VZZ=VZE(IZ,0)
              vxzz=vxze(iz,0)
              VZZI=VZE(-IZ,0)
              vxzzi=vxze(-iz,0)
*             RHOZ=RHO(0,0,IZ)
*             RHOZI=RHO(0,0,-IZ)
              RHOZ=RHOB(0,IZ)
              RHOZI=RHOB(0,-IZ)
              RHOZZ=RHOZ+RHOZI
              IF(RHOZZ.NE.0.)THEN
                VZZ=(RHOZ*VZZ-RHOZI*VZZI)/RHOZZ
                VxZZ=(RHOZ*VxZZ-RHOZI*VxZZI)/RHOZZ
              ELSE
                VZZ=0.
                vxzz=0.
              ENDIF
              RHOZZ=.5*RHOZZ
              WRITE(10,
     O          '(F6.2,1X,F8.4,1X,F7.4,1X,F7.4,1X,F8.4,1X,F9.6)')
     1          IZ*DLL,RHOZZ/AN0,VZZ,vxzz
     2          ,(.5*(tho(0,0,iz)+tho(0,0,-iz))
     3          +.25*(rhoB(0,iz)+rhoB(0,-iz)))/an0,spaz(iz,0)   !,DDLZ
*    3          +.25*(rho(0,0,iz)+rho(0,0,-iz)))/an0,DDLZ
*               WRITE(*,*)IZ*DLL,'  '
*     1          ,.5*(RHO(0,0,IZ)+RHO(0,0,-IZ))/AN0,VZZ
            ENDDO
          ELSE
 1111  continue
            DO IZ=-NL,NL
              DDLZ=0.
              DO IDLS=1,NADD
                DDLZ=DDLZ+NDLZ(IZ,IDLS)
              ENDDO
              DDLZ=DDLZ*FADD
              WRITE(10,
     W          '(F6.2,1X,F8.4,1X,F7.4,1X,F7.4,1X,F8.4,1X,F9.6)')
*     W          IZ*DLL,RHOB(0,IZ)/AN0,VZE(IZ),vXZe(iz),spaz(iz)   !DDLZ
     W          IZ*DLL,RHOB(k,IZ)/AN0,VZE(IZ,k),vXZe(iz,k),spaz(iz,k)   !DDLZ
*    W          IZ*DLL,RHOBZ(IZ)/AN0,VZE(IZ),DDLZ
*    W          IZ*DLL,RHO(0,0,IZ)/AN0,VZE(IZ),DDLZ
            ENDDO
          ENDIF
          enddo
          if(ia1.ne.0)then
            WRITE(10,*)'y direction'
            DO IY=0,NT
              VYY=VYE(IY)
              VYYI=VYE(-IY)
*             RHOY=RHO(0,IY,0)
*             RHOYI=RHO(0,-IY,0)
              RHOY=RHOBY(IY)
              RHOYI=RHOBY(-IY)
              RHOYY=RHOY+RHOYI
              IF(RHOYY.NE.0.)THEN
                VYY=(RHOY*VYY-RHOYI*VYYI)/RHOYY
              ELSE
                VYY=0.
              ENDIF
              RHOYY=.5*RHOYY
              WRITE(10,'(F6.2,1X,F8.4,1X,F7.4,1X,F8.4,1X,F8.4)')IY*DWL
     1          ,RHOYY/AN0,VYY
     2          ,(.5*(tho(0,iy,0)+tho(0,-iy,0))
     3          +.25*(rhoBY(iy)+rhoBY(-iy)))/an0
*    3          +.25*(rho(0,iy,0)+rho(0,-iy,0)))/an0
     4          ,.5*(spay(iy)+spay(-iy))
            ENDDO
          endif
        ENDIF
 1112   continue
        PAB=0.
        DO I=1,NIDP
          PAB=PAB+BAR(I)*(PARTI(I)+PARTIG(I))
        ENDDO
        PAB=MAX(1E-5,PAB)
        write(*,*)'ps ',(PPRX+PGOX)/IA,(PPRY+PGOY)/IA
     I    ,(PPRZ+PGOZ)/IA,(PPRZL+PGOZL)/IA
*       WRITE(10,*)TIM,ETO/APR0,EPR/APR,EKIN/APR,EPOT/APR
*       WRITE(10,*)PARTI(1)+PARTIG(1),PARTI(3)+PARTIG(3)
*       write(10,*)'apr = ',apr
        WRITE(*,'(1X,A,5(A,F12.3))')'P_r=0 ','PKT=',1E3*PRSKT0,
     W  ' PP=',1E3*PRSP0,' PTO=',1E3*(PRSKT0+PRSP0),' PKL=',1E3*PRSKL0,
     R  ' PXZ=',1E3*PRSXZ0
        IF(APR.NE.0.)WRITE(*,'(A,6(1X,F7.5))')'E K/P/KT/KZ/CT/CZ',
     W     EKIN/APR,EPOT/APR,(EKINT+EKINTG)/APR0,(EKINZ+EKINZG)/APR0
     R     ,ECINT/PAB,ECINZ/PAB
*    W     EKIN/APR,EPOT/APR,EKINT/APR,EKINZ/APR,ECINT/APR,ECINZ/APR
        WRITE(*,'(1X,A,5(1X,F7.5))')'EKIT P-T       ',
     W    (EKIT(I),I=1,5)
        WRITE(*,'(1X,A,5(1X,F7.5))')'EKITC P-T      ',
     W    (EKITC(I),I=1,5)
        WRITE(*,'(1X,A,5(1X,F7.5))')'ECVT P-T       ',
     W    (ECVT(I),I=1,5)
        WRITE(*,'(1X,A,5(1X,F7.5))')'EKIZ P-T       ',
     W    (EKIZ(I),I=1,5)
        WRITE(*,'(1X,A,5(1X,F7.5))')'EKIZC P-T      ',
     W    (EKIZC(I),I=1,5)
        WRITE(*,'(1X,A,5(1X,F7.5))')'ECVZ P-T       ',
     W    (ECVZ(I),I=1,5)
        WRITE(*,'(1X,A,5(1X,F7.5))')'EISH P-T       ',
     W    (EISH(I),I=1,5)
        WRITE(*,'(1X,A,1X,F7.2,1X,F7.5,5(1X,F7.3))')'tim E/N/P/3He/T',
     W     TIM,ETO/APR0,PARTI(1)+PARTIG(1)+PARTI(2)+PARTIG(2)
     W    ,PARTI(1)+PARTIG(1),PARTI(3)+PARTIG(3)
     W    ,PARTI(4)+PARTIG(4),PARTI(5)+PARTIG(5)
        WRITE(*,'(1X,A,5(1X,F7.3))')'WA NP/P/D/3He/T',
     W    ANPAR+ANPARG,ANPR(1)+ANPRG(1),ANPR(3)+ANPRG(3)
     W    ,ANPR(4)+ANPRG(4),ANPR(5)+ANPRG(5)
        WRITE(*,'(1X,A,6(1X,F7.3))')'DL/N*          ',
     W    (PARTI(I)+PARTIG(I),I=7,12)
        IF(PION)WRITE(*,'(1X,A,3(1X,F7.3))')'PI             ',
     W    (PARTI(I)+PARTIG(I),I=13,15)
        IF(BOX)WRITE(*,'(1X,A,8(1X,F7.2))')'EKI P-T        ',
     W    (1E3*EKI(I),I=1,5),(1E3*EKI(I),I=13,15)
*       WRITE(10,*)'ANPAR = ',ANPAR,'ANPARG = ',ANPARG
*       WRITE(10,*)'ANPR = ',ANPR(1),'ANPRG = ',ANPRG(1)
*       IF(DEPRO)WRITE(10,*)'ANDE = ',ANPR(3),'ANDEG = ',ANPRG(3)
        write(*,*)'apr = ',apr
*         volt=dwl*dwl*dll*(nt+nt+1)**2*(nl+nl+1)
*         anpio=0.
*         do i=10,12
*           anpio=anpio+andl(i)+andlg(i)
*         enddo
*         anpio=anpio/volt
*         write(*,*)'anpio = ',anpio,'  temp = ',temp
        WRITE(*,'()')
      ENDIF
C
      do nti=1,ntimo
        if(ktimo(nti).ne.0)then
          if(tim.ge.timo(nti))then
            ktimo(nti)=0
            goto 370
          endif
        endif
      enddo
      goto 380
c
 370  continue
      write(fiinam,'(i5.5)')nint(tim)
      k=1
      do ik=1,3
        if(fiinam(ik:ik).ne.'0')goto 372
        k=k+1
      enddo
 372  continue
      write(fbinam,'(i2.2)')nint(bim)
      kb=1
      if(fbinam(1:1).eq.'0')kb=2
      if(.not.isFcha4Set) then
       FCHA4=FCHA()//FCHA()//FCHA()//FCHA()
       isFcha4Set=.true.
      endif
      fillam=fname//fcha4//fbinam(1:2)//fiinam(1:5)//'.spa'
      open(37,file=fillam,status='unknown')
c      write(37,*)'grid of ',nl+nl+1,' z values starting with ',
c     w  -nl*dll,' and separated by ',dll,','
c      write(37,*)'by ',nt+nt+1,' x values starting with ',
c     w  -nt*dwl,' and separated by ',dwl,','
c      write(37,*)'by ',nt+nt+1,' x values starting with ',
c     w  -nt*dwl,' and separated by ',dwl,','
c      write(37,*)'first x changes, while y and z are fixed'
c      write(37,*)'next y advances and z last'
c      write(37,*)'the baryon density values, in the system cm,',
c     w  ' are normalized to normal density'
c      do iz=-nl,nl
c        do iy=-nt,nt
c          do ix=-nt,nt
c            vx=.25*(vxno(ix,iy,iz)+vxno(ix,-iy,iz)
c     v        -vxno(-ix,iy,-iz)-vxno(-ix,-iy,-iz))
c            vy=.25*(vyno(ix,iy,iz)-vyno(ix,-iy,iz)
c     v        +vyno(-ix,iy,-iz)-vyno(-ix,-iy,-iz))
c            vz=.25*(vzno(ix,iy,iz)+vzno(ix,-iy,iz)
c     v        -vzno(-ix,iy,-iz)-vzno(-ix,-iy,-iz))
c            ag=1.-vx*vx-vy*vy-vz*vz
c            if(ag.le.1e-3)ag=1.
c            write(37,'(1x,f6.3,3f9.5)')
c     e        .25*(rho(ix,iy,iz)+rho(ix,-iy,iz)
c     w        +rho(-ix,iy,-iz)+rho(-ix,-iy,-iz))/(an0*sqrt(ag))
c     r      ,vx,vy,vz
c          enddo
c        enddo
c      enddo
      write(37,*)'read mesh Y'
*     write(37,'(10(f6.2,1x))')(iz*dll,iz=-nl,nl)
*     do ix=-nt,nt
*       x=ix*dwl
*       write(37,*)'X ',x,' Z'
*       do iz=-nl,nl
*         if(ia1.eq.ia2)then
*           write(37,*).5*(rhob(ix,iz)+rhob(-ix,-iz))/an0
*         else
*           write(37,*)rhob(ix,iz)/an0
*         endif
*       enddo
*     enddo
c      write(37,'(10(f6.2,1x))')(ix*dwl,ix=-nt,nt) ! commented for gnuplot output BWB 2012-09-15
      write(37,*)'# gnuplot-formatted output'
      write(37,*)'# density for y=0'
      write(37,*)'# x (fm)   z (fm)   rho/rho_0'
      do iz=-nl,nl
        z=iz*dll
c        write(37,*)'X ',z,' Z'
        do ix=-nt,nt
          if(ia1.eq.ia2)then
            write(37,*)ix*dwl,z,.5*(rhob(ix,iz)+rhob(-ix,-iz))/an0
Cc pressure
C            write(37,*).5*(ptxz(ix,iz)+ptxz(-ix,-iz))*1e3
          else
            write(37,*)ix*dwl,z,rhob(ix,iz)/an0
          endif
        enddo
       write(37,*)
      enddo
      close(37)
      amo=.5*(ia1+ia2)/(4./3.*pi*pf*pf*pf)
      FILLAM=fname//fcha4//FBINAM(1:2)//FIINAM(1:5)//'.mom' !changed to include full filename BWB 2008-06-24
      OPEN(37,FILE=FILLAM,STATUS='UNKNOWN')
c      write(37,*)'read mesh Y' ! commented for gnuplot output BWB 2012-09-15
*     write(37,'(10(i5,1x))')(nint(iz*dmo*1e3),iz=-nl,nl)
*     do ix=-nt,nt
*       x=ix*dmo
*       write(37,*)'X ',x*1e3,' Z'
*       do iz=-nl,nl
*         if(ia1.eq.ia2)then
*           write(37,*).5*(rhomo(ix,iz)+rhomo(-ix,-iz))/amo
*         else
*           write(37,*)rhomo(ix,iz)/amo
*         endif
*       enddo
*     enddo
c      write(37,'(10(f6.2,1x))')(ix*dwl,ix=-nt,nt) ! commented for gnuplot output BWB 2012-09-15
      write(37,*)'# gnuplot-formatted output'
      write(37,*)'# momentum density for z=0'
      write(37,*)'# x (fm)   y (fm)   rho_k/rho_0'
      do iy=-nt,nt
        y=iy*dwl
c        write(37,*)'X ',y,' Z'
        do ix=-nt,nt
          if(ia1.eq.ia2)then
            write(37,*)ix*dwl,y,.25*(rho(ix,iy,0)+rho(ix,-iy,0)
     w        +rho(-ix,iy,0)+rho(-ix,-iy,0))/an0
Cc pressure
C            write(37,*).25*(ptxy(ix,iy)+ptxy(ix,-iy)
C     w        +ptxy(-ix,iy)+ptxy(-ix,-iy))*1e3
CC     w        ,.25*(ptpxy(ix,iy)+ptpxy(ix,-iy)
CC     w        +ptpxy(-ix,iy)+ptpxy(-ix,-iy))*1e3,prsp0*1e3
          else
            write(37,*)ix*dwl,y,.5*(rho(ix,iy,0)+rho(ix,-iy,0))/an0
          endif
        enddo
       write(37,*)
      enddo
*      pti=.6
*      eti=sqrt(am0k+pti*pti)
*      write(37,'(10(f6.2,1x))')(ix*dwl,ix=-nt,nt)
*      do iy=-nt,nt
*        y=iy*dwl
*        write(37,*)'X ',y,' Z'
*        do ix=-nt,nt
*          if(ia1.eq.ia2)then
*            rhoa=.25*(rho(ix,iy,0)+rho(ix,-iy,0)
*     w        +rho(-ix,iy,0)+rho(-ix,-iy,0))
*            vxa=.25*(vxno(ix,iy,0)+rho(ix,-iy,0)
*     w        -vxno(-ix,iy,0)-vxno(-ix,-iy,0))
*            vya=.25*(vyno(ix,iy,0)-vyno(ix,-iy,0)
*     w        +vyno(-ix,iy,0)-vyno(-ix,-iy,0))
*            vza=.25*(vzno(ix,iy,0)+vzno(ix,-iy,0)
*     w        -vzno(-ix,iy,0)-vzno(-ix,-iy,0))
*            uua=.25*(uu(ix,iy,0)+uu(ix,-iy,0)
*     w        +uu(-ix,iy,0)+uu(-ix,-iy,0))
*            CALL PTNO(VXa,VYa,VZa,RHOa/AN0
*     A        ,UUa,AM0,1.,0.,Pti,0.
*     L        ,EEI,PC,EC,VXI,VYI,VZI,2)  ! Note: recalculates EE!
**            write(37,*)'vxa/y/z',vxa,vya,vza
**            write(37,*)'rhoa/an0/uua',rhoa/an0,uua
**            uua=upo(rhoa/an0)
**            eei=sqrt((am0+uua)**2+pti*pti)
*            write(37,*)(eei-eti)*1e3
*          else
*            write(37,*).5*(rho(ix,iy,0)+rho(ix,-iy,0))/an0
*          endif
*        enddo
*      enddo
      close(37)
 380  continue
      IF(MOD(ISTEP,NPIC).EQ.0)THEN
        IF(KK.NE.0)THEN
          WRITE(11,'(A)')'NEW'
        ELSE
          KK=1
        ENDIF
        WRITE(11,'(A)')'SET SIZE 8. BY 10. SIDEWAYS'
        WRITE(11,'(A)')'SET FONT DUPLEX'
        WRITE(11,'(A)')'SET WINDOW X FROM 0. TO 8. Y FROM 0. TO 10.'
        WRITE(11,'(A)')'SET WINDOW X FROM 0. TO 8. Y FROM .5 TO 8.'
     W    //' INSIDE'
        WRITE(11,'(A,F7.2,A)')'TITLE 4. 9.4 CENTER ''t = '
     W    ,DT*(ISTEP+1),' fm/c'' SIZE=2.5'
        WRITE(11,'(A)')'SET LABELS INSIDE OFF'
        WRITE(11,'(A)')'SET LABELS ALL ON'
        WRITE(11,'(A)')'SET WINDOW Y 2 OF -2 INSIDE'
        WRITE(11,'(A)')'SET LIMITS X FROM -28. TO 28.'
     W    //' Y FROM -21. TO 21.'
        WRITE(11,'(A)')'SET ORDER Y X'
        DO I=1,NQ,3
          IF(ID(I).GT.0)    ! gamma's always negative so won't show up
     I      WRITE(11,'(F6.2,1X,F6.2)')XX(I),ZZ(I)
        ENDDO
        WRITE(11,'(A)')'SET SYMBOL ''9O'' SIZE=.05'
        WRITE(11,'(A)')'PLOT'
        WRITE(11,'(A)')'SET WINDOW 1'
*       WRITE(11,'(A)')'SET LIMITS X FROM -2. TO 2.'
*    W    //' Y FROM -1.5 TO 1.5'
        WRITE(11,'(A)')'SET LIMITS X FROM -28. TO 28.'
     W    //' Y FROM -21. TO 21.'
        DO I=1,NQ,3
          IDI=ID(I)
          IF(IDI.GT.0)THEN                 ! gammas always negative...
            WRITE(11,'(F6.2,1X,F6.2)')XX(I),yy(I)
*           BARI=BAR(IDI)
*           IF(BARI.GT.0.)THEN
*             WRITE(11,*)PX(I)/BARI,PZ(I)/BARI
*           ELSE
*             WRITE(11,*)PX(I),PZ(I)
*           ENDIF
          ENDIF
        ENDDO
        WRITE(11,'(A)')'PLOT'
      ENDIF
      IF(ISTEP.GE.NSTEP)GOTO 400
C
      ISTEP=ISTEP+1
      PRI=(ISTEP/NPRI)*NPRI.EQ.ISTEP.OR.ISTEP.EQ.NSTEP

c      call outParticles

      GOTO 350 !end main timestep loop
C
 400  CONTINUE
      CLOSE(11)
      DO IZ=-NL,NL
*       WRITE(10,*)IZ,RHO(0,0,IZ),UU(0,0,IZ)
      ENDDO
      IF(LPRO)CLOSE(10)
C
      IF(LINI)THEN
        if(.not.isFcha4Set) then
         FCHA4=FCHA()//FCHA()//FCHA()//FCHA()
         isFcha4Set=.true.
        endif
C
        FILNAM=FNAME//FCHA4//'.IFO'
        OPEN(21,FILE=FILNAM,FORM='FORMATTED'
     O    ,STATUS='UNKNOWN')
        IF(POT)THEN
          IF(NO.EQ.1)THEN
            WRITE(21,*)'SOFT EOS'
          ELSE
            WRITE(21,*)'STIFF EOS'
          ENDIF
          IF(LMO)THEN
            WRITE(21,*)'MO-DEP'
          ELSE
            WRITE(21,*)'MO-INDEP'
          ENDIF
        ENDIF
        IF(LMIX)WRITE(21,*)'PTCLE POSITIONS INTERCHANGED'
        IF(LCSM)THEN
          WRITE(21,*)'PRESCRIBED ELASTIC IN-MEDIUM CROSS-SECTIONS'
        ELSE
          WRITE(21,*)'FREE CROSS-SECTIONS'
        ENDIF
        IF(FRAT.NE.1.)WRITE(21,*)'RATES RENORMALIZED BY ',FRAT
        IF(FRCAS)WRITE(21,*)'FROZEN CASCADE'
        IF(NODEP)WRITE(21,*)'NO DEUTERON PRODUCTION'
        IF(NOPAU)WRITE(21,*)'NO PAULI PRINCIPLE'
        IF(NPION)WRITE(21,*)'FROZEN RESONANCES'
        IF(WRPION)WRITE(21,*)'WRONG DETAILED BALANCE'
        IF(OLAST)WRITE(21,*)'ONLY ELASTIC PROCESSES'
        IF(NOPOT)WRITE(21,*)'NO OPTICAL POTENTIAL'
        IF(NOCOU)WRITE(21,*)'NO COULOMB POTENTIAL'
        WRITE(21,*)'PHOTON C.S. RENORMALIZED BY ',PHOTR
        WRITE(21,*)'A1 = ',IA1,' Z1 = ',IZ1
        WRITE(21,*)'A2 = ',IA2,' Z2 = ',IZ2
        WRITE(21,*)'TLAB = ',TLAB,' GEV  '
        WRITE(21,*)'NB = ',NBI
        WRITE(21,*)'NSTEP = ',NSTEP,' DT = ',DT
        WRITE(21,*)'LO = ',ALO,' FM ','  TR = ',TRAN
        WRITE(21,*)'DWL = ',DWL,' FM  ','DLL = ',DLL,' FM  '
     W    ,' NQU = ',NQU
        IF(SYMMY)WRITE(21,*)'Y SYMMETRY EXPLOITED'
        IF(ASYII)WRITE(21,*)'ISOSPIN ASYMMETRY IN PAULI PRINCIPLE'
        WRITE(21,'()')
C
        IF(LFORM)THEN
          FILNAM=FNAME//FCHA4//'.frm'
          OPEN(29,FILE=FILNAM,FORM='UNFORMATTED'
     O      ,STATUS='UNKNOWN')
        ENDIF
        FILNAM=FNAME//FCHA4//'.DAT'
        OPEN(22,FILE=FILNAM,FORM='UNFORMATTED'
     O    ,STATUS='UNKNOWN')
cc f/deformed
c        open(24,file='lisus.',status='unknown')
c 210    read(24,'()',end=220,err=220)
c        goto 210
c 220    continue
c        write(24,'(a,f7.3,f6.2,4g11.3)')fname//fcha4,tlab,bim
c     w    ,th1,th2,fi1,fi2
c        close(24)
cc end deformed
C
        IF(LCOR)THEN
          FILNAM=FNAME//FCHA4//'.COR'
          OPEN(23,FILE=FILNAM,FORM='UNFORMATTED'
     O      ,STATUS='UNKNOWN')
        ENDIF
C
        LINI=.FALSE.
      ENDIF
C
      WRITE(21,*)' B = ',BIM,' FM'
      WRITE(21,'()')
      CALL DATE_AND_TIME(DATE=FDATE, TIME=FTIME)
!      CALL DATE(FDATE)
!      CALL TIME(FTIME)
      WRITE(21,*)FDATE//'  '//FTIME
      WRITE(21,'()')
*c  for Jorg
*      write(21,*)'for Jorg, collisions vs rel en'
*      anatto=0.
*      anasto=0.
*      do ijorg=1,njorg
*        ejorg=(ijorg-.5)*dejorg
*        if(dejorg.ne.0.)then
*          anatem(ijorg)=anatem(ijorg)/(dejorg*nqu)
*          anasuc(ijorg)=anasuc(ijorg)/(dejorg*nqu)
*          anatto=anatto+anatem(ijorg)
*          anasto=anasto+anasuc(ijorg)
*        endif
*        write(21,'(1x,f7.3,3(1x,e12.4))')ejorg*1e3
*     w    ,1e-3*anatem(ijorg),1e-3*anasuc(ijorg)
*      enddo
*      anatto=anatto*dejorg
*      anasto=anasto*dejorg
*      write(21,*)anatto,anasto
*      WRITE(21,'()')
*      write(21,*)'for Jorg, collision number probability'
*      anupro=0.
*      do ic=0,ncomx
*        proba(ic)=0.
*        probs(ic)=0.
*      enddo
*      do iq=1,nq
*        idp=abs(id(iq))
*        if(idp.ne.0)then
*          ipta=abs(ipti(iq))
*          if(ipta.eq.1)then
*            anupro=anupro+1.
*            najoi=najo(iq)
*            if(najoi.le.ncomx)proba(najoi)=proba(najoi)+1.
*            nasui=nasu(iq)
*            if(nasui.le.ncomx)probs(nasui)=probs(nasui)+1.
*          endif
*        endif
*      enddo
*      write(21,*)'anupro= ',anupro/nqu
*      do ic=0,ncomx
*        proba(ic)=proba(ic)/anupro
*        probs(ic)=probs(ic)/anupro
*        write(21,*)ic,proba(ic),probs(ic)
*      enddo
*      WRITE(21,'()')
*c  end for Jorg
      ANPRX=0.      !xtra p's fom delta decays...
      NPARX=0
      DO I=13,15
        ANE(I)=0.   !pions
      ENDDO
      DO I=1,NIDP
        PARI(I)=0.
        EKIT(I)=0.
        EKIZ(I)=0.
        EKITC(I)=0.
        EKIZC(I)=0.
        ECVT(I)=0.
        ECVZ(I)=0.
      ENDDO
      DO 450 I=1,NQ
      IDI=ABS(ID(I))
      IF(IDI.EQ.0)GOTO 450
      IF(IPTI(I).GT.0.)GOTO 450               !bound!  *Jorg!!!
*      eebef=ee(i)
*      mdei=mde(i)
*      xxi=xx(i)
*      yyi=yy(i)
*      zzi=zz(i)
***C      IF(COU)CALL COUL(I)
*      if(idi.eq.1)then
*        write(37,*)eebef,ee(i),mdei*petom
*        write(37,*)xxi,yyi,zzi,' xyz'
*      endif
      CALL BREK(I,PXI,PYI,PZI,EEI,DEI,IDI,PXE,PYE,PZE,EEE,IDE)  !try to break
C  IDI-NUCLEON, IDE-PION AFTER BREAK
      IF(IDE.NE.0)THEN                                  !pion resulting
        IF(IDI.EQ.1)THEN
          CALL TEST(IDI,PXI,PYI,PZI,EEI,DEI,-1,*450,IPART,IOV)
          NPARX=NPARX+IPART
          ANPRX=ANPRX+IOV
        ENDIF
        CALL TEST(IDE,PXE,PYE,PZE,EEE,0.,-1,*450,IPART,IOV)
**      ANE(IDE)=ANE(IDE)+IOV
        ANE(IDE)=ANE(IDE)+1.
        CALL LAB(EEE,PZE,EEEL,PZEL,AEMM,YEL)
        IDIC=IDE
        IXI=NINT(1E3*PXE)
        IYI=NINT(1E3*PYE)
        IZI=NINT(1E3*PZEL)
        WRITE(22)IDIC,IXI,IYI,IZI !,igx(i),igy(i),igz(i)  !,0 ! !extra write
      ELSE
        PARI(IDI)=PARI(IDI)+1.
        BARI=1E-3*MAX(BAR(IDI),1.)
        PFT=BARI*IFT(I)
        PFZ=BARI*IFZ(I)
        PFKT=PFT*PFT
        PFKZ=PFZ*PFZ
        PFK=PFKT+PFKZ
        AG=AMV(I)+SQRT(AMV(I)**2+PFK)
        IF(AG.NE.0.)THEN
          EPT=PFKT/AG
          EPZ=PFKZ/AG
        ELSE
          EPT=0.
          EPZ=0.
        ENDIF
        EKIT(IDI)=EKIT(IDI)+EPT
        EKIZ(IDI)=EKIZ(IDI)+EPZ
        PCT=BARI*IBT(I)
        PCZ=BARI*IBZ(I)
        PCKT=PCT*PCT
        PCKZ=PCZ*PCZ
        PCK=PCKT+PCKZ
        AG=AMV(I)+SQRT(AMV(I)**2+PCK)
        IF(AG.NE.0.)THEN
          ECPT=PCKT/AG
          ECPZ=PCKZ/AG
        ELSE
          ECPT=0.
          ECPZ=0.
        ENDIF
        EKITC(IDI)=EKITC(IDI)+ECPT
        EKIZC(IDI)=EKIZC(IDI)+ECPZ
        ECT=REAL(ICT(I))**2
        ECT=BARI*ECT*1E-3
        ECINT=ECINT+ECT
        ECZ=REAL(ICZ(I))**2
        ECZ=BARI*ECZ*1E-3
        ECINZ=ECINZ+ECZ
        ECVT(IDI)=ECVT(IDI)+ECT
        ECVZ(IDI)=ECVZ(IDI)+ECZ
      ENDIF
      CALL LAB(EEI,PZI,EEL,PZL,AMM,YL)
      IDIC=IDI
      IZI=NINT(PZL*1E3)
      IXI=NINT(PXI*1E3)
      IYI=NINT(PYI*1E3)
      if(idic.eq.16)write(*,*)'photon emitted'
      WRITE(22)IDIC,IXI,IYI,IZI !,igx(i),igy(i),igz(i)  !,i  !
      IF(LFORM)THEN
        if(idi.ge.3.and.idi.le.6)write(29)idic,itfo(i),irfo(i)
      ENDIF
      IF(LCOR.AND.IDE.EQ.0)THEN
       if(itcr(i).ge.0) then
*        PZL=IPCR(3,I)*1E-3
*        EEL=IECR(I)*1E-3
*        CALL LAB(EEI,PZI,EEL,PZL,AMM,YL)
*        IZI=NINT(PZI*1E3)
*        WRITE(23)(IRCR(J,I),J=1,3),(IPCR(J,I),J=1,3)
*     W    ,ITCR(I),IDIC,IRHCR(I)
        WRITE(23)IDIC,IXI,IYI,IZI,ITCR(I),(IRCR(J,I),J=1,3)
     W    ,IRHCR(I)
       endif !itcr(i).ge.0
      ENDIF
 450  CONTINUE
 452  CONTINUE
      IXI=-100
      WRITE(22)IXI,IXI,IXI,IXI !,ixi,ixi,ixi  !,0 !
      IF(LCOR)WRITE(23)IXI,IXI,IXI,IXI,IXI,IXI,IXI,IXI,IXI
C
      WRITE(21,*)'NO. PART. IN P-BALL DEF = '
     W  ,ANPAR+ANPARG+FLOAT(NPARX)/NQU
C
      DO I=13,15
        ANE(I)=ANE(I)/NQU+PARTI(I)+PARTIG(I)
      ENDDO
      ANPRX=ANPRX/NQU
C
      WRITE(*,*)'FROM DECAY P''S: ',ANPRX
      WRITE(*,*)'PI''S (+,0,-): ',ANE
C
      PARIT=0.
      EVT=0.
      EVZ=0.
      DO I=1,NIDP
        PARIT=PARIT+PARI(I)*BAR(I)
        PARTII=MAX(PARI(I),1.)
        EKIT(I)=EKIT(I)/PARTII
        EKIZ(I)=EKIZ(I)/PARTII
        EKITC(I)=EKITC(I)/PARTII
        EKIZC(I)=EKIZC(I)/PARTII
        EVT=EVT+ECVT(I)
        ECVT(I)=ECVT(I)/PARTII
        EVZ=EVZ+ECVZ(I)
        ECVZ(I)=ECVZ(I)/PARTII
      ENDDO
      PARIT=MAX(PARIT,1.)
      EVT=EVT/PARIT
      EVZ=EVZ/PARIT
C
      WRITE(*,*)' '
      WRITE(*,*)'Final emitted'
      WRITE(*,'(1X,A,2(1X,F7.5))')'CT/CZ ',EVT,EVZ
      WRITE(*,'(1X,A,5(1X,F7.5))')'EKIT P-T       ',
     W  (EKIT(I),I=1,5)
      WRITE(*,'(1X,A,5(1X,F7.5))')'EKITC P-T      ',
     W  (EKITC(I),I=1,5)
      WRITE(*,'(1X,A,5(1X,F7.5))')'ECVT P-T       ',
     W  (ECVT(I),I=1,5)
      WRITE(*,'(1X,A,5(1X,F7.5))')'EKIZ P-T       ',
     W  (EKIZ(I),I=1,5)
      WRITE(*,'(1X,A,5(1X,F7.5))')'EKIZC P-T      ',
     W  (EKIZC(I),I=1,5)
      WRITE(*,'(1X,A,5(1X,F7.5))')'ECVZ P-T       ',
     W  (ECVZ(I),I=1,5)
C
      DO I=13,15
*       ANE(I)=ANE(I)*RENAN
        ANET(I)=ANET(I)+ANE(I)
      ENDDO
C
      DANPR=(ANPR(1)+ANPRG(1)+ANPRX)*RENAN
      ANPRT(1)=ANPRT(1)+DANPR
      WRITE(21,*)'NO. WIDE-ANG. PROT. (NAG.) = ',DANPR
      DANDE=(ANPR(3)+ANPRG(3))*RENAN
      ANPRT(3)=ANPRT(3)+DANDE
      WRITE(21,*)'NO. WIDE-ANG. DEUT. (NAG.) = ',DANDE
      DANE3=(ANPR(4)+ANPRG(4))*RENAN
      ANPRT(4)=ANPRT(4)+DANE3
      WRITE(21,*)'NO. WIDE-AND. HE3   (NAG.) = ',DANE3
      DANTE=(ANPR(5)+ANPRG(5))*RENAN
      ANPRT(5)=ANPRT(5)+DANTE
      WRITE(21,*)'NO. WIDE-AND. TRIT. (NAG.) = ',DANTE
      WRITE(21,'()')
      DO I=7,12
        DANDL(I)=PARTI(I)+PARTIG(I)
        PARTIT(I)=PARTIT(I)+DANDL(I)
      ENDDO
      WRITE(21,*)'DELTAS (++,+,0,-):'
      WRITE(21,*)(DANDL(I),I=7,10)
      WRITE(21,*)'N*''S (+,0): ',(DANDL(I),I=11,12)
      WRITE(21,*)'PI''S (+,0,-): ',ANE
      WRITE(21,'()')
C
 500  CONTINUE
C
      FCS=PI*BMAXK*10.*1E-3/IBF
      WRITE(21,*)'W-ANG. P C.S. = ',FCS*ANPRT(1),' B'
      WRITE(21,*)'W-ANG. D C.S. = ',FCS*ANPRT(3),' B'
      WRITE(21,*)'W-ANG. HE3 C.S. = ',FCS*ANPRT(4),' B'
      WRITE(21,*)'W-ANG. T C.S. = ',FCS*ANPRT(5),' B'
      WRITE(21,*)'PIONS (+,0,-) :'
      WRITE(21,*)(FCS*ANET(I),I=13,15),' B'
C
      filnam=fname//fcha4
      write(21,*)filnam
      write(*,*)filnam
C
      END


      block data dmix
      common/mixc/mmix
      data mmix/-1/
      end


      FUNCTION FCHA()
      use global
      CHARACTER FCHA
      COMMON/CSEED/ISEED
C
      IC=48+35.99*getRan()
      IF(IC.GT.57)IC=IC+7
      FCHA=CHAR(IC)
C
      END


      BLOCK DATA ZEES            !!! NEEDS TO BE MODIFIED IF ID'S CHANGED !!!
      INCLUDE 'PARTIC15'
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


      BLOCK DATA VMAX
      INCLUDE 'PARTID'
      INCLUDE 'NQUA1'
      INCLUDE 'NUCLS'
      INCLUDE 'SIZE2'
      COMMON/CVMAX/VOLCM(NIDP,NIDP),VDMAX,VTMAX,GTMAX(7:12)
     C  ,VDPPMX,VDPNMX,VTPPMX,VTPNMX
      PARAMETER (NIDPK=NIDP*NIDP)
      COMMON/CPA/RHOPAM,UUP
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
      DATA VOLCM/NIDPK*1./,VDMAX/1E10/,VTMAX/1E10/,GTMAX/6*1./
*     DATA RHOPAM/.003/
      parameter (rhopm=6./(8.*vole*nqu))
      DATA RHOPAM/rhopm/
      DATA NOPAUL/2*.FALSE./NOPALN/4*.FALSE./
C
      END


      SUBROUTINE WIDTH(DECM,GAM,IDP,*)
      INCLUDE 'PARTID'
      COMMON/CVMAX/VOLCM(NIDP,NIDP),VDMAX,VTMAX,GTMAX(7:12)
     C  ,VDPPMX,VDPNMX,VTPPMX,VTPNMX
      COMMON/COQU/FV,FD,FT,FG,FTD
C
      PARAMETER (GX=.2)
      PARAMETER (DECMX=1.5,ND=20)
      EXTERNAL WIDD
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      IF(FIRST)THEN
*       GXT=GX*FG
*       GTMAX(11)=GXT
*       GTMAX(12)=GXT
        GDMAX=0.
        gdsmax=0.
        DO I=1,ND
          DECMM=I*DECMX/ND
          GDMAX=MAX(GDMAX,WIDD(DECMM,EMC,QK))
          gdsmax=max(gdsmax,widx(decmm,emc,qk))
        ENDDO
        GDT=1.1*GDMAX*FG
        DO I=7,10
          GTMAX(I)=GDT
        ENDDO
        gdst=1.1*gdsmax*fg
        gtmax(11)=gdst
        gtmax(12)=gdst
        FIRST=.FALSE.
      ENDIF
C
      IF(DECM.LT.0.)RETURN 1
C
      IF(IDP.EQ.11.OR.IDP.EQ.12)THEN
*       GAM=GX
        gam=widx(decm,emc,qk)
      ELSEIF(IDP.GE.7.AND.IDP.LE.10)THEN
        GAM=WIDD(DECM,EMC,QK)
      ELSE
        RETURN 1
      ENDIF
C
      END


      SUBROUTINE SETPIN
      INCLUDE 'PARTID'
      COMMON/CVMAX/VOLCM(NIDP,NIDP),VDMAX,VTMAX,GTMAX(7:12)
     C  ,VDPPMX,VDPNMX,VTPPMX,VTPNMX
      COMMON/COQU/FV,FD,FT,FG,FTD
C
      PARAMETER (AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER (AM0K=AM0*AM0)
      PARAMETER (AMEC=.13957,AMEN=.13496,AME=(AMEC+AMEC+AMEN)/3.)
      PARAMETER (AMEK=AME*AME)
      PARAMETER (AM0E=AM0+AME)
      PARAMETER (AM0EK=AM0K+AMEK,AM0E4=4.*AM0K*AMEK)
C
      PARAMETER (DECMX=1.5,DDE=.01)
C
      DO IIDP=1,2
        DO JJDP=13,15
          VOLCM(IIDP,JJDP)=0.
        ENDDO
      ENDDO
      DO DECMM=DDE,DECMX,DDE
        EMC=DDE+AM0E
        S=EMC*EMC
        QK=((S-AM0EK)**2-AM0E4)/(4.*S)
        EME=SQRT(AMEK+QK)
        EM0=EMC-EME
        FVE=FV*EMC*SQRT(QK)/(EME*EM0)
        DO IIDP=1,2
          DO JJDP=13,15
            CALL SIGPIN(SIGOO,SIGIE,SIGXXX,DECMM,IIDP,JJDP)
            VOL=SIGOO*FVE
            VOLCM(IIDP,JJDP)=MAX(VOLCM(IIDP,JJDP),VOL)
          ENDDO
        ENDDO
      ENDDO
      DO IIDP=1,2
        DO JJDP=13,15
          VOLCM(JJDP,IIDP)=VOLCM(IIDP,JJDP)
        ENDDO
      ENDDO
      WRITE(*,*)((VOLCM(IIDP,JJDP),IIDP=1,2),JJDP=13,15)
C
      END


      SUBROUTINE SIGPIN(SIGO,SIGIE,SIGXX,DECM,IDP,JDP)
      INCLUDE 'NUCLS'
      PARAMETER (AMV=1.232)
      PARAMETER (AMX=1.440)
      PARAMETER (GDX=.200)
      PARAMETER (GDXK=GDX*GDX)
      PARAMETER (GDXKQ=.25*GDXK)
C
      PARAMETER (AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER (AM0K=AM0*AM0)
      PARAMETER (AMEC=.13957,AMEN=.13496,AME=(AMEC+AMEC+AMEN)/3.)
      PARAMETER (AMEK=AME*AME)
      PARAMETER (AM0E=AM0+AME)
      PARAMETER (AM0EK=AM0K+AMEK,AM0E4=4.*AM0K*AMEK)
      PARAMETER (AMVK=AMV*AMV)
      PARAMETER (AMXK=AMX*AMX)
C
      PARAMETER (QRK=((AMVK-AM0EK)**2-AM0E4)/(4.*AMVK))
      PARAMETER (QRXK=((AMXK-AM0EK)**2-AM0E4)/(4.*AMXK))
C
C
      PARAMETER (H=.19733)
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      IF(FIRST)THEN
        PI=4.*ATAN(1.)
        FSI=PI*H*H
        FIRST=.FALSE.
      ENDIF
C
      GAM=WIDD(DECM,EMC,QK)
      IF(QK.LE.0.)THEN
        SIGO=0.
        SIGXX=0.
        RETURN
      ENDIF
      GAMK=GAM*GAM
C
      gams=widx(decm,emc,qk)
      gamsk=gams*gams
      SIGD=FSI*GAMK/((EMC-AMV)**2+.25*GAMK)
*     SIGX=FSI*GDXK/((EMC-AMX)**2+GDXKQ)
      SIGX=FSI*GamsK/((EMC-AMX)**2+.25*gamsk)
      IF(NRPION)THEN
        SIGD=SIGD/QK
        SIGX=SIGX/QK
      ELSE
        SIGD=SIGD/QRK
        SIGX=SIGX/QRXK
      ENDIF
C
C  ISOSPIN ...
      IJDP=IDP+JDP
      IF(IJDP.EQ.14.OR.IJDP.EQ.17)THEN
        SIGO=SIGD+SIGD
        SIGXX=0.
      ELSEIF(IDP.EQ.14.OR.JDP.EQ.14)THEN
        SIGXX=SIGX/3.
        SIGO=SIGXX+4.*SIGD/3.
      ELSE
        SIGXX=(SIGX+SIGX)/3.
        SIGO=SIGXX+(SIGD+SIGD)/3.
      ENDIF
C PATCH FOR MAINTAINING SUFFICIENT CROSS SECTION ABOVE RESONANCE
      SIGIE=SIGO
      IF(EMC.GT.AMX)THEN
        SIGE=2.7-SIGIE           !tot cs above N* put to 27mb
        SIGE=MAX(SIGE,0.)
      ELSE
        SIGE=0.
      ENDIF
      SIGO=SIGIE+SIGE
*      sigxx=0.
C
      END


      function widx(decm,emc,qk)
      parameter (amp=.9383,amn=.9396,am0=.5*(amp+amn))
      parameter (am0k=am0*am0)
      parameter (amec=.13957,amen=.13496,ame=(amec+amec+amen)/3.)
      parameter (amek=ame*ame)
      parameter (am0e=am0+ame)
      parameter (am0ek=am0k+amek,am0e4=4.*am0k*amek)
c
      parameter (amvs=1.440)
      parameter (amvsk=amvs*amvs)
      parameter (gds=.200)
      parameter (gdskq=.25*gds*gds)
c
      parameter (qrsk=((amvsk-am0ek)**2-am0e4)/(4.*amvsk))
c
      emc=decm+am0e
      s=emc*emc
      qk=((s-am0ek)**2-am0e4)/(4.*s)
      if(qk.le.0.)then
        widx=0.
        return
      endif
c
      widx=gds*qk/qrsk*sqrt(qk/qrsk)
c
      end


      FUNCTION WIDD(DECM,EMC,QK)
      PARAMETER (AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER (AM0K=AM0*AM0)
      PARAMETER (AMEC=.13957,AMEN=.13496,AME=(AMEC+AMEC+AMEN)/3.)
      PARAMETER (AMEK=AME*AME)
      PARAMETER (AM0E=AM0+AME)
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
      EMC=DECM+AM0E
      S=EMC*EMC
      QK=((S-AM0EK)**2-AM0E4)/(4.*S)
      IF(QK.LE.0.)THEN
        WIDD=0.
        RETURN
      ENDIF
      WIDD=FG*QK*SQRT(QK/S)
      WIDD=WIDD*(FORM/(BETK+QK))**2
**    write(*,*)decm,widd
C
      END


      SUBROUTINE SIGWP(SIGE,SGET,DECM,IIDP,JJDP)
C RETURNS CROSS SECTIONS FOR N + DELTA TO N + N
C  MULTIPLIED BY PCMK (OF ANY ONE PARTICLE)
C  !!!  THIS IS WRONG C.S., FROM WRONG DETAILED BALANCE  !!!
C  DECM IS CM ENERGY EXCESS OVER MINIMUM OF 2 N MASSES + PION (FREE)
      PARAMETER (AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER (AM02=AM0+AM0)
      PARAMETER (AMEC=.13957,AMEN=.13496,AME=(AMEC+AMEC+AMEN)/3.)
      PARAMETER (AMVM=AM0+AME)
      PARAMETER (SQMIN=AM02+AME)
C
      INCLUDE 'PARTID'
      COMMON/CVMAX/VOLCM(NIDP,NIDP),VDMAX,VTMAX,GTMAX(7:12)
     C  ,VDPPMX,VDPNMX,VTPPMX,VTPNMX
      COMMON/COQU/FV,FD,FT,FG,FTD
C
      PARAMETER (DENCM=1.)
      PARAMETER (SQMAX=SQMIN+DENCM)
      PARAMETER (NI=100)
      PARAMETER (NI1=NI-1)
      DIMENSION SIGIV(0:NI,3)
C
      PARAMETER (SIGEL=15.*.1)
      PARAMETER (SIGDL=40.*.1)
C
      COMMON/RN/RN(6,6)
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      IF(FIRST)THEN
        OPEN(32,FILE='GENSGAW.DAT',FORM='FORMATTED',STATUS='OLD')
        DSSD=SQRT(SQMAX-SQMIN)/NI
        DO I=1,NI
          READ(32,*)A,SIGIV(I,1),SIGIV(I,2),SIGIV(I,3)
        ENDDO
        CLOSE(32)
        SIGIV(0,1)=SIGIV(1,1)
        SIGIV(0,2)=SIGIV(1,2)
        VOLVV=FV*2.*SIGDL
        DO IDP=7,12
          DO JDP=7,12
            VOLCM(IDP,JDP)=VOLVV
          ENDDO
        ENDDO
        DO IDP=3,6
          DO JDP=7,12
            VOLCM(IDP,JDP)=VOLVV*BAR(IDP)*RN(IDP,1)
            VOLCM(JDP,IDP)=VOLCM(IDP,JDP)
          ENDDO
        ENDDO
        WRITE(*,*)VOLCM(3,7),VOLCM(4,7),VOLCM(6,7)
        FIRST=.FALSE.
      ENDIF
C
      IF(IIDP.LE.JJDP)THEN
        IDP=IIDP
        JDP=JJDP                   !delta
      ELSE
        IDP=JJDP
        JDP=IIDP                   !delta
      ENDIF
C
      IF(IDP.LE.2)THEN
        SIGE=SIGEL
      ELSE                         !pumped up delta-delta c.s. 2/20/98
        SIGE=SIGDL
      ENDIF
      SGET=0.
C
      IF(IDP.GE.3)THEN
        IF(IDP.GE.7)RETURN
        SIGE=SIGE*BAR(IDP)*RN(IDP,1)
        RETURN
      ENDIF
C
      IF(DECM.LE.0.)RETURN
C
      IJDP=IDP+JDP
      IF(IJDP.EQ.8.OR.IJDP.EQ.12.OR.IJDP.EQ.14)RETURN
C
      SQ=DECM+SQMIN
C
      SSD=SQRT(SQ-SQMIN)
      X=SSD/DSSD
      IS=X
      DX=X-IS
      IF(IS.GE.NI)THEN
        IS=NI1
        DX=1.
      ENDIF
      IS1=IS+1
      DX1=1.-DX
C
      SGE11=SIGIV(IS,1)*DX1+SIGIV(IS1,1)*DX
      SGE10=SIGIV(IS,2)*DX1+SIGIV(IS1,2)*DX
C
      IF(IJDP.EQ.13)THEN
        SGEX=SIGIV(IS,3)*DX1+SIGIV(IS1,3)*DX
        SGET=.5*SGEX
      ELSEIF(IJDP.EQ.10)THEN
        SGET=.25*SGE11+.125*SGE10
      ELSEIF(JDP-IDP.EQ.7)THEN
        SGET=.375*SGE11
      ELSE
        SGET=.25*SGE10+.125*SGE11
      ENDIF
C
      END


      SUBROUTINE SIGEP1(IDP,AM1,JDP,AM2,DECM,SNN,SND,SNT,SDD,SIL,SILE)
C  SERVICE ROUTINE RETURNS INELASTIC CROSS SECTIONS: SIL - TOT
C  SNN - NN CHANNEL, SND - ND, SNT - NN*, SDD - DD CHANNEL,
C  ADDITIONALLY A ROUGH ELASTIC SILE IS GIVEN,
C  FOR A RESONANCE IDP COLLIDING W/CLUSTER OR RESONANCE JDP
C
C  AM1 AND AM2 ARE VACUUM MASSES, DECM IS ENERGY ABOVE THRESHOLD
C  IN ENTRANCE CHANNEL, USED ONLY FOR BARYON NOS EQ. 1
C
      INCLUDE 'PARTID'
      COMMON/CVMAX/VOLCM(NIDP,NIDP),VDMAX,VTMAX,GTMAX(7:12)
     C  ,VDPPMX,VDPNMX,VTPPMX,VTPNMX
      COMMON/COQU/FV,FD,FT,FG,FTD
C
      PARAMETER (SIGDL=40.*.1)  ! generic baryon-baryon
C
      COMMON/RN/RN(6,6)
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      IF(FIRST)THEN
        VOLVV=FV*2.*SIGDL              !2 for relative velocity
        DO IIDP=7,12
          DO JJDP=7,12
            VOLCM(IIDP,JJDP)=VOLVV
          ENDDO
        ENDDO
        DO IIDP=3,6
          DO JJDP=7,12
            VOLCM(IIDP,JJDP)=VOLVV*BAR(IIDP)*RN(IIDP,1)
            VOLCM(JJDP,IIDP)=VOLCM(IIDP,JJDP)
          ENDDO
        ENDDO
        WRITE(*,*)VOLCM(3,7),VOLCM(4,7),VOLCM(6,7)
        FIRST=.FALSE.
      ENDIF
C
      IF(JDP.GE.3.AND.JDP.LE.6)THEN
        SIL=0.
        SILE=SIGDL*BAR(JDP)*RN(JDP,1)
      ELSE
        CALL CIBAR(IDP,AM1,JDP,AM2,DECM,SNN,SND,SNT,SDD,SIL)
        SILE=MAX(0.,SIGDL-SIL)
      ENDIF
C
      END


c  SIGEP obsolete as of 6/9/99
      SUBROUTINE SIGEP(SIGE,SGET,DECM,IIDP,JJDP)
C RETURNS CROSS SECTIONS FOR N + DELTA TO N + N
C  MULTIPLIED BY ECM (TOTAL) * PCM (OF ANY ONE PARTICLE)
C  DECM IS CM ENERGY EXCESS OVER MINIMUM OF 2 N MASSES + PION (FREE)
C  SIGE IS ELASTIC NOT MULTIPLIED
      PARAMETER (AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER (AM02=AM0+AM0)
      PARAMETER (AMEC=.13957,AMEN=.13496,AME=(AMEC+AMEC+AMEN)/3.)
      PARAMETER (AMVM=AM0+AME)
      PARAMETER (SQMIN=AM02+AME)
C
      INCLUDE 'PARTID'
      COMMON/CVMAX/VOLCM(NIDP,NIDP),VDMAX,VTMAX,GTMAX(7:12)
     C  ,VDPPMX,VDPNMX,VTPPMX,VTPNMX
      COMMON/COQU/FV,FD,FT,FG,FTD
C
      PARAMETER (DENCM=1.)
      PARAMETER (SQMAX=SQMIN+DENCM)
      PARAMETER (NI=100)
      PARAMETER (NI1=NI-1)
      DIMENSION SIGIV(0:NI,3)
C
      PARAMETER (SIGEL=15.*.1)
      PARAMETER (SIGDL=40.*.1)
C
      COMMON/RN/RN(6,6)
c
c      common/sgpach/amvip
c      parameter (am0k=am0*am0)
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      IF(FIRST)THEN
*       OPEN(31,FILE='GENSGAF.DAT',FORM='FORMATTED',STATUS='OLD')
        OPEN(31,FILE='GENSGAV.DAT',FORM='FORMATTED',STATUS='OLD')
        DSSD=SQRT(SQMAX-SQMIN)/NI
        DO I=1,NI
          READ(31,*)A,SIGIV(I,1),SIGIV(I,2),SIGIV(I,3)
        ENDDO
        CLOSE(31)
        SIGIV(0,1)=SIGIV(1,1)
        SIGIV(0,2)=SIGIV(1,2)
        VOLVV=FV*2.*SIGDL              !2 for relative velocity?
        DO IDP=7,12
          DO JDP=7,12
            VOLCM(IDP,JDP)=VOLVV
          ENDDO
        ENDDO
        DO IDP=3,6
          DO JDP=7,12
            VOLCM(IDP,JDP)=VOLVV*BAR(IDP)*RN(IDP,1)
            VOLCM(JDP,IDP)=VOLCM(IDP,JDP)
          ENDDO
        ENDDO
        WRITE(*,*)VOLCM(3,7),VOLCM(4,7),VOLCM(6,7)
        FIRST=.FALSE.
      ENDIF
C
      IF(IIDP.LE.JJDP)THEN         ! idp.le.jdp
        IDP=IIDP
        JDP=JJDP                   !delta
      ELSE
        IDP=JJDP
        JDP=IIDP                   !delta
      ENDIF
C
      IF(IDP.LE.2)THEN
        SIGE=SIGEL
      ELSE                         !pumped up delta-delta c.s. 2/20/98
        SIGE=SIGDL
      ENDIF
      SGET=0.
C
      IF(IDP.GE.3)THEN
        IF(IDP.GE.7)RETURN
        SIGE=SIGE*BAR(IDP)*RN(IDP,1)
        RETURN
      ENDIF
C
      IF(DECM.LE.0.)RETURN
C
c     if(jdp.ge.7.and.jdp.le.10)then
c       ecm=decm+sqmin
c       eic=.5*ecm
c       call sigi(sig11,sig10,sigx,eic-am0)         !inelastic cross section
c       sigit=sig11+.5*sig10
c       sigit=sigit*1.5
c       s=ecm*ecm
c       pcmk=((s-am0k-amvip*amvip)**2-4.*amvip*amvip*am0k)/(4.*s)
c       pick=eic*eic-am0k
c       if(pcmk.gt.0.)sige=sigit*amvip/am0*sqrt(pick/pcmk)
c     endif
C
      IJDP=IDP+JDP
      IF(IJDP.EQ.8.OR.IJDP.EQ.12.OR.IJDP.EQ.14)RETURN
C
      SQ=DECM+SQMIN
C
      SSD=SQRT(SQ-SQMIN)
      X=SSD/DSSD
      IS=X
      DX=X-IS
      IF(IS.GE.NI)THEN
        IS=NI1
        DX=1.
      ENDIF
      IS1=IS+1
      DX1=1.-DX
C
      SGE11=SIGIV(IS,1)*DX1+SIGIV(IS1,1)*DX
      SGE10=SIGIV(IS,2)*DX1+SIGIV(IS1,2)*DX
C
      IF(IJDP.EQ.13)THEN
        SGEX=SIGIV(IS,3)*DX1+SIGIV(IS1,3)*DX
        SGET=.5*SGEX
      ELSEIF(IJDP.EQ.10)THEN
        SGET=.25*SGE11+.125*SGE10
      ELSEIF(JDP-IDP.EQ.7)THEN
        SGET=.375*SGE11
      ELSE
        SGET=.25*SGE10+.125*SGE11
      ENDIF
C
      END


      FUNCTION FMAZ(X)
      COMMON/CFMAZ/ECM,CDECM,CSTAR
      LOGICAL CSTAR
      PARAMETER (AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER (AM0K=AM0*AM0)
      PARAMETER (AMEC=.13957,AMEN=.13496,AME=(AMEC+AMEC+AMEN)/3.)
      PARAMETER (AMEK=AME*AME)
      PARAMETER (AMVM=AM0+AME)
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
      PARAMETER (AMVS=1.440)
      PARAMETER (AMVSK=AMVS*AMVS)
      PARAMETER (GDS=.200)
C
      PARAMETER (QRSK=((AMVSK-AM0EK)**2-AM0E4)/(4.*AMVSK))
C
      AMVII=AMVM+X*CDECM
      SV=AMVII*AMVII
      QK=((SV-AM0EK)**2-AM0E4)/(4.*SV)
      IF(QK.LE.0.)THEN
        FMAZ=0.
        RETURN
      ENDIF
C
      IF(CSTAR)THEN
        GDX=GDS*QK/QRSK*SQRT(QK/QRSK)
        FMAZ=GDX/((AMVII-AMVS)**2+.25*GDX*GDX)
      ELSE
        GD=FG*QK*SQRT(QK/SV)
        GD=GD*(FORM/(BETK+QK))**2
        FMAZ=GD/((AMVII-AMV)**2+.25*GD*GD)
      ENDIF
C
      END


      SUBROUTINE GETMAZ(AMVII,DECM,STAR)
      use global
      LOGICAL STAR
C  RETURNS THE FREE MASS OF A DELTA
C  ACCORDING TO WRONG ASSUMPTIONS IN THE LITERATURE
C  DECM IS CM ENERGY EXCESS OVER MINIMUM OF 2 N MASSES + PION (FREE)
      PARAMETER (AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER (AMEC=.13957,AMEN=.13496,AME=(AMEC+AMEC+AMEN)/3.)
      PARAMETER (AMVM=AM0+AME)
      PARAMETER (ECMM=AM0+AMVM)
      COMMON/CSEED/ISEED
C
      PARAMETER (NIT=19)
C
      COMMON/CFMAZ/ECM,CDECM,CSTAR
      LOGICAL CSTAR
C
      DATA XP/.5/
C
      ECM=ECMM+DECM
      CDECM=DECM
      CSTAR=STAR
C
      FP=FMAZ(XP)
      DO 30 IT=1,NIT
      X=getRan()
      FX=FMAZ(X)
      IF(FX.LT.FP)THEN
        IF(FP*getRan().GT.FX)GOTO 30
      ENDIF
      XP=X
      FP=FX
 30   CONTINUE
C
      AMVII=AMVM+DECM*XP
C
      END


      SUBROUTINE GETMASS(AMVII,*,DECM,STAR)
      use global
      LOGICAL STAR
C  RETURNS THE FREE MASS OF A DELTA
C  DECM IS CM ENERGY EXCESS OVER MINIMUM OF 2 N MASSES + PION (FREE)
      PARAMETER (AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER (AMEC=.13957,AMEN=.13496,AME=(AMEC+AMEC+AMEN)/3.)
      PARAMETER (AMVM=AM0+AME)
      PARAMETER (ECMM=AM0+AMVM)
      PARAMETER (AM0K=AM0*AM0)
      PARAMETER (AMVMK=AMVM*AMVM)
      PARAMETER (AM0VMK=AM0K+AMVMK,AM0VM4=4.*AM0K*AMVMK)
      COMMON/CSEED/ISEED
C
      PARAMETER (NIT=19)
C
      COMMON/CFMASS/ECM,PCMX,CSTAR
      LOGICAL CSTAR
C
      DATA XP/.5/
C
      ECM=ECMM+DECM
      ECMK=ECM*ECM
      CSTAR=STAR
C
      PCMXK=((ECMK-AM0VMK)**2-AM0VM4)/(4.*ECMK)
      IF(PCMXK.LT.0.)RETURN 1
      PCMX=SQRT(PCMXK)
C
      FP=FMASS(XP)
      DO 30 IT=1,NIT
      X=getRan()
      FX=FMASS(X)
      IF(FX.LT.FP)THEN
        IF(FP*getRan().GT.FX)GOTO 30
      ENDIF
      XP=X
      FP=FX
 30   CONTINUE
C
      PCM=XP*PCMX
      PCMK=PCM*PCM
      OMN=SQRT(AM0K+PCMK)
      OMV=ECM-OMN
      AMVII=SQRT(OMV*OMV-PCMK)
C
      END


      FUNCTION FMASS(X)
      COMMON/CFMASS/ECM,PCMX,CSTAR
      LOGICAL CSTAR
      PARAMETER (AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER (AM0K=AM0*AM0)
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
      PARAMETER (AMVS=1.440)
      PARAMETER (AMVSK=AMVS*AMVS)
      PARAMETER (GDS=.200)
      PARAMETER (GDSKQ=.25*GDS*GDS)
C
      PARAMETER (QRSK=((AMVSK-AM0EK)**2-AM0E4)/(4.*AMVSK))
C
      PCM=X*PCMX
      PCMK=PCM*PCM
C
      OMN=SQRT(PCMK+AM0K)
      OOMV=ECM-OMN
      SV=OOMV*OOMV-PCMK
      QK=((SV-AM0EK)**2-AM0E4)/(4.*SV)
      IF(QK.LE.0.)THEN
        FMASS=0.
        RETURN
      ENDIF
C
      FMASS=PCMK/OMN
C
      IF(CSTAR)THEN
        OMV=SQRT(PCMK+AMVSK)
*       FMASS=FMASS/OMV*GDS/((OOMV-OMV)**2+GDSKQ)
        GDX=GDS*QK/QRSK*SQRT(QK/QRSK)
        FMASS=FMASS/OMV*GDX/((OOMV-OMV)**2+.25*GDX*GDX)
      ELSE
        GD=FG*QK*SQRT(QK/SV)
        GD=GD*(FORM/(BETK+QK))**2
        OMV=SQRT(PCMK+AMVK)
        FMASS=FMASS/OMV*GD/((OOMV-OMV)**2+.25*GD*GD)
      ENDIF
C
      END


      SUBROUTINE SIGI(SIG11,SIG10,SIGX,DECM)
C  CALCULATES INELASTIC NN CROSS SECTION
C  DECM IS CM ENERGY OF ONE NUCLEON MINUS ITS MASS
      PARAMETER (AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER (AMEC=.13957,AMEN=.13496,AME=(AMEC+AMEC+AMEN)/3.)
      PARAMETER (AM02=AM0+AM0)
      PARAMETER (AM0K=AM0*AM0)
      PARAMETER (SQMIN=AM02+AME)
C
      PARAMETER (DENCM=1.)
      PARAMETER (SQMAX=SQMIN+DENCM)
      PARAMETER (NI=100)
      PARAMETER (NI1=NI-1)
      DIMENSION SIGIV(0:NI,3)
C
      PARAMETER (H=.19733)
C
C   VERWEST AND ARNDT PRC 25, 1979 (1982)
      DIMENSION ALPHA(4),BETA(4),AMV0(4),GAM(4)
      DATA ALPHA/3.772,15.28,6.03 ,146.3/
      DATA BETA/ 1.262,0.   ,1.7  ,0./
      DATA AMV0/ 1.188,1.245,1.203,1.472/
      DATA GAM/ .09902,.1374,.1343,.02649/
C
      DIMENSION SIGIVE(4)
      PARAMETER (TCRI=.85)
C
      PARAMETER (D0MEK=(AM0-AME)**2)
      PARAMETER (U0MEK=(AM0+AME)**2)
C
      PARAMETER (AMVVV=1.220,GAMVV=.12)       !!horror
      PARAMETER (AMVVH=1.430,GAMVH=.20)       !!horror
C
      PARAMETER (NTLI=6)
      DIMENSION TLI(NTLI),CSPP(NTLI),CSPN(NTLI)
      DATA TLI/ .75,1.  ,1.25,1.5,2.11,2.6 /
      DATA CSPP/21.,23.5,24.5,24.,28. ,30.3/
      DATA CSPN/11.,12. ,13. ,15.,28. ,30.3/
C
      LOGICAL FIRST
c
      real*8 s,dssdk,pk,tla,sqs,zp,amvs,zph,amvhs,pzm,pzmh,azm,azmh
      real*8 qks,amvsk,amvhsk,amv0k,qk0,zm,zmh,a
C
C  FUNCTIONS
      QK(A)=(A-D0MEK)*(A-U0MEK)/(4.*A)
      PRK(A)=(A-(AM0-AMVS)**2)*(A-(AM0+AMVS)**2)/(4.*A)
C
      DATA FIRST/.TRUE./
C
      IF(FIRST)THEN
        PI=4.*ATAN(1.)
        FSIG=PI*H*H*.5
        ZM=(AM0+AME-AMVVV)*2./GAMVV
        AZM=ATAN(ZM)
        PZM=1./(1.+ZM*ZM)
        ZMH=(AM0+AME-AMVVH)*2./GAMVH
        AZMH=ATAN(ZMH)
        PZMH=1./(1.+ZMH*ZMH)
        DSSD=SQRT(SQMAX-SQMIN)/NI
        DSSDK=DSSD*DSSD
        SIGIV(0,1)=0.
        SIGIV(0,2)=0.
        DO I=1,NI
          S=(SQMIN+I*I*DSSDK)**2
          PK=.25*S-AM0K
          TLA=(PK+PK)/AM0
          SQS=SQRT(S)
          ZP=(SQS-AM0-AMVVV)*2./GAMVV
          AMVS=AMVVV+GAMVV*.25/(ATAN(ZP)-AZM)*LOG((1.+ZP*ZP)*PZM)
          AMVSK=AMVS*AMVS
          QKS=QK(AMVSK)
          QS=SQRT(QKS)
          ZPH=(SQS-AM0-AMVVH)*2./GAMVH
          AMVHS=AMVVH+GAMVH*.25/(ATAN(ZPH)-AZMH)*LOG((1.+ZPH*ZPH)*PZMH)
          AMVHSK=AMVHS*AMVHS
          QKHS=QK(AMVHSK)
          QHS=SQRT(QKHS)
          PRKS=PRK(S)
          PRS=SQRT(PRKS)
          DO II=1,4
            AMV0K=AMV0(II)**2
            QK0=QK(AMV0K)
            Q0=SQRT(QK0)
            AMVGK=AMV0K*GAM(II)**2
            SIGIVE(II)=FSIG/PK*ALPHA(II)*AMVGK/(QK0*Q0)
            IF(II.EQ.4)THEN
              SIGIVE(II)=SIGIVE(II)*QKHS*QHS/((AMVHSK-AMV0K)**2+AMVGK)
            ELSE
              SIGIVE(II)=SIGIVE(II)*QKS*QS/((AMVSK-AMV0K)**2+AMVGK)
            ENDIF
            IF(II.EQ.1.OR.II.EQ.3)THEN
              S0=(AM0+AMV0(II))**2
              P0K=.25*S0-AM0K
              P0=SQRT(P0K)
              SIGIVE(II)=SIGIVE(II)*(PRS/P0)**BETA(II)
            ENDIF
          ENDDO
          SPP=2.*SIGIVE(1)+SIGIVE(2)
          IF(TLA.LT.TCRI)THEN
            FS=(SPP+SIGIVE(3))/SPP     !+ sig10d
            SIGIV(I,1)=SIGIVE(1)*FS    !modified sig11
            SIGIV(I,2)=SIGIVE(2)*FS    !modified sig10
            SIGIV(I,3)=1.5*SIGIVE(4)   !+ 3/2 *sig01
          ELSE
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
            FS=(DTL*CSPP(ITT1)+DTL1*CSPP(ITT))*.1
            SIGIV(I,3)=(DTL*CSPN(ITT1)+DTL1*CSPN(ITT))*.1-.5*FS
            FS=FS/SPP
            SIGIV(I,1)=SIGIVE(1)*FS
            SIGIV(I,2)=SIGIVE(2)*FS
          ENDIF
        ENDDO
        FIRST=.FALSE.
      ENDIF
C
      SQ=DECM+DECM+AM02
      IF(SQ.LE.SQMIN)THEN
        SIG11=0.
        SIG10=0.
        SIGX=0.
        RETURN
      ENDIF
C
      SSD=SQRT(SQ-SQMIN)
      X=SSD/DSSD
      IS=X
      DX=X-IS
      IF(IS.GE.NI)THEN
        IS=NI1
        DX=1.
      ENDIF
      DX1=1.-DX
      IS1=IS+1
C
      SIG11=SIGIV(IS,1)*DX1+SIGIV(IS1,1)*DX
      SIG10=SIGIV(IS,2)*DX1+SIGIV(IS1,2)*DX
      SIGX=SIGIV(IS,3)*DX1+SIGIV(IS1,3)*DX
*      sigx=0.
C
      END


      FUNCTION FDEU(PMX)
C  SISTER OF SIGDB
C  CORRECTION FACTOR ON TOP OF IMPULSE APPROXIMATION
      PARAMETER (NPCM=50)
      PARAMETER (PCMXX=1.5)
      PARAMETER (DPCMX=PCMXX/NPCM)
      DIMENSION FINT(0:NPCM)
      INCLUDE 'PARTID'
      COMMON/CVMAX/VOLCM(NIDP,NIDP),VDMAX,VTMAX,GTMAX(7:12)
     C  ,VDPPMX,VDPNMX,VTPPMX,VTPNMX
      PARAMETER (AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER (AM02=AM0+AM0,AM0K=AM0*AM0)
      PARAMETER (AM02K=AM02*AM02)
      PARAMETER (BD=.002225,AMD=AM0+AM0-BD)
      PARAMETER (AMDK=AMD*AMD)
      PARAMETER (AMDKK=AMDK+AM0K)
      PARAMETER (AM4KDK=4.*AMDK*AM0K)
      PARAMETER (AMD0K=AMDK*AM0K)
      COMMON/COQU/FV,FD,FT,FG,FTD
C
      PARAMETER (PCT=.035)   !momentum cut (relevant space is important)
C
      LOGICAL FIRST
C
      DATA FIRST/.TRUE./
C
      IF(FIRST)THEN
        FIRST=.FALSE.
        OPEN(33,FILE='DEB.DAT',STATUS='OLD')
        DO I=1,NPCM
          READ(33,*)A,A,FINT(I)
        ENDDO
        CLOSE(33)
        FINT(0)=FINT(1)
        VDMAX=0.
        VDPPMX=0.
        VDPNMX=0.
        DO I=1,NPCM
          PCMX=I*DPCMX
          PCMXK=PCMX*PCMX
          ECX=SQRT(AM0K+PCMXK)
          ECM=SQRT(AM02K+PCMXK)+ECX
          S=ECM*ECM
          PDK=((S-AMDKK)**2-AM4KDK)/(4.*S)
          PD=SQRT(PDK)
          ED=SQRT(PDK+AMDK)
          E12TMX=ECM-AM0
          E12MX=.5*E12TMX
          P12MX=SQRT(E12MX*E12MX-AM0K)
          DP12=P12MX/NPCM
          FI=FINT(I)
          DO J=1,NPCM-1
            P12=J*DP12
            E12K=P12*P12+AM0K
            E12=SQRT(E12K)
            PSK=(S+AM0K-4.*E12K)**2/(4.*S)-AM0K
            PS=SQRT(PSK)
            ES=SQRT(AM0K+PSK)
            PMAX=SQRT((ED*ES+PD*PS)**2-AMD0K)/AMD
            AG=(ED*ES-PD*PS)**2-AMD0K
            IF(AG.GT.0.)THEN
              PMIN=SQRT(AG)/AMD
            ELSE
              PMIN=0.
            ENDIF
            VOL=(VOLDE(PMIN)-VOLDE(PMAX))/(ECM*PS*ES)*FI
            VDMAX=MAX(VDMAX,VOL)
            EP12=(E12+E12)*P12
            IF(P12.GT.PCT.AND.PS.GT.PCT)THEN
              VDPP=VOL*SIG(EP12,1,1)
              VDPPMX=MAX(VDPPMX,VDPP)
              VDPN=VOL*SIG(EP12,1,2)
              VDPNMX=MAX(VDPNMX,VDPN)
            ENDIF
          ENDDO
        ENDDO
        FAC=FV*FD*2.*AMDK
        VDMAX=VDMAX*FAC
        VDPPMX=SQRT(VDPPMX*FAC*2.)
        VDPNMX=SQRT(VDPNMX*FAC)
        WRITE(*,*)'VDMAX = ',VDMAX
        WRITE(*,*)'VDPPMX = ',VDPPMX,'  VDPNMX = ',VDPNMX
      ENDIF
C
      X=PMX/DPCMX
      I=X
      IF(I.GE.NPCM)THEN
        FDEU=1.
        RETURN
      ENDIF
      DX=X-I
      FDEU=FINT(I)*(1.-DX)+FINT(I+1)*DX
C
      END


      FUNCTION SIGDB(PMX)
C  CALCULATES INELASTIC CROSS SECTION
C  SISTER OF FDEU
      PARAMETER (NPCM=50)
      PARAMETER (PCMXX=1.5)
      PARAMETER (DPCMX=PCMXX/NPCM)
      DIMENSION SIGD(0:NPCM)
C
      LOGICAL FIRST
C
      DATA FIRST/.TRUE./
C
      IF(FIRST)THEN
        FIRST=.FALSE.
        OPEN(33,FILE='DEB.DAT',STATUS='OLD')
        DO I=1,NPCM
          READ(33,*)A,SIGD(I)
          SIGD(I)=.1*SIGD(I)
        ENDDO
        CLOSE(33)
        SIGD(0)=0.
      ENDIF
C
      X=PMX/DPCMX
      I=X
      IF(I.GE.NPCM)THEN
        SIGDB=SIGD(NPCM)
        RETURN
      ENDIF
      DX=X-I
      SIGDB=SIGD(I)*(1.-DX)+SIGD(I+1)*DX
C
      END


      FUNCTION SIGTB(PMX,IDP,JDP)
C  CALCULATES CROSS SECTION T/3HE + N --> 4N
C  IN FM**2
      PARAMETER (NPCM=80)
      PARAMETER (PCMXX=1.4)
      PARAMETER (DPCMX=PCMXX/NPCM)
      DIMENSION SIGTN(0:NPCM),SIGTP(0:NPCM)
      PARAMETER (NPCM1=NPCM-1)
C
      LOGICAL FIRST
C
      DATA FIRST/.TRUE./
C
      IF(FIRST)THEN
        FIRST=.FALSE.
        OPEN(33,FILE='TEBB.DAT',STATUS='OLD')
        DO I=1,NPCM
          READ(33,*)A,SIGTN(I),SIGTP(I)
          SIGTN(I)=.1*SIGTN(I)
          SIGTP(I)=.1*SIGTP(I)
        ENDDO
        CLOSE(33)
        SIGTN(0)=0.
        SIGTP(0)=0.
      ENDIF
C
      X=PMX/DPCMX
      I=X
      IF(I.GE.NPCM)THEN
        I=NPCM1
        X=NPCM
      ENDIF
      DX=X-I
      IF(IDP+JDP.EQ.6)THEN
        SIGTB=SIGTP(I)*(1.-DX)+SIGTP(I+1)*DX
      ELSE
        SIGTB=SIGTN(I)*(1.-DX)+SIGTN(I+1)*DX
      ENDIF
C
      END


      SUBROUTINE TRIMAX
C  FINDS MAXIMUM COLLISIONAL VOLUME (**2) FOR TRITON/HE3 PRODUCTION
      PARAMETER (AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER (AM0K=AM0*AM0)
      PARAMETER (BT=.0086,AMT=AMP+AMN+AMN-BT)
      PARAMETER (B3E=.0080,AM3E=AMP+AMP+AMN-B3E)
      PARAMETER (AMTE=.5*(AMT+AM3E))
      PARAMETER (AMTEK=AMTE*AMTE)
      PARAMETER (AMTEKK=AMTEK+AM0K)
      PARAMETER (AM4KTK=4.*AMTEK*AM0K)
C
      PARAMETER (NCM=20)
      PARAMETER (PCMAX=.9,DPCM=PCMAX/NCM)   !maximum tot mo of spectators
C
      COMMON/COQU/FV,FD,FT,FG,FTD
C
      INCLUDE 'PARTID'
      COMMON/CVMAX/VOLCM(NIDP,NIDP),VDMAX,VTMAX,GTMAX(7:12)
     C  ,VDPPMX,VDPNMX,VTPPMX,VTPNMX
C
      PARAMETER (PCU=.055)                  !for the 4 bodies
C
      VTMAX=0.
      VTPPMX=0.
      VTPNMX=0.
      DO ICM=1,NCM
        PCMX=ICM*DPCM
        PCMXK=PCMX*PCMX
        ECX=SQRT(PCMXK+4.*AM0K)            !assume max for zero relative
        ECM=ECX+ECX
        S=ECM*ECM
        PTK=((S-AMTEKK)**2-AM4KTK)/(4.*S)
        PT=SQRT(PTK)
        ET=SQRT(AMTEK+PTK)
        FI=FTRI(ECM-4.*AM0)
        AG=AM0*(ECM-4.*AM0)     !nrelat
        AG=MAX(AG,0.)
        P34MX=SQRT(AG)          !from teb.for
        DP34=P34MX/NCM
        DO I=1,NCM-1
          P34=I*DP34
          P34K=P34*P34
          E34=AM0+AM0+P34K/AM0
          E34K=E34*E34
          PL34MX=SQRT(((S+4.*AM0K-E34K)/(2.*ECM))**2-4.*AM0K)
          DPL34=PL34MX/NCM
          DO J=1,NCM-1
            PL34=J*DPL34
            PL34K=PL34*PL34
            E3K=AM0K+P34K+.25*PL34K
            EL34=SQRT(E34K+PL34K)
            E12K=((ECM-EL34)**2-PL34K)/4.
            E12=SQRT(E12K)
            P12K=E12K-AM0K
            E1K=AM0K+P12K+.25*PL34K
            P12=SQRT(P12K)
            PMINK=((ET*EL34-PT*PL34)/AMTE)**2-4.*AM0K
            PMAXK=((ET*EL34+PT*PL34)/AMTE)**2-4.*AM0K
            VOL=(VOLTI(1.5*PMINK+P12K+P12K)
     V        -VOLTI(1.5*PMAXK+P12K+P12K))/(PL34*ECM)*FI/(E1K*E3K)
            VOL=VOL*E12K
*           if(vol.gt.vtmax)p34m=p34
            VTMAX=MAX(VTMAX,VOL)
            IF(P12.GT.PCU.AND.PL34.GT.PCU+PCU.AND.P34.GT.PCU)THEN
              EP12=(E12+E12)*P12
              VTPP=VOL*SIG(EP12,1,1)
              if(vtpp.gt.vtppmx)then
                decm=ecm-(am0+amt)
                p34mmx=p34
                pl34mmx=pl34
                p12mx=p12
              endif
              VTPPMX=MAX(VTPPMX,VTPP)
              VTPN=VOL*SIG(EP12,1,2)
              VTPNMX=MAX(VTPNMX,VTPN)
            ENDIF
          ENDDO
        ENDDO
      ENDDO
      FAC=FV*FT*3.*2./AM0K*AM0K*AM0K
      VTMAX=VTMAX*FAC
      VTPPMX=(VTPPMX*FAC*2.)**(1./3.)
      VTPNMX=(VTPNMX*FAC*2.)**(1./3.)
      WRITE(*,*)'VTMAX = ',VTMAX
      WRITE(*,*)'VTPPMX = ',VTPPMX,'  VTPNMX = ',VTPNMX
      write(*,*)'decm = ',decm,'  p12mx = ',p12mx
      write(*,*)'p34mmx = ',p34mmx,'  pl34mmx = ',pl34mmx
C
      END


      FUNCTION FTRI(DECM)
C  CORRECTION FACTOR ON TOP OF IMPULSE APPROXIMATION
C  FOR TRITON/HE3 PRODUCTION/BREAK UP
C   DECM IS CM ENERGY - 4 NUCLEON MASSES
C
*      FTRI=(.65*DECM+.0033)/(DECM+.035)
      FTRI=.1+.8*decm/(decm+.015)
C
      END


      FUNCTION VOLTI(PK)
      PARAMETER (PMX=1.02)
      PARAMETER (PMXK=PMX*PMX)
      PARAMETER (NP=120)
      PARAMETER (DP=PMX/NP)
C
      DIMENSION VINT(0:NP)
C
      LOGICAL FIRST
C
      DATA FIRST/.TRUE./
C
      IF(FIRST)THEN
        OPEN(33,FILE='GENFIT.DAT',STATUS='OLD')
        DO I=0,NP
          READ(33,*)A,VINT(I)
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
        VOLTI=0.
        RETURN
      ENDIF
      DX=X-I
      VOLTI=VINT(I)*(1.-DX)+VINT(I+1)*DX
C
      END


      FUNCTION VOLDE(P)
      PARAMETER (PDMX=2.)
      PARAMETER (NPD=120)
      PARAMETER (DPD=PDMX/NPD)
      DIMENSION VINT(0:NPD)
C
      LOGICAL FIRST
C
      DATA FIRST/.TRUE./
C
      IF(FIRST)THEN
        OPEN(33,FILE='GENFIN.DAT',STATUS='OLD')
        DO I=0,NPD
          READ(33,*)A,VINT(I)
        ENDDO
        CLOSE(33)
        FIRST=.FALSE.
      ENDIF
C
      X=P/DPD
      I=X
      IF(I.GE.NPD)THEN
        VOLDE=0.
        RETURN
      ENDIF
      DX=X-I
      VOLDE=VINT(I)*(1.-DX)+VINT(I+1)*DX
C
      END


      subroutine enma(pfx,pfy,pfz,ef,idpf,sg)
*     SUBROUTINE ENMA(PFX,PFY,PFZ,EF,IDPF)
*     SUBROUTINE ENMA(PFX,PFY,PFZ,EF,IDPF,SG)
C  ENTROPY MANAGEMENT
      COMMON/CENTRO/ENGAIN,ENTROT,EDGAIN,EDTROT,DNCELL,DDCELL
     C  ,EBGAIN,EBTROT,DBCELL
     e  ,eptro,eptrog,anuc,anucg
      common/sma/f,s
C
      CALL PENTRO(S,PFX,PFY,PFZ,EF,IDPF)
*e*      write(24,'(a,i3,1x,4f10.4)')'id pxyz s ',idpf,pfx,pfy,pfz,s
      DNCELL=DNCELL+S
*      IF(IDPF.EQ.3)DDCELL=DDCELL+S
      IF(IDPF.GE.3.and.idpf.le.6)DDCELL=DDCELL+S
**    IF(IDPF.LE.9)DBCELL=DBCELL+S
      IF(IDPF.LE.6)DBCELL=DBCELL+S
*      IF(IDPF.LE.2)DBCELL=DBCELL+S
      sg=s
C
      END


      SUBROUTINE PENTRO(S,PFX,PFY,PFZ,EF,IDPF)
      INCLUDE 'NUCLS'
      INCLUDE 'PARTID'
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
      DIMENSION TCM(2),TCNM(2,2)
      common/sma/f,ss
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      IF(FIRST)THEN
        SMAX=2.5+2.55+1.5*LOG(TLAB)
        SMAX=MAX(SMAX,2.4)
        SMAX=SMAX*2.2
        SMAX2=SMAX+SMAX
      ENDIF
C
      DO IPT=1,2
        TCM(IPT)=TC(IPT)
        TC(IPT)=MAX(.006,TC(IPT))
*       TC(IPT)=MAX(.002,TC(IPT))
        IF(ASYII)THEN
          DO IIDN=1,2
            TCNM(IPT,IIDN)=TCN(IPT,IIDN)
            TCN(IPT,IIDN)=MAX(.006,TCN(IPT,IIDN))
          ENDDO
        ENDIF
      ENDDO
C
      IF(IDPF.GE.3.AND.IDPF.LE.6)THEN
        BARI=BAR(IDPF)
        CALL DSTR(FP,PFX,PFY,PFZ,EF,1,1./BARI,ASYII)
        CALL DSTR(FN,PFX,PFY,PFZ,EF,2,1./BARI,ASYII)
        IZP=NINT(ZPA(IDPF))
        F=FP**IZP*FN**(NINT(BARI)-IZP)
      ELSE
        CALL DSTR(F,PFX,PFY,PFZ,EF,IDPF,1.,ASYII)
        if(f.lt.1e-5*fmax)call getnu(f,pfx,pfy,pfz,idpf,asyii)
      ENDIF
*e*      write(24,*)'f ',f
C
      DO IPT=1,2
        TC(IPT)=TCM(IPT)
        IF(ASYII)THEN
          DO IIDN=1,2
            TCN(IPT,IIDN)=TCNM(IPT,IIDN)
          ENDDO
        ENDIF
      ENDDO
C
      IF(F.LE.1E-9)GOTO 13
C
      S=-LOG(F)
      IF(IDPF.EQ.3.OR.IDPF.GE.13)THEN
        IF(F.GT.1E-2)THEN
          S=S+(1.+F)/F*LOG(1.+F)
        ELSE
          S=S+1.
        ENDIF
      ELSE
        IF(F.LT..9999)THEN
          IF(F.GT.1E-2)THEN
            S=S-(1.-F)/F*LOG(1.-F)
          ELSE
            S=S+1.
          ENDIF
        ENDIF
      ENDIF
      IF(S.LT.SMAX)RETURN
      IF(S.LT.SMAX2.AND.IDPF.GE.3)RETURN
* 13   S=13.
*      IF(IDPF.GE.3)S=25.
 13   S=0.
C
      END


      SUBROUTINE DEU(FCR,BREAK,PFX,PFY,PFZ,EF,SP,IDPF)
      use global
      LOGICAL BREAK
      COMMON/CSPC/EM0K,PKMX,ECM,EDC,PIC                !for deuterons
      COMMON/GETD/EDKC,PDKC,EMDK,EMD0K
     G  ,VNX,VPMN,VPMX,CTHD
C
      COMMON/CLOPA/IX,IY,IZ,CALO
      LOGICAL CALO
      COMMON/CCLOPA/IXB,IXF,IYB,IYF,IZB,IZF,XC,YC,ZC,IIVOV
      COMMON/CLOPAC/IIXB,IIXF,IIYB,IIYF,IIZB,IIZF,IVOV,FPT
C
      INCLUDE 'PARTIC15'
      INCLUDE 'SIZE2'
C
      PARAMETER (AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER (BD=.002225,AMD=AM0+AM0-BD)
      PARAMETER (H=.19733)
C
      COMMON/CPA/RHOPAM,UUP
C
      PARAMETER (PDCU=.176)
      PARAMETER (PDCUK=PDCU*PDCU)
      PARAMETER (PDCUK4=4.*PDCUK)
      PARAMETER (PDCUQ=PDCU*PDCUK)
C
      COMMON/CSEED/ISEED
C
      PARAMETER (NSI=4)
      PARAMETER (NTRY=NSI*NSI*NSI)
C
      DIMENSION PCXI(NSI,NSI,NSI),PCYI(NSI,NSI,NSI),PCZI(NSI,NSI)
      DIMENSION PCMKI(NSI)
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      IF(FIRST)THEN
        PI=4.*ATAN(1.)
        PIPI=PI+PI
        DO I=1,NSI
          PCQ=PDCUQ*(I-.5)/NSI
          PCM=PCQ**(1./3.)
          PCMKI(I)=PCM*PCM
          DO J=1,NSI
            CTH=1.-2.*(J-.5)/NSI
            STH=SQRT(1.-CTH*CTH)
            PCZI(J,I)=PCM*CTH
            DO K=1,NSI
              PHI=PIPI*(K-.5)/NSI
              PCXI(K,J,I)=PCM*STH*COS(PHI)
              PCYI(K,J,I)=PCM*STH*SIN(PHI)
            ENDDO
          ENDDO
        ENDDO
        FR=(PIPI*H)**3/4./VOLE/(4./3.*PI*PDCU**3)/NQU
        PFR=1./FR
        FIRST=.FALSE.
      ENDIF
C
      IBAR=NINT(BAR(IDPF))
      IZPA=NINT(ZPA(IDPF))
C
      EMD=AMS(IDPF)+IBAR*UUP                   !regular deuteron mass
      BFX=PFX/EMD                              !gamma times beta...
      BFY=PFY/EMD
      BFZ=PFZ/EMD
      GMF=EF/EMD                               !gamma
C
      FSUCR=NTRY*FCR
C
      FSU=0.
      KK=NINT(getRan()*IBAR)
      DO I=1,NSI
        ECMM=SQRT(PCMKI(I)+EM0K)
        DO J=1,NSI
          PCZ=PCZI(J,I)
          DO K=1,NSI
            PCX=PCXI(K,J,I)
            PCY=PCYI(K,J,I)
            BPI=BFX*PCX+BFY*PCY+BFZ*PCZ
            EL=GMF*ECMM+BPI
            BCPI=ECMM+BPI/(GMF+1.)
            PLX=PCX+BFX*BCPI
            PLY=PCY+BFY*BCPI
            PLZ=PCZ+BFZ*BCPI
            KK=KK+1
            IDPK=MOD(KK,IBAR)+1
            IF(IDPK.LE.IZPA)THEN
              IDPK=1
            ELSE
              IDPK=2
            ENDIF
            CALL DSTR(F,PLX,PLY,PLZ,EL,IDPK,1.,ASYII)
            FSU=FSU+F
            IF(FSU.GE.FSUCR)GOTO 200
          ENDDO
        ENDDO
      ENDDO
C
      ANCR=PFR*FCR/FPT
      DD=FSUCR/ANCR
      ANN=-IBAR*DD*SP
      IF(SYMMY.AND.IIYF.EQ.-IIYB)ANN=ANN+ANN
      FSU=FSU+ANN
C
      IS=1
 40   CONTINUE
      DO 100 IIZ=IIZB,IIZF
      DO 90 IIY=IIYB*IS,IIYF*IS,IS
      DO 80 IIX=IIXB,IIXF
      IIMX=IMX(IIX,IIY,IIZ)
      IF(IIMX.LT.0)GOTO 80
      UUPP=UU(IIX,IIY,IIZ)
      IIMN=IMN(IIX,IIY,IIZ)
      DO 70 II=IIMN,IIMX
      IIP=IPO(II)
      IIDP=ID(IIP)
      IF(IIDP.GE.3.AND.IIDP.LE.6)THEN
        BARI=BAR(IIDP)
        EMK=(AMV(IIP)+BARI*UUPP)**2
        EC=GMF*EE(IIP)-(BFX*PX(IIP)+BFY*PY(IIP)*IS+BFZ*PZ(IIP))
        ECK=EC*EC
        PCK=MAX((ECK-EMK)/(BARI*BARI),0.)   !per nucleon
        IF(PCK.GT.PDCUK4)GOTO 70      !no overlap of the spheres
        PHPK=PCK/PDCUK4
        HP=1.-SQRT(PHPK)              !height of a cap in overlap of 2 spheres
        PPK=1.-PHPK                   !radius**2 of a cap
        FSU=FSU+.25*BARI*HP*(3.*PPK+HP*HP)*DD !2*ratio overlap volume/deut volume
      ELSEIF(IIDP.GE.7.AND.IIDP.LE.12)THEN
        EMK=(AMV(IIP)+UUPP)**2         !deltas also: give ... a break
        PCK=ECK-EMK
        IF(PCK.LT.PDCUK)FSU=FSU+DD
      ENDIF
 70   CONTINUE
      IF(FSU.GE.FSUCR)GOTO 200
 80   CONTINUE
 90   CONTINUE
 100  CONTINUE
C
      IF(SYMMY.AND.IS.EQ.1)THEN
        IS=-1
        GOTO 40
      ENDIF
C
*     ff=fsu/ntry
*     write(*,*)ff
*     break=fsu.ge.fsucr
      BREAK=.FALSE.
      RETURN
C
 200  CONTINUE
      BREAK=.TRUE.
C
      END


      SUBROUTINE DSTR(F,PFX,PFY,PFZ,EF,IDPF,HHA,ASYIP)
      LOGICAL ASYIP
      INCLUDE 'NUCLS'
      INCLUDE 'PARTID'
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
      COMMON/DU/NPT(2)
C
      F=0.
      IF(NOPAUL(1).AND.NOPAUL(2))RETURN
      IF(ASYIP.AND.IDPF.LE.2)THEN
        IF(NOPALN(1,IDPF).AND.NOPALN(2,IDPF))RETURN
      ENDIF
C
      IF(ASYIP.AND.IDPF.LE.2)THEN
        F=0.
        DO 50 IPT=1,2
          IF(NOPALN(IPT,IDPF))GOTO 50
          BPF=BXPAN(IPT,IDPF)*PFX
     I      +BYPAN(IPT,IDPF)*PFY+BZPAN(IPT,IDPF)*PFZ
          BCPF=GMPAN(IPT,IDPF)*(EF-GMPAN1(IPT,IDPF)*BPF)
          PFCX=PFX-BXPAN(IPT,IDPF)*BCPF
          PFCY=PFY-BYPAN(IPT,IDPF)*BCPF
          PFCZ=PFZ-BZPAN(IPT,IDPF)*BCPF
          PRK=0.
          DO I=1,3
            PCO=CXN(I,IPT,IDPF)*PFCX+CYN(I,IPT,IDPF)*PFCY
     P        +CZN(I,IPT,IDPF)*PFCZ
            PRK=PRK+PCO*PCO
          ENDDO
          ER=SQRT(EF*EF-(PFX*PFX+PFY*PFY+PFZ*PFZ)+PRK)*HHA
          F=F+FFAN(IPT,IDPF)
     F      /(FEXP(ER-AMICN(IPT,IDPF),TCN(IPT,IDPF))+1.)
 50       CONTINUE
*        fn=f
         RETURN
      ENDIF
C
      NNPT=0
      F=0.
      DO 100 IPT=1,2
      IF(NOPAUL(IPT))GOTO 100
      BPF=BXPA(IPT)*PFX+BYPA(IPT)*PFY+BZPA(IPT)*PFZ
      BCPF=GMPA(IPT)*(EF-GMPA1(IPT)*BPF)
      PFCX=PFX-BXPA(IPT)*BCPF
      PFCY=PFY-BYPA(IPT)*BCPF
      PFCZ=PFZ-BZPA(IPT)*BCPF
      PRK=0.
      DO I=1,3
        PCO=CX(I,IPT)*PFCX+CY(I,IPT)*PFCY+CZ(I,IPT)*PFCZ
        PRK=PRK+PCO*PCO
      ENDDO
      ER=SQRT(EF*EF-(PFX*PFX+PFY*PFY+PFZ*PFZ)+PRK)*HHA
      IF(IDPF.GE.3.AND.IDPF.LE.6)THEN        !clusters
        F=F+FFA(IPT)
     I    *MIN(FEXP(BAR(IDPF)*AMIC(IPT)-ER,TC(IPT)),1.)
      ELSEIF(IDPF.LE.12)THEN
        F=F+FFA(IPT)/(FEXP(ER-AMIC(IPT),TC(IPT))+1.)
      ELSE                        !pions
        F=F+NPT(IPT)*FEXP(-ER,TK(IPT))
        NNPT=NNPT+NPT(IPT)
      ENDIF
 100  CONTINUE
C
      IF(IDPF.GE.13)F=F/NNPT
*     if(idpf.le.2)write(*,*)'fn = ',fn,'  f = ',f,idpf
C
      END


      SUBROUTINE GEDF(PFX,PFY,PFZ,EF,BREAK,IDPF)
      LOGICAL BREAK
      INCLUDE 'NUCLS'
      INCLUDE 'PARTID'
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
      DIMENSION FDMAX(3:6)
      DATA FDMAX/.27,2*.34,.25/
*      DATA FDMAX/.27,2*.30,.27/
**      DATA FDMAX/4*.27/
      PARAMETER (FDFAC=1.3)
C
      IF(NOPAU)GOTO 30
      IF(ASYII)THEN
        IF(NOPALN(1,1).AND.NOPALN(2,1).AND.NOPALN(1,2).AND.
     I    NOPALN(2,2))GOTO 30
      ENDIF
*      FCOMP=MIN(FDDMAX*2./BAR(IDPF),.5)
      FCOMP=MIN(FDMAX(IDPF)*FDFAC,.5)
      CALL DEU(FCOMP,BREAK,PFX,PFY,PFZ,EF,1.,IDPF)
      RETURN
C
 30   CONTINUE
      BREAK=.FALSE.
C
      END


      SUBROUTINE GETFF(PFX,PFY,PFZ,EF,*,IDPF)
      use global
      INCLUDE 'NUCLS'
      COMMON/CSEED/ISEED
C
      IF(NOPAU)RETURN
      IF(IDPF.GT.6)RETURN
C
      IF(IDPF.GE.3)THEN
        CALL GETF(PFX,PFY,PFZ,EF,*100,IDPF)
        RETURN
 100    RETURN 1
      ELSE
        CALL GETNU(F,PFX,PFY,PFZ,IDPF,ASYII)
        F=MIN(F,1.)
        IF(F.LT.1E-3)RETURN
        IF(getRan().LT.F)RETURN 1
      ENDIF
C
      END


      SUBROUTINE GETF(PFX,PFY,PFZ,EF,*,IDPF)
      use global
      INCLUDE 'NUCLS'
      INCLUDE 'PARTID'
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
      COMMON/CDBB/ECMK,EM2KK,EM4K2K,EMDKK,EM4KDK
      PARAMETER (AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      COMMON/GETD/EDKC,PDKC,EMDK,EMD0K
     G  ,VNX,VPMN,VPMX,CTHD
C
*     PARAMETER (TMIN=4.3*2.)
*     PARAMETER (TMINI=1./TMIN)
      DIMENSION FDMAX(3:6)
      DATA FDMAX/.27,2*.34,.25/
*      DATA FDMAX/.27,2*.30,.27/
*      DATA FDMAX/4*.27/
      COMMON/CSEED/ISEED
      COMMON/CLCOLL/KK2,KK2R,EET2,KK3,KK3R,EET3,KKB,KKBR,EETB
     C  ,KKV,KKVR,EETV,KKVB,KKVBR,EETVB
     C  ,KBRA,KKVE,KKVER,EETVE,KKEV
     C  ,KK4,KK4R,EET4,KKBT,KKBTR,EETBT,KBRAT
     C  ,KKPH,KKPHR,kkhi,eehit,qc4,pqck,pqckl
     C  ,KKVV,KKVVR,EETVV,KKVVA,KKVVAR,EETVVA
      LOGICAL BREAK
C
*      PARAMETER (DICR=.5)   !max no. coll/ptcle/time with comp pro possible
**      PARAMETER (DICR=.5*1E3)   !max no. coll/ptcle/time with comp pro possible
*      COMMON/CDI/DINM
C
      IF(NOPAU)RETURN
      IF(IDPF.GT.6)RETURN
      IF(NOPAUL(1).AND.NOPAUL(2))RETURN
C
      IF(IDPF.GE.3)THEN
*        IF(DINM*EF/AMS(IDPF).GT.DICR)RETURN 1
        IF(ASYII)THEN
          IF(NOPALN(1,1).AND.NOPALN(2,1).AND.NOPALN(1,2).AND.
     I      NOPALN(2,2))RETURN
        ENDIF
*        CALL DEU(FDMAX*2./BAR(IDPF),BREAK,PFX,PFY,PFZ,EF,0.,IDPF)
        CALL DEU(FDMAX(IDPF),BREAK,PFX,PFY,PFZ,EF,0.,IDPF)
        IF(BREAK)RETURN 1
      ELSE
        IF(ASYII)THEN
          IF(NOPALN(1,IDPF).AND.NOPALN(2,IDPF))RETURN
        ENDIF
        CALL DSTR(F,PFX,PFY,PFZ,EF,IDPF,1.,ASYII)
        IF(F.LT.1E-3)RETURN
        IF(getRan().LT.F)RETURN 1
      ENDIF
C
      END


      FUNCTION FEXP(A,T)
C  CALCULATES EXPONENTIAL FOR THE DISTRIBUTION FUNCTION
C
      IF(T.LE.ABS(A)*.05)THEN
        IF(A.GE.0.)THEN
          FEXP=1E20
        ELSE
          FEXP=0.
        ENDIF
      ELSE
        FEXP=EXP(A/T)
      ENDIF
C
      END


      SUBROUTINE THETA2(CTH,DECM)
      use global
      PARAMETER (AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER (AMEC=.13957,AMEN=.13496,AME=(AMEC+AMEC+AMEN)/3.)
      PARAMETER (AMVM=AM0+AME)
      PARAMETER (ECMM=AM0+AMVM)
      COMMON/CSEED/ISEED
C
      ECM=DECM+ECMM
C
      IF(ECM.LT.2.14)THEN
        B1=.5
        B3=0.
      ELSEIF(ECM.GT.2.4)THEN
        B1=.06
        B3=.4
      ELSE
        B1=29.03+ECM*(-23.75+ECM*4.865)
        B3=-30.33+ECM*(25.53-ECM*5.301)
      ENDIF
C
      RS=getRan()
      IF(B3.NE.0.)THEN
        P=B1/(3.*B3)
        Q=(B1+B3)*(RS-.5)/B3
        SP=SQRT(P)
        PHI=ASINH(Q/(P*SP))
        CTH=-2.*SP*SINH(PHI/3.)
      ELSE
        CTH=1.-2.*RS
      ENDIF
C
      END


      FUNCTION ASINH(X)
C
      IF(X.GE.0.)THEN
        ASINH=LOG(SQRT(X*X+1.)+X)
      ELSE
        ASINH=-LOG(SQRT(X*X+1.)-X)
      ENDIF
C
      END


      SUBROUTINE THETA1(CTH,DEC)
      use global
      PARAMETER (AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER (AM02=AM0+AM0,AM0K=AM0*AM0)
      COMMON/CSEED/ISEED
C
      DECM=DEC+DEC
      ECM=DECM+AM02
C
      PK=.25*ECM-AM0K
C
      AG=(3.65*DECM)**6
      BB=6.*AG/(1.+AG)
      BB=BB*2.*PK
C
      RS=getRan()
      IF(BB.GT.0.)THEN
        EBB=EXP(BB)
        CTH=LOG(EBB*(1.-RS)+RS/EBB)/BB
      ELSE
        CTH=2.*(RS-.5)
      ENDIF
C
      END


      SUBROUTINE THETA(CTH,EPCM,IDP,JDP)
      use global
      PARAMETER (NEPC=11,NKSI=10)
      PARAMETER (NEPC1=NEPC-1)
      PARAMETER (DKSI=(1.+1.E5)/NKSI)
      DIMENSION COKSI(0:NKSI,0:NEPC,2)
      COMMON/CSEED/ISEED
C
      LOGICAL FIRST
C
      DATA FIRST/.TRUE./
C
      IF(FIRST)THEN
        OPEN (33,FILE='CALKSI.DAT',STATUS='OLD')
        READ(33,*)DEPCM
        DO IE=0,NEPC
          DO KS=0,NKSI
            READ(33,*)COKSI(KS,IE,1),COKSI(KS,IE,2)
          ENDDO
        ENDDO
        CLOSE(33)
        FIRST=.FALSE.
      ENDIF
C
      RS=getRan()/DKSI
      KS=RS
      DRS=RS-KS
C
      X=EPCM/DEPCM
      ICM=X
      DX=X-ICM
      IF(ICM.GT.NEPC1)THEN
        ICM=NEPC1
        DX=1.
      ENDIF
C
      IF(IDP.EQ.JDP)THEN
        KC=1                                     !pp, nn
      ELSE
        KC=2                                     !pn
      ENDIF
C
      CTH=(COKSI(KS,ICM,KC)*(1.-DRS)+COKSI(KS+1,ICM,KC)*DRS)*(1.-DX)
     C  +(COKSI(KS,ICM+1,KC)*(1.-DRS)+COKSI(KS+1,ICM+1,KC)*DRS)*DX
C
      END


      BLOCK DATA CSNORM
      COMMON/RN/RN(6,6)
      DATA RN(1,1)/1./RN(1,2)/1./RN(2,2)/1./
      DATA RN(1,3)/.96/RN(2,3)/.96/
      DATA RN(3,3)/.89/
      DATA RN(1,6)/.85/RN(2,6)/.85/
      DATA RN(3,6)/.75/
      DATA RN(6,6)/.29/
      DATA ((RN(I,J),I=1,2),J=4,5)/4*.90/       !check
      DATA (RN(3,J),J=4,5)/2*.79/
      DATA ((RN(I,J),I=4,5),J=4,5)/4*.69/
      DATA RN(4,6)/.65/RN(5,6)/.65/
C  NORMALIZATION OF TOTAL CROSS SECTION FOR CLUSTER-CLUSTER TO
C  EVERY NUCLEON WITH EVERY NUCLEON (I.E. PROPORTIONAL TO A1*A2)
C
      END


      FUNCTION SIG(EpPCM,IIDP,JJDP)
      INCLUDE 'PARTID'
C  JUST PLAIN CROSS SECTION IN FM**2,  (N OR CLUSTER) - (N OR CLUSTER)
C  BUT THE ARGUMENT IS WEIRD:           PI - RESONANCE OR CLUSTER  2/20/98
C  EPCM IS THE TOTAL CM ENERGY TIMES CM
C  MOMENTUM OF ONE PARTICLE IN GEV/C
C  ((THIS IS THE EASIEST KINEMATIC COMBINATION THAT GIVES SOME RESOLUTION))
      PARAMETER (NPCM=110,DPCM=.01)
      PARAMETER (NPCM1=NPCM+1)
      DIMENSION SIGPPR(0:NPCM1),SIGPNR(0:NPCM1)
C  ONE LINE OF THE VECTOR WILL BE WASTED ...
      DIMENSION SIGPP(0:NPCM1),SIGPN(0:NPCM1)
C
      PARAMETER (PCMX=NPCM*DPCM)
C
      PARAMETER (AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER (AM0K=AM0*AM0)
      PARAMETER (AM0Q=AM0K*AM0K)
C
      PARAMETER(SIGPIB=27.*.1,SIGPIP=18.*.1)
C
      COMMON/CVMAX/VOLCM(NIDP,NIDP),VDMAX,VTMAX,GTMAX(7:12)
     C  ,VDPPMX,VDPNMX,VTPPMX,VTPNMX
C  MAXIMUM COLLISIONAL VOLUME (NORMALIZED TO CELL)
      COMMON/COQU/FV,FD,FT,FG,FTD
C
      COMMON/RN/RN(6,6)
C
      LOGICAL FIRST
C
      DATA FIRST/.TRUE./
C
      IF(FIRST)THEN
        OPEN(33,FILE='CSCALL.DAT',STATUS='OLD')
        DO I=0,NPCM1
          READ(33,*)A,A,SIGPPR(I),SIGPNR(I)
        ENDDO
        CLOSE(33)
C
C  STEPS IN EPCM...
        PCMXK=PCMX*PCMX
        E0MX=SQRT(AM0K+PCMXK)
        EPCMX=(E0MX+E0MX)*PCMX
        DEPCM=EPCMX/NPCM
C
        VOLPP=0.
        VOLPN=0.
        DO I=0,NPCM
          EP=I*DEPCM
          PCMK=.5*(SQRT(AM0Q+EP*EP)-AM0K)
          PCM=SQRT(PCMK)
          X=PCM/DPCM
          ICM=X
          DX=X-ICM
          IF(ICM.GE.NPCM)THEN
            ICM=NPCM
            DX=0.
          ENDIF
          V=2.*PCM/SQRT(AM0K+PCMK)
          SIGPP(I)=.1*(SIGPPR(ICM)*(1.-DX)+SIGPPR(ICM+1)*DX)
          IF(V*SIGPP(I).GT.VOLPP)VOLPP=V*SIGPP(I)
          SIGPN(I)=.1*(SIGPNR(ICM)*(1.-DX)+SIGPNR(ICM+1)*DX)
          AG=V*(SIGPN(I)+SPNPH(EP,1,2))
          IF(AG.GT.VOLPN)VOLPN=AG
        ENDDO
        SIGPP(NPCM1)=.1*SIGPPR(NPCM1)
        SIGPN(NPCM1)=.1*SIGPNR(NPCM1)
        VOLPP=VOLPP*FV
        VOLPN=VOLPN*FV
        VOLCM(1,1)=VOLPP
        VOLCM(2,2)=VOLPP
        VOLCM(1,2)=VOLPN
C  WILL BE SYMMETRIZED LATER
C  NN COMPLETED
C
        DO J=13,15         !pion-anything    2/20/98
          DO I=3,6
            VOLCM(I,J)=RN(1,I)*BAR(I)*FV*2.*SIGPIB
            VOLCM(J,I)=VOLCM(I,J)
          ENDDO
          DO I=7,12
            VOLCM(I,J)=FV*2.*SIGPIB
            VOLCM(J,I)=VOLCM(I,J)
          ENDDO
          DO I=13,J
            VOLCM(I,J)=FV*2.*SIGPIP
            VOLCM(J,I)=VOLCM(I,J)
          ENDDO
        ENDDO
c
        DO I=1,NIDP
          DO J=MAX(I,16),NIDP
            VOLCM(I,J)=0.
            VOLCM(J,I)=0.
          ENDDO
        ENDDO
C  GAMMA'S DO NOT INTERACT
C
        VOLCPP=VOLCM(1,1)
        VOLCPN=VOLCM(1,1)
        DO I=1,6
          ZI=ZPA(I)
          UI=BAR(I)-ZI
          DO J=I,6
            ZJ=ZPA(J)
            UJ=BAR(J)-ZJ
            VOLCM(I,J)=RN(I,J)
     V        *(ZI*ZJ+UI*UJ)*VOLCPP+(ZI*UJ+ZJ*UI)*VOLCPN
            VOLCM(J,I)=VOLCM(I,J)
            RN(J,I)=RN(I,J)
          ENDDO
        ENDDO
        WRITE(*,*)((VOLCM(I,J),I=J,6),J=1,6)
C  IMPOSSIBLE..:  DD COMPLETED
        FIRST=.FALSE.
        fepcm=4.*am0k*.065**2
      ENDIF
C
      IF(IIDP.LE.JJDP)THEN         ! idp.le.jdp
        IDP=IIDP
        JDP=JJDP
      ELSE
        IDP=JJDP
        JDP=IIDP
      ENDIF
C
      BARI=BAR(IDP)
      IF(JDP.EQ.16)THEN
        SIG=0.
      ELSEIF(JDP.GE.13)THEN
        IF(IDP.LE.6)THEN
          SIG=RN(1,IDP)*BARI*SIGPIB
        ELSEIF(IDP.GE.13)THEN
          SIG=SIGPIP
        ELSE
          SIG=SIGPIB
        ENDIF
      ELSE
        BARJ=BAR(JDP)
        BARIJ=BARI*BARJ
        epcm=sqrt(eppcm*eppcm+fepcm*barij*barij
     e    *(bari+barj-2.)/(bari+barj))
        X=EPCM/(DEPCM*BARIJ)
        ICM=X
        DX=X-ICM
        IF(ICM.GE.NPCM)THEN
          ICM=NPCM
          DX=0.
        ENDIF
        ZI=ZPA(IDP)
        UI=BARI-ZI
        ZJ=ZPA(JDP)
        UJ=BARJ-ZJ
        SIG=RN(IDP,JDP)
     I    *((ZI*ZJ+UI*UJ)*(SIGPP(ICM)*(1.-DX)+SIGPP(ICM+1)*DX)
     S    +(ZI*UJ+UI*ZJ)*(SIGPN(ICM)*(1.-DX)+SIGPN(ICM+1)*DX))
      ENDIF
C
      END


      SUBROUTINE FINDI(IPO,IVAL,NQ)
C  PUTS VALUES INTO IMN AND IMX
      DIMENSION IPO(*),IVAL(*)
      INCLUDE 'NUCLS'
      INCLUDE 'NQUA1'
      INCLUDE 'SIZE2'
      INCLUDE 'PARTID'
C
      IQC=1
      DO 60 IY=-NT,NT
      IYN=IY*NTL1
      DO 60 IX=-NT,NT
      IYXN=IYN+IX*NLL1
      DO 60 IZ=-NL,NL
      IMN(IX,IY,IZ)=IQC
      IVXYZ=(IYXN+IZ)*NIDP+NIDP
**    IVXYZ=IYXN+IZ
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


      SUBROUTINE GIVAL
      INCLUDE 'PARTIC15'
      INCLUDE 'SIZE2'
      COMMON/ENERGY/EKIN,EPOT,EPR,APR,ETOG,PARTI(NIDP),PARTIG(NIDP)
     E  ,ANPAR,ANPARG,ANPR(6),ANPRG(6)
     N  ,EKINT,EKINZ,ECINT,ECINZ
     N  ,EKINTG,EKINZG
     E  ,PPRX,PPRY,PPRZ,PPRZL,PGOX,PGOY,PGOZ,PGOZL
     R  ,GST,BGSTX,BGSTY,BGSTZ
     G  ,PRSKT0,PRSP0,PRSKL0,PRSXZ0
      COMMON/CENTRO/ENGAIN,ENTROT,EDGAIN,EDTROT,DNCELL,DDCELL
     C  ,EBGAIN,EBTROT,DBCELL
     e  ,eptro,eptrog,anuc,anucg
      COMMON/COULP/PXCOU,PYCOU,PZCOU
C
      PXCOU=0.
      PYCOU=0.
      PZCOU=0.
C
      DO 100 I=1,NQ
      IDI=ID(I)
      IF(IDI.LE.0)GOTO 80              ! gamma's out
      IF(IDI.EQ.16)GOTO 75
*     IF(NBOX)THEN
*       IF(ABS(XX(I)).GT.RNT)GOTO 70
*       IF(ABS(YY(I)).GT.RNT)GOTO 70
*       IF(ABS(ZZ(I)).GT.RNL)GOTO 70
*     ENDIF
      IX=NINT(XX(I)/DWL)
      IY=NINT(YY(I)/DWL)
      IZ=NINT(ZZ(I)/DLL)
      IF(ABS(IX).GT.NT.OR.ABS(IY).GT.NT.OR.ABS(IZ).GT.NL)GOTO 70
      IVAL(I)=(IY*NTL1+IX*NLL1+IZ)*NIDP+IDI
**    IVAL(I)=IY*NTL1+IX*NLL1+IZ
C  THERE IS MOST MOTION IN THE Z DIRECTION ...
      GOTO 100
 70   CONTINUE
C  ***  EXIT OF A PARTICLE FROM THE COMPUTATIONAL AREA
      IF(IDI.LE.6)THEN
        IF(ABS(IDI).EQ.2)THEN
          IDII=1
        ELSE
          IDII=IDI
        ENDIF
        IPT=IPTI(I)
        CALL TEST(IDII,PX(I),PY(I),PZ(I),EE(I),PETOM*MDE(I)
     C    ,IPT,*80,IPART,IOV)
        IF(IDI.NE.2)ANPARG=ANPARG+FLOAT(IPART)/NQU      !check...
        IF(IDI.LE.6)THEN
          ANPRG(IDI)=ANPRG(IDI)+REAL(IOV)/NQU
          if(pent)then
            if(isr(i).ne.0)then
               eptrog=eptrog+isr(i)*.01/nqu*iov
               anucg=anucg+bar(idi)/nqu*iov
            endif
          endif
        ENDIF
      ENDIF
      IF(IDI.GE.3.OR.NCS(I).NE.0)THEN
        PARTIG(IDI)=PARTIG(IDI)+1./NQU
      ENDIF
      APR=APR-BAR(IDI)/NQU
      ETOG=ETOG+(EE(I)+PETOM*MDE(I))/NQU
c  energy gone - perturbative, so 1-ptcle energy mde
      IF(COU.AND.FCOU.NE.0.)CALL COUL(I)
      PGOX=PGOX+PX(I)/NQU
      PGOY=PGOY+PY(I)/NQU
      PGOZ=PGOZ+PZ(I)/NQU
      CALL LAB(EE(I),PZ(I),EEL,PZL,AMM,YL)
      PGOZL=PGOZL+PZL/NQU
      IF(IDI.LE.6)THEN
        PTK=PX(I)**2+PY(I)**2
        PZK=PZ(I)**2
        AE=AMVI+SQRT(AMVI*AMVI+PTK+PZK)
        AMVI=AMV(I)
        EKINTG=EKINTG+PTK/AE/NQU
        EKINZG=EKINZG+PZK/AE/NQU
      ENDIF
 75   CONTINUE
      ID(I)=-IDI
 80   CONTINUE
      IVAL(I)=MAXIPO
 100  CONTINUE
C
      END


      SUBROUTINE GIVER(X,Y,Z,IDP,IPT)
      use global
      COMMON/CHARGE/R1,X1,Z1,R2,X2,Z2,ZCEN
      COMMON/RCHA/P1X,P1Z,G1,SG1,P2X,P2Z,G2,SG2
      COMMON/PAS/IDPB,IPTB
      COMMON/RMK/RRK(2,2)
C
      INCLUDE 'NUCLS'
      INCLUDE 'NQUA1'
      INCLUDE 'SIZE2'
      COMMON/CSEED/ISEED
C
      PARAMETER(NIT=15)
C
      IF(IPT.EQ.1)THEN
        RMX=R1
      ELSE
        RMX=R2
      ENDIF
      RMX=2.*(RMX+1.9)
C
      IDPB=IDP
      IPTB=IPT
C
 10   CONTINUE
      X=RMX*(getRan()-.5)
      Y=RMX*(getRan()-.5)
      Z=RMX*(getRan()-.5)
      RK=X*X+Y*Y+Z*Z
      R=SQRT(RK)
      S=SHAP(R)
      IF(S.EQ.0.)GOTO 10
      DO 30 IT=1,NIT
 20   CONTINUE
      XN=RMX*(getRan()-.5)
      YN=RMX*(getRan()-.5)
      ZN=RMX*(getRan()-.5)
      RNK=XN*XN+YN*YN+ZN*ZN
      RN=SQRT(RNK)
      SN=SHAP(RN)
      IF(SN.EQ.0.)GOTO 20
      IF(SN.LT.S)THEN
        IF(S*getRan().GT.SN)GOTO 30
      ENDIF
      X=XN
      Y=YN
      Z=ZN
      S=SN
 30   CONTINUE
      RRK(IDP,IPT)=RRK(IDP,IPT)+X*X+Y*Y+Z*Z
C
      IF(IPT.EQ.1)THEN
        X=X+X1
        Z=Z*SG1+Z1
      ELSE
        X=X+X2
        Z=Z*SG2+Z2
      ENDIF
C
      END


      FUNCTION SHAP(R)
      INCLUDE 'NUCLS'
      COMMON/PAS/IDPB,IPTB
      PARAMETER (NIC=100,DR=.2)
      DIMENSION SHAPE(0:NIC,2,2)
C
      CHARACTER*14 FNAS
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      IF(FIRST)THEN
        FNAS=FNAME//'.SHP'
        CALL SPACO(FNAS)
        OPEN(17,FILE=FNAS,FORM='FORMATTED',STATUS='OLD')
        IF(IA1.EQ.0)THEN
          KS=2
        ELSE
          KS=1
        ENDIF
        IF(IA2.EQ.0)THEN
          KF=1
        ELSE
          KF=2
        ENDIF
        DO 20 K=KS,KF
        IR=0
 10     CONTINUE
        READ(17,*)A,SHAPE(IR,1,K),SHAPE(IR,2,K)
        SHAPE(IR,1,K)=MAX(SHAPE(IR,1,K),0.)
        SHAPE(IR,2,K)=MAX(SHAPE(IR,2,K),0.)
        IF(A.GE.0.)THEN
          IR=IR+1
          GOTO 10
        ENDIF
        DO I=MIN(IR,NIC),NIC
          SHAPE(I,1,K)=0.
          SHAPE(I,2,K)=0.
        ENDDO
        READ(17,'(1X)')
 20     CONTINUE
        CLOSE(17)
        do k=ks,kf
          do idp=1,2
            arf=0.
            rrk=0.
            do i=1,nic
              rr=i*dr
              dar=shape(i,idp,k)*rr*rr
              arf=arf+dar
              rrk=rrk+dar*rr*rr
            enddo
            if(arf.ne.0.)rrk=rrk/arf
            rrk=sqrt(rrk)
            write(*,*)'id/nucls/rrms ',idp,k,rrk
          enddo
        enddo
**      stop
        FIRST=.FALSE.
      ENDIF
C
      X=R/DR
      IR=X
      DX=X-IR
      IF(X.GE.NIC)THEN
        IR=NIC-1
        DX=1.
      ENDIF
      IR1=IR+1
      DX1=1.-DX
      SHAP=SHAPE(IR,IDPB,IPTB)*DX1+SHAPE(IR1,IDPB,IPTB)*DX
*      ag=(r-6.38)/.535
*      if(ag.gt.20.)then
*        shap=0.
*      else
*        shap=1./(1.+exp(ag))
*      endif
C
      END


      SUBROUTINE GIVE(PX,PY,PZ,PF,PX0,PZ0,G)
      use global
C  RANDOMLY FILLS A SPHERE WITH SQUEEZE OR ELONGATION
      PARAMETER (S3=1./3.)
C
      COMMON/CSEED/ISEED
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      IF(FIRST)THEN
        PI=4.*ATAN(1.)
        PIPI=PI+PI
        FIRST=.FALSE.
      ENDIF
C
      FI=PIPI*getRan()
      COFI=COS(FI)
      SIFI=SIN(FI)
C
      CTH=2.*getRan()-1.
      STH=SQRT(1.-CTH*CTH)
C
      P=PF*getRan()**S3
!*Jorg!!!      call pmxwl(p)
!*Jorg!!!      p=p*1e-3
      PT=P*STH
      PX=PT*COFI+PX0
      PY=PT*SIFI
C
      PZ=G*P*CTH+PZ0
C
      END


      SUBROUTINE RESET
      use global
      COMMON/CLMAIN/KK
      COMMON/CPRI/PRI,TIM,IZCEN
      LOGICAL PRI
      COMMON/CLCOLL/KK2,KK2R,EET2,KK3,KK3R,EET3,KKB,KKBR,EETB
     C  ,KKV,KKVR,EETV,KKVB,KKVBR,EETVB
     C  ,KBRA,KKVE,KKVER,EETVE,KKEV
     C  ,KK4,KK4R,EET4,KKBT,KKBTR,EETBT,KBRAT
     C  ,KKPH,KKPHR,kkhi,eehit,qc4,pqck,pqckl
     C  ,KKVV,KKVVR,EETVV,KKVVA,KKVVAR,EETVVA
C
      INCLUDE 'PARTIC15'
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
*     COMMON/CENTRO/ENGAIN,ENLOSS,ENTROT,EDGAIN,EDLOSS,EDTROT
      COMMON/CENTRO/ENGAIN,ENTROT,EDGAIN,EDTROT,DNCELL,DDCELL
     C  ,EBGAIN,EBTROT,DBCELL
     e  ,eptro,eptrog,anuc,anucg

! arrays for random number seed. We do not need to share them around.
      integer, allocatable, dimension(:) :: iseeds
      integer :: nseeds

*c
*c  for jorg
*      parameter(njorg=70)
*      common/cjorg/dejorg,anatem(njorg),anasuc(njorg)
*c  end for Jorg
C
      KK=0
C  FOR PLOTTING ?
C
      PRI=.TRUE.
C
*     CLOSE(10)
      CLOSE(11)
C
      BGSTX=0.
      BGSTY=0.
      BGSTZ=0.
      GST=1.
C
      ETOG=0.
      PGOX=0.
      PGOY=0.
      PGOZ=0.
      PGOZL=0.
      EKINTG=0.
      EKINZG=0.
      ANPARG=0.
      DO I=1,6
        ANPRG(I)=0.
      ENDDO
      DO I=1,15
        PARTI(I)=0.
        PARTIG(I)=0.
      ENDDO
c
      eptrog=0.
      anucg=0.
C
        ISEED=48081209
      call global_ranInit(iseed) !initialize intrinsic random number generator with ISEED
C
      KK2=0
      KK2R=0
      EET2=0.
      KK3=0
      KK3R=0
      EET3=0.
      KKB=0
      KKBR=0
      EETB=0.
      KKV=0
      KKVR=0
      EETV=0.
      KKVB=0
      KKVBR=0
      EETVB=0.
      KBRA=0
      KKVE=0
      KKVER=0
      EETVE=0.
      KKEV=0
      KK4=0
      KK4R=0
      EET4=0.
      KKBT=0
      KKBTR=0
      EETBT=0.
      KBRAT=0
      KKPH=0
      KKPHR=0
      kkhi=0
      eehit=0.
      qc4=0.
      pqck=0.
      pqckl=0.
      KKVV=0
      KKVVR=0
      EETVV=0.
      KKVVA=0
      KKVVAR=0
      EETVVA=0.
C
      ENTROT=0.
      EBTROT=0.
      EDTROT=0.
c
      do i=1,nq
        igx(i)=0
        igy(i)=0
        igz(i)=0
      enddo
*c
*c  for jorg
*      do ijorg=1,njorg
*        anatem(ijorg)=0.
*        anasuc(ijorg)=0.
*      enddo
*      DO IQ=1,NQ
*        NAJO(IQ)=0
*        NASU(IQ)=0
*      ENDDO
*c  end for Jorg
C
      END


      SUBROUTINE SIGDBB(SIGB)
C DYNAMIC N-D BREAKUP CROSS-SECTION, I.E. TAKING INTO ACCOUNT THAT
C SHIFT OF NUCLEON AND DEUTERON MASSES MAY BE DIFFERENT
      PARAMETER(NP=16)
      DIMENSION XGG(NP),WGG(NP)
C
      DATA XGG/ -0.989401    , -0.944575
     2        , -0.865631    , -0.755404
     3        , -0.617876    , -0.458017
     4        , -0.281604    , -0.950125E-01
     5        ,  0.950125E-01,  0.281604
     6        ,  0.458017    ,  0.617876
     7        ,  0.755404    ,  0.865631
     8        ,  0.944575    ,  0.989401     /
C
      DATA WGG/  0.271525E-01,  0.622535E-01
     2        ,  0.951585E-01,  0.124629
     3        ,  0.149596    ,  0.169157
     4        ,  0.182603    ,  0.189451
     5        ,  0.189451    ,  0.182603
     6        ,  0.169157    ,  0.149596
     7        ,  0.124629    ,  0.951585E-01
     8        ,  0.622535E-01,  0.271525E-01 /
C
      COMMON/CSPC/EM0K,PKMX,ECM,EDC,PIC
      COMMON/GETD/EDKC,PDKC,EMDK,EMD0K
     G  ,VNX,VPMN,VPMX,CTHD
      COMMON/CDBB/ECMK,EM2KK,EM4K2K,EMDKK,EM4KDK
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      PARAMETER (H=.19733)
C
      IF(FIRST)THEN
        PI=4.*ATAN(1.)
        CSII=.5/(PI*PI*H*H*H)
        FIRST=.FALSE.
      ENDIF
C
      PDK=((ECMK-EMDKK)**2-EM4KDK)/(4.*ECMK)
      PD=SQRT(PDK)
      ED=SQRT(EMDK+PDK)
      CSIL=CSII*EMDK/(ECM*PDK)
      A=0.
      DO 30 IP=1,NP
      PK=PKMX*.5*(XGG(IP)+1.)
      PKK=PK*PK
      E3=SQRT(EM0K+PKK)
      EQK=.25*((ECM-E3)**2-PKK)
      EQ=SQRT(EQK)
      Q=SQRT(EQK-EM0K)
      FIP=PK*EQ*Q/E3
      EPCM=(EQ+EQ)*Q
      FIP=FIP*(SIG(EPCM,1,1)+SIG(EPCM,1,2))
C  TWO AVERAGE CROSS SECTIONS ...
      PMAX=SQRT(((ED*E3+PD*PK)**2-EMD0K)/EMDK)
      AG=(ED*E3-PD*PK)**2-EMD0K
      IF(AG.GT.0.)THEN
        PMIN=SQRT(AG/EMDK)
      ELSE
        PMIN=0.
      ENDIF
      A=A+FIP*(VOLDE(PMIN)-VOLDE(PMAX))*WGG(IP)
 30   CONTINUE
      SIGB=A*CSIL*PKMX*.5
      SIGB=SIGB*FDEU(PKMX)
C
      END


      SUBROUTINE ENCELL
C  CALCULATES ENTROPY IN A CELL & PUTS IT INTO ENGAIN, CLUMSY!!!
*     COMMON/CENTRO/ENGAIN,ENLOSS,ENTROT,EDGAIN,EDLOSS,EDTROT
      COMMON/CENTRO/ENGAIN,ENTROT,EDGAIN,EDTROT,DNCELL,DDCELL
     C  ,EBGAIN,EBTROT,DBCELL
     e  ,eptro,eptrog,anuc,anucg
      COMMON/CLOPA/IX,IY,IZ,CALO
      LOGICAL CALO
C
      INCLUDE 'PARTIC15'
      INCLUDE 'SIZE2'
C
      COMMON/CECL/IECL(NQU),NECL    !local stack for entropy calculation
c
      logical inpo
      common/cinpo/inpo             !cell called first time
c
      common/mixc/mmix
C
      LOGICAL COUNT
      COMMON/CCOUN/COUNT
C
      DNCELL=0.
      DDCELL=0.
      DBCELL=0.
      IMXXYZ=IMX(IX,IY,IZ)
      IF(IMXXYZ.LT.0)RETURN
      IMNXYZ=IMN(IX,IY,IZ)
      DO 100 I=IMNXYZ,IMXXYZ+NECL
      IF(I.LE.IMXXYZ)THEN
        IP=IPO(I)
      ELSE
        IP=IECL(I-IMXXYZ)
      ENDIF
      IDP=ID(IP)
      IF(IDP.LE.0)GOTO 100
      IF(NCS(IP).EQ.0)GOTO 100
      call enma(px(ip),py(ip),pz(ip),ee(ip),idp,s)
      if(inpo)then
*       isrb(ip)=100.*s          !value before ..., never used??
      else                                          !cell called 2nd time
        if((ipti(ip).lt.0.or.mmix.eq.1).AND.COUNT)
     I    isr(ip)=100.*s                            !count is false if cell
      endif                                         !was enlarged
 100  CONTINUE                         !updated only if interacted ??
C
      END            !all obsolete ??


      FUNCTION SPNPH(EPCM,IDP,JDP)
      INCLUDE 'NUCLS'
C  CALCULATES CROSS SECTION FOR PN-->PN+GAMMA IN SCHAFER'S ET AL
C  PARAMETRIZATION, IN FM**2 MULTIPLIED BY A SCALING FACTOR
      PARAMETER (NPCM=110,DPCM=.01)
      PARAMETER (NPCM1=NPCM+1)
      DIMENSION SIGPNP(0:NPCM1)
C
      PARAMETER (PCMX=NPCM*DPCM)
      PARAMETER(PCMXK=PCMX*PCMX)
C
      PARAMETER (AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER (AM0K=AM0*AM0)
C
      LOGICAL FIRST
C
      DATA FIRST/.TRUE./
C
C
      IF(FIRST)THEN
C
        E0MX=SQRT(AM0K+PCMXK)
        EPCMX=(E0MX+E0MX)*PCMX
        DEPCM=EPCMX/NPCM
C
        OPEN(33,FILE='PHOTPN.DAT',STATUS='OLD')
        DO I=0,NPCM1
          READ(33,*)A,A,SIGPNP(I)
        ENDDO
        CLOSE(33)
C
        FIRST=.FALSE.
      ENDIF
C
      SPNPH=0.
      IF(IDP*JDP.NE.2)RETURN
C
      X=EPCM/DEPCM
      ICM=X
      DX=X-ICM
      IF(ICM.GE.NPCM)THEN
        ICM=NPCM
        DX=0.
      ENDIF
C
      SPNPH=SIGPNP(ICM)*(1.-DX)+SIGPNP(ICM+1)*DX
      SPNPH=SPNPH*PHOTR
C
      END


      SUBROUTINE GENPH(OMPH,DECM)
      use global
      PARAMETER(AMP=.9383,AMN=.9396,AM0=.5*(AMP+AMN))
      PARAMETER(AM02=AM0+AM0)
      PARAMETER(AM04=AM02+AM02)
C
      PARAMETER(OMI=.004)   !minimum cut-off value
C
      COMMON/CENPH/DECC,OMX
      COMMON/CSEED/ISEED
C
      PARAMETER (NIT=19)
C
      DATA XP/.5/
C
      OMX=.5*DECM*(DECM+AM04)/(DECM+AM02)
      DECC=DECM*1E3   !in MeV
C
      FP=DIPH(XP*OMX)
      DO 30 IT=1,NIT
      X=getRan()
      FX=DIPH(X*OMX)
      IF(FX.LT.FP)THEN
        IF(FP*getRan().GT.FX)GOTO 30
      ENDIF
      XP=X
      FP=FX
 30   CONTINUE
C
      OMPH=OMX*XP
C
      END


      FUNCTION DIPH(OM)
C  RETURNS DIFFERENTIAL CROSS SECTION FOR PHOTON EMISSION
C  PER UNIT ENERGY, IN FM**2/GEV
C
      COMMON/CENPH/DECC,OMX
C
      PARAMETER(OMI=.004)   !minimum cut-off value
C
      AG=OM/OMX
      IF(AG.LE.0..OR.AG.GE.1.)THEN
        DIPH=0.
        RETURN
      ENDIF
C
      DIPH=SQRT(1.-AG)
C
      OMC=OM*1E3      !in MeV
C
      A=9.74+.037*DECC                     !from Z. Phys. A339, 391 (1991)
      B=.91-.14*ATAN(.045*DECC-7.66)
      C=12.365/(18.27+DECC)**2.38
      D=1.5*DECC
C
      DIPH=DIPH*(A+B*OMC*exp(-c*(omc-d)**2))
      DIPH=DIPH/MAX(OM,OMI)*1E-3*.1              !in fm**2/GeV
C
      END
