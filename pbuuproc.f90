!! pbuuproc - reads '.parts' files that were written by outParticles subroutine.
!+ Continue reading comments to see how to read from that file.
program pbuuproc
 use arbstorage
 use global
 use class_BVector
 implicit none

 include 'NQUA1'

! choose which 7-character pbuu basename to process
 character(len=7) :: filename="bwbW9BK"

! choose which timestep to process
 integer :: iFile = 50 !< number of file output

 integer(kind=INT8), dimension(0:0) :: intArray !< working array for kind=INT8 integer
 integer :: tempInt

 integer(kind=INT8), dimension(0:1) :: realArray !< working array for my-real format\
 real (kind=REAL64) :: tempReal

 type (BVector_int) :: ids !< vector of particle IDs (auto-sizing array)
 type (BVector_int) :: ncs !< vector of number of collisions
 type (BVector_dble) :: xx !< vector of positions
 type (BVector_dble) :: yy !< vector of positions
 type (BVector_dble) :: zz !< vector of positions
 type (BVector_dble) :: px !< vector of positions
 type (BVector_dble) :: py !< vector of positions
 type (BVector_dble) :: pz !< vector of positions

 real(kind=REAL64), parameter :: pi = 4*atan(1._real64)
 real(kind=REAL64), parameter :: mass = 0.938_REAL64


 character(len=4) :: charIFile !< character representation of iFile, padded w zeros

 integer :: infileu
 integer :: ii, stat, ip

 ! average kinetic energy, number density variables
 real (kind=REAL64) :: kav,dkav,nn,dnn

 ! momentum histogram for temperature calculation
 logical, parameter :: histp = .true. !< create histogram?
 real (kind=REAL64), parameter :: histp_minp = -0.5 & !< minimum momentum
                                , histp_maxp = 0.5 !< maximum momentum
 integer, parameter            :: histp_nbins = 11 !< number of bins
 real (kind=REAL64), parameter :: histp_dp = (histp_maxp-histp_minp)/histp_nbins !< bin width
 integer, parameter :: histp_ipmin = nint(sign(1.d0,histp_minp)*(abs(histp_minp)-0.0001)/histp_dp)
 integer, parameter :: histp_ipmax = nint(sign(1.d0,histp_maxp)*(abs(histp_maxp)-0.0001)/histp_dp)
 real(kind=REAL64) &
  , dimension(histp_ipmin:histp_ipmax) :: histp_pp    &          !< momenta of bins
                                        , histp_dndpx &          !< magnitude of bin
                                        , histp_dndpy &          !< magnitude of bin
                                        , histp_dndpz            !< magnitude of bin

 real(kind=REAL64), parameter  :: histp_thCmsMin = 0 & !< CMS theta min
                                , histp_thCmsMax = 180  !< CMS theta max

 real(kind=REAL64), parameter :: histp_rmax = 2._REAL64 !< radius of central cell to gate on

 ! initialize all vectors
 ids = new_BVector_int()
 ncs = new_BVector_int()
 xx = new_BVector_dble()
 yy = new_BVector_dble()
 zz = new_BVector_dble()
 px = new_BVector_dble()
 py = new_BVector_dble()
 pz = new_BVector_dble()

 histp_dndpx = 0
 histp_dndpy = 0
 histp_dndpz = 0

 write(charIFile,'(I4)')iFile
 
 do ii=1,3
  if(charIFile(ii:ii)<=' ') charIFile(ii:ii)='0'
 enddo

! Set base name of output file here
 open(newunit=infileu, file=filename//charIFile//".parts", status="old", access="stream")

!! read in all particles from file
 do !loop through particles in given timestep file
  read(infileu, iostat=stat)intArray
  if(stat==iostat_end) exit
!  write(*,'(b8.8)')intArray
  call fromBitstoInt(tempInt, intArray, 8, .true.)
  call ids%push_back(tempInt)

  read(infileu)intArray
  call fromBitsToInt(tempInt, intArray, 8, .true.)
  call ncs%push_back(tempInt)

  read(infileu)realArray
  call fromBitsToReal(tempReal, realArray, 5, 10, .true.)
  call xx%push_back(tempReal)

  read(infileu)realArray
  call fromBitsToReal(tempReal, realArray, 5, 10, .true.)
  call yy%push_back(tempReal)

  read(infileu)realArray
  call fromBitsToReal(tempReal, realArray, 5, 10, .true.)
  call zz%push_back(tempReal)
 
  read(infileu)realArray
  call fromBitsToReal(tempReal, realArray, 5, 10, .true.)
  call px%push_back(tempReal)

  read(infileu)realArray
  call fromBitsToReal(tempReal, realArray, 5, 10, .true.)
  call py%push_back(tempReal)

  read(infileu)realArray
  call fromBitsToReal(tempReal, realArray, 5, 10, .true.)
  call pz%push_back(tempReal)

 enddo

 close(infileu)

!!!!!!!!!!!!!!!!!!!!!!
! The values are now stored inside the various vectors referenced above.
!
! For example, the x-momentum of the ii-th particle can be referenced by
! "px%values(ii)"
!!!!!!!!!!!!!!!!!!!!!!

 ! pre-loop calc for 'histp'
 histp_dndpx = 0
 histp_dndpy = 0
 histp_dndpz = 0


 do ii=histp_ipmin,histp_ipmax
  histp_pp(ii) = histp_minp+(ii-histp_ipmin+0.5_REAL64)*histp_dp
  write(*,*)'histp_ip,histp_pp=',ii,histp_pp(ii)
 enddo

 ! pre-loop calc for kav,nn
 kav=0
 dkav=0
 nn=0
 dnn=0

  do ii=0,ids%size()-1
!  if(ncs%values(ii)>=0) then

  if(ids%values(ii).ne.1) cycle ! filter for protons only

  if(sqrt(xx%values(ii)**2+yy%values(ii)**2+zz%values(ii)**2).gt.histp_rmax) cycle ! filter for central region

!  write(*,*) ids%values(ii), xx%values(ii), yy%values(ii), zz%values(ii), px%values(ii), py%values(ii), pz%values(ii)
!  endif

  ! in-loop calc of kav,nn
  kav=kav+sqrt(px%values(ii)**2 + py%values(ii)**2 + pz%values(ii)**2 + mass**2) - mass
  nn = nn + 1

  ! in-loop calc of momentum histogram 'histp'
  ip = nint(px%values(ii)/histp_dp)
  if(ip.ge.histp_ipmin.and.ip.le.histp_ipmax) histp_dndpx(ip) = histp_dndpx(ip)+1

  ip = nint(py%values(ii)/histp_dp)
  if(ip.ge.histp_ipmin.and.ip.le.histp_ipmax) histp_dndpy(ip) = histp_dndpy(ip)+1

  ip = nint(pz%values(ii)/histp_dp)
  if(ip.ge.histp_ipmin.and.ip.le.histp_ipmax) histp_dndpz(ip) = histp_dndpz(ip)+1

  ! check for in angular range

 enddo

 ! after-loop calc for kav, nn
 kav = kav/nn
 dkav = kav / sqrt(nn)
 nn = nn / ((4._REAL64/3._REAL64)*pi*histp_rmax**3) / nqu
 dnn = dkav/kav * nn
 write(*,*)'kav,dkav,nn,dnn=',kav,dkav,nn,dnn

 do ii=histp_ipmin,histp_ipmax

  histp_dndpx(ii) = histp_dndpx(ii)/nqu
  histp_dndpy(ii) = histp_dndpy(ii)/nqu
  histp_dndpz(ii) = histp_dndpz(ii)/nqu

 enddo


 write(*,*)
 write(*,*)
 write(*,*)'# kav (GeV), dkav (GeV)'
 write(*,*)kav,dkav

 write(*,*)
 write(*,*)
 write(*,*)'# nn (fm^-3), dnn (fm^-3)'
 write(*,*)nn,dnn


 write(*,*)
 write(*,*)
 write(*,*)'# histp histograms'
 write(*,*)'# px, dNdp_x'
 do ii=histp_ipmin,histp_ipmax
  write(*,*)histp_pp(ii),histp_dndpx(ii), histp_dndpy(ii), histp_dndpz(ii)
 enddo

end program pbuuproc
