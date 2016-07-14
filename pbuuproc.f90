program pbuuproc
 use arbstorage
 use class_BVector
 implicit none

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

 integer :: iFile = 100 !< number of file output
 character(len=4) :: charIFile !< character representation of iFile, padded w zeros

 integer :: infileu
 integer :: ii, stat

 ! momentum histogram for temperature calculation
 logical, parameter :: histp = .true. !< create histogram?
 real (kind=REAL64), parameter :: histp_minp = 0 & !< minimum momentum
                                , histp_maxp = 0.5 !< maximum momentum
 integer, parameter            :: histp_nbins = 10 !< number of bins
 real (kind=REAL64), parameter :: histp_dp = (histp_maxp-histp_minp)/histp_nbins !< bin width
 real(kind=REAL64) &
  , dimension(0:histp_nbins-1) :: histp_pp &            !< momenta of bins
                                , histp_vals              !< magnitude of bin
 real(kind=REAL64), parameter  :: histp_thCmsMin = 70 & !< CMS theta min
                                , histp_thCmsMax = 110  !< CMS theta max

 ids = new_BVector_int()
 ncs = new_BVector_int()
 xx = new_BVector_dble()
 yy = new_BVector_dble()
 zz = new_BVector_dble()
 px = new_BVector_dble()
 py = new_BVector_dble()
 pz = new_BVector_dble()

 write(charIFile,'(I4)')iFile
 
 do ii=1,3
  if(charIFile(ii:ii)<=' ') charIFile(ii:ii)='0'
 enddo

 open(newunit=infileu, file="outMyParticles"//charIFile//".dat", status="old", access="stream")
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

 ! pre-loop calc for 'histp'
 histp_vals = 0
 do ii=0,histp_nbins-1
  histp_pp(ii) = (ii+0.5_REAL64)*histp_dp
 enddo

 do ii=0,ids%size()-1
!  if(ncs%values(ii)>=0) then
   write(*,*) ids%values(ii), xx%values(ii), yy%values(ii), zz%values(ii), px%values(ii), py%values(ii), pz%values(ii)
!  endif

  ! in-loop calc of momentum histogram 'histp'
  ! check for in angular range

 enddo



end program pbuuproc
