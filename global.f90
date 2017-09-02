! generic global module for pBUU
module global
 use prec_def
 implicit none

 ! array for random number seed
 integer, allocatable, dimension(:), private :: iseeds

 integer, private :: piseed !< iseed for pawel's RNG

 !> basename of run (7-characters, first 3 set in NUCLS, others random
 character(len=7) :: fbase

 ! output settings
 character(len=:), allocatable :: inputFileName
 character(len=132) :: stringBuffer

 logical :: writeOutParticles !< write all particles out every timestep

 real(kind=REAL64), parameter, dimension(1:5) &
  :: masses = (/.9383,  .9396, 1.8757, 2.8077 &
             , 2.8098, 3.728,  1.232,  1.232  &
             , 1.232,  1.232,  1.6,    1.6 &
             , 0.13957018, 0.1349766, 0.13957018, 1.0 /)

contains

 !turns the random number call from a subroutine into a function
 real function getRan()
  call random_number(getRan)
 end function getRan

 !> tests whether the random number generator has been seeded at least once by the global_ranInit subroutine
 logical function global_isRanSeeded() 
  global_isRanSeeded=allocated(iseeds)
 end function global_isRanSeeded

 !> initialize random number generator with given integer iseed. Maintains backward compatibility for iseed argument with deprecated ran(iseed) format, even though the new generator takes an array of integers instead of a single one. We just set each of the integers equal to the same input seed value.
 subroutine global_ranInit(iseed)
  integer, intent(in) :: iseed

  integer :: nseeds

  if(.not.allocated(iseeds)) then
   call random_seed(size = nseeds)
   allocate(iseeds(nseeds))
  endif

  iseeds=iseed
  call random_seed(put = iseeds)

 end subroutine global_ranInit

! !> global_ranInit --- initializes RNG from Pawel
!subroutine global_ranInit(iseed)
!  integer, intent(in) :: iseed
!
!  piseed=iseed
!
! end subroutine global_ranInit

! !> getRan --- returns pseudo-random number from Pawel's PRNG.
! real(long) function getRan()
!
!  real(long), parameter :: aa = 69069._Long, &
!                             mm = 2147483648._Long, &
!                             rm = 1._Long/mm
!  
!  piseed=mod(piseed*aa,mm)
!  getRan = piseed*rm
!
! end function getRan

end module global
