! generic global module for pBUU
module global
 use prec_def
 implicit none

 ! array for random number seed
 integer, allocatable, dimension(:), private :: iseeds

 integer, private :: piseed !< iseed for pawel's RNG

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
