!> bvector - a class that mimics some functionality of the C std::vector class, an automatically resizing array
module class_BVector
 use iso_fortran_env
 implicit none
 private
 public BVector_dble, BVector_int, new_BVector_dble, new_BVector_int

! type BVector
!  class(BOneValue), dimension(:), allocatable :: values
!  integer                             :: sizeOf
! end type BVector

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 type BVector_int
  integer, dimension(:), allocatable :: values
  integer :: sizeOf
 contains
  procedure :: size => BVector_int_size
  procedure :: push_back => BVector_int_push_back
  procedure :: resize => BVector_int_resize
 end type BVector_int

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 type BVector_dble
  real(kind=REAL64), dimension(:), allocatable :: values
  integer :: sizeOf
 contains
  procedure :: size => BVector_dble_size
  procedure :: push_back => BVector_dble_push_back
  procedure :: resize => BVector_dble_resize
 end type BVector_dble
 
contains

! type(BVector) function new_BVector()
!  use iso_fortran_env
!
!  class(BOneValue), dimension(:), allocatable :: myOneValue
!  integer, dimension(10) :: temp
!
!  allocate(myOneValue(10))
!
!  allocate(thisValues(10), source=temp)
!  thisValues => temp
!
!  new_BVector = BVector(myOneValue,0)
!
! end function new_BVector
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 type(BVector_int) function new_BVector_int()

  integer, dimension(:), allocatable :: thisValues

  allocate(thisValues(0:9))

  new_BVector_int = BVector_int(thisValues, 0)

 end function new_BVector_int

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 type(BVector_dble) function new_BVector_dble()

  real(kind=REAL64), dimension(:), allocatable :: thisValues

  allocate(thisValues(0:9))

  new_BVector_dble = BVector_dble(thisValues, 0)

 end function new_BVector_dble

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 subroutine BVector_int_push_back(this,myValue)
  class(BVector_int), intent(inout) :: this
  integer, intent(in) :: myValue

  if (this%sizeOf>=size(this%values)) then
   call this%resize(int(1.5*this%sizeOf))
  endif

  this%values(this%sizeOf) = myValue
  this%sizeOf = this%sizeOf + 1

 end subroutine BVector_int_push_back

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 subroutine BVector_dble_push_back(this,myValue)
  class(BVector_dble), intent(inout) :: this
  real(kind=REAL64), intent(in) :: myValue

  if (this%sizeOf>=size(this%values)) then
   call this%resize(int(1.5*this%sizeOf))
  endif

  this%values(this%sizeOf) = myValue
  this%sizeOf = this%sizeOf + 1

 end subroutine BVector_dble_push_back

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 subroutine BVector_dble_resize(this,newCapacity)

  class(BVector_dble), intent(inout) :: this
  integer, intent(in) :: newCapacity

  real(kind=REAL64), dimension(:), allocatable :: temp

  allocate(temp(0:this%sizeOf-1))

  temp(0:this%sizeOf-1)=this%values(0:this%sizeOf-1)

  deallocate(this%values)

  allocate(this%values(0:newCapacity-1))

  if(newCapacity>=this%sizeOf) then
   this%values(0:this%sizeOf-1)=temp(0:this%sizeOf-1)
  else
   this%values(0:newCapacity-1)=temp(0:newCapacity-1)
   this%sizeOf=newCapacity
  endif

 end subroutine BVector_dble_resize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 subroutine BVector_int_resize(this,newCapacity)

  class(BVector_int), intent(inout) :: this
  integer, intent(in) :: newCapacity

  integer, dimension(:), allocatable :: temp

  allocate(temp(0:this%sizeOf-1))

  temp(0:this%sizeOf-1)=this%values(0:this%sizeOf-1)

  deallocate(this%values)

  allocate(this%values(0:newCapacity-1))

  if(newCapacity>=this%sizeOf) then
   this%values(0:this%sizeOf-1)=temp(0:this%sizeOf-1)
  else
   this%values(0:newCapacity-1)=temp(0:newCapacity-1)
   this%sizeOf=newCapacity
  endif

 end subroutine BVector_int_resize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 integer function BVector_dble_size(inBVector)

  class(BVector_dble), intent(in) :: inBVector

  BVector_dble_size = inBVector%sizeOf

 end function BVector_dble_size

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 integer function BVector_int_size(inBVector)

  class(BVector_int), intent(in) :: inBVector

  BVector_int_size = inBVector%sizeOf

 end function BVector_int_size

end module class_BVector
