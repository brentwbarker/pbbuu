module arbstorage
 use iso_fortran_env
 implicit none

 ! todo beginning of output file should be a specification of nExp, nSig, and isSigned. So it should start with 2 integers, again bit-enforced by this module, to account for variation of bit storage by different compilers.

 ! in storing, negative zero gets translated to positive zero

 ! there is no explicit handling of NaN or Inf values

 ! in storing, if number is closer to zero than can be handled by the size of exponent, then the number is instead stored as zero.

 ! store log base e of 2 here to use below.
 real(kind=8), parameter, private :: arbstorage_invln2 = 1.0d0/log(2.0d0)

 ! use this subroutine to get reals or integers
! interface getBits
!  module procedure getBits_int, getBits_real
! end interface

 contains

 subroutine fromBitsToInt(outNum, inArray, nBits, isSigned)
  integer,      intent(out) :: outNum !< output number
  integer,      intent(in) :: nBits !< number of binary digits in storage
  logical,      intent(in) :: isSigned !< true if output should be signed, false otherwise
  integer(kind=INT8), dimension(0:), intent(in) :: inArray

  integer(kind=INT8), dimension(0:size(inArray)-1) :: procArray
  logical :: isNegative
  integer :: ii

  isNegative=.false.
  procArray=inArray

  outNum=0

  !detect if negative, if signed
  if(isSigned.and.getArrayBit(inArray,0)==1) then
   isNegative=.true.

   !invert two's complement   
   procArray=not(procArray)
   call intBitsAddOne(procArray)
  endif

  do ii=0,nBits-1
   outNum=outNum+2**(nBits-ii-1)*getArrayBit(procArray,ii)
  enddo

  if(isNegative) outNum=-outNum

 end subroutine fromBitsToInt

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 subroutine fromBitsToReal(outNum, inArray, nExp, nSig, isSigned)

  real(kind=8), intent(out) :: outNum !< output double precision floating-point number
  integer(kind=int8) &
      ,dimension(0:) &
                ,intent(in) :: inArray !< array of 1 to 8 8-bit bytes
  integer,       intent(in) :: nExp !< number of binary digits in exponent
  integer,       intent(in) :: nSig !< number of binary digits in significand
  logical,       intent(in) :: isSigned !< true if output should be signed, false otherwise

  integer :: ii !< loop variable
  integer :: nSigned !< 1 if signed, 0 if not
  integer :: expo !< num=sig*2**expo

  if(isSigned) then
   nSigned=1
  else
   nSigned=0
  endif

  ! implicit 1 from the significand (we only store the 1.xxxxxx part)
  outNum=1.0d0

  ! extract significand
  do ii=nSigned+nExp,nSigned+nExp+nSig-1
!   write(*,*)'getArrayBit-sig=',getArrayBit(inArray,ii)*2.0**(-1)
   outNum=outNum + getArrayBit(inArray,ii)*2.0d0**(-(ii-nSigned-nExp+1))
  enddo

  ! extract exponent
  expo=0
  do ii=nSigned,nSigned+nExp-1
   expo=expo+getArrayBit(inArray,ii) * 2**(nSigned+nExp-1-ii)
  enddo

!  write(*,*)'firstexpo,outNum=',expo,outNum

  ! detect special case of zero
  if(expo==0.and.outNum==1.0d0) then
   outNum=0.0d0
   return
  endif

  expo=2**(nExp-1)-1-expo
  
!  write(*,*)'expo=',expo

  outNum=outNum * 2.0d0**expo

  if(isSigned.and.ibits(inArray(0),7,1)==1) outNum=-outNum

 end subroutine fromBitsToReal

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 subroutine getBits_real(outArray, inNum, nExp, nSig, isSigned)

  !todo let this function accept any KIND of real. (or complex?)
  real(kind=8), intent(in) :: inNum !< input number to be converted
  integer,      intent(in) :: nExp !< number of binary digits in exponent
  integer,      intent(in) :: nSig !< number of binary digits in significand
  logical,      intent(in) :: isSigned !< true if output should be signed, false otherwise
  integer(kind=INT8), dimension(0:), intent(out) :: outArray

  integer :: nSigned !< set to 1 if isSigned=.true., 0 otherwise
  integer :: iBit, thisBit !< loop variable for array loop
  real(kind=8) :: procNum !< number in processing
  integer :: expNum !< exponent in base-2

!  write(*,'(a,b64.64)')'inNum=',inNum

  ! check to ensure that the file storage unit size is 8 bits
  if(FILE_STORAGE_SIZE/=8) then
   error stop 'In module arbstorage, getBits: file_storage_size is not 8 bits.' &
              //'Modify code to handle this compiler implementation.'
  endif

  if(isSigned) then
   nSigned=1
  else
   nSigned=0
  endif

  ! check to ensure that nExp + nSig + isSigned is integer number of bytes and that number is the size of outArray
  if( mod(nExp+nSig+nSigned,8) /= 0 ) error stop 'In module arbstorage, getBits: nExp+nSig+nSigned must be integer number of bytes'

  if( (nExp+nSig+nSigned)/8 /= size(outArray) ) then
   error stop 'In module arbstorage, getBits: (nExp+nSig+nSigned)/8 must be size of outArray.'
  endif

  ! if number is too near zero, then set to zero
  if(abs(inNum)<2.0_8**(-2.0_8**(nExp-1))) then
   procNum=0
  else
   procNum=inNum
  endif

  ! start dividing and storing bits.

  outArray=0

  !set sign bit, if needed
  if(isSigned.and.procNum<0) then
!   outArray(1)=ibset(outArray(1),7)
   call setArrayBit(outArray,0)
  endif

!  write(*,'(b8.8)')outArray


  ! find exponent in base-2
  expNum = floor(log(abs(procNum))*arbstorage_invln2)
!  write(*,*)'expNum=',expNum

  ! number to use to find significand later. Should end up a positive number between 0 and 1.
  procNum = abs(procNum)/2.0d0**expNum-1
!  write(*,*)'procNum=',procNum

  ! round the last bit in advance. According to IEEE 754, standard is to round to even (least significant bit = 0) if there is exactly 0.5 left in procNum.
  procNum=procNum*2**nSig
!  write(*,*)'rounding',procNum
  if( procNum-int(procNum) > 0.5 .or. ( procNum-int(procNum)==0.5 .and. procNum/2 - int(procNum/2) > 0.5 ) ) then
   procNum = abs(inNum)/2.0**expNum-1 + 2.0d0**(-nSig)
  else
   procNum = abs(inNum)/2.0**expNum-1
  endif

  ! write exponent using two's complement: 2^(nSig-1)-1
  expNum=2**(nExp-1)-1-expNum
!  write(*,*)'expNum_biased=',expNum

  ! write exponent into bits
  thisBit=nSigned+nExp 
  do while (expNum>0)
   thisBit=thisBit-1
   if(thisBit<nSigned) then
    write(ERROR_UNIT,*)'Error dump: nExp,inNum=',nExp,inNum
    error stop 'in module arbstorge, getBits: exponent too large for length of exponent bits.'
   endif
   if(mod(expNum,2)==1) call setArrayBit(outArray,thisBit)
   expNum=expNum/2
!   write(*,'(A,2b8.8)')'outArray=',outArray
  enddo

  ! write significand into bits. 
  do iBit=nSigned+nExp,size(outArray)*8-1
   procNum=procNum*2
   if(procNum>=1) then
    call setArrayBit(outArray,iBit)
    procNum=procNum-1
   endif
!   write(*,*)'iBit,procNum:',iBit,procNum
  enddo

!  write(*,*)'round procNum v2',procNum

 end subroutine getBits_real

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 subroutine getBits_int(outArray,inNum,nBits,isSigned)
  integer,      intent(in) :: inNum !< input number to be converted
  integer,      intent(in) :: nBits !< number of binary digits in storage
  logical,      intent(in) :: isSigned !< true if output should be signed, false otherwise
  integer(kind=INT8), dimension(0:), intent(out) :: outArray

  integer :: thisBit !< loop variable for array loop
  integer :: procNum !< working variable for inNum

  outArray=0

  ! check to ensure that the file storage unit size is 8 bits
  if(FILE_STORAGE_SIZE/=8) then
   error stop 'In module arbstorage, getBits: file_storage_size is not 8 bits.' &
              //'Modify code to handle this compiler implementation.'
  endif

  ! check to ensure that nExp + nSig + isSigned is integer number of bytes and that number is the size of outArray
  if( mod(nBits,8) /= 0 ) error stop 'In module arbstorage, getBits_int: nBits must be integer number of bytes'

  if( nBits/8 /= size(outArray) ) then
   error stop 'In module arbstorage, getBits_int: (nBits+nSigned)/8 must be size of outArray.'
  endif

  if(isSigned.and.(inNum<-2**(nBits-1).or.inNum>2**(nBits-1)-1)) then
   error stop 'In module arbstorage, getBits_int: integer magnitude is too large to store.'
  elseif(.not.isSigned.and.(inNum<0.or.inNum>2**(nBits)-1)) then
   error stop 'In module arbstorage, getBits_int: integer magnitude is too large to store.'
  endif

  procNum=abs(inNum)

  ! write into bits
  thisBit=nBits
  do while (procNum>0)
   thisBit=thisBit-1
   if(thisBit<0) error stop 'in module arbstorge, getBits: exponent too large for length of exponent bits'
   if(mod(procNum,2)==1) call setArrayBit(outArray,thisBit)
   procNum=procNum/2
!   write(*,'(A,2b8.8)')'outArray=',outArray
  enddo

  if(inNum<0) then
   outArray=not(outArray)
   call intBitsAddOne(outArray)
  endif

 end subroutine getBits_int  

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 !> intBitsAddOne - add 1 to a binary number stored as an array of 8-bit integers. Ignore overflow in the case where it is already all 1's.
 subroutine intBitsAddOne(bitArray)

  integer(kind=INT8), dimension(0:), intent(inout) :: bitArray

  integer :: thisBit

  thisBit=size(bitArray)*8-1

  do while(.true.)

   if(getArrayBit(bitArray,thisBit)==0) then
    call setArrayBit(bitArray,thisBit)
    exit
   else
    call clearArrayBit(bitArray,thisBit)
    thisBit=thisBit-1
    if(thisBit<0) then
     exit
    endif
   endif
  enddo


 end subroutine intBitsAddOne

 !> getArrayBit - for an array of 1-byte integers that is logically one string of bits, this returns the bit at position 'pos' as an integer. the right-most (least significant) bit has pos=size(bitArray)*8-1, and the left-most (most significant) bit has pos=0.
 integer function getArrayBit(bitArray,pos)

  integer(kind=INT8), dimension(0:), intent(in) :: bitArray
  integer, intent(in) :: pos

  integer :: baindex

  baindex=pos/8

  getArrayBit=ibits(bitArray(baindex),7-(pos-baindex*8),1)

 end function getArrayBit

 !> clearArrayBit - for an array of 1-byte integers that is logically one string of bits, this sets the bit at position 'pos' to 1. the right-most (least significant) bit has pos=size(bitArray)*8-1, and the left-most (most significant) bit has pos=0.
 subroutine clearArrayBit(bitArray,pos)
  integer(kind=INT8), dimension(0:), intent(inout) :: bitArray
  integer, intent(in) :: pos

  integer :: baindex

  baindex=pos/8

  bitArray(baindex) = ibclr(bitArray(baindex),7-(pos-baindex*8))

 end subroutine clearArrayBit

 !> setArrayBit - for an array of 1-byte integers that is logically one string of bits, this sets the bit at position 'pos' to 1. the right-most (least significant) bit has pos=size(bitArray)*8-1, and the left-most (most significant) bit has pos=0.
 subroutine setArrayBit(bitArray,pos)
  integer(kind=INT8), dimension(0:), intent(inout) :: bitArray
  integer, intent(in) :: pos

  integer :: baindex

  baindex=pos/8

  bitArray(baindex) = ibset(bitArray(baindex),7-(pos-baindex*8))

 end subroutine setArrayBit

end module arbstorage
