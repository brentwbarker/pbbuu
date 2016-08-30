module bwbfunc
 implicit none

contains

 subroutine outParticles
  use iso_fortran_env
  use arbstorage
  use global
  include 'PARTIC15'

  integer :: outFileU, outFile2U !< file unit for output file

  integer :: iFile=1 !< number of file output
  character(len=4) :: charIFile !< character representation of iFile, padded w zeros

  integer :: ii !< loop variable
  integer(kind=INT8), dimension(1) :: myInt
  integer(kind=INT8), dimension(2) :: myReal
  
  write(charIFile,'(I4)')iFile

  write(*,*)'iFile=',iFile

  do ii=1,3
   if(charIFile(ii:ii)<=' ') charIFile(ii:ii)='0'
  enddo

  write(*,'(3A)')'charIFile="',charIFile,'"'

!  open(newunit=outFileU, file="outParticles"//charIFile//".dat", status="replace", access="stream")

  open(newunit=outFile2U, file=fbase//charIFile//".parts", status="replace", access="stream")

  do ii=1,nq
   if(id(ii).ne.0) then
!    write(outFileU)int(id(ii),INT8),int(ipti(ii),INT8),ncs(ii),xx(ii) &
!                   ,yy(ii),zz(ii),px(ii),py(ii),pz(ii)

    call getBits_int(myInt,int(id(ii),INT32),8,.true.)
    write(outFile2U)myInt

    call getBits_int(myInt,int(ncs(ii),INT32),8,.true.) ! no. collisions, neg if just formed
    write(outFile2U)myInt

    call getBits_real(myReal, real(xx(ii),REAL64), 5, 10, .true.)
    write(outFile2U)myReal

    call getBits_real(myReal, real(yy(ii),REAL64), 5, 10, .true.)
    write(outFile2U)myReal

    call getBits_real(myReal, real(zz(ii),REAL64), 5, 10, .true.)
    write(outFile2U)myReal

    call getBits_real(myReal, real(px(ii),REAL64), 5, 10, .true.)
    write(outFile2U)myReal

    call getBits_real(myReal, real(py(ii),REAL64), 5, 10, .true.)
    write(outFile2U)myReal

    call getBits_real(myReal, real(pz(ii),REAL64), 5, 10, .true.)
    write(outFile2U)myReal

   endif

  enddo

  iFile=iFile+1

!  close(outFileU)
  close(outFile2U)

 end subroutine outParticles

end module bwbfunc
