program arbstoragetest
 use arbstorage

 integer(kind=1), dimension(8) :: myBits, myInBits
 integer :: outFileUnit, inFile, myInt
 real(kind=8) :: myReal
 integer(kind=1), dimension(4) :: myBitsInt, myInBitsInt

! call getBits(myBits,-1.00146484375d0,5,10,.true.)
 call getBits_real(myBits,1.0d0,11,52,.true.)
! call getBits(myBits,-0.0d0,5,10,.true.)

 call getBits_int(myBitsInt,-921,32,.true.)

 write(*,'(8b8.8)')myBits
 write(*,'(a,4b8.8)')'myBitsInt=',myBitsInt
 write(*,'(b32.32)')-921


 open(newunit=outFileUnit, file="output.dat", status="unknown", access="stream")

 write(outfileUnit)myBits, myBitsInt

 close(outFileUnit)

 open(newunit=inFile, file="output.dat", status="old", access="stream")

 read(inFile)myInBits, myInBitsInt

 call fromBitstoReal(myReal, myInBits, 11, 52, .true.)
 call fromBitstoInt(myInt, myInBitsInt, 32, .true.)

 write(*,*)'myReal=',myReal
 write(*,'(b64.64)')myReal
 write(*,'(b64.64)')1.0d0

 write(*,*)'myInt=',myInt
 write(*,'(b32.32)')myInt
 write(*,'(b32.32)')-921

  close(inFile)



end program arbstoragetest

