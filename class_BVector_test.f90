program class_BVector_test
 use class_BVector
 implicit none

 type(BVector_int) :: ids

 ids = new_BVector_int()

 write(*,*)ids%size()

 call ids%push_back(2)

 write(*,*)ids%values

end program class_BVector_test
