 program xif
    implicit none
    real :: x
    real, parameter :: x1 = 0.3, x2 = 0.6
    call random_seed()
    call random_number(x)
    if (x < x1) then
       print*,x,"<",x1
    else if (x < x2) then
       print*,x,"<",x2
    else
       print*,x,">=",x2
    end if
 end program xif