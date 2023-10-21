program Media
    implicit none
    integer i,C(5)
    real :: soma
    real med

    write(*,*) " Digite os valores consecutivamente"
    read *,C
    print *, C
    soma = 0.0

    do i=1,5
        soma = soma + C(i)
    enddo
    med = soma / 5
    print *,'=========='
    print *," A média é = ", med 
end program Media