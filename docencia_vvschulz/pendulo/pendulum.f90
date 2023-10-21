program pendulo
    implicit none
    
    real, parameter :: pi = 3.14159, g = 9.8
    real :: periodo = 0 ! variaveis reais (ponto flutuante)
    integer :: l
    
    do 1 = 1, 50, 5
        periodo = 2* pi * sqrt (l/g)
        print*, "L = ", l, "período T = ", periodo
    end do
    
    end program   
