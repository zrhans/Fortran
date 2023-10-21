program VarDesvioP
    implicit none
    integer i
    real :: soma,C(5)
    real med, dp

    write(*,*) " Digite os valores obtidos do desvio ao quadrado consecutivamente"
    read *,C
    soma = 0.0

    do i=1,5
        soma = soma + C(i)
    enddo
    med = soma / 5
    dp = SQRT(med)
    print *," A soma dos quadrados dos desvios é = ", soma
    print *,'================================'
    print *," A variância é = ", med
    print *,'================================'
    print *," O desvio padrão é = ", dp

end program VarDesvioP