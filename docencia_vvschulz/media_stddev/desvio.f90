program desvio2
    implicit none
    real med, d, n, d2

    write(*,*) " Digite o valor da média"
    read *,med
    
    write(*,*) " Digite a nota"
    read *,n
    d = n-med    
    d2 = d**2

    print *," O desvio é = ", d
    print *," O quadrado do desvio é = ", d2
end program desvio2