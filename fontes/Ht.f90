program trabalho
implicit none

    ! Declaração de tipos e variáveis
    real :: C, F, K
    
    ! Perguntar ao usuario uma temperatura em celsius
    write(*,*) "Escreva uma temperatura em graus Celsius"
    
    ! Ler a temperatura
    read(*,*) C
    
    ! Conversão para Fahrenheit (F)
    F = (C*1.8)+32
    
    ! Conversão para Kelvin (K)
    K = C + 273.15
    
    ! Resultado das conversões 
    write(*,*) "A temperatura em fahrenheit e ", F
    write(*,*) "A temperatura em Kelvin e ", K
    
endprogram trabalho
