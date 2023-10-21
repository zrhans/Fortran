!Programa feito por Paloma Pauli 
!Vamos aprender : cometarios, ler e escrever na tela e operacoes algebricas
! Exercicio:Ler uma temperatura em graus celsius e converter para Fahrenheit e kelvin
!------------------------------------------------------------------------------------------
program aula
Implicit none

real*4:: tempC,tempF,tempK
! Perguntar para o usuário uma tempertatura em celsius
 write(*,*) "Escreva uma temperatura em graus Celsius"
!Ler temperatuta 
 Read(*,*) tempC
!Converter para Fahrenheit
tempF= tempC*1.8+32

!Converter para kelvin
tempK= tempC+273.15

!Devolver a tempertatura em Farenheit  e em Kelvin
write(*,*) "A temperatura em Fahrenheit e ", tempk
write(*,*) "A temperaratura em kelvin e", tempK

end program aula
