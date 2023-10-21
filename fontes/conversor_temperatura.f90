program conversor_temp

!Este programa converte temperaturas em graus Celsius para temperaturas em graus Fahrenheit e graus Kelvin
!Autor: Gustavo Gelson Cassenott - 201712608
!Disciplina: COMPUTACAO BASICA PARA FISICA-FORTRAN
!Professor: Hans Rogerio Zimermann

implicit none

!Declaracao das variaveis
real*4 :: tempC, tempF, tempK

!Programa pede para que o Usuario insira a temperatura
write(*,*) "Digite a temperatura em graus Celsius"

!Programa le a temperatura inserida pelo Usuario
read(*,*), tempC

!Programa converte a temperatura inserida em graus Celsius para graus Fahrenheit
tempF = tempC * 1.8 + 32

!Programa converte a temperatura inserida em graus Celsius para graus Kelvin
tempK = tempC + 273.15

!Programa retorna os resultados ao Usuario
write(*,*), "A temperatura em graus Celsius e", tempC, "C"
write(*,*), "A temperatura em graus Fahrenheit e", tempF, "F"
write(*,*), "A temperatura em graus Kelvin e", tempK, "K"

end program conversor_temp
