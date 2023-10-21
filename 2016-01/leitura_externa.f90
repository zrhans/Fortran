! programa para ler um conjunto de dados de temperatura
! em Celsius e converter cada uma para Fahrenheit e Kelvin
!
! C = 9/5 * (F - 32 ) 
! K = ºC + 273.15
!-------------------------------------------------------------------


program conversor_de_temperaturas
implicit none

! Declaração de variáveis
real    :: Temp_C=0.0, Temp_F=0.0, Temp_K=0.0
real  :: Soma_Temp_C=0.0, Soma_Temp_F=0.0, Soma_Temp_K= 0.0
real  :: Media_Temp_C=0.0, Media_Temp_F=0.0, Media_Temp_K=0.0
integer :: i


! Abrindo um arquivo
open(unit=10, file="temperaturas_celcius.txt") ! Fonte de dados

open(unit=11, file="temperaturas.txt") ! Fonte de dados

! Usando estrutura de repetição para escrever 10 numeros no arquivo
do i = 1,10
  
  !Sintaxe: read(<unit>, <formato>) lista_de_variaveis
  read(10,*) Temp_C
  
  
  Temp_F = 1.8 * Temp_C + 32 ! convertendo para Fahrenheit
  Temp_K = Temp_C + 273.15   ! convertendo para Kelvin
  
  
  print *, Temp_C, Temp_F, Temp_K
  
  write(11,100)Temp_C, Temp_F, Temp_K
  
  ! somatorios (necessarios para calculo da media)
  Soma_Temp_C = Soma_Temp_C + Temp_C
  Soma_Temp_F = Soma_Temp_F + Temp_F
  Soma_Temp_K = Soma_Temp_K + Temp_K
 
end do

!Definindo o formato
100 format(f5.2,1x,f5.2,1x,f8.2)
101 format(",f5.2,f5.2,f8.2)


   !calculo das médias das temperaturas
   Media_Temp_C = Soma_Temp_C / (i-1)
   Media_Temp_F = Soma_Temp_F / (i-1)
   Media_Temp_K = Soma_Temp_K / (i-1)

  ! Escrevendo as medias no arquivo
  write(11,*)"-------------------------------------------------"
  write(11,101) Media_Temp_C, Media_Temp_F, Media_Temp_K

close(10)
close(11)
end program conversor_de_temperaturas