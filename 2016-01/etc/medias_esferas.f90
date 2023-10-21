program media_esferas
 
! Ler as n mediada do valor do diametro e da massa de uma esfera
! calcular o volume medio e a densidade media ds esferas

! Declarando variaveis

real, parameter :: pi=3.14

integer :: i, num_amostras = 4

real :: diametro, raio, massa, volume, rho_medio 
real :: diametro_medio, raio_medio,massa_media, volume_medio
real :: s_diametro, s_massa
! Determinar com precisao o diametro (diametro), a massa (massa)
! e a densidade (rho) das esferas fornecidas sendo:
! D = 2 r; V = 1/6 pi D^3; rho = 6/pi m/D^3

! Primeiro passo ler os diametros, para tantovamos usar um laço!

do i = 1, num_amostras ! de 1 ate o numero maximo de valores lidos
  print*,"Digite o valor para o diametro e para a massa: "
  read(*,*) diametro, massa
  ! Somatorio
  s_massa = s_massa + massa
  s_diametro = s_diametro + diametro
end do
  ! Calculo do diametro medio
  diametro_medio = s_diametro / num_amostras
  ! Calculo da massa media
  massa_media = s_massa / num_amostras
  ! calculo do raio medio
  raio_medio = diametro_medio / 2.0
  
  ! Calculo do volume medio V = 1/6 pi D^3
  volume_medio = (1.0 / 6) * pi * (diametro_medio**3)
  
  ! Calculando a densidade rho = 6/pi m/D^3
  rho_medio = (6.0 / pi) * ( massa_media / diametro_medio**3)
  
  ! Escrever os resultados
  print *, " Resultados "
  print *, "==================="
  print *, 'Diametro medio  :', diametro_medio
  print *, 'Volume medio    :', volume_medio
  print *, 'Densidade media :', rho_medio
  print *, "------------------"
  print *,''

end
