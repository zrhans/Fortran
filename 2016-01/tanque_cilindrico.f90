program tanque_cilindrico
implicit none

!Declaração de vars e constantes

real , parameter    :: pi = 3.14159
real                :: diametro = 16.0  ! 16 metros
real                :: raio = 0.0
real                :: altura = 15.0    ! 15 metros
real                :: volume, volume_20

! Calculando Volume original
! V = pi.r^2.h    <- volume de um cilindro reto

! Encontrando o raio
raio = diametro / 2.0

volume = altura * pi * raio**2 

! Definindo um volume 20% maior
! V20 = V + (V * 20.0/100.)
! volume_20 = volume * 1.2
! volume_20 = Volume + volume * 0.2

volume_20 = volume + (volume * (1000.0/100.0)) 

! Calculando Raio do Volume 20% maior
! V20 = pi.r20^2.h    <- volume de um cilindro reto
! r20 = sqrt( V20 / (pi * h)  )
! r20 = ( V20 / (pi * h)  )**(1.0/2.0)

raio = ( volume_20 / (pi * altura)  )**(1.0/2.0)

write(*,200), raio
print *, "O raio do tanque com volume 20% maior vale: ", raio

! Formatação
! Caractere aw onde w é o tamanho
! Real fw.d onde f = flutuante( numeros reais) w é o tamanho e d o numero de casas decimais

! Escrevendo com especificação de formato
write(*, 100), "O raio do tanque com volume 20% maior vale: ", raio
write(*, 101), "O raio do tanque com volume 20% maior vale: ", raio
write(*, 102), raio, volume_20


!DEfinindo o rotulo 100
! Descritopres do formato (a,f10.3)

100 format(1x,a, f15.3)
101 format(1x,a, f6.3)
102 format(50('-'),/,2x,'Raio [m]',8x,'Volume [m^3]',/,50('-'),/,f8.3,5x,f10.3,/)

200 format(1x, 'O raio do tanque com volume 20% maior vale:',1x, f8.3)


end program tanque_cilindrico