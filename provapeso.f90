program CelsiusToKelvin

real:: celsius
character:: opt*1
real :: w = 0


print*, "qual o peso da avliação? "
read*,w

DO
print*, 'Entre com a nota da prova:'
read(*,*) nota
print*, nota, ' peso = ', w, ' Nota ponderada:' ,nota*w/10.0
print*, 'Deseja sair (S/N)'
read(*,*) opt

if(opt.eq.'s'.or.opt.eq.'S') exit
ENDDO

end program