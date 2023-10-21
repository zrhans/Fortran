!************************************************************
!Nome: Daniel de Moraes Becker
!Matrícula: 2018510249
!Este programa pede o raio e cor de 10 esferas, dando como
!resultado a cor da esfera de maior area.
!************************************************************
program esferas

! declaracao de variaveis
integer :: n, i, raio, pi = 3.1415
real:: raio, raioM
character*15 :: cor, corM

!laco de repeticao que pergunta o raio e cor das esferas de 1 a 10
do n= 1,10
i = i + 1
print*, "Digite o ", i, " raio e sua cor:"
read*, raio, cor

!comando if para determinar se o raio digitado é maior que o antigo maior raio digitado
	if (raio > raioM) then
		raioM = raio
		corM = cor
	end if
end do

!calculo da area da esfera 
area = 4*pi*(raio**2)

!printa os valores finais 
print*, "A esfera de maior area eh: ", area, ", com um raio = ", raioM
print*, "Sua cor eh: ", corM

end program esferas
