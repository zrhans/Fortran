program esferas

 implicit none

  !primeiro declaramos as variaveis
  character (len=10) :: c1, c2, c3, c4, c5, c6, c7, c8, c9, c10
  real :: as
  integer :: r
  
  !logo é feito a estrutura do calculo da area superficial
  do r = 1,10
   as = 4 * 3.14 * (r**2)
   
  !aqui usamos uma estrutura de repetição para calcular as areas superficiais 
  !também é pedido a cor das esferas
  if (r .eq. 1) then
    print*, "Digite a cor da primeira esfera:"
    read*, c1
  else if (r .eq. 2) then
    print*, "Digite a cor da segunda esfera:"
    read*, c2
  else if (r .eq. 3)then
    print*, "Digite a cor da terceira esfera:"
    read*, c3
  else if (r .eq. 4) then
    print*, "Digite a cor da quarta esfera:"
    read*, c4
  else if (r .eq. 5) then
    print*, "Digite a cor da quinta esfera:"
    read*, c5
  else if (r .eq. 6) then
    print*, "Digite a cor da sexta esfera:"
    read*, c6
  else if (r .eq. 7) then
    print*, "Digite a cor da setima esfera:"
    read*, c7
  else if (r .eq. 8) then
    print*, "Digite a cor da oitava esfera:"
    read*, c8
  else if (r .eq. 9) then
    print*, "Digite a cor da nona esfera:"
    read*, c9
  else if (r .eq. 10) then
    print*, "Digite a cor da decima esfera:"
    read*, c10
 end if

  !com o comando seguinte é mostrado o raio e a area superficial da esfera
  !em seguida a estrutura de repetição é finalizada
  print*, "raio", r, "area", as
 enddo

  !e por ultimo mostramos apenas a cor e a area da esfera de maior area superficial
  print*, "A cor da maior esfera é:   ", c10, "e sua area superficial é:", as

end program esferas
