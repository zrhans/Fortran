program logicanumerica
! **************************************************************
! Nome :Juliano Streb domingues    Matricula : 201812517
! Data de entrega : 08|05|2018 

implicit none
 real ::  x, y ! Definindo variaveis
 
 print*, "Digite dois numeros, x e y "  ! pedir dois valores x e y ao usuario
 
 read*, x, y      ! Ler x e y

!Inicio do bloco de Logica codicional
if( x == y ) then     

!Se x for igual a ,mostrar na tela a mensagem seguinte
      print*, "x e igual a y"


else if( x > y ) then

! Se x for maior do que y, mostrar na tela a mensagem seguinte
         print*, "x e maior do que y"

else if(x < y) then

!Se x for menor do que y, mostrar na tela a mensagem seguinte
         print*, "x e menor do que y"

end if                    ! Fim do bloco condicional



end program logicanumerica
