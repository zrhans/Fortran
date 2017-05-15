program chama_func
  implicit none

  real :: a
  real :: area_do_circulo

  a = area_do_circulo(2.0)  !!! IMPORTANTE

  print *, "A área do circulo com raio 2.0 é "
  print *, a

end program chama_func


! this function computes the area of a circle with radius r  
function area_do_circulo(r)

! resultado da função    
implicit none

   ! argumentos fictícios    
   real :: area_do_circulo

   ! variaveis locais
   real :: r
   real :: pi

   pi = 4 * atan (1.0)
   area_do_circulo = pi * r**2

end function area_do_circulo
