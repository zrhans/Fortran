program calling_func
  implicit none

  real :: a
  real :: area_do_circulo

  a = area_do_circulo(2.0)  !!! IMPORTANTE

  Print *, "A área do circulo com raio 2.0 é "
  Print *, a

end program calling_func


! this function computes the area of a circle with radius r
function area_do_circulo(r)

! function result
implicit none

   ! dummy arguments
   real :: area_do_circulo

   ! local variables
   real :: r
   real :: pi

   pi = 4 * atan (1.0)
   area_do_circulo = pi * r**2

end function area_do_circulo
