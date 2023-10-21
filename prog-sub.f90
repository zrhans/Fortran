! Advanced: https://web.stanford.edu/class/me200c/tutorial_90/08_subprograms.html
!
program subprogramas
implicit none

real :: x = 0., y = 0.
real :: resultado = 0.
integer :: i

print*, "Função para calcular x+y"
print*, "-------interna----------"

write(*,200, advance='no') "Digite x: "
read(*,*) x
write(*,200, ADVANCE="NO")"Digite y: "
read(*,*) y
200 format(A)

resultado = fun(x,y)

write(*,*)"f = x + y --> ",resultado

contains

function fun(a,b)
  implicit none
  real, intent(IN) :: a,b
  real :: fun

  fun = a + b

end function fun  

end program subprogramas
