program teste_escopo

integer   :: n1 = 5, n2 = 3

  print*, n1,n2
  call escreve(n1,n2)
  print*, n1,n2

end program teste_escopo

!---------- SUBPROGRAMAS --------------
subroutine escreve(a,b)
implicit none
integer, intent(in) :: a, b
integer :: x, y
  n1 = 4
  x = a
  y = b
  print*,n1,n2,a,b,x,y
end subroutine