program fatorial
implicit none

! Definicao de variaveis
integer :: xfat = 1
integer :: x 

! computacao do fatorial
do x = 1, 15
  xfat = xfat * x
  print *, x, xfat
end do

end program fatorial
! Fatorial $$n!=\prod_{k=1}^{n} k \,\, \forall n \in \mathbb{N}$$