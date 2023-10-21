program arrays
implicit none

integer :: i
real, dimension(3) :: velocidades


print*, velocidades

! Adiconando o valor 3.0 para cada elemento do vetor velocidades
do i = 1, 3
  velocidades(i) = 3.0
end do

print*, velocidades

end program arrays