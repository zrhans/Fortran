program binario
integer, parameter :: bits = 8
character(bits) :: num_binario = '00101010'
integer*1 :: bit
integer :: inf, sup

read(num_binario(1:1),'(I1)') bit

num_decimal = -2.0**(bits-1) * bit

inf = 1; sup = bits

print*, "Numero binario: --> ", num_binario

do i = inf, sup-1 ! String Fortran comeca com indice 1 entao sup-1 equivale n-2 pq na formula n=0 -> n-2
  n = i - 1
  read(num_binario(bits-n:bits-n),'(I1)') bit
  res = res + ( 2**(n) ) * bit
  !print*, '2^',n,'a(',i,')'," -->", num_binario(bits-n:bits-n)
end do

num_decimal = num_decimal + res

print*, "Numero decimal: ", num_decimal

end program binario