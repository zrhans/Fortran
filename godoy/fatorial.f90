program fat

implicit none

!definir variaveis

integer :: nfact = 1

integer :: n

do while (n <= 10)

nfact = nfact * n

print *, n, " " nfact

n = n + 1

end do

end program fat