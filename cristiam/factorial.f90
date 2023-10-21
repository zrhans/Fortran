program factorial
implicit none

!define variáveis
integer :: nfact = 1
integer :: n = 1

! calcula fatoriais
do while (n <= 10)
    nfact = nfact*n
    print*, n, " ", nfact
n = n + 1
end do
end program factorial