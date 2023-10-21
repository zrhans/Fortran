program substring
implicit none

!Declaracao das variaveis
                          !12345678901234567890123456
character(26) :: substr = 'Seja bem vindo ao sistema!'
                       
print *, substr
print *, substr(6:14)

end program substring

! Programa errado.
! Cap 3.3, (SAM KEY, FORTRAN Programming successin a day - 2nd Edition)
