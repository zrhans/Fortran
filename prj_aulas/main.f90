program common_1
implicit none
character dummy
real :: a = 6.0, b = 2.0, c
common /bloco1/ a, b

       call SUB
print*,"Digite uma letra para finalizar"
read(*,*),dummy
stop 'Programa finalizado'
end program common_1

subroutine SUB
real :: u, v
common /bloco1/ u, v
    print*,"u e v: ",u,v
end subroutine SUB
