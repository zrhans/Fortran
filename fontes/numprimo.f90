program numprimo

integer :: j = 20
print*,'Verificando, em uma lista, quais numeros são primos primo'

!do j = 1,20
    p = primo(j)
!end do

contains

function primo(n)

    do i = 1, n
      if (n == 1) then
          print*,' 1 eh especial'
      else
        if (MOD(n,i) == 0) then
            print*,i,' igual ',i,' x ',n/i
        else
            print*,i,' eh um numero primo'
        endif
      endif
    end do

end function primo

end program numprimo
