implicit none

real :: soma=0.0, media = 0.0, v=0.0
integer :: i,n


print*, "quantos valores?"
read*,n



do i = 1, n
    print*,i,'digite um valor'
    soma = soma + v
end do
media = soma / n

print*,"Media"
print*, media

end
