!Fonte: https://www.youtube.com/watch?v=Q0q3_HrqWJg
program ordenador
implicit none

integer, dimension(10) :: v
integer :: i,j,m,n

print *," Tamanho do vetor:"
read *,n

do i = 1,n
    print *,'v(',i,')='
    read *,v(i)
end do

do i = 1, n
    m = v(i)
    j = i - 1
    do while ( v(j) > m .and. j > 0)
        v(j+1) = v(j)
        j = j - 1
    end do
    
    v(j+1) = m
end do

do i = 1, n
    print *,'v(',i,')=',v(i)
end do

end program ordenador