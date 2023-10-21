program teste

character, dimension(5) :: lista*20
integer*1 :: i

call system('echo "123456789012345678901234567890" > lista.txt')
call system('ls -lf *.F90 >> lista.txt')

open(1,file='lista.txt')
do i = 1, 5
    read(1,*, end=100) lista(i)
    print*,i, lista(i)
    
end do

100 print *, 'Fim de arquivo.'

end program