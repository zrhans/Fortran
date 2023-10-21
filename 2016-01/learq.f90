!http://www.cs.mtu.edu/~shene/COURSES/cs201/NOTES/chap04/iostatus.html
program exemplo
implicit none
integer :: io = 0 !end of file reached ... < 0 do normal stuff  < Something Wrong... 
real :: a,b,c
 character(10)  :: arquivo
 character(100) :: colunas

!arquivo = 'dados.csv'
arquivo = 'dados.txt'

open(1,file = arquivo)

!read(1,'(a)',iostat=io) colunas
!print *, trim(colunas)

do
    read(1,*,iostat=io)a,b,c
    if (io < 0) exit
    print*,a,b,c
enddo

close(1)
end program exemplo