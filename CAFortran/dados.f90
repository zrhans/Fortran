real a,b,c

nLines = 0

open(10,file='dados.txt', status='old')

!read(10,'(3f10.5)')a,b,c,d
 

do while ( ios == 0 )
      read(10, *, iostat=ios, end=100)a,b,c,d
      print*,a,b,c,d
      !print*,ios
      nLines = nLines + 1
enddo
100 continue
print*, 'O arquivo tem ',nLines,' linhas'
end
