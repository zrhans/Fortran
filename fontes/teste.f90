! Leitura e escrita em arquivos -
!
! open(unit=*, file='nome_do_arquivo')
! write(unit=*,FMT=*)
! read(unit=*,FMT=*)
! close(unit=*)
!

program arquivos
implicit none

!Declaração de constantes e variaveis
integer :: i
real    :: x

open(10,file='paloma.txt')
do i = 1, 100
   write(10,111) sqrt( REAL(i) )
enddo
close(10)

!Lendo um arquivo
open(10,file='paloma.txt')
do i = 1, 10
   read(10,111) x
   print 111, x
   print 115,x
    print *,x
enddo

111 FORMAT(F8.2)
115 FORMAT(F8.5)

close(10)




end program arquivos
