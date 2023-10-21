program arquivos


integer m,n
real r,s

m = 2;    n = 213;
r = -1.0; s = 1e+3

open(9,file='dados.txt',status='old')

do i = 1, 5
    write(9,*) m+i,n+i,r+i,s+i
enddo


!Procedimento acima gterou um arquivo de dados de 295 bytes.
open(10,file='dados2.txt')

do i = 1, 5
    write(10,100) m+i,n+i,r+i,s+i
enddo
!Procedimento acima gerou um arquivo de dados de 115 bytes.
!Isto Mostra a importancia da formatacao adequada da saida de dados

100 format(I2,1X,I3,F8.2,F8.2)


open(11,file='dados3.txt')

do i = 1, 5
    write(11,110) m+i,n+i,r+i,s+i
enddo
!Procedimento acima gerou um arquivo de dados de 115 bytes.
!Isto Mostra a importancia da formatacao adequada da saida de dados

110 format(2(1x,I3),2(F8.2))

 close(9)
 close(10)
 close(11)

!/*----------------------------------
!*        LEITURA
!*/

open(9,file='dados.txt',status='old')

do i = 1, 5
    read(9,*) m,n,r,s
    write(*,*) m,n,r,s
enddo
 close(9)

open(9,file='dados.txt',status='old')

do i = 1, 5
    read(9,*) m,n,r,s
    !Mostrando no formato 110
    write(*,110) m,n,r,s
enddo




end program arquivos

!Resultados Escrita
!-rw-rw-r-- 1 hans hans   115 Jun  3 08:34 dados2.txt!
!-rw-rw-r-- 1 hans hans   125 Jun  3 08:34 dados3.tx
!-rw-rw-r-- 1 hans hans   295 Jun  3 08:34 dados.txt


!hans@hasus:~/tmp$ cat dados.txt dados2.txt dados3.txt 
!           3         214   0.00000000       1001.00000    
!           4         215   1.00000000       1002.00000    
!           5         216   2.00000000       1003.00000    
!           6         217   3.00000000       1004.00000    
!           7         218   4.00000000       1005.00000    
! 3 214    0.00 1001.00
! 4 215    1.00 1002.00
! 5 216    2.00 1003.00
! 6 217    3.00 1004.00
! 7 218    4.00 1005.00

!   3 214    0.00 1001.00
!   4 215    1.00 1002.00
!   5 216    2.00 1003.00
!   6 217    3.00 1004.00
!   7 218    4.00 1005.00

!Resultados Leitura
!hans@hasus:~/tmp$ gfortran arquivos.f90
!hans@hasus:~/tmp$ ./a.out 
!           3         214   0.00000000       1001.00000    
!           4         215   1.00000000       1002.00000    
!           5         216   2.00000000       1003.00000    
!           6         217   3.00000000       1004.00000    
!           7         218   4.00000000       1005.00000    
!   3 214    0.00 1001.00
!   4 215    1.00 1002.00
!   5 216    2.00 1003.00
!   6 217    3.00 1004.00
!   7 218    4.00 1005.00
!hans@hasus:~/tmp$ 



