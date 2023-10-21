program matriz_3x3
! Matriz
!
!     | 3 -7 4 |   | a11 a12 a13 |   | A(1,1) A(1,2) A(1,3) |
! A = | 0  1 5 | = | a21 a22 a23 | = | A(2,1) A(2,2) A(2,3) |
!     | 4  2 2 |   | a31 a32 a33 |   | A(3,1) A(3,2) A(3,3) |
!

integer A(3,3)

data A /3,0,4,-7,1,2,4,5,2/
print *," Matriz A "
print *,"=========="
print 100, ((A(i,j),j=1,3),i=1,3)

100 format(3(i2,1X),/)

101 format("A(",i1,",",i1,") = ",i2)

! Imprimindo elemento por elemento
print *,"_______________________"
print *," Elementos da Matriz A "
print *,"-----------------------"
do i=1,3
  do j=1,3
    print 101,i,j,A(i,j) ! mostrando primeiro as colunas
  enddo
enddo

stop '>>> Programa finalizado!'
end program matriz_3x3
