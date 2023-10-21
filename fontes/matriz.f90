program mat

integer, dimension(4) :: vals_b
integer, dimension(2,2) :: mtx_a
integer, dimension(2,2) :: mtx_b

!---------------------------
! Propriedades de uma matriz
!---------------------------
print*,'Rank  : ', rank(mtx_b)
print*,'Shape : ', shape(mtx_b)
print*,'Size  : ', size(mtx_b)

! adicionando elementos modelo f77
do i = 1, 2
  do j = 1, 2
    mtx_a(i,j)=i
  end do
end do

print*,'--- l x c ---'
print*, 'mtx_a(1,1) ', mtx_a(1,1)
print*, 'mtx_a(2,1) ', mtx_a(2,1)
print*, 'mtx_a(1,2) ', mtx_a(1,2)
print*, 'mtx_a(2,2) ', mtx_a(2,2)

! adicionando elementos modelo f90
! Um construtor de matrizes cria um vetor (matriz de posto 1) contendo valores constantes.
! Sintaxe: A forma geral de um construtor de matrizes ´e a seguinte:
! (/ <lista de valores do construtor> /)
vals_b  = (/1,2,1,2/)
print*, vals_b

! Uma matriz de posto maior que um pode ser constru´ıda a partir de um construtor de matrizes atrav´es
! do uso da função intrínseca RESHAPE. Por exemplo,
! RESHAPE( SOURCE= (/ 1, 2, 3, 4, 5, 6 /), SHAPE= (/ 2, 3 /) )

! mtx_b = reshape( SOURCE = (/1,2,1,2/), SHAPE = (/2,2/) )
! mtx_b = reshape((/1,2,1,2/), (/2,2/))
! mtx_b = reshape( vals_b, (/2,2/) )
mtx_b = reshape(vals_b, shape(mtx_b))


print*,'--- l x c ---'
print*, 'mtx_b(1,1) ', mtx_b(1,1)
print*, 'mtx_b(2,1) ', mtx_b(2,1)
print*, 'mtx_b(1,2) ', mtx_b(1,2)
print*, 'mtx_b(2,2) ', mtx_b(2,2)
print*,

! Imprimindo Formatada

write(*,100) mtx_b

100 format('|',i2,1x,i2,'|')



end program mat
