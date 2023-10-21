program var_avancada
implicit none

integer,dimension(5) :: vetor1 = (/1,2,3,4,5/)
integer,dimension(5) :: vetor
                 !colunas e linhas
real,dimension(3,2) :: mtx_01 = reshape((/1.,2.,3.,4.,5.,6./), (/3,2/))
integer :: i,j, eltos, dims

print *, 'vetor1= ',vetor1

!print*, size(vetor)
!print*, rank(vetor)
eltos = size(mtx_01) ! elementos
dims = rank(mtx_01) ! dimensoes
print*,'Elementos na linha : ', size(mtx_01(:,1))
print*,'Elementos na matriz: ', eltos
print*,'Dimensoes          : ', dims

print '(3f8.2)', mtx_01
print*,"-----"

do i = 1, 2
    print*, (mtx_01(i,j),j=1,3)
end do

end program var_avancada