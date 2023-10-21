program generico

integer, parameter :: n = 10
integer, dimension(0:n) :: x
integer :: xmax = 100, xmin = 0
real :: dx = 0.0
!=============================================
! Gerando eixo X
!=============================================

dx = (xmax - xmin) / n
print*,'dx: ',dx
x(0:n) = ( i*dx , i = 0, n)
print '(10(f8.2,/))',( (i*dx) , i=0,n)
print '(a,/)','====================='
print*,x
end program generico