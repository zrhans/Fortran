program simple_plot
implicit none
real, parameter :: PI = 4*atan(1.0)
integer, parameter :: N = 100
real, dimension(1:N) :: x, y
real :: a = 0.0, b = 2 * PI
real :: incremento
integer :: i


incremento = (b - a) / (real(N) - 1)

x(1) = 0
do i =  2, n
   x(i) = x(i-1) + incremento
end do

open(1,file='data.dat')
y = sin(x)
do i = 1, N
  write(1,*),x(i),y(i)
end do

close(1)
end program simple_plot