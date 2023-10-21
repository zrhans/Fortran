program plotter
 !use nome-do-modulo
 use gnuplot_fortran
 implicit none
 
 real, parameter :: PI = 4*atan(1.0)
 integer, parameter :: N = 100
 real, dimension(0:N) :: x, y
 real :: x_start = 0.0, x_end = 20.0 !2 * PI
 real :: dx
 integer :: i

 

 dx = (x_end - x_start) / real(N)

 ! Criando array x 
 ! Usando do implicito para a variavel i >f90
 x(0:N) = [(i*dx, i=0,N)]
 ! Criando array y
 y = sin(x) / (x + 1)

 !===================================================
 ! Gerando os dados para plotagem usando a subrotina 
 ! plot2ddo modulo gnuplot_fortran.mod
 !===================================================

 call plot2d(x, y)


end program plotter

