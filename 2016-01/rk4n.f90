! Origem: http://ww2.odu.edu/~agodunov/computing/programs/book2/Ch05/rk4n.f90
program main
!---------------------------------------------------------------------
! Solver for a system of n first-order Ordinary Differential Equations 
! Initial value problem.
! Method: calls 4th-order Runge-Kutta
! Can be used for solving a system of n/2 second order ODE
! There are two examples
! example 1: Physics - projectile motion in the (x,y) plane
! example 2: Biology - 
! AG: Last revision February 2010
!----------------------------------------------------------------------
implicit none

call projectile

call animals

end program main

  subroutine projectile
!------------------------------------------------------------------
! projectile motion in the (x,y) plane
! described by a system of 2 second-order ODEs
!------------------------------------------------------------------
implicit none
integer, parameter:: n=4          ! number of first-order equations
double precision ti, tf, dt, tmax
double precision xi(n), xf(n)
double precision v0, a0, energy
double precision, parameter ::  rad = 3.1415926/180.0   ! radians
integer i
external d2x

! open file for writing results
open (unit=6, file='projectile.dat')

!*** initial data 
ti =    0.0             ! initial value for variable t
v0 =  100.0             ! initial speed (m/s)
a0 =   45.0             ! initial angle (degrees)
xi(1) = 0.0             ! initial position in x (m)
xi(2) = 0.0             ! initial position in y (m)
!*** the itegration limit and step
dt = 0.2                ! step size for integration (s)
tmax = 60.0             ! integrate till tmax (s)
!*** end of initial data

xi(3) = v0*cos(a0*rad)  ! initial speed in x direction (m/s)
xi(4) = v0*sin(a0*rad)  ! initial speed in y direction (m/s)

!* print the header and initial conditions
write (6,*) '  Projectile motion in the (x,y) plane '
write (6,*) '     Method: Runge-Kutta 4th order     '
write (6,100)
write (6,102) ti, xi(1), xi(2), xi(3), xi(4)

! integration of ODEs
do while (ti <= tmax)
   tf = ti + dt

   call rk4n(d2x,ti, tf, xi, xf, n)

   write(6,102) tf, xf(1), xf(2), xf(3), xf(4)
   if (xf(2) <= 0.0) exit  ! the projectile is under the ground 
! prepare for the next step
   ti = tf
   do i = 1,n
      xi(i) = xf(i)
   end do
end do

100 format(5x,'t',11x,'x',11x,'y',11x,'dx/dt',7x,'dy/dt') 
102 format(5(1pe12.3))
end subroutine projectile

subroutine d2x(t, x, dx, n)
!=================================================================
! A system of n/2 second order ODEs for the RK4n solver
! input ...
! n   - number of equation
! t   - time
! follow the agreement
! for x(n)
! # the first n/2 elements are x
! # the last  n/2 elements are dx/dt
! for dx(n)
! # the first n/2 elements are dx/dt
! # the last  n/2 elements are d2x/dt2 (physics is here)
! example: 2D projectile motion in the (x,y) plane
! In           Out
! x(1) - x     dx(1) - x'
! x(2) - y     dx(2) - y'
! x(3) - x'    dx(3) - x"
! x(4) - y'    dx(4) - y"
!=================================================================
implicit none
integer n
double precision t
double precision x(n), dx(n)

! simple projectile motion in the (x,y) plane
double precision, parameter :: g = 9.81 ! free fall acceleration in m/s^2
!* first order
   dx(1) = x(3)
   dx(2) = x(4)
!* second order
   dx(3) = 0.0          ! x direction
   dx(4) = (-1.0)*g     ! y direction
end subroutine d2x 


  subroutine animals
!------------------------------------------------------------------
! A system of two first-order ODEs
! Predator-prey model with rabbits and foxes. Lotka-Volterra model
!------------------------------------------------------------------ 
implicit none
integer, parameter:: n=2          ! number of first-order equations
double precision ti, tf, dt, tmax
double precision xi(n), xf(n)
integer i
external d1x

! open file for writing results
open (unit=6, file='animals.dat')

!*** initial data 
ti =    0.0               ! initial value for variable t
xi(1) = 100.0             ! initial prey population
xi(2) =  15.0             ! initial predator population
!*** the itegration limit and step
dt   = 0.2                ! step size for integration
tmax = 10.0               ! integrate till tmax 
!*** end of initial data

!* print the header and initial conditions
write (6,*) '  Rabbits and foxes. Lotka-Volterra model '
write (6,*) '     Method: Runge-Kutta 4th order     '
write (6,100)
write (6,102) ti, xi(1), xi(2)

! integration of ODEs
do while (ti <= tmax)
   tf = ti + dt

   call rk4n(d1x,ti, tf, xi, xf, n)

   write(6,102) tf, xf(1), xf(2)
! prepare for the next step
   ti = tf
   do i = 1,n
      xi(i) = xf(i)
   end do
end do

100 format(5x,'t',11x,'x',11x,'y') 
102 format(3(1pe12.3))
end subroutine animals

subroutine d1x(t, x, dx, n)
!=================================================================
! A system of 1st-order differential equations for the RK4n solver
! input ...
! n   - number of equation
! t   - time
! x() - x values
! output ...
! dx()- dx/dt (defined by the equations)
!=================================================================
implicit none
integer n
double precision t
double precision x(n), dx(n)
! Rabbits and foxes. Lotka-Volterra model
double precision, parameter :: kr = 2.0  ! prey birth rate
double precision, parameter :: kf = 1.0  ! predator death rate
double precision, parameter :: kfr= 0.02 ! prey death from predators
double precision, parameter :: krf= 0.01 ! predator birth 
!* first order
   dx(1) =  kr*x(1) - kfr*x(1)*x(2)
   dx(2) = -kf*x(2) + krf*x(1)*x(2)
end subroutine d1x

  subroutine rk4n(fcn,ti, tf, xi, xf, n)
!===========================================================
! Solution for a system of n first-order ODEs
! Method:  Runge-Kutta 4th-order
! Comment: can be easily used for n/2 second order ODEs
! Alex G. February 2010
!-----------------------------------------------------------
! call ...
! fcn(t,x,dx,n)- functions dx/dt   (supplied by a user)
! input ...
! ti    - initial time
! tf    - solution time
! xi()  - initial values 
! n     - number of first order equations
! output ...
! xf()  - solutions
!===========================================================
implicit none
integer n
double precision ti, tf
double precision xi(n), xf(n)

integer j
double precision h, t
double precision x(n), dx(n)
double precision k1(n),k2(n),k3(n),k4(n)

h = tf-ti
t = ti

!* evaluate k1
call fcn(t, xi, dx, n)
do j=1,n
   k1(j) = h*dx(j)
   x(j)  = xi(j) + k1(j)/2.0  
end do      

!* evaluate k2
call fcn(t+h/2.0, x, dx, n)
do j=1,n
   k2(j) = h*dx(j) 
   x(j)  = xi(j) + k2(j)/2.0   
end do

!* evaluate k3
call fcn(t+h/2.0, x, dx, n)
do j=1,n
   k3(j) = h*dx(j) 
   x(j)  = xi(j) + k3(j)   
end do     

!* evaluate k4 and the result      
call fcn(t+h, x, dx, n) 
do j=1,n
   k4(j) = h*dx(j) 
   xf(j) = xi(j) + k1(j)/6.0+k2(j)/3.0+k3(j)/3.0+k4(j)/6.0 
end do     

end subroutine rk4n


