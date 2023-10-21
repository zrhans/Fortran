! Qual a velocidade que com que uma particula do tamamnho de um 
! grao de areia escoa verticalmente dentro de um fluido?

! Eq: v = (rho_s - rho_l) D^2 g /  18 n
! v     = velocidade
! rho_s  = densidade da particula [g/cm^3]
! rho_l  = densidade do fluido [g/cm^3]
! D     = diametro da particula [cm]
! g     = acc gravidade [cm/s^2]
! n   = viscosidade do fluido [g/cm s]
! ------------------------------------------------------------
program particle
implicit none

real :: v = 0.0
real :: rho_s = 2.7
real :: rho_l = 1.0 ! Agua
real :: D = 0.01 
real :: g = 981 
real :: n = 0.01

v = ( (rho_s - rho_l) * (D**2) * g ) / (18 * n)

print*,v


end program particle