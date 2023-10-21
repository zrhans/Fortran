program obliquo
implicit none
real, parameter :: PI = 3.14159265359, g = -9.80665 !m/s²
integer, parameter :: v0 = 10 ! modulo da velocidade de lancamento, em m/s

real :: x = 0.0, y = 0.0 ! posicoes x e y do objeto
real :: x0 = 0.0   ! coordenada x de lancamento
real :: y0 = 1.0   ! coordenada y de lancamento
real :: theta = 45 ! angulo de lancamento 
real :: vx0 = 0.0  ! velocidade inicial na direcao x
real :: vy0 = 0.0  ! velocidade inicial na direcao y
real :: t = 0.0    ! tempo em segundos
real :: dt = 0.2   ! intervalo de tempo entre calculos sucessivos

theta = theta * PI / 180 ! angulo de lancamento em radianos
vx0 = v0 * cos(theta)    ! direcao x
vy0 = v0 * sin(theta)    ! direcao y


print '( 3(8x,a)  )','t(s)','x(m)' ,'y(m)'
print '( 40("-") )' 
print*,y
y = y0 + vy0 * t + 0.5 * g * t**2
! Laco (repeticao) enquanto y > 0 
do while( y .ge. 0.0)
    x = x0 + vx0 * t
    y = y0 + vy0 * t + 0.5 * g * t**2
    write(*,*)t,x,y
    t = t + dt
end do

end program
