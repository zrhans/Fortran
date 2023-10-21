program homework_01_1
implicit none
real, parameter :: mu = 1.0, rho_s = 1.0, rho_f = 0.5
real :: delta_p, diametro, velocidade
integer :: i
integer*2 :: val_i, val_f

val_i = 50; val_f = 1000
delta_p = (val_f - val_i) / 100

open(10,file='vel-viscoso.txt')
write(10,*) 'diametro velocidade'

    do i = 1, 100
        diametro = 50 + (i-1) * delta_p
        velocidade = ( (0.524) * (diametro)**2 * (rho_s - rho_f) ) / mu
        write(10,*) diametro, velocidade
        
    end do
close(10)
stop 'Programa finalizado!'
end program