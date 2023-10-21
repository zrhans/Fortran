	program modelo_conveccao
	implicit none
!
	integer, parameter :: im = 16, km = 16, tm = 6000.
	integer :: i , k, l
	real, dimension(im, km, tm) :: vort, teta, psi, uwnd, wwnd
	real, parameter :: dx = 3., dz = 300., dt = 30., g = 9.81, teta0 = 270.
	real :: du, dv, ajuste, omega_trop, soma, divm
!
!Condicoes iniciais
!
	vort(:, :, 1) = 0.
	teta(:, :, 1) = 270.
	teta(6:11, 1:5, 1) = 275.
	psi(:, :, 1) = 0.
	uwnd(:, :, 1) = 0.
	wwnd(:, :, 1) = 0.
!
!Diferenca pra frente
!
	l = 1
	do i = 1, im-1
		do k = 1, km
				teta(i, k, l+1) = teta(i, k, l)
				vort(i, k, l+1) = dt*(g/teta0)*((teta(i+1, k, l)-teta(i, k, l))/dx)+vort(i, k, l)
! 				print *, i, k, vort(i, k, l+1), teta(i+1, k, l)-teta(i, k, l), teta(i+1, k, l), teta(i, k, l)
		enddo
	enddo
!
	end program
