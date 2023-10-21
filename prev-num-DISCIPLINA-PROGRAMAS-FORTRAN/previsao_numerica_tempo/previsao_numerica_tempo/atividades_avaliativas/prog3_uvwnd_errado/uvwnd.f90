	program uvwnd
	implicit none
!
	integer, parameter :: im = 144, jm = 73
	integer :: i , j
	real, dimension(jm) :: lat, dx, dx2
	real, dimension(im, jm) :: vort, psi, ddxpsi, ddypsi, vort_calc, res
	real, parameter :: a = 6.37e6, rd = atan(1.0)/45., dphi = 2.5, dy = a*dphi*rd, dy2 = dy**2, dlam = 2.5
	integer :: chave
!
	open(1, file = 'vorticidade_global.bin', status = 'unknown', form = 'unformatted', access = 'direct',&
	action = 'read', recl = 4*im*jm)
	read(1, rec = 1)vort
	close (1)
!
	lat(1) = -90.
	do j = 1, jm-1
		lat(j+1) = lat(1)+real(j)*dphi
	enddo
!
	do j = 1, jm
		dx(j) = a*cos(lat(j))*dlam
		dx2(j) = dx(j)**2
! 		print *, lat(j), dx(j), dx2(j)
	enddo
!
!Condicoes de contorno
!
	psi(:, 1) = 0.
	psi(:, jm) = 0.
	psi(1, :) = 0.
	psi(im, :) = 0.
!
!Calculando o psi
!
	chave = 1
	do while(chave.eq.1)
		chave = 0
		i = 1; j = 1
		ddxpsi(i, j) = (psi(i+1, j)+psi(im, j)-2*psi(i, j))/dx2(j)
		ddypsi(i, j) = (psi(i, j+1)+psi(i, jm)-2*psi(i, j))/dy2
! 		print*, ddxpsi, ddypsi
		do i = 2, im-1
			do j = 1, jm
				ddxpsi(i, j) = (psi(i+1, j)+psi(i-1, j)-2*psi(i, j))/dx2(j)
			enddo
		enddo
		do i = 1, im
			do j = 2, jm-1
				ddypsi(i, j) = (psi(i, j+1)+psi(i, j-1)-2*psi(i, j))/dy2
			enddo
		enddo
		do i = 1, im
			do j = 1, jm
				vort_calc(i, j) = ddxpsi(i, j)+ddypsi(i, j)
				res(i, j) = vort_calc(i, j)-vort(i, j)
! 				print*, i, j, vort_calc(i, j), res
				if(abs(res(i, j)).gt.1.0e-6)then
					psi(i, j) = psi(i, j)+0.5*res(i, j)*dy2*dx2(j)/(dy2+dx2(j))
					chave = 1
! 				print*, psi(i, j), res, chave
				endif
! 				print*, i, j, psi(i, j), res(i, j), 0.5*res(i, j)*dy2*dx2(j)/(dy2+dx2(j)), vort(i, j)
			enddo
		enddo
	enddo
!
!Calculando o vento horizontal
!
	i = 1; j = 1
	u(i, j) = -1.*(psi(i, j+1)-psi(i, im))/(2*dy)
	v(i, j) = (psi(i+1, j)-psi(im, j))/(2*dx)
!
	do i = 1, im
		do j = 2, jm-1
			u(i, j) = -1.*(psi(i, j+1)-psi(i, j-1))/(2*dy)
		enddo
	enddo
	do i = 2, im-1
		do j = 1, jm
			v(i, j) = (psi(i+1, j)-psi(i-1, j))/(2*dx)
		enddo
	enddo
!
	j = jm
	end program
