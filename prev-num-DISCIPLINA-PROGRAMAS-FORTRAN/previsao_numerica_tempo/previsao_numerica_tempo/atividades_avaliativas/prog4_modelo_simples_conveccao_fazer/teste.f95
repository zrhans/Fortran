	program modelo_conveccao
	implicit none
!
	integer, parameter :: im = 16, km = 16, lm = 80, lminm = lm/20
	integer :: i , k, l, lmin
	real, dimension(im, km, lm) :: vort, teta, psi, ddxpsi, ddzpsi, vort_psi, res, uwnd, wwnd
	real, dimension(im, km, lm) :: dif_teta_x, dif_teta_z, dif_vort_x, dif_vort_z, dif_vort_uw
	real, dimension(im, km, lminm) :: vort_min, teta_min, psi_min, uwnd_min, wwnd_min
	real, parameter :: dx = 3000., dz = 300., dt = 30., g = 9.81, teta0 = 270., dx2 = dx*dx, dz2 = dz*dz
	integer :: chave, cont
	real :: var_maior
!!!!!!!!!!!!!!!!!!!!
!Condicoes iniciais!
!!!!!!!!!!!!!!!!!!!!
	vort(:, :, :) = 0.
	teta(:, :, :) = 270.
	teta(6:11, 1:2, :) = 275.
	psi(:, :, :) = 0.
	uwnd(:, :, :) = 0.
	wwnd(:, :, :) = 0.
!!!!!!!!!
!Passo 1!
!!!!!!!!!
!Calculando o teta e a vort no tempo 2
!Diferenca pra frente no tempo
	l = 1
	teta(:, :, l+1) = teta(:, :, l)
	do i = 2, im-1
		do k = 2, km-1
			vort(i, k, l+1) = dt*(g/teta0)*((teta(i+1, k, l)-teta(i-1, k, l))/(2*dx))+vort(i, k, l)
		enddo
	enddo
!Calculando o psi
	cont = 0
	chave = 1
	do while(chave.eq.1)
		chave = 0
		cont = cont+1
		var_maior = 0.
		do i = 2, im-1
			do k = 2, km-1
				ddxpsi(i, k, l+1) = (psi(i+1, k, l+1)+psi(i-1, k, l+1)-2.*psi(i, k, l+1))/dx2
				ddzpsi(i, k, l+1) = (psi(i, k+1, l+1)+psi(i, k-1, l+1)-2.*psi(i, k, l+1))/dz2
				vort_psi(i, k, l+1) = ddxpsi(i, k, l+1)+ddzpsi(i, k, l+1)
				res(i, k, l+1) = vort_psi(i, k, l+1)-vort(i, k, l+1)
				if(abs(res(i, k, l+1)).gt.var_maior)var_maior = abs(res(i, k, l+1))
			enddo
		enddo
!
		do i = 2, im-1
			do k = 2, km-1
				if(var_maior.gt.1.0e-3)then
					psi(i, k, l+1) = psi(i, k, l+1)+0.5*res(i, k, l+1)*dz2*dx2/(dz2+dx2)
					chave = 1
				endif
			enddo
		enddo
	enddo
!Calculando o vento u e w
	do i = 2, im-1
		do k = 2, km-1
			uwnd(i, k, l+1) = -1.*(psi(i, k+1, l+1)-psi(i, k-1, l+1))/(2*dz)
			wwnd(i, k, l+1) = (psi(i+1, k, l+1)-psi(i-1, k, l+1))/(2*dx)
		enddo
	enddo
!!!!!!!!!
!Passo 2!
!!!!!!!!!
	do l = 2, lm-1
!Calculando o teta e a vort
!Diferenca centrada no tempo
		do i = 2, im-1
			do k = 2, km-1
				dif_teta_x(i, k, l) = (teta(i+1, k, l)-teta(i-1, k, l))/(2*dx)
				dif_vort_x(i, k, l) = (vort(i+1, k, l)-vort(i-1, k, l))/(2*dx)
				dif_teta_z(i, k, l) = (teta(i, k+1, l)-teta(i, k-1, l))/(2*dz)
				dif_vort_z(i, k, l) = (vort(i, k+1, l)-vort(i, k-1, l))/(2*dz)
				dif_vort_uw(i, k, l) = uwnd(i, k, l)*dif_vort_x(i, k, l)+wwnd(i, k, l)*dif_vort_z(i, k, l)
!
				teta(i, k, l+1) = -2*dt*(uwnd(i, k, l)*dif_teta_x(i, k, l)+wwnd(i, k, l)*dif_teta_z(i, k, l))+teta(i, k, l-1)
				vort(i, k, l+1) = 2*dt*((g/teta0)*dif_teta_x(i, k, l)-dif_vort_uw(i, k, l))+vort(i, k, l-1)
! 				print *, i, k, l, teta(i, k, l+1), vort(i, k, l+1)
			enddo
		enddo
!Calculando o psi
		chave = 1
		do while(chave == 1)
			chave = 0
			cont = cont+1
			var_maior = 0.
			do i = 2, im-1
				do k = 2, km-1
					ddxpsi(i, k, l+1) = (psi(i+1, k, l+1)+psi(i-1, k, l+1)-2*psi(i, k, l+1))/dx2
					ddzpsi(i, k, l+1) = (psi(i, k+1, l+1)+psi(i, k-1, l+1)-2*psi(i, k, l+1))/dz2
					vort_psi(i, k, l+1) = ddxpsi(i, k, l+1)+ddzpsi(i, k, l+1)
					res(i, k, l+1) = vort_psi(i, k, l+1)-vort(i, k, l+1)
					if(abs(res(i, k, l+1)) > var_maior)then
						var_maior = abs(res(i, k, l+1))
					endif
				enddo
			enddo
!
			do i = 2, im-1
				do k = 2, km-1
					if(var_maior > 1.e-3)then
						psi(i, k, l+1) = psi(i, k, l+1)+0.5*res(i, k, l+1)*dz2*dx2/(dz2+dx2)
						chave = 1
					endif
! 					print*, l, var_maior, psi(i, k, l+1)
				enddo
			enddo
		enddo
!Calculando o vento u e w
		do i = 2, im-1
			do k = 2, km-1
				uwnd(i, k, l+1) = -1.*(psi(i, k+1, l+1)-psi(i, k-1, l+1))/(2*dz)
				wwnd(i, k, l+1) = (psi(i+1, k, l+1)-psi(i-1, k, l+1))/(2*dx)
			enddo
		enddo
		print *, 'Tempo =', l
	enddo
!!!!!!!!!!!!!!!!!
!Filtrando saida!
!!!!!!!!!!!!!!!!!
	lmin = 0
	do l = 1, lm, 20
		lmin = lmin+1 
		do i = 2, im-1
			do k = 2, km-1
				vort_min(i, k, lmin) = vort(i, k, l)
				teta_min(i, k, lmin) = teta(i, k, l)
				psi_min(i, k, lmin) = psi(i, k, l)
				uwnd_min(i, k, lmin) = uwnd(i, k, l)
				wwnd_min(i, k, lmin) = wwnd(i, k, l)
! 				print*, uwnd_min(i, k, lmin)
			enddo
		enddo
		print*, lmin, l
	enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Escrevendo os binario de saida!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	open(1, file = 'vort.bin', status = 'unknown', form = 'unformatted', access = 'direct',&
	action = 'write', recl = 4*im*km*lminm)
	write(1, rec = 1)vort_min
	close (1)
!
	open(1, file = 'teta.bin', status = 'unknown', form = 'unformatted', access = 'direct',&
	action = 'write', recl = 4*im*km*lminm)
	write(1, rec = 1)teta_min
	close (1)
!
	open(1, file = 'psi.bin', status = 'unknown', form = 'unformatted', access = 'direct',&
	action = 'write', recl = 4*im*km*lminm)
	write(1, rec = 1)psi_min
	close (1)
!
	open(1, file = 'uwnd.bin', status = 'unknown', form = 'unformatted', access = 'direct',&
	action = 'write', recl = 4*im*km*lminm)
	write(1, rec = 1)uwnd_min
	close (1)
!
	open(1, file = 'wwnd.bin', status = 'unknown', form = 'unformatted', access = 'direct',&
	action = 'write', recl = 4*im*km*lminm)
	write(1, rec = 1)wwnd_min
	close (1)
!
	end program
