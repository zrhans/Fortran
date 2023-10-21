	subroutine relaxacao(psi, zeta, dx2, dz2, im, km, n)
	integer, intent(in) :: im, km, n
	real, dimension(im, km, n), intent(in) :: zeta
	real, dimension(im, km, n), intent(inout) :: psi
	real, dimension(im, km, n) :: ddxpsi, ddzpsi, zeta_psi, res
	real, intent(in) :: dx2, dz2
	integer :: i, k, icont, chave
	real :: var_maior
!
	chave = 1
	do while(chave.eq.1)
		chave = 0
		icont = icont+1
		var_maior = 0.
		do i = 2, im-1
			do k = 2, km-1
				ddxpsi(i, k, n) = (psi(i+1, k, n)+psi(i-1, k, n)-2.*psi(i, k, n))/dx2
				ddzpsi(i, k, n) = (psi(i, k+1, n)+psi(i, k-1, n)-2.*psi(i, k, n))/dz2
				zeta_psi(i, k, n) = ddxpsi(i, k, n)+ddzpsi(i, k, n)
				res(i, k, n) = zeta_psi(i, k, n)-zeta(i, k, n)
				if(abs(res(i, k, n)).gt.var_maior)var_maior = abs(res(i, k, n))
			enddo
		enddo
!
		do i = 2, im-1
			do k = 2, km-1
				if(var_maior.gt.1.0e-3)then
					psi(i, k, n) = psi(i, k, n)+0.5*res(i, k, n)*dz2*dx2/(dz2+dx2)
					chave = 1
				endif
			enddo
		enddo
	enddo
	return
	end subroutine
