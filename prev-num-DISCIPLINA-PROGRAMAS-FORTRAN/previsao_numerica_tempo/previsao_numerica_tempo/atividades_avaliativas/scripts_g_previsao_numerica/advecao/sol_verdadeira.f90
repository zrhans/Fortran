	program sol_verdadeira
	implicit none
!
	integer :: i, l, tempo, x, l2
	integer, parameter :: im = 101, mt = 721, u = 8
	real, parameter :: tzero = 20., ehle = 1000., pi = 4.*atan(1.)
	real, dimension (im, mt) :: temp
	real, dimension (im, 25) :: temp2
	real, dimension (im, 10) :: temp3
!
	do l = 1, mt
		tempo = (l-1)*120
! 		print 10, 'temp(', l-1, '):', tempo, 's', tempo/3600, 'h'
! 	 10 format(a5, i3, a2, x, i6, 1a, 2x, i2, 1a)
		do i = 1, im
			x = (i-1)*100
			temp(i, l) = tzero*cos((2.*pi/ehle)*real(x-u*tempo))
		enddo
	enddo
!
	l2 = 0
	do l = 1, mt, 30
		l2 = l2+1
! 		print 11, 'l:', l, 'tempo =', (l-1)*120, l2
		do i = 1, im
			temp2(i, l2) = temp(i, l)
		enddo
	enddo
!
	open(1, file = 'sol_verdadeira_1h.bin', status = 'unknown', form = 'unformatted', access = 'direct',&
	action = 'write', recl = 4*im*25)
	write(1, rec = 1)temp2
	close (1)
!
	l2 = 0
	do l = 1, 10
		l2 = l2+1
		print 11, 'l:', l, 'tempo =', (l-1)*120, l2
	 11 format(a2, x, i3, 2x,a7, x, i5, 2x, i2)
		do i = 1, im
			temp3(i, l2) = temp(i, l)
		enddo
	enddo
!
	open(1, file = 'sol_verdadeira_2m.bin', status = 'unknown', form = 'unformatted', access = 'direct',&
	action = 'write', recl = 4*im*10)
	write(1, rec = 1)temp3
	close (1)
!
	end program
