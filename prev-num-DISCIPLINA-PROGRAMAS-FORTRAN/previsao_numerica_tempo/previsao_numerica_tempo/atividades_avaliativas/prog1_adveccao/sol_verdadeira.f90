	program solucao_verdadeira
	implicit none
! declaracao de variaveis
	integer :: i,l,tempo,x,l2
	integer, parameter :: im=101,mt=721,u=8
	real, parameter :: tzero=20.,ehle=1000.,pi=4.*atan(1.)
	real, dimension (im, mt) :: temp
	real, dimension (im, 25) :: temp2
	real, dimension (im, 10) :: temp3
! fim da declaracao de variveis
!
	do l=1,mt
		tempo=(l-1)*120
		write(*,*)'tempo(',l-1,'):',tempo,'s',tempo/3600,'h'
		do i=1,im
			x=(i-1)*100
			temp(i,l)=tzero*cos((2.*pi/ehle)*real(x-u*tempo))
		enddo
	enddo
!
	l2=0
	do l=1,mt,30
		l2=l2+1
		write(*,*)'l:',l,'tempo=',(l-1)*120,l2
		do i=1,im
			temp2(i,l2)=temp(i,l)
		enddo
	enddo
	open(1,file='solucao_verdadeira_1h.bin',status='unknown',form='unformatted',access='direct',&
		action='write',recl=4*im*25)
	write(1,rec=1)temp2
	close(1)
!
	l2=0
	do l=1,10
		l2=l2+1
		write(*,*)'l:',l,'tempo=',(l-1)*120,l2
		do i=1,im
			temp3(i,l2)=temp(i,l)
		enddo
	enddo
	open(2,file='solucao_verdadeira_2m.bin',status='unknown',form='unformatted',access='direct',&
		action='write',recl=4*im*10)
	write(2,rec=1)temp3
	close(2)
!
	end program solucao_verdadeira
