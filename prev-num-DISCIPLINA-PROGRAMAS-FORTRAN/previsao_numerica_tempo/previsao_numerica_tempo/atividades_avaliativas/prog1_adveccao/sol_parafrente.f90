	program solucao_parafrente
	implicit none
! declaracao de variaveis
	integer :: i,l,tempo,x,l2
	integer,parameter :: im=101,mt=1441,delt=10,u=8,delx=100
	real,parameter :: tzero=20.,ehle=1000.,pi=4.*atan(1.)
	real,dimension (im,mt) :: temp
	real,dimension (im,10) :: temp2
! fim da declaracao de variaveis
!
! Condicao Inicial
!
	l=1
	temp=(l-1)*delt
	do i=1,im
		x=(i-1)*delx
		temp(i,l)=tzero*cos((2*pi/ehle)*real(x))
	enddo
!
! Metodo de diferencas finitas para frente
!
	do l=2,mt
	tempo=(l-1)*delt
!	print*,l,tempo
!	i=1
!	temp(i,l)=temp(i,l-1)-real(u*delt)*((temp(2,l-1)-temp(im,l-1))/real(2*delx))
	i=im
	temp(i,l)=temp(i,l-1)-real(u*delt)*((temp(2,l-1)-temp(im-1,l-1))/real(2*delx))
	temp(1,l)=temp(i,l)
		do i=2,im-1
			temp(i,l)=temp(i,l-1)-real(u*delt)*((temp(i+1,l-1)-temp(i-1,l-1))/real(2*delx))
		enddo
	enddo
!
	l2=0
	do l=1,119,12
		l2=l2+1
		write(*,*)'l:',l,'tempo=',(l-1)*10,l2
		do i=1,im
			temp2(i,l2)=temp(i,l)
		enddo
	enddo
!
	open(1,file='solucao_parafrente_2m.bin',status='unknown',form='unformatted',access='direct',&
		action='write',recl=4*im*10)
	write(1,rec=1)temp2
	close(1)
!
	end program solucao_parafrente
