	program equacao_omega
	implicit none
! declaracao de variaveis
	integer,parameter :: im=144,jm=37,km=13
	integer :: i,j,k
	real,dimension(jm) :: lat
	real,dimension(im, jm, km) :: uwnd,vwnd,div,omega
	real,parameter :: a=6.37e6,rd=atan(1.0)/45.,dphi=2.5,dy=a*dphi*rd,dlam=2.5
	real :: du,dv,ajuste,omega_trop,soma,divm
	real,dimension(km) :: p
	data p /100000.,92500.,85000.,70000.,60000.,50000.,40000.,30000.,25000.,20000.,15000.,10000.,7000./
! fim da declaracao de variaveis
!
!    print(*,*)'abrindo conjunto 1 de dados...'
	open(1,file='uwnd.bin',status='unknown',form='unformatted',access='direct',&
		action='read',recl=4*im*jm*km)
! leitura do arquivo binario
	read(1,rec=1)uwnd
	close(1)
!    print(*,*)'abrindo conjunto 2 de dados...'
	open(2,file='vwnd.bin',status='unknown',form='unformatted',access='direct',&
		action='read',recl=4*im*jm*km)
! leitura do arquivo binario
	read(2,rec=1)vwnd
	close(2)
! calculos utilizando conjunto 1 e 2 de dados
 ! condicoes iniciais => nivel 1 (k=1)
	omega_trop=0.
	omega(:, :, 1)=0.0
	div(: , :, 1)=0.0
	lat(1)=-90.
 ! estabelecendo latitudes a partir do referencial [lat(1)=-90.]*
	do j=1,jm-1
		lat(j+1)=lat(1)+real(j)*dphi
	enddo
! latitudes estabelecidas*
!
! como se trata de diferenca centrada, usa-se dimensao=2
	do i=2,im-1
		do j=2,jm-1
			do k=2,km
				du=(uwnd(i+1,j,k)-uwnd(i-1,j,k))/(2.*dlam*rd)
				dv=(vwnd(i,j+1,k)*cos(lat(j+1)*rd)-vwnd(i,j-1,k)*cos(lat(j-1)*rd))/(2*dphi*rd)
				div(i,j,k)=(du+dv)/(a*cos(lat(j)*rd))
			enddo
		enddo
	enddo
! calculando du, dv e div(em funcao de du e dv)
	do k=2,km
		do j=1,jm
			i=1
			du=(uwnd(i+1,j,k)-uwnd(im,j,k))/(2.*dlam*rd)
			dv=(vwnd(i,j+1,k)*cos(lat(j+1)*rd)-vwnd(i,j-1,k)*cos(lat(j-1)*rd))/(2*dphi*rd)
			div(i,j,k)=(du+dv)/(a*cos(lat(j)*rd))
			i=im
			du=(uwnd(1,j,k)-uwnd(i-1,j,k))/(2.*dlam*rd)
			dv=(vwnd(i,j+1,k)*cos(lat(j+1)*rd)-vwnd(i,j-1,k)*cos(lat(j-1)*rd))/(2*dphi*rd)
			div(i,j,k)=(du+dv)/(a*cos(lat(j)*rd))
		enddo
	enddo
	div(: , 1, :)=-9.96921e36
	div(: , jm, :)=-9.96921e36
! Integral na vertical: soma todos os niveis na vertical
	do j=2,jm-1
		do i=1,im
			soma=0.
			do k=2,km
				divm=0.5*(div(i,j,k)+div(i,j,k-1))
				soma=soma+divm*(p(k)-p(k-1))
				omega(i,j,k)=omega(i,j,1)+soma/(p(k)-p(1))
				print*,div(i,j,k),omega(i,j,k)
			enddo
		enddo
	enddo
!    print(*,*)'abrindo conjunto 3 de dados...'
	open(3,file='omega.bin',status='unknown',form='unformatted',access='direct',&
		action='write',recl=4*im*jm*km)
	write(3,rec=1)omega
	close(3)
	end program equacao_omega
!