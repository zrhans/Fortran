! Esse programa calcula as componentes horizontais (u e v) do vento
	program vento_horizontal
	implicit none
! Declaracao de variaveis
	integer,parameter :: im=144,jm=73
	integer :: i,j
	real,dimension(jm) :: lat,dx,dx2
	real,dimension(im) :: lon
	real,dimension(im,jm) :: zeta,psi,u,v
	real,parameter :: a=6.37e6,rd=atan(1.0)/45.,dphi=2.5,dy=a*dphi*rd,dy2=dy*dy,dlam=2.5
! fim da declaracao de variaveis
!
! abrindo/lendo arquivo binario (entrada)
	open(1,file='vorticidade_global.bin',status='unknown',form='unformatted',access='direct',&
		action='read',recl=4*im*jm)
	read(1,rec=1)zeta
	close(1)
!
	lat(1)=-90.
	do j=1,jm-1
		lat(j+1)=lat(1)+real(j)*dphi
	enddo
!
	lon(1)=0.
	do i=1,im-1
		lon(i+1)=lon(1)+real(i)*dlam
	enddo
!
	do j=1,jm
		dx(j)=a*cos(rd*lat(j))*(rd*dlam)
		dx2(j)=dx(j)*dx(j)
	enddo
!
! Condicoes de contorno
!
	psi(:,:)=0.
!
! Calculo da variavel psi
!
	call relaxacao(psi,zeta,dx2,dy2,im,jm)
!
! Calculo do vento horizontal
!
	call uv(psi,dx,dy,im,jm,u,v)
!
! Escrevendo/salvando arquivos saida (binario)
!
! para componente u:
	open(2,file='uwnd.bin',status='unknown',form='unformatted',access='direct',&
		action='write',recl=4*im*jm)
	write(2,rec=1)u
	close(2)
!
!
! para componente v:
	open(3,file='vwnd.bin',status='unknown',form='unformatted',access='direct',&
		action='write',recl=4*im*jm)
	write(3,rec=1)v
	close(3)
!
! Programas internos (subroutine)
!
!!!!!! relaxacao
!
	CONTAINS
	subroutine relaxacao(psi,zeta,dx2,dy2,im,jm)
	implicit none
!
! declaracao de variaveis
	integer,intent(in) :: im,jm
	real,dimension(im,jm),intent(in) :: zeta
	real,dimension(im,jm),intent(inout) :: psi
	real,dimension(im,jm) :: ddxpsi,ddypsi,zeta_psi,res
	real,dimension(jm),intent(in) :: dx2
	real,intent(in) :: dy2
	integer :: i,j,icont,chave
	real :: var_maior
! fim da declaracao de variaveis
!
	chave=1
	do while(chave.eq.1)
		chave=0
		icont=icont+1
		var_maior=0.
		do i=2,im-1
			do j=2,jm-1
				ddxpsi(i,j)=(psi(i+1,j)+psi(i-1,j)-2.*psi(i,j))/dx2(j)
				ddypsi(i,j)=(psi(i,j+1)+psi(i,j-1)-2.*psi(i,j))/dy2
				zeta_psi(i,j)=ddxpsi(i,j)+ddypsi(i,j)
				res(i,j)=zeta_psi(i,j)-zeta(i,j)
				if(abs(res(i,j)).gt.var_maior)var_maior=abs(res(i,j))
			enddo
		enddo
!
		do i=2,im-1
			do j=2,jm-1
				if(var_maior.gt.1.0e-6)then
					psi(i,j)=psi(i,j)+0.5*res(i,j)*dy2*dx2(j)/(dy2+dx2(j))
					chave=1
				end if
			enddo
		enddo
	enddo
	return
	end subroutine
!
!!!!!! uv
!
	subroutine uv(psi,dx,dy,im,jm,u,v)
	implicit none
!
! declaracao de variaveis
	integer,intent(in) :: im,jm
	real,dimension(im,jm),intent(in) :: psi
	real,dimension(im,jm),intent(out) :: u,v
	real,dimension(jm) :: dx
	real,intent(in) :: dy
	integer :: i,j
!
! fim da declaracao de variaveis
!
	do i=2,im-1
		do j=2,jm-1
			u(i,j)=-1.*(psi(i,j+1)-psi(i,j-1))/(2*dy)
			v(i,j)=(psi(i+1,j)-psi(i-1,j))/(2*dx(j))
		enddo
	enddo
	return
	end subroutine
! fim dos programas internos (subroutine)
	end program
