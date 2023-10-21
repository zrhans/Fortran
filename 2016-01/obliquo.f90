program obliquo
implicit none
real, parameter     :: PI = 3.14159265359, g = -9.80665 !m/s²
integer, parameter  :: v0 = 20 ! modulo da velocidade de lancamento, em m/s
character :: flinha             ! Declarando o tipo das funções
real :: x = 0.0,  y = 0.0 ! posicoes x e y do objeto
real :: x0 = 0.0          ! coordenada x de lancamento
real :: y0 = 1.0          ! coordenada y de lancamento
real :: theta = 45        ! angulo de lancamento (graus)
real :: vx0 = 0.0         ! velocidade inicial na direcao x
real :: vy0 = 0.0         ! velocidade inicial na direcao y
real :: t = 0.0           ! tempo em segundos
real :: dt = 0.2          ! intervalo de tempo entre calculos sucessivos
 
theta = theta * PI / 180 ! angulo de lancamento em radianos


vx0 = v0 * cos(theta)    ! direcao x
vy0 = v0 * sin(theta)    ! direcao y
 
call linha(30,'-') 
print '( 3(4x,a)  )','t(s)','x(m)' ,'y(m)'
call linha(30,'=')

open(10,file='bidi.txt')

! Laco (repeticao) enquanto y > 0
do while( y .ge. 0.0)
    x = x0 + vx0 * t
    y = y0 + vy0 * t + 0.5 * g * t**2
    if ( y .lt. 0.0 ) exit
    print '(3(f8.2))',t,x,y
    write(10,'(3(f8.2))'),t,x,y
    t = t + dt
end do

close(10)

call linha(30,'-')

print*,'Muito bem ', flinha(5,"#")
print*, flinha(10,'bartira ')

stop 'Programa finalizado!'
end program

!---------------------------------------
! Subrotina cria uma linha na tela
!
! Argumentos:
! Inteiro   a = comprimento da linha
! Caractere b = caractere para a linha
!---------------------------------------
subroutine linha(a,b)
integer, intent(in)     :: a
character(len=*), intent(in) :: b
  do i = 1, a
    write(*,'(A)', ADVANCE = "NO") b
  end do
  print*
end subroutine linha

character function flinha(a,b)
integer, intent(in)     :: a
character(len=*), intent(in) :: b
  do i = 1, a
    write(*,'(A)', ADVANCE = "NO") b
  end do
  print*
end function flinha


! Resultado: 
! https://onedrive.live.com/redir?resid=5BB1268B33E9E16C!1010081
!
