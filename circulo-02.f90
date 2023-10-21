program area_circulo
implicit none

! ******************************************************
! Programa para cálculo da area e volume de uma esfera
!
! Equações:   
!           AREA  : 4*Pi*r^2   : (4*pi*r**2) ou (4*pi*r*r)
!           VOLUME: 4/3*Pi*r^3 : (4./3. * pi*r**3) ou (4./3.pi*r*r*r)
!
! pi deve ser declarado como constante
!-------------------------------------------------------

real, parameter :: pi = 3.14159265359       ! Declaracao e atribuicao de valor à constante pi
integer :: i
real :: r, area,volume
real :: dr=0.01

print *, 'Digite um valor para o raio (r) do circulo: '
read *, r

print '(a)', '     Raio             Area              Volume     '
print '(a)', '--------------------------------------------'

do i = 1, 10
	area = 4 * pi * r ** 2
	volume = (4.0/3.0) * pi * r ** 3
	r = r + dr
	print *, '|', r,'|', area,'|', volume,'|'
end do

end program area_circulo