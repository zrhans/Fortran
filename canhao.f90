program canhao
implicit none

integer, parameter :: r = 10 ! Alcance máximo a borda de um círculo de raio r (r = 10 Km)
integer :: x, y
real :: distancia
! Equação de um círculo de Raio r:  x^2 + y^2 <= r^2

print *, 'Digite as coordenadas x e y: '
read *, x, y

! Testa se estas coordenadas ficam dentro do circulo

if ((x ** 2. + y ** 2.) .LE. r ** 2) then
    print *, 'Acertou o Alvo'
    
    ! Calcula a distancia
    distancia = SQRT(x ** 2. + y ** 2.)
    print *, 'Distancia entre o canhao e o Alvo (Km): ', distancia

else
    print *, 'Errou o Alvo!'
endif

end program canhao
