program area_circulo
implicit none

! ******************************************************
! Programa para cálculo da area de um círcuilo
!
! Equação:   Pi*r^2 : (pi*r**2) ou (pi*r*r)
!
! pi deve ser declarado como constante
! Use 4 valores para o raio: 3.1, 12.70, 17.9 e 3798.12;
!-------------------------------------------------------

real, parameter :: pi = 3.14159265359       ! Declaracao e atribuicao de valor à constante pi

real :: raio = 3.1                          ! Declaracao e atribuicao de valor à variável raio
real :: area(4)
real :: vetor_raio(4)

integer :: ans = 0;





100 vetor_raio(1) = raio
area(1) = pi*raio**2

write(*,*)" A area do circulo de raio ", raio ," metros vale: ", pi*raio**2, " metros quadrados"

raio =  12.70

vetor_raio(2) = raio
area(2) = pi*raio**2

write(*,*)" A area do circulo de raio ", raio ," metros vale: ", pi*raio**2, " metros quadrados"

raio =  17.9

vetor_raio(3) = raio
area(3) = pi*raio**2

write(*,*)" A area do circulo de raio ", raio ," metros vale: ", pi*raio**2, " metros quadrados"

raio =  3798.12

vetor_raio(4) = raio
area(4) = pi*raio**2

write(*,*)" A area do circulo de raio ", raio ," metros vale: ", pi*raio**2, " metros quadrados"

print*,'+------------------------------+'
print*,"|  Raio [m]        Area [m^2]  |"
print*,'+------------------------------+'
print*,vetor_raio(1), area(1)
print*,vetor_raio(2), area(2)
print*,vetor_raio(3), area(3)
print*,vetor_raio(4), area(4)
print*,'_______________________________' 

print*,'Digite 0 para terminar. '
read*,ans

if(ans.ne.0) goto 100

stop '>>> Programa finalizado'
end program area_circulo