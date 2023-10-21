program generico
implicit none

integer :: i
real :: s=0
real :: variancia=0.
real :: xmedio=0.


do i = 1,5
  s=s+i
  print*, i
enddo


! Calculo do x médio
xmedio = s / (i-1)

! Calculo da variancia

s=0.0
do i = 1,5
  s = s  + (i - xmedio)**2
enddo

variancia = s / (i -1 )



print*,"Xmedio    : ", xmedio
print*,"Variancia : ", variancia


end
