program cecilia
implicit none

integer ::i
real:: raio=0
real:: area1, area10, cor1, cor10
!area1= area superficial
!area10= area maior
!character*15 :: cor1, cor10
!cor1= primeira cor
!cor10= decima cor

i=1
do while (i<=4444t*, "digite o valor do raio de 10 esferas e sua cor correspondente"
read *,  raio


if ( area1 > area10 ) then
area10=area1
cor10=cor1
end if


end do
   
area1=4*(3.14)*(raio**2)

print*, "A esfera de maior area eh:", area10
print*, "A cor da esfera eh:", cor10

end program cecilia

