! Aluno Arthur R. A. Santos
! -Licenciatura em Fisica
! --Questao 9 da avaliação final
!
! -O programa deve pedir a potencia e a voltagem,
! calcular a corrente elétrica e ainda informar
! o cabo ideal (A ou B) para as condições de
! potencia e voltagem informadas.
!
program questao_9
implicit none
! Declaracao das variaveis
real :: P=0.0, I=0.0, V=0.0
! Solicitacao das variaveis necessarias
print*, " Informe a Potencia e a voltagem "
read*, P,V
! Calculo da corrente eletrica
I = P/V
! Dados obtidos
print*, "Voltagem:",V,"Volts"
print*, "Potencia:",P,"Watts"
print*, "Corrente:",I,"Amperes"
! Verificacao do cabo correspondente ao valor de I
if(I>0.0 .and. I<=5.0) then
print*, "Deve-se utilizar o cabo A"
else if(I>5.0 .and. I<=13.0) then
print*, "Deve-se utilizar o cabo B"
else if(I>13.0 .and. I<=30.0) then
print*, "Deve-se utilizar o cabo C"
else if(I>30.0) then
print*, "Este cabo nao esta disponivel, volte outra dia"
end if

print*, "Finalizado..."
end program questao_9

