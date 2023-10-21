!------------------------------------------------------------

! Este programa gera e calcula uma fatura de consumo de

! energia El�trica

! Equa��es acess�rias:

! * CONSUMO = POTENCIA * HORAS

! * VALOR_PAGAR = (CONSUMO/1000.0) * VALOR_QUILOWATT_HORA

! Autor: Mariane Dorneles
! Matr�cula: 201611702
! Data: 9/jun/2016

!--------------------------------------------------------------

program conta_luz

!ESTE PROGRAMA CALCULA A CONTA DE LUZ DE UMA CASA E ESCREVE EM UM DOCUMENTO

!Declarando Variaveis

real :: consumo , potencia, horas , valor_pagar , valor_quilowatt_hora, valor_total_pagar, media_h, media_p, soma_p, soma_h

integer :: i

character (len=15) :: eletro_domestico

!ABRINDO DOCUMENTO

open(unit=2, file="conta.txt")

!'EQUA��O

!valor_pagar = (potencia  * horas/1000.0) * valor_quilowatt_hora

print*, ' Digite aqui o valor do quilowatt [R$]' !colocando o valor do kwatt antes do la�o, pois, s� precisa escrever uma vez

read (*,*) valor_quilowatt_hora

!escrevendo no documento antes do la�o para n�o se repetir inumeras vezes e poluir o documento

write (2,*)"****************************************************************************************"

write (2,*)" "

write (2,'(1x,a,3x,f8.2)') "Valor do Quilowatt [R$]:", valor_quilowatt_hora

write (2,*)" "

write (2,*)"****************************************************************************************"

write (2,*)" "

write (2,'(1x,a,7x,a,6x,a,4x,a)') "Eletro-Domestico:","Potencia [W]:","Horas de Uso [Hrs]:","Valor a pagar [R$]:"

write (2,*)" "

write (2,*)"========================================================================================"

write (2,*)" "

!INICIANDO O LA�O

do while ( eletro_domestico .ne. 'fim') 

    !"NESTA PARTE O PROGRAMA ESTA ADQUIRINDO INFORMA��ES, COMO: NOME DO APARELHO
    print*, 'Digite aqui o eletro-domestico:'
    read (*,*) eletro_domestico
    
    if (eletro_domestico .eq. 'fim ') exit
    
    !!"NESTA PARTE O PROGRAMA ESTA ADQUIRINDO INFORMA��ES, COMO: POTENCIA DO APARELHO
    print*, ' Digite aqui a potencia [W]:'
    read (*,*) potencia
    
soma_p = soma_p + potencia

    !!"NESTA PARTE O PROGRAMA ESTA ADQUIRINDO INFORMA��ES, COMO: TEMPO DE ATIVIDADE DO APARELHO
    print*, ' Digite aqui a quantidade de horas que o aparelho ficou ligado [hrs]:'
    read (*,*) horas
    
soma_h = soma_h + horas

    !calculando usando a equa��o
    valor_pagar = (potencia  * horas /1000.0) * valor_quilowatt_hora
    !escrenvendo no documento, os resultados e os dados obtidos
    write (2,'(1x,a,10x,f8.2,15x,f8.2,15x,f8.2)') eletro_domestico,potencia,horas,valor_pagar
    
    write (2,*)" "

    !'NESTA PARTE O PROGRAMA VAI MOSTRA NA TELA O VALOR TOTAL DA CONTA
    print*,  'Valor total a pagar pelo consumo do aparelho [R$]'

    print"(f8.2)", valor_pagar
    
    valor_total_pagar = valor_total_pagar + valor_pagar
    
    
    
    

    !contador
    i = i + 1
!FIM DO LA�O
end do



media_h = (soma_h)/i

write (2,*) "----------------------------------------------------------------------------------------"

write (2,*)" "

write (2,"(1x,a,1x,f8.2)") "Valor total da conta [R$]", valor_total_pagar

write (2,*)" "

write (2,*) "----------------------------------------------------------------------------------------"

write (2,*)" "

media_p = (soma_p)/i

write (2,"(1x,a,1x,f8.2)") "Valor M�dio de Potencia [W]", media_p

write (2,*)" "

write (2,*) "----------------------------------------------------------------------------------------"

write (2,*)" "

media_h = (soma_h)/i

write (2,"(1x,a,1x,f8.2)") "Valor M�dio de Horas de Consumo [Hrs]", media_h

write (2,*)" "

write (2,*) "----------------------------------------------------------------------------------------"


!FECHANDO O DOCUMENTO

close (2)

!FINALIZANDO O PROGRAMA

end program conta_luz
