!------------------------------------------------------------
!
! Este programa gera e calcula uma fatura de consumo de 
! energia Elétrica
!
! Equações acessórias:
!
! * CONSUMO = POTENCIA * HORAS
! * VALOR_PAGAR = (CONSUMO/1000.0) * VALOR_QUILOWATT_HORA
!
! Autor: Hans
! Data : 19/05/2016
!--------------------------------------------------------------

program fatura_eletrica
implicit none

! Declaração de variáveis
integer :: i
real    :: primeira_coluna, segunda_coluna
real    :: quilowatt_hora = 0.0, potencia = 0.0
real    :: consumo = 0.0, valor_consumo = 0.0
integer :: horas_de_uso = 0
character(len = 15 )  :: equip = ''

!2345678901234567



! Inicio do Programa

open(unit=10, file="fatura.txt")
write(10,*)' --------------------------------------------------------------------'
write(10,*)' EQUIPAMENTO, POTENCIA, HORAS DE USO  CONSUMO VALOR'
write(10,*)' --------------------------------------------------------------------'



print*,'Qual o valor do kW/h ? '
read*, quilowatt_hora

do 
  
  call system('clear')
  print*,'Digite o nome do equipamento eletrônico. [fim] para sair: '
  read '(a15)', equip

  if ( equip .eq. 'fim') exit

  print*,'Potência [W]: '
  read*,potencia
  
  print*,'Horas de uso [h]: '
  read*, horas_de_uso

  ! Calculando consumo
  consumo = ( potencia * horas_de_uso )
  valor_consumo = (consumo / 1000.0) * quilowatt_hora  ! em Moeda
  
  write(10,100) equip, potencia, horas_de_uso, consumo, valor_consumo

end do

100 format(2x,a,1x,f10.2,1x,i5,1x,2(f10.2))

write(10,*)' --------------------------------------------------------------------'
write(10,*)'1234567890'

close(10)

print *,'==============================================================='

call system('cat fatura.txt')

stop

end program fatura_eletrica

! -------------------------------------------------------------------------------
! Atualmente, a maioria dos aparelhos eletrônicos, mesmo quando desligados, 
! mantém-se em standby, palavra inglesa que pode ser entendida como "pronto para usar".
! Manter o equipamento nesse modo de operação reduz o tempo necessário para
! que volte a operar e evita o desgaste provocado nos circuitos internos devido
! a picos de tensão que aparecem no instante em que é ligado.  Em outras palavras,
! um aparelho nessa condição está sempre parcialmente ligado e, por isso, consome energia.
! 
! Se tivermos uma lista de equipamentos eletrônicos e suas respectivas
! potências e horas de uso, podemos calcular o consumo, tanto individual
! de cada equipamento quanto o consumo total, bem como o valor a ser 
! pago pela energia elétrica utilizada por eles.
! 
! Como exemplo, suponha que uma televisão mantida em standby dissipe uma
! potência de 12 watts e que o custo do quilowattt-hora custa R$ 0,50. 
! Se ela for mantida em standby durante aproximadamente um ano 8.800 horas,
! o seu consumo de energia será 105,6 kW/h.
!
! Faça um programa que leia um equipamento elétrico, sua potência, quantas horas 
! de funcionamento, valor do kW/h em Reais.
! Ao ser digitado "fim" no nome do equipamento, o programa deve fechar a fatura
! listando na tela e e em um arquivo, disposto em colunas o nome do equipamento,
! a potência, horas de funcionamento, o consumo e valor gasto. Nas últimas linhas
! deve-se apresentar um resumo dos valores médios e totais.
!
! Avaliação de competências:
!
! * Sintaxe, entrada e saída (genérica e em arquivos), expressões aritmeticas,
!   expressões lógicas, estruturas de decisão, estruturas de repetição, formatos. 
! 
! -------------------------------------------------------------------------------

