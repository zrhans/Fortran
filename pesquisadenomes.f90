!Perograma que efetue uma pesquisa sequencial sobre o vetor para encontrar nomes!
!Se encontrar, escrever nome e posicao de armazenamento no vetor. Se n∆o, informar da inexistencia do mesmo!

program pesquisadenomes
implicit none
character*5 nom(10), nome
integer i
logical achou

nom(1)="aaaaa";nom(2)="bbbbb";nom(3)="ccccc";nom(4)="ddddd";nom(5)="eeeee";        !nome e posicao dos vetores!
nom(6)="fffff";nom(7)="ggggg";nom(8)="hhhhh";nom(9)="iiiii";nom(10)="jjjjj";

write(*,"(a,$)") "Informe um nome: "                    !usu†rio declara um nome!
read(*,*) nome
achou=.false.
read*,

do 100 i=1,10                                         !pesquisa do nome e posicao!
 if (nome==nom(i)) then
  write (*,"(/,a,i2)")"Nome encontrado na posiá∆o " ,i
   achou=.true.                       !caso for encontrado!
  exit
 endif
100 continue
read*,

 if (.not.achou) then                      !Caso nao for encontrado!
  write(*,"(/,a,a,a)") "Nome: " ,nome, " nao encontrado "
 endif
read*,

stop
end
