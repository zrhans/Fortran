!programa para escrever 10 numeros em um arquivo de texto
program texto
implicit none

! Declaração de variáveis
integer   :: i

! Abrindo um arquivo
open(unit=10, file="dados.txt")

! Usando estrutura de repetição para escrever 10 numeros no arquivo

do i = 10, 20
  
  !Sintaxe: write(<unit>, <formato>) lista_de_variaveis
  write(10, *) i, i**2
  
end do

close(10)

end program texto