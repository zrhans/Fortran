! Data: 21-06-2018
! Este programa calcula a média, a variação e 
! o desvio padrão de uma aluno com notas atribuídas 
! diretamente no código.

program notas_estatistica
implicit none

!Declaração das váriaveis
real :: desvio_padrao, media=0.0, soma=0.0, variancia=0.0
integer :: i
integer, parameter :: n = 6 ! número de notas   
real, dimension(n) :: nota

! Inicialização da variável nota com os valores
! das Notas do aluno
nota = (/9.0,7.0,5.0,3.0,2.0,5.5/)

!Cálculo do somatório e da média
do i = 1, n
  soma = soma + nota(i)
end do
media = soma / n

! Cálculo da variância das notas
variancia = 1.0 / size(nota(:)) * sum((nota(:)-media)**2) 

! Desvio padrão
desvio_padrao = sqrt(variancia)
print*,"---------------------------------"
print*,"        notas estatistica"
print*,"---------------------------------"
!Mostrando o resultado na tela
print*,'Média das notas'
print 100, media
print*,'Variância das notas'
print 100, variancia
print*,'Desvio padrão das notas'
print 100, desvio_padrao 
print*,"================================="

100 format(f7.2)

end program notas_estatistica
