!Exemplo: Programa que soma os valores de duas matrizes
program soma_matriz  
implicit none

integer, parameter :: lin = 2 , col = 2   !dimensões das matrizes
integer :: linha                          !variável controladora das linhas nos loops

        !arrays bidimensionais
real :: matriza(lin, col), matrizb(lin, col), matrizc(lin, col)
        

call system('clear')

!leitura dos elementos da matriz A

print *, '-------------------------------------------- '
print '(A,i1,"x",i1,A)', ' A e B são Matrizes ',lin,col,' (linhas x colunas). '
print *, '--------------------------------------------'
print *, ' Digite elemento por elemento '
print *, ' ou todos da linha e pressione ENTER'
print *, '____________________________________________'
print *, 'Digite os valores da matriz A: '
do linha = 1, lin 
    print*, "lendo linha:",linha
    read *, matriza(linha, 1: col)      
end do
print *

    ! Faz a mesma leitura que o anterior (sai apos todos o elementos digitados)                    
    ! read *, matriza(1:linha, 1: col)      


!leitura dos elementos da matriz B
print *, 'Digite os valores da matriz B: '
do linha = 1, lin                                         !leitura dos elementos
read *, matrizb(linha, 1: col)     !da matriz B
end do

!formação da matriz C atraves da soma dos elementos das matrizes A e B
do linha = 1, lin
      matrizc(linha, 1: col) = matriza(linha, 1: col) + matrizb(linha, 1: col)
end do         


print *,'======================'
print *, 'Matriz A :'
do linha = 1, lin
print '(3F8.2)', matriza(linha, 1: col)   !visualização da
end do  !matriz A

print *,'======================'
print *, 'Matriz B :'
do linha = 1, lin
print '(3F8.2)', matrizb(linha, 1: col)   !visualização da
end do  !matriz B

print *
print *, 'Matriz C = A + B: '
do linha = 1, lin
print '(3F8.2)', matrizc(linha, 1: col)   !visualização da
end do  !matriz C

print * 
stop "Programa finalizado!"
end program soma_matriz

! Desafio:
! Implementar um subprograma para imprimir as matrizes
! após a inserção de seus valores no formato de exibição
! matemático (linhas x colunas)