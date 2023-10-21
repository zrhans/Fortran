program mediade2provas
implicit none
real prova1(3), prova2(3) !vetores
integer i
print*, "Programa que calcula a m‚dia de duas provas para trˆs alunos."

do 100,i=1,3
      write(*,10)"Infome as duas notas do ",i,"o aluno"
      read(*,*)prova1(i), prova2(i)
100   continue

      write(*,*) !escreve uma linha em branco
      do 200,i=1,3
      write(*,20)"M‚dia do ",i,"o aluno", (prova1(i)+prova2(i))/2
200   continue

      write(*,*)
      print*, "Aperte qualquer tecla para finalizar o programa."
      read*
      stop
10    format(a,i2,a)
20    format(a9,i2,a,f4.1)
      end program

