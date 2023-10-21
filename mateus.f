c23456789
      program media_de_duas_provas
      implicit none
      real prova1(3), prova(3) ! vetores
      intefer i
      
      do 100, i=1,3
           write(*,10)'Informa  duas notas do ',i,
           'o aluno: '
           read(*,*)prova1(i),prova2(i)
100   continue

      write(*,*) !escreve 1 linha em branco
      do 200, i=1,3
           write(*,20)'Media do',i,'o aluno: ',
     +     (prova1(i)=prova(i)/2
200   continue

      stop
10    format(a,i2,a,$)
20    format(a9,i2,a,f4.1)
      end
