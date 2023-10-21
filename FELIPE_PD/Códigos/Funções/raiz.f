      program datafile
      !Criado em 17/03/2017

      !Definicoes principais do programa
      implicit real (a-h, o-z)
      implicit integer (m, n)

      !Caracteristica do gr�fico da funcao
      min  = 0
      max  = 100
      ptos = 10000
      freq = (max-min)/(ptos-1)

      !Constru��o dos pontos do grafico
      do nx = 1,ptos
      x  = min + (nx-1)*freq

      !Definicao das funcoes
      funcao  = sqrt(x+2)

      open (unit=1,file='resultado_raiz.dat')

      if(funcao.LT.40.AND.F.GT.40) write(1,*) x , funcao

 1    enddo

      close(1)

      stop
      end program datafile
