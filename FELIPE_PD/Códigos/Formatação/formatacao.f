      program formatacao
      !Criado em 06/04/2017

      implicit real (a-h, q-z)
      
      xpi=4*datan(1.d0)
      c=2.99792458E8

      !Caracteristica da funcao
      min=0
      max=10
      ptos=20
      
      freq = (max-min)/(ptos-1)

      !Constru‡Æo dos pontos do grafico
      do 1 nx = 1,ptos
      x  = xmin + (nx-1)*freq

      !Definicao das funcoes
      funcao  = x**3

      open (unit=1,file='resultados.txt')
      
      write(1,11) x , funcao, xpi, c
      
 11   format(' ',1p,4d30.10)

 1    enddo

      close(1)

      stop
      end
