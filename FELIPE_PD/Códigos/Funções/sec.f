      program datafile
      !Criado em 17/03/2017

      !Definicoes principais do programa
      implicit real (a-h, o-z)
      implicit integer (m, n)
      parameter (pi=3.14159265)
      !Caracteristica do gr fico da funcao
      min  = 0
      max  = 4*pi
      ptos = 10000
      freq = (max-min)/(ptos-1)

      !Constru‡Æo dos pontos do grafico
      do nx = 1,ptos
      x  = min + (nx-1)*freq

      !Definicao das funcoes
      funcao  = 1/cos(x)+1
      
      open (unit=1,file='resultado_sec.dat')

      if(funcao.lt.20.and.funcao.gt.-20) write(1,*) x , funcao

 1    enddo

      close(1)

      stop
      end program datafile
