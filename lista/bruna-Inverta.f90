      program inverte
      implicit none
      integer num(10),i
      print*, "Programa que lˆ 10 n£meros e imprime na tela a lista invertida dos n£meros"
      do 100,i=1,10
      write(*,10)"Imforme o ",i,"o numero: "
      read*,num(i)
100   continue

      write(*,*) !escreve uma linha em branco
      do 200,i=10,1,-1
      write(*,20)num(i)

200   continue

read*
      stop
10    format(a,i2,a)
20    format(i2,2x)

      end program inverte
         
