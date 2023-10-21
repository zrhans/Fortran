c23456789
      program altura_media
      implicit none
      real alt(100)*10    !vetor
      integer i,j
      i=1; soma=0
      write(*,10)'Entre com o ',i,'o.nome: '
      read(*,*)nome(i)
      do while(nome(i)/='fim)
         while(*,20)'Entre com a altura: '
         read(*,*)alt(i)
         soma=soma+alt(i)
         i=i+1
         write(*,10)'Entre com o ',i,'o.nome: '
         read(*,*)nome(i)
      enddo
      media=soma/(i-1)
      write(*,30)'Altura media: ',media
      do 100 j=1,i-1
         if (alt(j)>media)then
            write(*,40)nome(j), ' ', alt(j)
         endif
      continue
      stop
      format(a,i2,a,$)
      format(a,$)
      format(/,a,f4.2,/)
      format(a,a,f4.2)
      end
