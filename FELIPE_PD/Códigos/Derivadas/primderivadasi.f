      program derivada primeira simetrizada

      implicit real (a-h, o-z)
      integer n
      real x
      external f1

      h=1

      write (*,*) 'Qual o valor para x?'
      read  (*,*) x
      write (*,*) 'Quantas vezes reduzir (/10)?'
      read  (*,*) n

      call derivada(f1, x, n, h, der)

      write (*,*) 'Derivada primeira e:', der

      stop
      end

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine derivada(f1, x, n, h, der)

      real f1, der, x, h
      integer n, i
      external f1

      do i=1,n

      h=h/2

      der=((f1(x+h)-f1(x-h))/(2*h))

      write (*,*) 'h=', h, 'df/dx=', der

      enddo
      return
      end

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      function f1(x)

      real f1

      f1=((x**2)+(3*x)+5)

      return
      end


