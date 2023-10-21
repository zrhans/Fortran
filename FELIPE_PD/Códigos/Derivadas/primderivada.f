      program derivada primeira
      
      implicit real (a-h, o-z)
      integer n
      real x
      external f1
      !common der

      write (*,*) 'Qual o valor para x?'
      read  (*,*) x
      write (*,*) 'Quantas vezes reduzir (/10)?'
      read  (*,*) n

      call derivada(f1,x,h,n,der)
      

      
      stop
      end
      
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      subroutine derivada(f1,x,h,n,der)
      
      real f1, x, h
      integer n, i
      external f1
      !double precision der
      !common der
      
      h=1

      do i=1,n

      der=((f1(x+h)-f1(x))/(h))
      h=h/2
      
      write (*,*) 'h=', h, 'df/dx=', der
      
      enddo
      write (*,*) 'Derivada primeira e:', der
      
      return
      end
      
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      function f1(x)
      
      real f1
      
      f1=((x**2)+(3*x)+5)
      
      return
      end
      
      
