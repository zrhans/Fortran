      program iteracao
      
      real x, f, xm
      integer n, i
      
      write(*,*) 'Chute um valor para x'
      read (*,*) x
      write(*,*) 'Quantas vezes iterar'
      read (*,*) n
      
      do i=1,n
      
      xm=x-((x**2-9)/(2*x))
      x=xm
      
      write(*,*) 'xm=', xm
      
      enddo

      write(*,*) 'A raiz e', xm
      
      stop
      end
