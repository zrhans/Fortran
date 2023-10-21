      program integral
      
      real*8 areac, dx, areat, x, a, b
      integer n
      external f
      
      a   =5
      b   =10
      n   =10000
      
      dx=(b-a)/n

      
      do 1 i=1,n-1
      
      areaC=(f(a+(i*dx)))+areaC

      open (unit=1,file='area.dat')

      write(1,*) areaC

 1    enddo

      areaT=(areaA+areaB+areaC)*dx
      write(1,*) 'Area final=', areaT
      write(1,*) f(a)/2, f(b)/2
      close(1)


      stop
      end
      
      function f(x)
      f=x
      end
