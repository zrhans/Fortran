      program integral
      external f
      real*8 areaa, areab, areac, dx, areat, a, b
      integer n



      call integrar
      end
      
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine integrar

      a  = 0
      b  = 1
      n  = 1000

      dx=(b-a)/n
      areaa = f(a)/2.0
      areab = f(b)/2.0
      areac = 0

      do i = 1,n-1
      areac = f(a + i*dx) + areaC

      open (unit=1,file='area.dat')

      write(1,*) areaC

      end do

      areat=(areaa+areab+areac)*dx
      
      write(1,*) 'Integral=', areaT
      print *, 'Integral = ',areat
      write(1,*) areaa, areab
      print *, 'area a = ',areaa,'area b = ',areab
      close(1)
      
      end subroutine integrar !termina a subrotina

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      function f(x) !fun‡Æo usada
      f = x**2
      end
