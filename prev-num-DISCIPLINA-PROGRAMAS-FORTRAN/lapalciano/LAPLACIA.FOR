      program LAPLACIAN
c                                                                               
c  this program computes the laplacian using the five-point second 
c  order,nine-point second order and the iterated nine-point fourth
c  order laplacian schemes.it also computes the root mean square er
c -rors and compares the accuracy of the different schemes to the a
c -nalytical solution.                                                  
c                                                                               
      parameter (l=10,m=20)
c
c  declare variables and defines some constants.
c                                                     
      real psi(l,m),zta(l,m),a(l,m)
      real b (l,m),c(l,m),x(l),y(m)                     
      pi        = 4.*atan(1.0)                                                         
      h         = 200.                                                                 
      yk        = 2.*pi/1000.                                                          
      yl        = pi / 1000.
c                                                           
      x(1)      = 0.                                                                 
      y(1)      = 0.                                                                 
      do 2200 i = 2, l                                                            
         im1    = i-1                                                                 
      do 2200 j = 2, m                                                            
         jm1    = j-1                                                                 
         x(i)   = x(im1) + h                                                         
         y(j)   = y(jm1) + h                                                         
 2200 continue                                                                  
      sum       = 0.
c                                                                  
c  construct the stramfunction(psi) psi = sin (kx)*sin(ly)+ cos(ly)
c  and the vorticity (zta) as       zta = d2(psi)/dx2 + d2(psi)/dy2
c
      do 2202 i = 1, l                                                            
      do 2202 j = 1, m                                                            
         psi(i,j)  = sin(yk*x(i)) * sin(yl*y(j)) + cos(yl*y(j))                     
         zta(i,j)  = -(yk**2+yl**2)*sin(yk*x(i)) * sin(yl*y(j))                      
     &               -yl**2 * cos(yl*y(j))                                                     
      a(1,j)    = zta(1,j)                                                         
      a(l,j)    = zta(l,j)                                                         
      a(i,1)    = zta(i,1)                                                         
      a(i,m)    = zta(i,m)                                                         
      sum       = sum + (zta(i,j) / (l*m))**2                                         
 2202 continue                                                                  
      su        = sqrt( sum )                                                          
      n         = 1                                                                     
   25 go to ( 30,40,50,60 ) n                                                   
   30 write(6,1000)
c                                                              
c  compute the 9 pts 4th order.
c                                                                               
      call LAP94 (psi,a,b,c,h,l,m)                                               
c                                                                               
      go to 70                                                                  
   40 write(6,1001)                                                              
c                                                              
c  compute the 9 pts 2th order.
c                                                                               
      call LAP92 (psi,a,h,l,m)                                                   
c                                                                               
      go to 70                                                                  
   50 write(6,1002)                                                              
c                                                              
c  compute the 5 pts 2th order.
c                                                                               
      call LAP52 (psi,a,h,l,m)                                                   
c                                                                               
   70 continue                                                                  
      dif = 0.                                                                  
      do 2204 i = 1, l                                                            
      do 2204 j = 1, m                                                            
         dif    = dif + ((zta(i,j) - a(i,j)) / (l*m))**2                              
 2204 continue                                                                  
      dif       = sqrt(dif)                                                           
      sum       = (dif / su ) * 100                                                   
      write(6,1003) dif, sum
c                                                     
c  output display for one colone.
c                                               
      write(6,1004) ((i,j,zta(i,j),a(i,j),i=1,l), j=4,4)                         
      n         = n + 1                                                                 
      go to 25                                                                  
   60 continue                                                                  
 1000 format(//,20x,'nine points fourth order laplacian scheme.'//)             
 1001 format(//,20x,'nine points second order laplacian scheme.'//)             
 1002 format(//,20x,'five points second order laplacian scheme.'//)             
 1003 format(9x, 'rms error = ',e13.7,8x, 'rms error/rms zta = ',
     &e9.3,1x, 'percent.'//,2x,'i  j',5x, 'analytical sol',1x,'estimated
     & sol.',2x,'i  j',5x, 'analytical sol',1x,'estimated sol',/)
 1004 format( 2(2i3,4x,2e15.8) )
      stop                                                                      
      end                                                                       
      subroutine LAP52 (psi,a,h,l,m)
c                                             
c  This subroutine computes the second order                                     
c  Laplacian using five-point stencil.
c                                           
c  input  : psi                                   
c  output : a    is the Laplacian of psi using a                         
c                nine point second order scheme.
c  h      : grid distance in both x-and y-directions                           
c  l,m    : first and second dimension of psi.
c                                                                               
      real psi(l,m), a(l,m)                                                
      l1        = l-1                                                                  
      m1        = m-1                                                                  
      do 2230 i = 2, l1                                                           
      do 2230 j = 2, m1                                                           
         ip1    = i+1                                                                 
         im1    = i-1                                                                 
         jp1    = j+1                                                                 
         jm1    = j-1                                                                 
      a(i,j)    = (psi(ip1,j)+psi(im1,j)+psi(i,jp1)+psi(i,jm1)                     
     &          - 4. * psi(i,j)) / h**2                                                  
 2230 continue                                                                  
      return                                                                    
      end 
      subroutine LAP92 (psi,a,h,l,m)
c                                             
c  This subroutine computes the second order                                     
c  Laplacian using a nine-point stencil.                                         
c                                                                               
c  input  : psi                                   
c  output : a    is the Laplacian of psi using a                         
c                nine point second order scheme.
c  h      : grid distance in both x-and y-directions                           
c  l,m    : first and second dimension of psi.
c                                                                             
      real  psi(l,m), a(l,m)                                                
      l1        = l-1                                                                  
      m1        = m-1                                                                  
      do 2220 i = 2, l1                                                           
      do 2220 j = 2, m1                                                           
         ip1    = i+1                                                                 
         im1    = i-1                                                                 
         jp1    = j+1                                                                 
         jm1    = j-1                                                                 
      a(i,j)    = (psi(ip1,jp1)+psi(ip1,jm1)+psi(im1,jp1)+                         
     &  psi(im1,jm1)+4.*(psi(ip1,j)+psi(im1,j)+psi(i,jp1)+                       
     &  psi(i,jm1))-20.*psi(i,j)) / (6.*h**2)                                    
 2220 continue                                                                  
      return                                                                    
      end 
      subroutine LAP94 (psi,a,b,c,h,l,m)
c                                         
c  This subroutine computes the itterated 
c  9 points fourth  order Laplacian.                     
c                                                                               
c  input  : psi                                   
c  output : a    is the Laplacian of psi using a                         
c                nine point fourth order scheme.
c  b,c    : working arrays .                         
c  h      : grid distance in both x-and y-directions                           
c  l,m    : first and second dimension of psi.
c                                                                             
      real psi(l,m), a(l,m), b(l,m), c(l,m)                                
      del2(x0,x1,x2,x3,x4,x5,x6,x7,x8,d) = (x1+x2+x3+x4+4.*                     
     & (x5+x6+x7+x8)-20.*x0) / (6.*d**2)
      k         = 0                                                                     
      n         = 1                                                                     
      m1        = m-1                                                                  
      m2        = m-2                                                                  
      l1        = l-1                                                                  
      l2        = l-2                                                                  
      dmax      = 1.                                                                 
      eps       = 1.e-02                                                              
      su        = 0.                                                                   
      do 2210 i = 2, l1                                                           
         ip1    = i+1                                                                 
         im1    = i-1                                                                 
      do 2210 j = 2, m1                                                           
         jp1    = j+1                                                                 
         jm1    = j-1                                                                 
      a(i,j)    = del2(psi(i,j),psi(ip1,jp1),psi(im1,jp1),                         
     &    psi(ip1,jm1),psi(im1,jm1),psi(i,jp1),psi(i,jm1),                         
     &    psi(ip1,j),psi(im1,j),h)                                                 
      b(i,j)    = a(i,j)                                                           
      su        = su + abs(a(i,j)) / (l*m)                                             
 2210 continue                                                                  
   30 if( n .eq. 0 ) go to 70                                                   
      k         = k + 1                                                                 
      dif       = 0.
c                                                                  
c  calculate del4 and iterate                                                   
c
      do 2211 i = 2, l1                                                           
         ip1    = i+1                                                                 
         im1    = i-1                                                                 
      do 2211 j = 2, m1                                                           
         jp1    = j+1                                                                 
         jm1    = j-1                                                                 
      c(i,j)    = del2(a(i,j),a(ip1,jp1),a(im1,jp1),a(ip1,jm1),                     
     &       a(im1,jm1),a(i,jp1),a(i,jm1),a(ip1,j),a(im1,j),h)                        
      dif     = dif+abs(c(i,j)*h**2 / 12.+a(i,j)-b(i,j))/(l*m)                      
 2211 continue                                                                  
      do 2212 i = 2, l1                                                           
      do 2212 j = 2, m1                                                           
         a(i,j) = b(i,j) - c(i,j) * h ** 2 / 12.                                   
 2212 continue                                                                  
      dif       = dif / su                                                            
      if ( dif .le. eps  ) go to 70                                               
      if ( k   .ge. 100  ) go to 60                                                 
      if ( dif .gt. dmax ) go to 60                                              
      go to 30                                                                  
   60 write(6,100)                                                              
   70 continue                                                                  
  100 format( '   the scheme does not converge. ' )                             
  300 format( 10x, 'k = ', i10, e30.13 )                                        
      return                                                                    
      end 
