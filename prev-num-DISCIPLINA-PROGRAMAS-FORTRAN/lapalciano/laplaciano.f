       program laplaciano
       implicit none
       parameter (l=10,m=20)
       
       real psi(l,m),zta(l,m),a(l,m)
       real b(l,m),c(l,m),x(l),y(m)
       pi=4.*atan(1.0)
       h=200.
       yk=2.*pi/1000.
       yl=pi/1000.

       x(1)=0.
       y(1)=0.
       do 2200 i=2,l
       iml=i-1
       do 2200 j=2,m
       jml=j-1
       x(i)=x(iml)+h
       y(j)=Y(jml)+h
 2200  continue
       sum=0.
       
c     construct the stramfunction(psi) psi = sin (kx)*sin(ly)+ cos(ly)
c  and the vorticity (zta) as       zta = d2(psi)/dx2 + d2(psi)/dy2

       do 2202 i=1,l
       do 2202 j=1,m
       psi(i,j)=sin(yk*x(i))*sin(yl*y(j))+cos(yl*y(j))
       zta(i,j)=-(yk**2+yl**2)*sin(yk*x(i))*sin(yl*y(j))
     & -yl**2*cos(yl*y(j))
       a(1,j)=zta(1,j)
       a(l,j)=zta(l,j)
       a(i,1)=zta(i,1)
       a(i,m)=zta(i,m)
       sum=sum+(zta(i,j)/(l*m))**2
       
 2202  continue
       su=sqrt(sum)
       n=1
   25  go to (30,50,60)n
   30  write(6,1000)
       
       go to 70
   40  write(6,1001)
       
c      calcular para 9 pontos 2th ordem

       call LAP92(psi,a,h,l,m)
       
       go to 70
   50  write(6,1002)


c      calcular para 5 pontos 2th ondem

       call LAP52(psi,a,h,l,m)
       
   70  continue
       dif=0
       do 2204 i=1, l
       do 2204 j=1, m
       dif=dif+((zta(i,j)-a(i,j))/(l*m))**2
 2204  continue
       dif=sqrt(dif)
       sum=(dif/su)*100
       write(6,1003) dif, sum
       
       write(6,1004) ((i,j,zta(i,j),a(i,j),i=1,l),j=4,4)
       n=n+1
       go to 25
   60  continue
 1001  format(//,20x,'esquema lapalciano de 9 pontos 2 ordens.'//)
 1002  format(//,20x,'esquema laplaciano de 5 pontos 2 ordens.'//)
 1003  format(9x,'rms erro=',e13.7,8x,'rms erro/rms zta=',e9.3,1x,
     & 'percent.'//,2x,'i j',5x,'sol analitica',1x,'sol estimada',
     &  2x,'i j',5x,'sol analitica',1x,'sol estimada',/)
 1004  format(2,(2i3,4x,2e15.8))
       stop
       end


       
       
       
       
       
       
