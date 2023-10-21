       program laplaciano
       implicit none
       parameter (l=10,m=20,l1=l-1,m1=m-1,l2=l-2,m2=m-2)
       real psi(l,m),zta(l,m),a(l,m),x(l),y(m),dx(m)
       pi=4.*atan(1.0)
       h=200.
       dy=h
       
       do 2300 j=1, m
 2300  dx(j)=dy
       yk=2.*pi/1000.
       yl=pi/1000.
       x(1)=0.
       y(1)=0.

       do 2302 i=2,l
       iml=i-1
       do 2302 j=2,m
       jml=j-1
       x(i)=x(iml)+h
       y(j)=Y(jml)+h
 2302  continue
       sum=0.

c  construct the stramfunction(psi) psi = sin (kx)*sin(ly)+ cos(ly)
c  and the vorticity (zta) as       zta = d2(psi)/dx2 + d2(psi)/dy2

       do 2304 i=1,l
       do 2304 j=1,m
       psi(i,j)=sin(yk*x(i))*sin(yl*y(j))+cos(yl*y(j))
       zta(i,j)=-(yk**2+yl**2)*sin(yk*x(i))*sin(yl*y(j))
     & -yl**2*cos(yl*y(j))
       a(1,j)=zta(1,j)
       a(l,j)=zta(l,j)
       a(i,1)=zta(i,1)
       a(i,m)=zta(i,m)
       sum=sum+(zta(i,j)/(l*m))**2

 2304  continue
 
c  calcular o jacobiano

       call JAC(a,psi,zta,dx,dy,l,m,l1,m1,l2,m2)
       
       write(6,1000)
       write(6,1001)
       write(6,1002)((i,j,a(i,j),i=1,l),j=4,4)
 1000  format(//,20x,'esquema jacobiano.',//)
 1002  format(2x,'i j',10x,'jacobiano estimado',//)
 1003  format((2i3,8x,e15.8))
       stop
       end
       
 
 
       
       
