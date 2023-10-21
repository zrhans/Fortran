       program diferencafinita
       implicit none
       integer k,km
       real dp,dz,dp2,dz2,pzero,a
       real p(10),z(10),verd(10),d1(10),d2(10),d4(10)

       open(1,file='diffin.xls')
       
       km=10
       pzero=1000.
       a=0.000125062
       
       z(1)=0
       do k=1,km
       z(k)=z(1)+(k-1)*1000.
       enddo
       
       do K=1,km
       p(k)=pzero*exp(-a*z(k))
       verd(k)=-a*pzero*exp(-a*z(k))
       enddo
       
       do k=1,km-1
       dp=p(k+1)-p(k)
       dz=z(k+1)-z(k)
       d1(k)=dp/dz
       enddo
       
       do k=2,km-1
       dp=p(k+1)-p(k-1)
       dz=z(k+1)-z(k-1)
       d2=dp/dz
       enddo
       
       do k=3,km-2
       dp=p(k+1)-p(k-1)
       dz=z(k+1)-z(k-1)
       dp2=p(k+2)-p(k-2)
       dz2=z(k+2)-z(k-2)
       d4(k)=4./3.*dp/dz-1./3.*dp2/dz2
       enddo
       
       do k=1,km
       write(1,*)k,p(k),verd(k),d1(k),d2(k),d4(k)
       enddo
       
       end program diferencafinita
       
