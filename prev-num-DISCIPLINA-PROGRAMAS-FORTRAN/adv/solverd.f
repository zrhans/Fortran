       program solverd
       implicit none
       integer :: i,l,tempo,x,l2
       integer, parameter :: im=101, mt=721, u=8
       real, parameter :: tzero=20., ehle=10000.,pi=4.0*atan(1.0)
       real, dimension(im,mt) :: temp
       real, dimension(im,25) :: temp2
       real, dimension(im,10) :: temp3
       
       do l=1, mt
       tempo=(l-1)*2*60
       write(*,*)'tempo(',l-1,'):',tempo,'s',tempo/3600,'h'
       do i=1,im
       x=(i-1)*100
       temp(i,l)=tzero*sin((2.*pi/ehle)*real(x-u*tempo))
       enddo
       enddo
       
       
       l2=0
       do l=1,mt,30
       l2=l2+1
       write(*,*)'l:',l,'tempo=',(l-1)*120,l2
       do i=1,im
       temp2(i,l2)=temp(i,l)
       enddo;enddo
       
       open(1,file='solverd_1h.bin',status='unknown',form=
     & 'unformatted',access='direct',action='write',recl=4*im*25)
       write(1,rec=1)temp2
       close(1)
       
       
       l2=0
       do l=1,10
       l2=l2+1
       write(*,*)'l:,',l,'tempo=',(l-1)*120,l2
       do i=1,im
       temp3(i,l2)=temp(i,l)
       enddo;enddo
       
       open(1,file='solverd_2m.bin',status='unknown',form=
     &  'unformatted',access='direct',action='write',recl=4*im*10)
       write(1,rec=1)temp3
       close(1)
       
       end program solverd
       
       
       
       
       
