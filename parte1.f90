

clns=0
clps=0
do i=1,t_lines
 READ(10,*,end=10)a,a,a,hhmmns(i),a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a
   clns=clns+1
end do
!10        continue

do j=1,t_lines
 READ(10,*,end=10)a,a,a,hhmmps(j),a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a
   clps=clps+1
end do
10        continue

  do i=1,clns
       hhns(i)=hhmmns(i)(1:1)
       mmns(i)=hhmmns(i)(2:3)
  end do

  do i=1,clns
    WRITE(11,100)hhns(i),mmns(i)
100        format(a1,2x,a2)
  end do

do j=1,clps
       hhps(j)=hhmmps(j)(1:2)
       mmps(j)=hhmmps(j)(3:4)
  end do

  do j=1,clps
    WRITE(12,200)hhps(j),mmps(j)
200        format(a2,2x,a2)
  end do
