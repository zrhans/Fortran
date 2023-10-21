program ex1
real num1
integer i, nump, numn
nump = 0
numn = 0
i = 0
do while (i .eq. 0)
print *,"digite um número:"
read *,num
do while (num .ne. 0)
if (num .ge. 0) then      
   nump = nump + 1
else
   numn = numn + 1
end if
end do
stop
end do
print *, nump," são positivos e ", numn," são negativos"
end program ex1