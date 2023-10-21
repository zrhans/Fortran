program temp

character(len=8) :: r=''

print*, 3 + 4 / 2
print*, 3. * 4**2
print*, -1.**2
print*, 6 / 4 / 2
print*, 3.**3 / 2
print*, (-1.)**3




print*, 'ABCDEFGH'
print*, 'ABCD' // '01234'
r(:7) = 'ABCDEFGH'
print*, r
r(:6) = 'ABCD'
print*,r

end program temp