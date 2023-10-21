program nestedIfProg
implicit none
   ! local variable declaration
   integer :: a = 100, b= 200

   ! check the logical condition using if statement
   if( a == 100 ) then

        ! if condition is true then check the following 
      if( b == 200 ) then

             ! if inner if condition is true 
             print*, "Valor de a é 100 e b é 200" 

       end if
      
       
   end if

   print*, "======================="
   print*, "valor exato a é ", a
   print*, "valor exato b é ", b

end program nestedIfProg