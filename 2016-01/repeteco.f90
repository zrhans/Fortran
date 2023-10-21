program fatorial  
implicit none  

   ! define variaveis
   integer          :: nfact = 1   
   integer(kind=1)  :: n  

   ! calculando o fatorial 
   print*,'   n       |  FATORIAL'
   print*,'========================'
   do n = 1,20         
      nfact = nfact * n 
      
      ! mostrando valores de n e seu fatorial
      if (n == 8) cycle         
      if (nfact < 0 ) stop 'Limite do tipo Integer alcaçado.'

      print*,n, nfact     
      
   end do 

end program fatorial