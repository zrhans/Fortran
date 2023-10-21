      PROGRAM Main                       
        REAL :: X=100. , Y=30.             
        PRINT *, F1(), X, Y               
      CONTAINS                          
        FUNCTION F1()                      
          Y = 55.                           
          F1 = X*Y 
        END FUNCTION                       
      END PROGRAM Main