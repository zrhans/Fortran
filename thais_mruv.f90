PROGRAM mruv
      IMPLICIT NONE                ! isto nos obriga a declarar todas as
                             ! variaveis que usaremos (recomendavel)
      REAL :: x0, x, v0, a, t, dt  ! variaveis reais (ponto flutuante)
      INTEGER :: I, N

      PRINT*, 'entre x0, v0, a, t, N'
      READ*, x0, v0, a, t, N
      dt = t/N       ! passo de tempo: definido pelo tempo maximo e o
               ! numero de pontos da trajetoria
      t = 0

      DO i = 1, N
      x = x0 + v0*t + 0.5*a*t**2
      PRINT*, t, x
      t = t + dt
      END DO
      STOP
	  ! essa equacao se declarava variavel (V) indefinida  este era o erro deste programa
      !para corrigir o programa substitui na equacao a variavel indefinida pela variavael declarada (v0)
END PROGRAM mruv
      
