open(3,file='data.txt')
DO 10, I = 1,101
   !READ(3,*,END=20,ERR=1000) A
   READ(3,*,end=20) A
   print*,i,a
   FILENO = I
10 CONTINUE

20 WRITE (*, *) ' Entrada concluída. Número de registos: ', FILENO
close(3)

STOP ' Programa finalizado!'
end
