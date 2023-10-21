      program Calculo_med
C      parameter(colunas=32,linhas=452)

      INTEGER i.J
      REAL Sv,Su,Sw,f
      REAL D(linhas,1:colunas)

C     COMECANDO A LER O ARQUIVO DE ENTRADA

C      OPEN(UNIT=10,FILE='INPUT.DAT')
C      OPEN(UNIT=11,FILE='ADIMENSIONAIS.DAT')
      DO i=1,linhas
         f=0.;su=0.;sv=0.;sw=0.;
         READ(10,*)(D(i,J),J=1,colunas)
         f=(D(i,11)*D(i,5))/D(i,10)
         su=(D(i,11)*D(i,12))/( (0.35*9.*D(i,27))**(2./3.) )
         sv=(D(i,11)*D(i,13))/( (0.35*9.*D(i,29))**(2./3.) )
         sw=(D(i,11)*D(i,14))/( (0.35*9.*D(i,25))**(2./3.) )
         WRITE(11,100)F,Su,Sv,Sw
 100     format(F7.5,1X,F12.5,1x,F12.5,1x,F12.5)
      enddo
      END
 
