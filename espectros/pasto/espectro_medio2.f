c não teclar entre acima desta linha
C
C
      DO 10 i=1,l
        READ(54,*,END=10)(DATA(i,J),J=1,4)
10    CONTINUE
C	CALCULO DA SOMA DAS COLUNAS DE ESPECTRO DE SV, SU, SW E ST
 
      WRITE (*,*)'ENTRADA'
      m=0;
	SSv=0.;SSu=0.;SSw=0.;fm=0.;
        sumf=0.;sumu=0.;sumv=0.;sumw=0.;
 	DO 45 M=1,L
	   	sumf=sumf+data(m,1)
		sumu=sumu+data(m,2)
		sumv=sumv+data(m,3)
		sumw=sumw+data(m,4)
 45     CONTINUE
	   	fm=sumf/l
	   	SSu=sumu/l
		SSv=sumv/l
         	SSw=sumw/l
	 WRITE(10,100)fm,SSu,SSv,SSw
 100     format(F13.6,1X,F12.5,1x,F12.5,1x,F12.5)
      END
      
