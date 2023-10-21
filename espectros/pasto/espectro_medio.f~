      program Calculo_med
c
c
c
c
c
c
c
c
c
 !PARAMETER(NN=$L,M=$BANDAS)
      

C     PARAMETER(N=16384,NN=524288,KK=1800,K=8192,M=22)


C     COMECANDO A LER O ARQUIVO DE ENTRADA
C	

	
      i=1
      OPEN(UNIT=54,FILE='PSWS1M.DAT')
      OPEN(UNIT=10,FILE='MPSWS1M.DAT')

c não teclar entre acima desta linha
C
C
      DO i=1,L
         READ(54,*,END=10)(DATA(i,J),J=1,4)
10    CONTINUE
C
      PRINT*,'Leu o arquivo com ',l,' linhas.'	
C	CALCULO DA SOMA DAS COLUNAS DE ESPECTRO DE SV, SU, SW E ST
 
      WRITE (*,*)'ENTRADA'
      K=0;x=1;t=0;f=0;
      m=0;sb=0;i=0;
	sb=L/B	
	do m=1,B
      SSv(m)=0.;SSu(m)=0.;SSw(m)=0.;fm(m)=0.;
      MSv(m)=0.;MSu(m)=0.;MSw(m)=0.;Mfm(m)=0.;
      enddo
    	open(unit=30, file='analasie')
	
 20   continue
   
        sumf=0.;sumu=0.;sumv=0.;sumw=0.;
 	DO 45 M=1,SB
	   	sumf=sumf+data(m+k,1)
!	print*,sumf,' ',sumu,' ',sumv,' ',sumw
		sumu=sumu+data(m+k,2)
		sumv=sumv+data(m+k,3)
		sumw=sumw+data(m+k,4)
 45     CONTINUE
           f=f+1 
	   	fm(f)=sumf/sb
	   	SSu(f)=sumu/sb
		SSv(f)=sumv/sb
         	SSw(f)=sumw/sb
  	write(30,*)'Banda: ',k+1,'>f su sv sw ',fm(f),sSv(f),sSu(f),sSw(f)
!	PRINT*,'Banda: ',k+1,'>f su sv sw ',fm(f),sSv(f),sSu(f),sSw(f)
	write(30,*)'1 sb:',sb,' banda: ',f+1,' k: ',k,' L: ',L
      k=k+sB
      IF (K.GE.L) then
      goto 5 
      else 
      goto 20
      endif
 5    continue
  	write(30,*)'Escrevendo ',m-1,' bands'
	close(30)
C     ESCREVENDO ARQUIVOS DE SAIDA
      
      do 50 x=1,B
         WRITE(*,*)'\\n ',x,'>',fm(x),sSv(x),sSu(x),sSw(x)
         WRITE(10,100)fm(x),sSv(x),sSu(x),sSw(x)
 100     format(F13.6,1X,F12.5,1x,F12.5,1x,F12.5)
 50      continue  
  
      
      END
      
