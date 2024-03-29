CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C								                                    C
C  ######  PROGRAMA MAIN "PARA OS DADOS DO EXP. XICA-99  ########     C
C								                                    C
C   INICIADO EM 1999. SOFREU DIVERSAS ATUALIZACOES. A ULTIMA DELAS EM C
C   FEVEREIRO DE 2005. NESTA ULTIMA VERSAO FORAM INCORPORADAS AS      C
C   MATRIZES NINPUT, NNINPUT, NNNINPUT, AS QUAIS POSSUEM OS VALORES   C
C   DE u, w E w APOS AS ROTACOES SUCESSIVAS (Kaimal e Finnigan, 94)   C
C   ESTE PROGRAMA ESTA ADAPTADO PARA LER O ARQUIVO DE DADOS DE        C
C   EXPERIMENTO DE DONA FRANCISCA (1999), COM 15 COLUNAS, E APOS A    C
C   SUPRESSAO DAS VIRGULAS ENTRE AS COLUNAS - DADOS TRATADOS -        C
C								                                    C
C   O PROGRAMA CALCULA VALORES MEDIOS, VARIANCAS, FLUXOS, ESPECTROS   C
C   E COESPECTROS PARA UMA SERIE TEMPORAL DE N DADOS. NN E O TAMANHO  C
C   TOTAL DO ARQUIVO A SER LIDO INICIALMENTE. O PROGRAMA CALCULA      C
C   SUCESSIVAMENTE E AVANCA EM STEPS DE KK. K  E A DIMENSAO DAS       C
C   MATRIZES DE ESPECTRO. OBSERVE QUE K DEVE SER SETADO COMO N/2      C
C   (Stull, 88). M � O N�MERO DE BLOKS EM QUE OS ESPECTROS S�O        C
C   SUAVISADOS.                                                       C
C								                                    C
C   ESTA VERSAO NAO ESTA EFETUANDO A MEDIA MOVEL E TAMBEM N�O EST�    C
C   CALCULANDO A FUN��O DE CORRELA��O. A M�DIA M�VEL PODE SER         C
C   INCORPORADO, POIS EST� FEITA EM OUTRO PROGRAMA.                   C
C                                                                     C
C   TAMBEM PODE SER INCORPORADO A ESTE PROGRAMA A TECNICA DE CALCULAR C
C   OS FLUXOS, etc. EM SUBINTERVALOS DE DIVIDIDOS POR 2               C
C   (Howell & Sun, 98; Moraes et al, 02).                             C
C                                                                     C
C                                          Bras�lia, Fevereiro 2005   C
C                                                                     C
C								      C
C   AP�S AS MODIFICA��ES EFETUADAS EM FEV.2005 O PROGRAMA FOI         C
C   ADPTADO PARA SER USADO PARA A TORRE DA PASTAGEM DO LBA            C
C								      C
C                                          Santa Maria, Dezembro 2005 C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC     
C
C
C N   = TAMANHO DA SERIE TEMPORAL EM CADA LOOPING (1/2 HORA,POR EXEMPLO)
C NN  =
C MNN = TAMANHO DO ARQUIVO DE ENTRADA (1 DIA, POR EXEMPLO) 
C KK  = AVANCO DO LOOPING DE ANALISE
C K   = ENTRADA PARA A ANALISE DE FOURIER (SEMPRE IGUAL A N/2)
C M   = NUMERO DE BANDAS DO ESPECTRO SUAVIZADO
C MNN = NUMERO DE PONTOS DO ESPECTRO A SEREM LOG-FITADOS PARA DISSIPA
c 2**16 = 65536 = 109 min p/ 10Hz
c 2**15 = 32768 = 54 min p/ 10Hz
c 2**14 = 16384 = 27 min p/ 10Hz
c 2**13 = 8192  = 13 min p/ 10Hz


c	PARAMETER(N=32768,NN=846000,KK=3600,K=16384,M=44,MNN=12)
	PARAMETER(N=2**16,NN=870000,KK=1800,K=N/2,M=45,MNN=12)

c! variaveis p o FDR
	PARAMETER(TR=350.0) !Escala de tempo do FDR
	REAL FDU(N),FDV(N),FDW(N),FDT(N),FDQ(N),FDC(N)
	PARAMETER(AD=0.35*9.) !AD utilizada para adimensionalizacao dos
	! espectro, onde 0.35 (Von K�ram�n cte) e 9=z (Altura do sensor)	
		
	
	REAL INPUT(NN,22)
	real NINPUT(N,6),NNINPUT(N,6),NNNINPUT(N,6)
C AS MATRIZES ACIMA POSSUEM DIMENS�O (N,6), ONDE AS 6 COLUNAS REFEREM-SE
C AS 3 VELOCIDADES DO 3D E AS 3 VELOCIDADES DO GILL.
	REAL NTURBU(N),NTURBV(N),NTURBW(N)
	REAL TURBU(N),TURBV(N),TURBW(N)
	REAL TURBT(N),TURBQ(N),TURBC(N)
C
      REAL INPUT1,INPUT2,INPUT3,INPUT4,INPUT5,INPUT6
C AS VARIAVEIS ACIMA S�O USADAS NAS ROTA��ES DO 3D E DO GILL
c
	REAL NEWTURBW(N),NEWTURBT(N),NEWTURBU(N),NEWTURBV(N)
	REAL NEWTURBQ(N),NEWTURBC(N)
	REAL NNEWTURBW(N),NNEWTURBT(N),NNEWTURBU(N),NNEWTURBV(N)
	REAL NNEWTURBQ(N),NNEWTURBC(N)
	REAL WINTURBW(N),WINTURBT(N),WINTURBU(N),WINTURBV(N)
	REAL WINTURBQ(N),WINTURBC(N)
      COMPLEX FFT1(N),FFT2(N),FFT3(N),FFT4(N),FFT5(N),FFT6(N)
 	REAL SPCW(K+1),SPCT(K+1),SPCU(K+1),SPCV(K+1)
	REAL SPCQ(K+1),SPCC(K+1)
C FFT1 e FFT2 ASSOCIADOS COM SPCU e SPCV
C FFT3 e FFT4 ASSOCIADOS COM SPCW e SPCT
C FFT5 e FFT6 ASSOCIADOS COM SPCQ e SPCC
 	REAL SSPCW(K),SSPCT(K),SSPCU(K),SSPCV(K)
	REAL SSPCQ(K),SSPCC(K)
C  FREQUENCIAS PARA ESPECTROS E CO-ESPECTROS
	REAL HZWS(M),HZTS(M),HZUS(M),HZVS(M)
	REAL HZQS(M),HZCS(M)
	REAL HZwtC(M),HZwqC(M),HZwcC(M) 
C  ESPECTROS E CO-ESPECTROS SUAVISADOS
	REAL WSMOOTH(M),TSMOOTH(M),USMOOTH(M),VSMOOTH(M)
	REAL QSMOOTH(M),CSMOOTH(M)
      REAL WTSMOOTH(M),WCSMOOTH(M),WQSMOOTH(M)
C ESPECTROS DE QUADRATURA SUAVISADOS - ENTENDER !!!!
	REAL COT(N),QOT(N),COQ(N),QOQ(N)
	REAL CCOT(K),QQOT(K),CCOQ(K),QQOQ(K)
	REAL CCOC(K),COC(N),QQOC(K),QOC(N)
      REAL HERTZ(K+1),HHERTZ(K)
      REAL EPSU(MNN),EPSV(MNN),EPSW(MNN),EPSHZ(MNN)
	REAL DISSIPW,DISSIPV,DISSIPU
	REAL DISMEANW,PHIEPSW23
	REAL DISMEANU,PHIEPSU23
	REAL DISMEANV,PHIEPSV23
	REAL PHIEPSW(M),DISSIPW2(M)
	REAL PHIEPSU(M),DISSIPU2(M)
	REAL PHIEPSV(M),DISSIPV2(M)
	REAL ADISW,ADISV,ADISU
	REAL FREQ 
C
	REAL CORU,DPU,VARU
	REAL CORV,DPV,VARV
	REAL CORW,DPW,VARW
	REAL CORT,DPT,VART
	REAL CORQ,DPQ,VARQ
	REAL CORC,DPC,VARC
C
	REAL SOMA(6),MEDIO(6)
	REAL NSOMA(6),NMEDIO(6)
	REAL NNSOMA(6),NNMEDIO(6)
	REAL NNNSOMA(6),NNNMEDIO(6)
C AS DIMENSOES DAS MATRIZES ACIMA COMTEMPLAM AS 6 VARIAVEIS ROTADAS
C
C WIND SPEED AND DIRECTION PARA O SONICO E GILL
C 2D SIGNIFICA VENTO NO PLANO HORIZONTAL
C 3D SIGNIFICA VENTO NO ESPA�O 3D
	REAL WIND2D,NWIND2D,NNWIND2D,NNNWIND2D
	REAL WIND3D
C
	REAL AT1,AT2,AT3
C
C VARI�VEIS ASSOCIADAS COM O DETREND
C
	REAL SNEWTU,SNEWTV,SNEWTW,SNEWTT
	REAL SNEWTQ,SNEWTC
        REAL MNEWTU,MNEWTV,MNEWTW,MNEWTT
	REAL MNEWTQ,MNEWTC
C
	REAL USTAR,USTAR1,USTAR2
	REAL ECT,ECT1,ECT2,ECT3
	REAL WQ,LAT
	REAL WT,SEN
	REAL MOL,TSTAR,QSTAR,CSTAR
	REAL WU,WV
C
C VARIAVEIS DEFINIDAS PARA TESTAR SE AS COISAS ESTAO FUNCIONANDO
C VER STULL, PAG
C
	REAL WTW,WTFFT
C
C PARAMETRO US=UNIDADE DE SAIDA, FEITO PELO HANS, PARA EVITAR A
C TRIPA DE WRITE NOS ESPECTROS
C
      INTEGER US
C
C AGORA VAMOS DEFINIR QUANTOS SAO OS ARQUIVOS EM QUE OS ESPECTROS
C SERAO ESCRITOS
C
      CHARACTER*8 ARQUIVO(24)
C
C
      COMMON /KOEFF/FREQ
C
CHAVES PARA ESCREVER E CALCULAR ESPECTROS
C
      ESCESP=1
	CALESP=1
C     
      FREQ=10.
      DATA Z3D/9./
C
C AQUI VAMOS DEFINIR OS NOMES DOS ARQUIVOS DE SAIDA PARA OS ESPECTROS
C
      DATA ARQUIVO/"SWS1.D16","SWS2.D16","SWS3.D16","SWS4.D16",
     &             "SWS5.D16","SWS6.D16","SCS1.D16","SCS2.D16",
     &             "SCS3.D16","SCS4.D16","SCS5.D16","SCS6.D16",
     &             "SWC1.D16","SWC2.D16","SWC3.D16","SWC4.D16",
     &             "SWC5.D16","SWC6.D16","SCC1.D16","SCC2.D16",
     &             "SCC3.D16","SCC4.D16","SCC5.D16","SCC6.D16"/
C
CLAREANDO AS NOMENCLATURAS PARA O NOME DOS ARQUIVOS
C
C    SXYN.DAT
C 
C  ONDE S=SPECTRO
C       X= (W ou C) WINDY OR CALM (DEFINIDO PELO LIMITE DO VENTO MEDIO
C       Y= (S ou C (STABLE OU CONVECTIVO)
C       N=1,2,3,4,5 OU 6 (FAIXAS DE INTERVALO DE z/L)
C       ONDE 1= 0<z/L<0.1
c            2= 0.1<z/L<0.3
c            3= 0.3<z/L<0.5
c            4= 0.5<z/L<0.8
c            5= 0.8<z/L<1.1
c 	           6= 1.1<z/L 
C
	OPEN(UNIT=13,FILE='STATIS.D16')
C
	OPEN(UNIT=15,FILE='GERAL.D16',STATUS='UNKNOWN')
	OPEN(UNIT=80,FILE='SW3D.D16',status='unknown')
C
COMECANDO A LER O ARQUIVO DE ENTRADA
C
C A ORDEM DOS DADOS DE ENTRADA E COMO SEGUE:
C
C CODIGO,ANO, DJ,HHMM,SEGUNDO, V,U,W,T,W1D,T1D,KRYPRON,WG,UG,VG
C
      OPEN(UNIT=11,FILE='INPUT.DAT',STATUS='UNKNOWN')
      DO 10 I=1,NN
      READ (11,*,END=10)(INPUT(I,J),J=1,22)
10    CONTINUE 
C
      PRINT*,'LEU O ARQUIVO'
C
C ARQUIVOS PARA OS ESPECTROS SUAVISADOS
C
!       J=1
! 	DO I = 24,47
! 	OPEN(UNIT=I,FILE=ARQUIVO(J))
! 	J=J+ 1
! 	END DO
C
C
      KTURB=0
C
6688  CONTINUE
C
CALCULANDO AS COISAS ASSOCIADAS COM OS DADOS FAST
C	
	DO I=1,N
	TURBU(I)=0.
	TURBV(I)=0.
	TURBW(I)=0.
	TURBT(I)=0.
	TURBQ(I)=0.
	TURBC(I)=0.
	NEWTURBU(I)=0.
	NEWTURBV(I)=0.
	NEWTURBW(I)=0.
	NEWTURBT(I)=0.
	NEWTURBQ(I)=0.
	NEWTURBC(I)=0.
      END DO
C
	DO 2020 J=1,6
	MEDIO(J)=0.
	SOMA(J)=0.
2020  CONTINUE
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c
CALCULA MEDIAS E COMPONENTES TURBULENTAS DOS DADOS FAST
C
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	DO 5 J=1,6
	DO 5 I=1,N
	SOMA(J)=SOMA(J)+INPUT(I+KTURB,J+4)
5     CONTINUE 
	DO 3030 J=1,6
	MEDIO(J)=SOMA(J)/N
3030  CONTINUE
C+++++++++++++fim osv
c+++++++++++++begin hans
C LIMPANDO OS VETORES DO FDR
        do 6010 I=1,N
          TURBT(I)=0.
 6010   continue

!  C CHAMADA DE FDR PARA TEMPERATURA
!       CALL FDR(FDT,N,TR) 

! FLUT CALCULADA AP�S FDR
!       DO I=1,N
!          TURBT(I)=INPUT(I+KTURB,8)-FDT(I)
!       END DO
!
!++++++++++++ fim hans
!+++++++++++++ inicio osvaldo
! ! 
       DO I=1,N
 	TURBT(I)=INPUT(I+KTURB,8)-MEDIO(4)
 	TURBQ(I)=INPUT(I+KTURB,10)-MEDIO(6)
 	TURBC(I)=INPUT(I+KTURB,9)-MEDIO(5)
 	END DO
C
      T0=273+MEDIO(4)
C
C      PRINT*,INPUT(I+KTURB,3),INPUT(I+KTURB,4),T0,MEDIO(6),MEDIO(5)
C
CALCULA THE WIND SPEED
C
	WIND2D=SQRT(MEDIO(1)**2+MEDIO(2)**2)
	write(15,*)'KTURB=',kturb
	write(15,*)
	write(15,*)'Medias dos dados de entrada' 
	write(15,*)medio(1),medio(2),medio(3),medio(4),medio(5),MEDIO(6)
C
	write(15,*)
	WRITE(15,*)'RAPIDEZ DO VENTO HORIZONTAL =',WIND2D
C
c
!         PRINT*,KTURB,INPUT(KTURB,3),INPUT(KTURB,4),T0,
!      &         MEDIO(6),MEDIO(5),WIND2D
CALCULA A DIRECAO DO VENTO horizontal para padrao meteorologico
C
        AT=ATAN(MEDIO(1)/MEDIO(2))
	  D=AT*180./(4.*ATAN(1.))
        DA=ABS(D)
C
        ATT=ATAN(WIND2D/MEDIO(3))
C
CALCULA A DIRECAO DO VENTO HORIZONTAL NO SONICO
C
	IF(MEDIO(2).GT.0.AND.MEDIO(1).GT.0) THEN
	 DR=180.+DA
	ENDIF
C
	IF(MEDIO(2).GT.0.AND.MEDIO(1).LT.0) THEN
	 DR=180.-DA
	ENDIF
C
	IF(MEDIO(2).LT.0.AND.MEDIO(1).LT.0) THEN
	 DR=DA
	ENDIF
C
	IF(MEDIO(2).LT.0.AND.MEDIO(1).GT.0) THEN
	 DR=360.-DA
	ENDIF
C
C
C ESCREVE AS DIRECOES DO VENTO HORIZONTAL 
C
	RDR=DR*(4.*ATAN(1.))/180.
	WRITE(15,*)'DIRECAO DO VENTO HOZ. NO SONICO (RAD) =',RDR
	WRITE(15,*)'DIRECAO DO VENTO HOZ. NO SONICO (GRA) =',DR
	write(15,*)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C AS COMPONENTES u E v SERAO ROTADAS DE MODO A COINCIDIR u COM
C A DIRECAO DO VENTO MEDIO E v COM A DIRECAO TRANSVERSAL
C VER KAIMAL E FINNIGAN (1994)
C
CALCULA O PRIMEIRO ANGULO
C
      AT1=ATAN(MEDIO(2)/MEDIO(1))
C
!	print*,'primeira rota��o'
C EFETUA A PRIMeIRA ROTACAO 
C
C NO SONICO
        DO I=1,N
	INPUT1=INPUT(I+KTURB,5)*cos(AT1)+INPUT(I+KTURB,6)*sin(AT1)
	INPUT2=INPUT(I+KTURB,6)*COS(AT1)-INPUT(I+KTURB,5)*SIN(AT1)
	NINPUT(I,1)=INPUT1
	NINPUT(I,2)=INPUT2
	NINPUT(I,3)=INPUT(I+KTURB,7)
        end do
c
C RECALCULA O  NOVO VALOR MEDIO DE u E v
C
	DO  J=1,3
	NMEDIO(J)=0.
	NSOMA(J)=0.
      ENDDO

	DO 106 J=1,3
	DO 106 I=1,N
	NSOMA(J)=NSOMA(J)+NINPUT(I,J)
106   CONTINUE
C
	DO 4041 J=1,3
	NMEDIO(J)=NSOMA(J)/N
4041  CONTINUE
C
	WRITE(15,*)
	WRITE(15,*)'VELOCIDADES MEDIAS APOS A ROTACAO 1'
	WRITE(15,812)
812     FORMAT(3X,'<U3D>',5X,'<V3D>',5X,'<W3D>')
        WRITE(15,2873)NMEDIO(1),NMEDIO(2),NMEDIO(3)
2873     FORMAT(1X,F5.2,3X,F5.2,3X,F5.2)
C
C RECALCULA THE WIND SPEED NO SONICO
C
      NWIND2D=SQRT(NMEDIO(1)**2+NMEDIO(2)**2)
	write(15,*)
	WRITE(15,*)'RAPIDEZ DO VENTO HORIZONTAL EM 9M (m/s) =',NWIND2D
C
CALCULA O SEGUNDO ANGULO 
C
      AT2=ATAN(NMEDIO(3)/NMEDIO(1))
C
	WRITE(15,*)'ANGULO DA SEGUNDA ROTACAO (RAD) SON. =',AT2
C
!	print*,'segunda rota��o'
C EFETUA A SEGUNDA ROTACAO 
C
C  NO SONICO
        DO I=1,N
        INPUT3=NINPUT(I,1)*cos(AT2)+NINPUT(I,3)*sin(AT2)
        INPUT4=NINPUT(I,3)*COS(AT2)-NINPUT(I,1)*SIN(AT2)
        NNINPUT(I,1)=INPUT3
	  NNINPUT(I,2)=NINPUT(I,2)  
        NNINPUT(I,3)=INPUT4
        END DO
C
C RECALCULA OS  NOVOS VALORES MEDIO DE u,v E w
C
	DO  J=1,3
	NNMEDIO(J)=0.
	NNSOMA(J)=0.
      ENDDO
C
      DO 166 J=1,3
	DO 166 I=1,N
	NNSOMA(J)=NNSOMA(J)+NNINPUT(I,J)
166   CONTINUE
C
	NNMEDIO(1)=NNSOMA(1)/N
	NNMEDIO(2)=NNSOMA(2)/N
	NNMEDIO(3)=NNSOMA(3)/N
C
C RECALCULA THE WIND SPEED NO SONICO
      NNWIND2D=SQRT(NNMEDIO(1)**2+NNMEDIO(2)**2)
C
	write(15,*)
	WRITE(15,*)'RAPIDEZ DO VENTO HORIZONTAL EM 9M (m/s) =',NNWIND2D
C
	WRITE(15,*)
	WRITE(15,*)'VELOCIDADES MEDIAS APOS A ROTACAO 2'
	WRITE(15,8992)
8992     FORMAT(3X,'<U3D>',5X,'<V3D>',5X,'<W3D>')
        WRITE(15,28113)NNMEDIO(1),NNMEDIO(2),NNMEDIO(3)
28113     FORMAT(1X,F5.2,3X,F5.2,3X,F5.2)
C
CALCULA AS NOVAS COMPONENTES TURBULENTAS
C
	DO I=1,N
	NTURBU(I)=NNINPUT(I,1)-NNMEDIO(1)
	NTURBV(I)=NNINPUT(I,2)-NNMEDIO(2)
	NTURBW(I)=NNINPUT(I,3)-NNMEDIO(3)
      END DO
C
CALCULA OS PARAMETROS DE MODO A DETERMINAR O TERCEIRO ANGULO
C
      FLU1=0.
	V32=0.
	W32=0.
	DO I=1,N
	   FLU1=FLU1+(NTURBV(I)*NTURBW(I))
	   V32=V32+(NTURBV(I)*NTURBV(I))
	   W32=W32+(NTURBW(I)*NTURBW(I))
	END DO
C
	FLU1=FLU1/N
	V32=V32/N
	W32=W32/N
CALCULA O TERCEIRO ANGULO
C
      AT3=0.5*ATAN(2*FLU1/(V32-W32))
C
C	WRITE(15,*)'ANGULO DA TERCEIRA ROTACAO NO SON. (RAD) =',AT3
C	WRITE(15,*)'ANGULO DA TERCEIRA ROTACAO NO GILL (RAD) =',AT3G
C
C EFETUA A ROTACAO FINAL NO SONICO
C
        DO I=1,N
	INPUT5=NNINPUT(I,2)*cos(AT3)+NNINPUT(I,3)*sin(AT3)
	INPUT6=NNINPUT(I,3)*COS(AT3)-NNINPUT(I,2)*SIN(AT3)
	NNNINPUT(I,1)=NNINPUT(I,1)
	NNNINPUT(I,2)=INPUT5  
	NNNINPUT(I,3)=INPUT6
        END DO
C
C RECALCULA OS  NOVOS VALORES MEDIO DE u,v E w
C
	DO  J=1,3
	NNNMEDIO(J)=0.
	NNNSOMA(J)=0.
	ENDDO
C
	DO 186 J=1,3
	DO 186 I=1,N
	NNNSOMA(J)=NNNSOMA(J)+NNNINPUT(I,J)
186     CONTINUE
C
    	DO J=1,3
	    NNNMEDIO(J)=NNNSOMA(J)/N
        END DO
C
	WRITE(15,*)
	WRITE(15,*)'VELOCIDADES MEDIAS APOS A ROTACAO 3'
	WRITE(15,8792)
8792     FORMAT(3X,'<U3D>',5X,'<V3D>',5X,'<W3D>')
        WRITE(15,28114)NNNMEDIO(1),NNNMEDIO(2),NNNMEDIO(3)
28114     FORMAT(1X,F5.2,3X,F5.2,3X,F5.2)
C
      NNNWIND2D=SQRT(NNNMEDIO(1)**2+NNNMEDIO(2)**2)
	WIND3D=0.
	WIND3D=((NNNMEDIO(1)**2)+(NNNMEDIO(2)**2)+(NNNMEDIO(3)**2))**.5
	write(15,*)
	WRITE(15,*)'RAPIDEZ DO VENTO HORIZONTAL EM 9M (m/s) =',NNNWIND2D
	write(15,*)'WIND SPEED =',WIND3D
	write(15,*)
	write(15,*)'########################################'
	write(15,*)
C
C
C CALCULA THE WIND SPEED
c      WIN3D=0.
c	WIND3D=SQRT((NNNMEDIO(1)**2)+(NNNMEDIO(2)**2)+(NNNMEDIO(3)**2))
C
CALCULA AS COMPONENTES TURBULENTAS APOS A ROTACAOA 3D
c
ccccccccccccccccccccccccc inicio hans
C CALCULA AS COMPONENTES TURBULENTAS apos remocao linear de tendencia
C
!       DO 6023 I=1,N
!          TURBU(I)=0.;TURBV(I)=0.;TURBW(I)=0.;TURBT(I)=0.
!          FDU(I)=0.;FDV(I)=0.;FDW(I)=0.;FDT(I)=0.
! 	   FDQ(N)=0.;FDC(N)=0.
! 6023  continue
! 	do I=1,n
! 	FDU(I)=NNNINPUT(I,1)     
! 	FDV(I)=NNNINPUT(I,2)      
! 	FDW(I)=NNNINPUT(I,3)       
! 	FDT(I)=INPUT(I+KTURB,8)
! 	FDC(I)=INPUT(I+KTURB,9)
! 	FDQ(I)=INPUT(I+KTURB,10)
!        
!         enddo
! !        	print*,'calling FDR'
! 	CALL FDR(FDU,N,TR)   
! 	CALL FDR(FDV,N,TR)   
! 	CALL FDR(FDW,N,TR)   
! 	CALL FDR(FDT,N,TR)   
! 	CALL FDR(FDC,N,TR)
! 	CALL FDR(FDQ,N,TR)
! C OBS: As flutua��es com FDR � a sinal do filtro (low-pass) 
! !subtraido da serie (NESTE CASO J� ROTADA PARA U,V E W [NNINPUT(I,J)]).
! 	DO 66010 I=1,N
! 	TURBU(I)=NNNINPUT(I,1)-FDU(I)
! 	TURBV(I)=NNNINPUT(I,2)-FDV(I)
! 	TURBW(I)=NNNINPUT(I,3)-FDW(I)
! 	TURBT(I)=INPUT(i+KTURB,8)-FDT(I)
! 	TURBC(I)=INPUT(I+KTURB,9)-FDC(I)
! 	TURBQ(I)=INPUT(I+KTURB,10)-FDQ(I)
! 
! c o codigo abaixo salva em arquivo os dados originais e ap�s o filtro
! !  612    format(13(F15.8,1x))
! !         WRITE(12,612)INPUT(KTURB+i,3),INPUT(KTURB+i,4),INPUT(KTURB+i,5),
! !      &   INPUT(KTURB+i,6),INPUT(KTURB+i,7),INPUT(KTURB+i,11),
! !      &   INPUT(KTURB+i,12),INPUT(KTURB+i,13),INPUT(KTURB+i,14),
! !      &   FDU(I),FDV(I),FDW(I),FDT(I)
! 	 
! 	 
! 	 
! 66010 CONTINUE 
!        WRITE(*,*)'>>> FDR_DONE !!!' 

cccccccccccccccccccccccccccccc fim hans
c recome�o osvaldo
 	DO I=1,N
 	TURBU(I)=NNNINPUT(I,1)-NNNMEDIO(1)
 	TURBV(I)=NNNINPUT(I,2)-NNNMEDIO(2)
 	TURBW(I)=NNNINPUT(I,3)-NNNMEDIO(3)
       ENDDO
c
CALCULA OS NOVOS DESVIO PADRAO E A VARIANCA
	CORU=0.
	CORV=0.
	CORW=0.
	CORT=0.
	CORQ=0.
	CORC=0.
C
	DO 200 I=1,N
	   CORU=CORU+(TURBU(I)**2)
	   CORV=CORV+(TURBV(I)**2)
	   CORW=CORW+(TURBW(I)**2)
	   CORT=CORT+(TURBT(I)**2)
	   CORQ=CORQ+(TURBQ(I)**2)
	   CORC=CORC+(TURBC(I)**2)
200   CONTINUE
C
	VARU=CORU/N
	DPU=SQRT(VARU)
	VARV=CORV/N
	DPV=SQRT(VARV)
	VARW=CORW/N
 	DPW=SQRT(VARW)
	VART=CORT/N
	DPT=SQRT(VART)
	VARQ=CORQ/N
	DPQ=SQRT(VARQ)
	VARC=CORC/N
	DPC=SQRT(VARC)
C
C      WRITE(15,*)
C	WRITE(15,*)'DESVIOS PADROES SEM SPIKES E ROTA�AO DE u E v'
C	WRITE(15,*)DPU,DPV,DPW,DPT,DPQ,DPC
C
CALCULA PARAMETROS MICROMETEOROLOGICOS
C 
CALCULA USTAR FLUXOS DE MOMENTUM
        USTAR1=0.
        USTAR2=0.
	  DO 4510 I=1,N
	   USTAR1=USTAR1+(TURBU(I)*TURBW(I))
	   USTAR2=USTAR2+(TURBV(I)*TURBW(I))
4510    CONTINUE
	  WU=(USTAR1)/N
	  WV=(USTAR2)/N    
        USTAR1=(USTAR1/N)**2
        USTAR2=(USTAR2/N)**2
        USTAR=USTAR1+USTAR2
	  USTAR=SQRT(SQRT(USTAR))
C
CALCULA ENERGIA CINETICA TURBULENTA
C
        ECT1=0.
        ECT2=0.
        ECT3=0.
        DO 4515 I=1,N
	   ECT1=ECT1+(TURBU(I)**2)
	   ECT2=ECT2+(TURBV(I)**2)
	   ECT3=ECT3+(TURBW(I)**2)
4515    CONTINUE
	   ECT1=ECT1/N
	   ECT2=ECT2/N
	   ECT3=ECT3/N
	   ECT=0.5*(ECT1+ECT2+ECT3)
C
CALCULA O FLUXO DE CALOR SENSIVEL
C
	   RHO=1.21; CP=1005.; WT=0.
        DO 4525 I=1,N
	   WT=WT+(TURBW(I)*TURBT(I))
4525    CONTINUE
	   WT=WT/N
	   SEN=RHO*CP*WT
CALCULA O FLUXO DE CALOR LATENTE
C          
        WQ=0.; LV=2262.6
        DO 4526 I=1,N
	   WQ=WQ+(TURBW(I)*TURBQ(I))
4526    CONTINUE
        WQ=WQ/N
        LAT=LV*WQ
C
CALCULA O FLUXO DE CARBONO
C        
        WC=0.  
        DO I=1,N
	   WC=WC+(TURBW(I)*TURBC(I))
        ENDDO
C
CALCULA O COMPRIMENTO DE OBUKHOV
C     
	   MOL=-((USTAR**3)*T0)/(0.35*9.81*WT)
C
CALCULA TSTAR, QSTAR E CSTAR
C
         TSTAR=WT/USTAR
         QSTAR=WQ/USTAR
	   CSTAR=WC/USTAR 
C
Ccccccccccccccccccccccccccccccccccccccccccc
C
c
!      print*,'calculando os espectros'
      IF(CALESP.EQ.1)THEN
C
COMECA OS CALCULOS ESPECTRAIS AQUI,
COM A APLICACAO DA JANELA AOS NOVOS VALORES TURBULENTOS.
C
	CALL SEMTREND(TURBU,NEWTURBU,N)
	CALL SEMTREND(TURBV,NEWTURBV,N)
	CALL SEMTREND(TURBW,NEWTURBW,N)
	CALL SEMTREND(TURBT,NEWTURBT,N)
	CALL SEMTREND(TURBQ,NEWTURBQ,N)
	CALL SEMTREND(TURBC,NEWTURBC,N)
cccccccccccccccccccccccccccccccccccccccc
    	SNEWTU=0.
	SNEWTV=0.
     	SNEWTW=0.
    	SNEWTT=0.
     	SNEWTQ=0.
	SNEWTC=0.
C
	DO 1836 I=1,N
	SNEWTU=SNEWTU+NEWTURBU(I)
	SNEWTV=SNEWTV+NEWTURBV(I)
	SNEWTW=SNEWTW+NEWTURBW(I)
	SNEWTT=SNEWTT+NEWTURBT(I)
	SNEWTQ=SNEWTQ+NEWTURBQ(I)
	SNEWTC=SNEWTC+NEWTURBC(I)
1836   CONTINUE
C
	MNEWTU=SNEWTU/N
	MNEWTV=SNEWTV/N
	MNEWTW=SNEWTW/N
	MNEWTT=SNEWTT/N
	MNEWTQ=SNEWTQ/N
	MNEWTC=SNEWTC/N
C
CALCULA AS COMPONENTES TURBULENTAS APOS O DETREND
C
	DO 2017 I=1,N
	NNEWTURBU(I)=NEWTURBU(I)-MNEWTU
	NNEWTURBV(I)=NEWTURBV(I)-MNEWTV
	NNEWTURBW(I)=NEWTURBW(I)-MNEWTW
	NNEWTURBT(I)=NEWTURBT(I)-MNEWTT
	NNEWTURBQ(I)=NEWTURBQ(I)-MNEWTQ
	NNEWTURBC(I)=NEWTURBC(I)-MNEWTC
2017  CONTINUE
ccccccccccccccccccccccccccccccccccccccccc
c
	CALL WINDOW(NNEWTURBU,WINTURBU,N)
	CALL WINDOW(NNEWTURBV,WINTURBV,N)
	CALL WINDOW(NNEWTURBW,WINTURBW,N)
	CALL WINDOW(NNEWTURBT,WINTURBT,N)
	CALL WINDOW(NNEWTURBQ,WINTURBQ,N)
	CALL WINDOW(NNEWTURBC,WINTURBC,N)
C
c
CCHAMA A SUBROTINA PARA CALCULAR AS FFT
CONFORME NUMERICAL RECIPES PAGS 397-398
C     
CONSIDERE QUE AS FUNCOES QUE RETORNAM DA SUBROTINA TWOFFT
C NAO SAO NORMALIZADAS, ISTO E, NAO ESTAO DIVIDIDAS POR N
C  
c
!	print*,'calling TWOFFT'
 	CALL TWOFFT(WINTURBV,WINTURBU,FFT1,FFT2,N)
  	CALL TWOFFT(WINTURBW,WINTURBT,FFT3,FFT4,N)
  	CALL TWOFFT(WINTURBQ,WINTURBC,FFT5,FFT6,N)
!	print*,'OUT FROM TWOFFT'
C
      WTFFT=0.
      do 2927 i=1,n
	WTFFT=WTFFT+(REAL(FFT3(I))*REAL(FFT4(I))+
     &         (AIMAG(FFT3(I))*AIMAG(FFT4(I)))) 
C
2927  continue
c
        WTW=0.
        DO 4025 I=1,N
        WTW=WTW+(WINTURBW(I)*WINTURBT(I))
4025    CONTINUE
        WTW=WTW/N
C
CALCULO, PELA SUBROTINA SPECDIS, DA DISCRETE ENERGY SPECTRUM
CONFORME STULL PG 312
C
C
! Montagem da CADAeia de frequencias n
        DO 8084 I=1,K+1
         HERTZ(I)=(I)*FREQ/N
!	   if (i.eq.k) print*,'n(',i,') ->',hertz(i)
8084    CONTINUE
C

C
!	print*,'entrando em SPECDIS'
        CALL SPECDIS(FFT1,SPCV,N,K)
        CALL SPECDIS(FFT2,SPCU,N,K)
        CALL SPECDIS(FFT3,SPCW,N,K)
        CALL SPECDIS(FFT4,SPCT,N,K)
        CALL SPECDIS(FFT5,SPCQ,N,K)
        CALL SPECDIS(FFT6,SPCC,N,K)
!	print*,'saindo da SPECDIS'
c
c
!	print*,'entrando em CROSS'
	CALL CROSS(FFT3,FFT4,COT,QOT,N)
!	print*,'saindo da CROSS COT'
!	CALL CROSS(FFT3,FFT8,COQ,QOQ,N)
!	print*,'saindo da CROSS COQ'
	CALL CROSS(FFT5,FFT6,COC,QOC,N)
!	print*,'saindo da CROSS COC'
C
      DO I=1,K
	SSPCU(I)=SPCU(I+1)
	SSPCV(I)=SPCV(I+1)
	SSPCW(I)=SPCW(I+1)
	SSPCT(I)=SPCT(I+1)
	SSPCQ(I)=SPCQ(I+1)
	SSPCC(I)=SPCC(I+1)
	CCOT(I)=COT(I+1)
	QQOT(I)=QOT(I+1)
	CCOQ(I)=COQ(I+1)
	QQOQ(I)=QOQ(I+1)
	CCOC(I)=COC(I+1)
	QQOC(I)=QOC(I+1)
	HHERTZ(I)=HERTZ(I+1)
      END DO
C
C
CONSIDERE, NO WRITE ACIMA, QUE A DENSIDADE ESPECTRAL NAO E'
C ESCRITA PARA I=1, POIS 
C
CHAMA A SUBROTINA QUE SUAVIZA O ESPECTRO EM M BANDAS
C
c
!	if (kturb.lt.3601) print*,'entrando em BADAS',kturb
	CALL BANDAS(HHERTZ,SSPCW,HZwS,WSMOOTH,K,M)
	CALL BANDAS(HHERTZ,SSPCT,HZtS,TSMOOTH,K,M)
	CALL BANDAS(HHERTZ,SSPCU,HZuS,USMOOTH,K,M)
	CALL BANDAS(HHERTZ,SSPCV,HZvS,VSMOOTH,K,M)
	CALL BANDAS(HHERTZ,SSPCQ,HZqS,QSMOOTH,K,M)
	CALL BANDAS(HHERTZ,SSPCC,HZqS,CSMOOTH,K,M)
	CALL BANDAS(HHERTZ,CCOT,HZwtC,WTSMOOTH,K,M)
	CALL BANDAS(HHERTZ,CCOC,HZwcC,WCSMOOTH,K,M)
	CALL BANDAS(HHERTZ,CCOQ,HZwqC,WQSMOOTH,K,M)
!	if (kturb.lt.3601) print*,'saindo de BANDAS',kturb

c
C
CALCULANDO A TAXA DE DISSIPACAO
COM O FITTING DO ESPECTRO NO SUB-INTERVALO INERCIAL
C
COM O FITTING SENDO FEITO PARA UMA RETA TEMOS QUE CALCULAR
COM O LOG DOS DADOS
C
      DO I=1,MNN
      EPSU(I)=LOG10(USMOOTH(I+29))
      EPSV(I)=LOG10(VSMOOTH(I+29))
      EPSW(I)=LOG10(WSMOOTH(I+29))
      EPSHZ(I)=LOG10(HZWS(I+29))
      ENDDO
C
      CALL FIT(EPSHZ,EPSW,MNN,1,0,AW,BW,SIGAW,SIGBW,CHI2W,QW)
      CALL FIT(EPSHZ,EPSU,MNN,1,0,AU,BU,SIGAU,SIGBU,CHI2U,QU)
      CALL FIT(EPSHZ,EPSV,MNN,1,0,AV,BV,SIGAV,SIGBV,CHI2V,QV)
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
COMECA AQUI O CALCULO DA TAXA DE DISSIPACAO
C
C INICIALMENTE Epsilon e Phi_Epsilon sao
CALCULADOS ATRAVES DO FITING DO ESPACTRO, CONFORME DETERMINADO
C PELA SUBROUTINE FIT
C
	DISSIPW=(11.54/(WIND3D**1.5))*(10**(1.5*AW)) 
	DISSIPV=(11.54/(WIND3D**1.5))*(10**(1.5*AV))
	DISSIPU=(17.77/(WIND3D**1.5))*(18**(1.5*AU))
C
      ADISW=0.35*Z3D*DISSIPW/(USTAR**3)
	ADISV=0.35*Z3D*DISSIPV/(USTAR**3)
	ADISU=0.35*Z3D*DISSIPU/(USTAR**3)
C
CALCULO DE Epsilon e Phi_Epsilon assumindo que a Lei de Kolmogorov
C EXISTE. NOTE-SE QUE O VALOR MEDIO E CALCULADO, LOGO ABAIXO
COM O MESMO NUMERO DE PONTOS QUE E USADO PARA FITAR O ESPECTRO ACIMA
C
        DO I=1,M-1
	DISSIPW2(I)=(WSMOOTH(I)**(1.5))*(HZWS(I)**(2.5))/(.9*WIND3D)
	PHIEPSW(I)=2.5*(HZWS(I)**(5./3))*WSMOOTH(I)*(Z3D**(2./3))/
     &            ((USTAR**2)*(WIND3D**(2/3)))
	DISSIPU2(I)=(USMOOTH(I)**(1.5))*(HZWS(I)**(2.5))/(.9*WIND3D)
	PHIEPSU(I)=2.5*(HZWS(I)**(5./3))*USMOOTH(I)*(Z3D**(2./3))/
     &            ((USTAR**2)*(WIND3D**(2/3)))
	DISSIPV2(I)=(VSMOOTH(I)**(1.5))*(HZWS(I)**(2.5))/(.9*WIND3D)
	PHIEPSV(I)=2.5*(HZWS(I)**(5./3))*VSMOOTH(I)*(Z3D**(2./3))/
     &            ((USTAR**2)*(WIND3D**(2./3)))
	ENDDO
C
       DISMEANW=0.
       DISMEANU=0.
       DISMEANV=0.
       PHIEPSW23=0.
       PHIEPSU23=0
       PHIEPSV23=0.
          DO I=1,12
       DISMEANW=DISMEANW+DISSIPW2(I+29)
       DISMEANU=DISMEANU+DISSIPU2(I+29)
       DISMEANV=DISMEANV+DISSIPV2(I+29)
       PHIEPSW23=PHIEPSW23+PHIEPSW(I+29)
       PHIEPSU23=PHIEPSU23+PHIEPSU(I+29)
       PHIEPSV23=PHIEPSV23+PHIEPSV(I+29)
          ENDDO
       DISMEANW=DISMEANW/MNN
       DISMEANU=DISMEANU/MNN
       DISMEANV=DISMEANV/MNN
       PHIEPSW23=PHIEPSW23/MNN
       PHIEPSU23=PHIEPSU23/MNN
       PHIEPSV23=PHIEPSV23/MNN
C
C A COMPARACAO ENTRE OS Epsilons e Phi-Epsilon deve ser da seguinte forma
c
C   DISSIPW = DISMEAN W (etc) e ADISW**(2/3) = PHIEPSW23
C
!
COMECANDO A ESCREVER OS ESPECTROS SUAVISADOS
C
! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
	US=0
c		 print*,"chamndo class US,WIND3D,Z3D,MOL",US,WIND3D,Z3D,MOL
      CALL CLASSESPC(US,WIND3D,Z3D,MOL)
	OPEN(UNIT=US,FILE=ARQUIVO(US-23))
c	print*,"retornou US,WIND3D,Z3D,MOL",US,WIND3D,Z3D,MOL
	
	DO I=1,M-1
!	   print*,'BANDA: ',i,'U = ',wind3d,'  HZwS(',i,') = ',HZwS(i)
!	   write(80,*)'U = ',wind3d,'HZxS(',i,')= ',HZwS(i),((HZwS(I)*Z3D)/WIND3D)
	WRITE(US,3781)INPUT(KTURB+1,3),INPUT(KTURB+1,4),
     &  DR,Z3D/MOL,Z3D,USTAR,
     &  WT,WC,WQ,WIND3D,
     &  HZwS(I),USMOOTH(I),VSMOOTH(I),WSMOOTH(I),TSMOOTH(I),
     &  QSMOOTH(I),CSMOOTH(I),
     &  AU,BU,AV,BV,AW,BW,
     &  DISSIPW,DISMEANW,DISSIPU,DISMEANU,DISSIPV,DISMEANV,
     &  WTSMOOTH(I),WCSMOOTH(I),WQSMOOTH(I),
     &  ((HZwS(I)*Z3D)/WIND3D),
     &  ((HZwS(I)*USMOOTH(I))/(AD*DISMEANU)**(2./3)),
     &  ((HZwS(I)*VSMOOTH(I))/(AD*DISMEANV)**(2./3)),
     &  ((HZwS(I)*WSMOOTH(I))/(AD*DISMEANW)**(2./3))
C
3781    FORMAT(F5.0,1X,F5.0,
     &  1X,F5.0,1X,F5.2,1X,F3.0,1X,F5.2,
     &  1X,F12.5,1X,F12.6,1X,F12.5,1X,F5.2,
     &  1X,F8.6,1X,F15.7,1X,F15.7,1X,F15.7,1X,F15.7,
     &  1X,F15.7,1X,F15.7,
     &  1X,F5.2,1X,F5.2,1X,F5.2,1X,F5.2,1X,F5.2,1X,F5.2,
     &  1X,F10.7,1X,F10.7,1X,F10.7,1X,F10.7,1X,F10.7,1X,F10.7,
     &  1X,F15.9,1X,F15.9,1X,F15.9,
     &  1X,F10.6,
     &  1X,F16.9,
     &  1X,F16.9,
     &  1X,F16.9)

	END DO
C
	   WRITE(13,815)INPUT(KTURB+1,3),INPUT(KTURB+1,4),
     &   Z3D/MOL,USTAR,TSTAR,CSTAR,QSTAR,
     &   ECT,WU,WV,WT,WQ,WC,
     &   T0,MEDIO(5),MEDIO(6),
     &   WIND2D,DR,
     &   DPU,DPV,DPW,DPT,DPQ,DPC,
     &   DISSIPW,DISMEANW,DISSIPU,DISMEANU,DISSIPV,DISMEANV           

C
815   FORMAT(1X,F5.0,1X,F5.0,
     &   1X,F11.2,1X,F11.2,1X,F6.2,1X,F12.3,1X,F12.3,
     &   1X,F7.3,1X,F9.6,1X,F9.6,1X,F9.6,1X,F13.5,1X,F13.5,
     &   1X,F10.5,1X,F10.5,1X,F10.5,
     &   1X,F10.5,1X,F10.5,
     &   1X,F10.5,1X,F10.4,1X,F10.5,1X,F10.2,1X,F10.5,1X,F10.2,
     &    1X,F10.7,1X,F10.7,1X,F10.7,1X,F10.7,1X,F10.7,1X,F10.7)
c
c
C
cccccccccccccccccccccccccccccccccccccccccccccccccc
C
CCC SUBROUTINE MINIMIZE: PARA FITAR OS PONTOS DO ESPECTRO,
CCC ISTO E': PARA APROXIMAR POR FUNCOES DO TIPO:
CCC    S(n)= (A * (n/nm))/(1+B*(n/nm)^(5/3))
CCC A SUBROTINA BUSCA A,B e nm QUE MELHOR FITAM OS PONTOS
CCC   
      ELSE
      GO TO 9977
      ENDIF
9977  CONTINUE
c
C
      KTURB=KTURB+KK
    	do i=1,n
    	ninput(i,1)=0.
    	ninput(i,2)=0.
    	ninput(i,3)=0.
    	nninput(i,1)=0.
    	nninput(i,2)=0.
    	nninput(i,3)=0.
    	nnninput(i,1)=0.
    	nnninput(i,2)=0.
    	nnninput(i,3)=0.
       enddo
	IF(KTURB.LT.(NN-N+1))GOTO 6688
C      IF(KTURB.LT.20000)GOTO 6688
6699  CONTINUE 
C
      CLOSE(13)
      CLOSE(15)
	close(80) !arquivo de teste de wind3d e hertz/wind3d
C
      DO I=24,47
	CLOSE(I)
	END DO
C
C
      STOP
     	END
