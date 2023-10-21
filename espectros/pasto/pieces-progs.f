--------------------- from gillfdr350.f ----------------------------
PARAMETER(TR=350.0) !TR Escala de tempo do filtro FDR
        PARAMETER(AD=0.35*29.0) ! 29 = (Z-d) GILL  FLORESTA 
////////////

CALCULA MEDIAS E COMPONENTES TURBULENTAS DOS DADOS FAST
C
        DO 5 J=1,4
        DO 5 I=1,N
        SOMA(J)=SOMA(J)+INPUT(I+KTURB,J+10)
5     CONTINUE
        DO 3030 J=1,4
        MEDIO(J)=SOMA(J)/N
3030  CONTINUE
C TR <-- ESCALA DE TEMPO DO FDR
       
C
C LIMPANDO OS VETORES DO FDR
        do 6010 I=1,N
          FDU(I)=0.;FDV(I)=0.;FDW(I)=0.;FDT(I)=0.
          TURBT(I)=0.
 6010   continue
!  C CHAMADA DE FDR PARA TEMPERATURA
       CALL FDR(FDT,N,TR) 

      DO I=1,N

C FLUT CALCULADA APÓS FDR
         TURBT(I)=INPUT(I+KTURB,14)-FDT(I)
!        TURBT(I)=INPUT(I+KTURB,14)-MEDIO(4)
      END DO
C
      T0=273+MEDIO(4)

C      PRINT*,INPUT(I+KTURB,3),INPUT(I+KTURB,4),T0,MEDIO(6),MEDIO(5)
C
CALCULA THE WIND SPEED
C
///////////////////
CALCULA AS COMPONENTES TURBULENTAS APOS A ROTACAOA 3D
c
c fim osvaldo
ccccccccccccccccccccccccc inicio hans
C CALCULA AS COMPONENTES TURBULENTAS apos remocao linear de tendencia
C
      DO 6023 I=1,N
         TURBU(I)=0.;TURBV(I)=0.;TURBW(I)=0.;TURBT(I)=0.
         FDU(I)=0.;FDV(I)=0.;FDW(I)=0.;FDT(I)=0.
6023  continue
	do I=1,n
           FDU(I)=NNNINPUT(I,1)     
	   FDV(I)=NNNINPUT(I,2)      
	   FDW(I)=NNNINPUT(I,3)       
	   FDT(I)=INPUT(I+KTURB,14)       
        enddo
        
	CALL FDR(FDU,N,TR)   
	CALL FDR(FDV,N,TR)   
	CALL FDR(FDW,N,TR)   
	CALL FDR(FDT,N,TR)   

C OBS: As flutuações com FDR é a sinal do filtro (low-pass) subtraido da serie (NESTE CASO JÁ ROTADA PARA U,V E W [NNINPUT(I,J)]).
      DO 66010 I=1,N
         TURBU(I)=NNNINPUT(I,1)-FDU(I)
         TURBV(I)=NNNINPUT(I,2)-FDV(I)
         TURBW(I)=NNNINPUT(I,3)-FDW(I)
         TURBT(I)=INPUT(i+KTURB,14)-FDT(I)         
c o codigo abaixo salva em arquivo os dados originais e após o filtro
!  612    format(13(F15.8,1x))
!         WRITE(12,612)INPUT(KTURB+i,3),INPUT(KTURB+i,4),INPUT(KTURB+i,5),
!      &   INPUT(KTURB+i,6),INPUT(KTURB+i,7),INPUT(KTURB+i,11),
!      &   INPUT(KTURB+i,12),INPUT(KTURB+i,13),INPUT(KTURB+i,14),
!      &   FDU(I),FDV(I),FDW(I),FDT(I)
	 
	 
	 
66010 CONTINUE 
!        WRITE(*,*)'>>> FDR_DONE !!!' 

cccccccccccccccccccccccccccccc fim hans
c recomeço osvaldo
! 
!         DO I=1,N
!         TURBU(I)=NNNINPUT(I,1)-NNNMEDIO(1)
///////////////

         DO I=1,M-1
      WRITE(US,3781)INPUT(KTURB+1,3),INPUT(KTURB+1,4),INPUT(KTURB+1,5),
     &   INPUT(KTURB+1,6),INPUT(KTURB+1,7),
     &    DR,Z3D/MOL,WT,USTAR,TSTAR,WIND3D,
     &    HZwS(I),USMOOTH(I),VSMOOTH(I),WSMOOTH(I),TSMOOTH(I),
     &    AU,BU,AV,BV,AW,BW,
     &    DISSIPW,DISMEANW,DISSIPU,DISMEANU,DISSIPV,DISMEANV,
     &    WTSMOOTH(I),(HZwS(I)*Z3D)/WIN3D,
     &    (HZwS(I)*USMOOTH(I))/(AD*DISSIPU)**2/3,
     &    (HZwS(I)*VSMOOTH(I))/(AD*DISSIPV)**2/3,
     &    (HZwS(I)*WSMOOTH(I))/(AD*DISSIPW)**2/3


3781    FORMAT(1X,F3.0,1X,F3.0,1X,F3.0,
     &   1X,F3.0,1X,F3.0,
     &  1X,F5.0,1X,F7.2,1X,F6.2,1X,F5.2,1X,F8.3,1X,F5.2,
     &  1X,F8.6,1X,F15.7,1X,F15.7,1X,F15.7,1X,F15.7,
     &  1X,F5.2,1X,F5.2,1X,F5.2,1X,F5.2,1X,F5.2,1X,F5.2,
     &  1X,F10.7,1X,F10.7,1X,F10.7,1X,F10.7,1X,F10.7,1X,F10.7,
     &  1X,F15.9,1X,F10.7,
     &  1X,F15.7,
     &  1X,F15.7,
     &  1X,F15.7)
          END DO
C
!       WRITE(13,815)INPUT(KTURB+1,3),INPUT(KTURB+1,4),INPUT(KTURB+1,5),
 611    format(25(F15.8,1x))
        WRITE(13,611)INPUT(KTURB+1,3),INPUT(KTURB+1,4),INPUT(KTURB+1,5),
     &   INPUT(KTURB+1,6),INPUT(KTURB+1,7),Z3D/MOL,
     &   USTAR,TSTAR,ECT,WU,WV,WT,T0,WIND2D,DR,
     &   DPU,DPV,DPW,DPT,
     &   DISSIPW,DISMEANW,DISSIPU,DISMEANU,DISSIPV,DISMEANV           
C
815   FORMAT(1X,F3.0,1X,F3.0,1X,F3.0,
     &   1X,F3.0,1X,F3.0,1X,F7.2, 
     &   1X,F11.2,1X,F11.2,1X,F6.2,1X,F9.2,1X,F9.2,1X,F8.2,1X,F8.2,
     &   1X,F10.5,1X,F10.5,
     &   1X,F10.5,1X,F10.4,1X,F10.5,1X,F10.2,
     &    1X,F10.7,1X,F10.7,1X,F10.7,1X,F10.7,1X,F10.7,1X,F10.7)
     
c
C############  SUBROTINAS CHAMADAS   ##################
C
C
c       ================================================================
c       rotina prog by hans accordingly to filtro low-pass Mc-Millen 1988 
c       Franceschi & Zardi 2004. 
      SUBROUTINE FDR(Dinput,n,RCtscale)
      integer n
      real Dinput(n)
      real a,x,RTC
     
      a=0.0
c       .1 == 10Hz
c       .2 == 5Hz 
c       16384 == 27min
c        a=exp(-.10/(200))
c old mcmillen         a=1-(.10/RCtscale)     

        a=1-2*3.141592653589793*(.2/RCtscale)  
          Dinput(1)=Dinput(1)
        do i=2,n
           Dinput(i)=a*Dinput(i-1)+(1-a)*Dinput(i)
        enddo
        return
        end
c      ================================================================


++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

--------------------- from gillfdr350ad.f ----------------------------

          DO I=1,M-1
      WRITE(US,3781)INPUT(KTURB+1,3),INPUT(KTURB+1,4),INPUT(KTURB+1,5),
     &   INPUT(KTURB+1,6),INPUT(KTURB+1,7),
     &    DR,Z3D/MOL,WT,USTAR,TSTAR,WIND3D,
     &    HZwS(I),USMOOTH(I),VSMOOTH(I),WSMOOTH(I),TSMOOTH(I),
     &    AU,BU,AV,BV,AW,BW,
     &    DISSIPW,DISMEANW,DISSIPU,DISMEANU,DISSIPV,DISMEANV,
     &    WTSMOOTH(I),(HZwS(I)*Z3D)/WIN3D,
     &    (HZwS(I)*USMOOTH(I))/(AD*DISSIPU)**2/3,RRRRR
     &    (HZwS(I)*VSMOOTH(I))/(AD*DISSIPV)**2/3,
     &    (HZwS(I)*WSMOOTH(I))/(AD*DISSIPW)**2/3


3781    FORMAT(1X,F3.0,1X,F3.0,1X,F3.0,
     &   1X,F3.0,1X,F3.0,
     &  1X,F5.0,1X,F7.2,1X,F6.2,1X,F5.2,1X,F8.3,1X,F5.2,
     &  1X,F8.6,1X,F15.7,1X,F15.7,1X,F15.7,1X,F15.7,
     &  1X,F5.2,1X,F5.2,1X,F5.2,1X,F5.2,1X,F5.2,1X,F5.2,
     &  1X,F10.7,1X,F10.7,1X,F10.7,1X,F10.7,1X,F10.7,1X,F10.7,
     &  1X,F15.9,1X,F10.7,
     &  1X,F15.7,
     &  1X,F15.7,
     &  1X,F15.7)
          END DO
C
!       WRITE(13,815)INPUT(KTURB+1,3),INPUT(KTURB+1,4),INPUT(KTURB+1,5),
 611    format(25(F15.8,1x))
        WRITE(13,611)INPUT(KTURB+1,3),INPUT(KTURB+1,4),INPUT(KTURB+1,5),
     &   INPUT(KTURB+1,6),INPUT(KTURB+1,7),Z3D/MOL,
     &   USTAR,TSTAR,ECT,WU,WV,WT,T0,WIND2D,DR,
     &   DPU,DPV,DPW,DPT,
     &   DISSIPW,DISMEANW,DISSIPU,DISMEANU,DISSIPV,DISMEANV           
C
815   FORMAT(1X,F3.0,1X,F3.0,1X,F3.0,
     &   1X,F3.0,1X,F3.0,1X,F7.2, 
     &   1X,F11.2,1X,F11.2,1X,F6.2,1X,F9.2,1X,F9.2,1X,F8.2,1X,F8.2,
     &   1X,F10.5,1X,F10.5,
     &   1X,F10.5,1X,F10.4,1X,F10.5,1X,F10.2,
     &    1X,F10.7,1X,F10.7,1X,F10.7,1X,F10.7,1X,F10.7,1X,F10.7)
     
c

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




