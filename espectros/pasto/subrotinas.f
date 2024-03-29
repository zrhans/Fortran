C
C
C############  SUBROTINAS CHAMADAS   ##################
C
C
C
c       ================================================================
c       rotina prog by hans accordingly to filtro low-pass Mc-Millen
c       1988 
c       Franceschi & Zardi 2004. 
	SUBROUTINE FDR(Dinput,n,RCtscale)
c	integer n
	real Dinput(n)
	real a,x,RTC
	   
	a=0.0
c       .1 == 10Hz
c       .2 == 5Hz 
c       16384 == 27min
c        a=exp(-.10/(200))
c old mcmillen  a=1-(.10/RCtscale)     
	
	a=1-2*3.141592653589793*(.1/RCtscale)  
	Dinput(1)=Dinput(1)
	do i=2,n
	Dinput(i)=a*Dinput(i-1)+(1-a)*Dinput(i)
	enddo
	return
	end
c      ================================================================
c      
c +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c       rotina para selecao de nome de arquivo d acordo com a
c       classe de z/l
	
	subroutine CLASSESPC(US,WIND3D,Z3D,MOL)
	integer US
	us=0
	IF((WIND3D.GT.1.).AND.((Z3D/MOL).GT.(0.).AND.(Z3D/MOL).LT.(0.1)))
     & US=24
C
	IF((WIND3D.GT.1.).AND.((Z3D/MOL).GT.(0.1).AND.(Z3D/MOL).LT.(0.3)))
     & US=25
C
	IF((WIND3D.GT.1.).AND.((Z3D/MOL).GT.(0.3).AND.(Z3D/MOL).LT.(0.5)))
     & US=26
C
	IF((WIND3D.GT.1.).AND.((Z3D/MOL).GT.(0.5).AND.(Z3D/MOL).LT.(0.8)))
     & US=27
C
	IF((WIND3D.GT.1.).AND.((Z3D/MOL).GT.(0.8).AND.(Z3D/MOL).LT.(1.1)))
     & US=28
C
	IF((WIND3D.GT.1.).AND.((Z3D/MOL).GT.(1.1).AND.(Z3D/MOL).LT.(5.)))
     & US=29
C
	IF((WIND3D.LT.1.).AND.((Z3D/MOL).GT.(0.).AND.(Z3D/MOL).LT.(0.1)))
     & US=30
C
	IF((WIND3D.LT.1.).AND.((Z3D/MOL).GT.(0.1).AND.(Z3D/MOL).LT.(0.3)))
     & US=31
C
	IF((WIND3D.LT.1.).AND.((Z3D/MOL).GT.(0.3).AND.(Z3D/MOL).LT.(0.5)))
     & US=32
C
	IF((WIND3D.LT.1.).AND.((Z3D/MOL).GT.(0.5).AND.(Z3D/MOL).LT.(0.8)))
     & US=33
C
	IF((WIND3D.LT.1.).AND.((Z3D/MOL).GT.(0.8).AND.(Z3D/MOL).LT.(1.1)))
     & US=34
C
	IF((WIND3D.LT.1.).AND.((Z3D/MOL).GT.(1.1).AND.(Z3D/MOL).LT.(5.)))
     & US=35
C
	IF((WIND3D.GT.1.).AND.((Z3D/MOL).GT.(-0.1).AND.(Z3D/MOL).LT.(0.)))
     & US=36
C
	IF((WIND3D.GT.1.).AND.((Z3D/MOL).GT.(-.3).AND.
     & (Z3D/MOL).LT.(-0.1)))US=37
C
	IF((WIND3D.GT.1.).AND.((Z3D/MOL).GT.(-.5).AND.
     & (Z3D/MOL).LT.(-0.3)))US=38
C
	IF((WIND3D.GT.1.).AND.((Z3D/MOL).GT.(-.8).AND.
     & (Z3D/MOL).LT.(-0.5)))US=39
C
	IF((WIND3D.GT.1.).AND.((Z3D/MOL).GT.(-1.1).AND.
     & (Z3D/MOL).LT.(-.8)))US=40
C
	IF((WIND3D.GT.1.).AND.((Z3D/MOL).GT.(-5.).AND.
     & (Z3D/MOL).LT.(-1.1)))US=41
C
	IF((WIND3D.LT.1.).AND.((Z3D/MOL).GT.(-0.1).AND.
     & (Z3D/MOL).LT.(0.)))US=42
C
	IF((WIND3D.LT.1.).AND.((Z3D/MOL).GT.(-.3).AND.
     & (Z3D/MOL).LT.(-0.1)))US=43
C
	IF((WIND3D.LT.1.).AND.((Z3D/MOL).GT.(-.5).AND.
     & (Z3D/MOL).LT.(-0.3)))US=44
C
	IF((WIND3D.LT.1.).AND.((Z3D/MOL).GT.(-.8).AND.
     & (Z3D/MOL).LT.(-0.5)))US=45
C
	IF((WIND3D.LT.1.).AND.((Z3D/MOL).GT.(-1.1).AND.
     & (Z3D/MOL).LT.(-.8)))US=46
C
	IF((WIND3D.LT.1.).AND.((Z3D/MOL).GT.(-5.).AND.
     & (Z3D/MOL).LT.(-1.1)))US=47
	return
	end
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C


      SUBROUTINE TWOFFT(DATA1,DATA2,FFT1,FFT2,N)
      DIMENSION DATA1(N),DATA2(N)
      COMPLEX FFT1(N),FFT2(N),H1,H2,C1,C2
      C1=CMPLX(0.5,0.0)
      C2=CMPLX(0.0,-0.5)
        DO 11 J=1,N
        FFT1(J)=CMPLX(DATA1(J),DATA2(J))
11    CONTINUE
      CALL FOUR1(FFT1,N,1)
      FFT2(1)=CMPLX(AIMAG(FFT1(1)),0.0)
      FFT1(1)=CMPLX(REAL(FFT1(1)),0.0)
      N2=N+2
      DO 12 J=2,N/2+1
        H1=C1*(FFT1(J)+CONJG(FFT1(N2-J)))
        H2=C2*(FFT1(J)-CONJG(FFT1(N2-J)))
        FFT1(J)=H1
        FFT1(N2-J)=CONJG(H1)
        FFT2(J)=H2
        FFT2(N2-J)=CONJG(H2)
12    CONTINUE
      RETURN
      END


      SUBROUTINE FOUR1(DATA,NN,ISIGN)
      REAL*8 WR,WI,WPR,WPI,WTEMP,THETA
      DIMENSION DATA(*)
      N=2*NN
      J=1
      DO 11 I=1,N,2
        IF(J.GT.I)THEN
          TEMPR=DATA(J)
          TEMPI=DATA(J+1)
          DATA(J)=DATA(I)
          DATA(J+1)=DATA(I+1)
          DATA(I)=TEMPR
          DATA(I+1)=TEMPI
        ENDIF
        M=N/2
1       IF ((M.GE.2).AND.(J.GT.M)) THEN
          J=J-M
          M=M/2
        GO TO 1
        ENDIF
        J=J+M
11    CONTINUE
      MMAX=2
2     IF (N.GT.MMAX) THEN
        ISTEP=2*MMAX
        THETA=6.28318530717959D0/(ISIGN*MMAX)
        WPR=-2.D0*SIN(0.5D0*THETA)**2
        WPI=SIN(THETA)
        WR=1.D0
        WI=0.D0
        DO 13 M=1,MMAX,2
          DO 12 I=M,N,ISTEP
            J=I+MMAX
            TEMPR=SNGL(WR)*DATA(J)-SNGL(WI)*DATA(J+1)
            TEMPI=SNGL(WR)*DATA(J+1)+SNGL(WI)*DATA(J)
            DATA(J)=DATA(I)-TEMPR
            DATA(J+1)=DATA(I+1)-TEMPI
            DATA(I)=DATA(I)+TEMPR
            DATA(I+1)=DATA(I+1)+TEMPI
12        CONTINUE
          WTEMP=WR
          WR=WR*WPR-WI*WPI+WR
          WI=WI*WPR+WTEMP*WPI+WI
13      CONTINUE
        MMAX=ISTEP
      GO TO 2
      ENDIF
      RETURN
      END



	SUBROUTINE WINDOW(WIN,WINTURB,N)
C	PARAMETER(N=16384)
	REAL JANELA(N),WIN(N),WINTURB(N)
	M=FLOAT(N/10)
	K=FLOAT(9*N/10)
	PI=3.141596
C
C JANELA QUADRADA
C	DO 6513 I=1,N
C	JANELA(I)=1.
C6513    CONTINUE
C
C JANELA BELL TAPER (STULL, PG 310)
	DO 6513 I=1,N
	IF(I.LT.M.OR.I.GT.K) THEN
		JANELA(I)=(SIN(5*PI*I/N))**2
	ELSE
		JANELA(I)=1.
	ENDIF 
6513    CONTINUE
	DO 6514 I=1,N
	WINTURB(I)=WIN(I)*JANELA(I)
6514	CONTINUE
	RETURN
	END


      SUBROUTINE SPECDIS(VEM,DSPEC,N,K)
C BYH K METADE DA SERIE DEFINIDO NO INICIO
C      PARAMETER(N=16384,K=8192)
      COMPLEX VEM(N)  
      real SPEC(N)
      REAL DSPEC(K+1)
      COMMON /KOEFF/FREQU
C A DIVISAO POR (N) NO DO ABAIXO E PARA COMPENSAR A
C NAO-NORMALIZACAO NA SUBROTINA TWOFFT
      DO 7615 I=1,N

cbh ver esta divis�o N*N
c	SPEC(I)=(VEM(I)*(CONJG(VEM(I))))/(N*N)
	
	SPEC(I)=((VEM(I)*(CONJG(VEM(I))))/(N))/N
	

CHANGE DA DISCRETE ENERGY SPECTRUM TO SPECTRAL DENSITY
c
7615  CONTINUE
      DELTAN=N/FREQU
c       deltan=1.
C
c
      DO 7320 I=1,K
      DSPEC(I)=2*SPEC(I)*DELTAN
7320  CONTINUE
      DSPEC(K+1)=SPEC(K+1)*DELTAN
      RETURN
      END


	SUBROUTINE CROSS(DAT1,DAT2,COESP,QUADRAT,N)
cbh hans em 01-05-2006 PARAMETER(N=32768)
	
	COMPLEX DAT1(N),DAT2(N)
	COMPLEX CROSESP(N)
	REAL COESP(N),QUADRAT(N)
	DO 5413 I=1,N

cbh ver esta divisa� N*N 
c	CROSESP(I)=(DAT1(I)*(CONJG(DAT2(I))))/(N*N)
!	print*,'...fazendo a tal divisao na CROSS :',I
	CROSESP(I)=((DAT1(I)*(CONJG(DAT2(I))))/(N))/N
!	PRINT*,'CROSESP(',I,')= ',CROSESP(I)
!	print*,'...DIVISAO FEITA NA CROSS..'
5413  CONTINUE
	DO 5414 I=1,N
	COESP(I)=REAL(CROSESP(I))
	QUADRAT(I)=IMAG(CROSESP(I))
5414  CONTINUE
	RETURN
	END
c
C 
C      
      SUBROUTINE BANDAS(fqin,spin,fqout,spout,k,M)
C BYH  parameter(n=16384,m=44) !ORIGINAL

      
      integer nout(k)
      integer ipos
      real fqin(k),spin(k),fqout(k),spout(k)
      real fmin,fmax
      real ratio,xpos
c
c
	fmin=fqin(1)
      fmax=fqin(k)/1.
      nsamp2=k/2.
      nclass=M
      nclass2=0.
c
      do 100 i=1,nclass
      spout(i)=0.
      fqout(i)=0.
	nout(i)=0.
100   continue
c
c
      do 200 i=1,k
      ratio=log10(fqin(i)/fmin)/log10(fmax/fmin)
      xpos=ratio*nclass
      ipos=anint(xpos)
      if(ipos.eq.0)ipos=1
      spout(ipos)=spout(ipos)+spin(i)
      fqout(ipos)=fqout(ipos)+fqin(i)
      nout(ipos)=nout(ipos)+1
200   continue
C      pause
c     
      ipos=0
      do 300 i=1,nclass
          if(nout(i).ne.0) then
          ipos=ipos+1
          nclass2=nclass2+1
          spout(nclass2)=spout(i)/float(nout(i))
          fqout(nclass2)=fqout(i)/float(nout(i))
	    nout(ipos)=nout(i)
          endif
300   continue
      RETURN
      END 
C 
      SUBROUTINE REALFT(DATA,N,ISIGN)
      REAL*8 WR,WI,WPR,WPI,WTEMP,THETA
      DIMENSION DATA(*)
      THETA=6.28318530717959D0/2.0D0/DBLE(N)
      C1=0.5
      IF (ISIGN.EQ.1) THEN
        C2=-0.5
        CALL FOUR1(DATA,N,+1)
      ELSE
        C2=0.5
        THETA=-THETA
      ENDIF
      WPR=-2.0D0*DSIN(0.5D0*THETA)**2
      WPI=DSIN(THETA)
      WR=1.0D0+WPR
      WI=WPI
      N2P3=2*N+3
      DO 11 I=2,N/2+1
        I1=2*I-1
        I2=I1+1
        I3=N2P3-I2
        I4=I3+1
        WRS=SNGL(WR)
        WIS=SNGL(WI)
        H1R=C1*(DATA(I1)+DATA(I3))
        H1I=C1*(DATA(I2)-DATA(I4))
        H2R=-C2*(DATA(I2)+DATA(I4))
        H2I=C2*(DATA(I1)-DATA(I3))
        DATA(I1)=H1R+WRS*H2R-WIS*H2I
        DATA(I2)=H1I+WRS*H2I+WIS*H2R
        DATA(I3)=H1R-WRS*H2R+WIS*H2I
        DATA(I4)=-H1I+WRS*H2I+WIS*H2R
        WTEMP=WR
        WR=WR*WPR-WI*WPI+WR
        WI=WI*WPR+WTEMP*WPI+WI
11    CONTINUE
      IF (ISIGN.EQ.1) THEN
        H1R=DATA(1)
        DATA(1)=H1R+DATA(2)
        DATA(2)=H1R-DATA(2)
      ELSE
        H1R=DATA(1)
        DATA(1)=C1*(H1R+DATA(2))
        DATA(2)=C1*(H1R-DATA(2))
        CALL FOUR1(DATA,N,-1)
      ENDIF
      RETURN
      END


	subroutine SEMTREND(VEMCA,VAILA,n)
c	parameter(N=16384)
      real VEMCA(n),VAILA(N)
      real media, soma
      real somay,somax,somaxy,somax2
      real a0,a1,f,a
      real carajo(50,2)
      integer nl,N1
C
      do 6875 i=1,n
	VAILA(I)=0.
c	vaila(i)=vemca(i)
6875  continue
      nn=30000
      nl=0 
      f=10
      ni=1
      med=1  ! must be an integer
      nf=f*60*med
      dnf=nf
C        
        m=1
C
    9   continue
        soma=0.
CC
        do i=ni,nf
        soma=soma + VEMCA(i)
        enddo
        media=soma/(dnf)
C
        a=ni+nf 
        carajo(m,1)=a/2 
        carajo(m,2)=media 
C
        m=m+1
        ni=ni+dnf
        nf=nf+dnf 
        if(m.lt.27) goto 9 !CUIDADO
        x=0.
        y=0.
        somax=0.
        somay=0.
        somaxy=0.
        somax2=0.
        N1=0
        a0=0.
        a1=0.
C
        do 15 k=1,m-1
        x=carajo(k,1)
        y=carajo(k,2)
C      
        N1=N1+1
        somax=somax+x
        somay=somay+y
        somaxy=somaxy+x*y
        somax2=somax2+x**2
C
   15  continue
       a0=(somay*somax2-somax*somaxy)/(N*somax2-somax**2)
        a1=(N1*somaxy-somax*somay)/(N1*somax2-somax**2)
C
C
        do i=1,n
        x=float(i)
        VAILA(i)=VEMCA(i)-(a1*x) !desativando para anular a subroitna
c        vaila(i)=vemca(i) !o que entra � o mesmo que sai.
        enddo
C       
      do 6877 i=1,n
	  VEMCA(i)=0.
6877  continue

      
	return
      end  

      SUBROUTINE FIT(X,Y,NDATA,SIG,MWT,A,B,SIGA,SIGB,CHI2,Q)
      DIMENSION X(NDATA),Y(NDATA),SIG(NDATA)
      SX=0.
      SY=0.
      ST2=0.
      B=0.
      IF(MWT.NE.0) THEN
        SS=0.
        DO 11 I=1,NDATA
          WT=1./(SIG(I)**2)
          SS=SS+WT
          SX=SX+X(I)*WT
          SY=SY+Y(I)*WT
11      CONTINUE
      ELSE
        DO 12 I=1,NDATA
          SX=SX+X(I)
          SY=SY+Y(I)
c	PRINT*,X(I),Y(I)
12      CONTINUE
        SS=FLOAT(NDATA)
      ENDIF
      SXOSS=SX/SS
      IF(MWT.NE.0) THEN
        DO 13 I=1,NDATA
          T=(X(I)-SXOSS)/SIG(I)
          ST2=ST2+T*T
          B=B+T*Y(I)/SIG(I)
13      CONTINUE
      ELSE
        DO 14 I=1,NDATA
          T=X(I)-SXOSS
          ST2=ST2+T*T
          B=B+T*Y(I)
14      CONTINUE
      ENDIF
      B=B/ST2
      A=(SY-SX*B)/SS
      SIGA=SQRT((1.+SX*SX/(SS*ST2))/SS)
      SIGB=SQRT(1./ST2)
      CHI2=0.
      IF(MWT.EQ.0) THEN
        DO 15 I=1,NDATA
          CHI2=CHI2+(Y(I)-A-B*X(I))**2
15      CONTINUE
        Q=1.
        SIGDAT=SQRT(CHI2/(NDATA-2))
        SIGA=SIGA*SIGDAT
        SIGB=SIGB*SIGDAT
      ELSE
        DO 16 I=1,NDATA
          CHI2=CHI2+((Y(I)-A-B*X(I))/SIG(I))**2
16      CONTINUE
        Q=GAMMQ(0.5*(NDATA-2),0.5*CHI2)
      ENDIF
      RETURN
      END

      FUNCTION GAMMQ(A,X)
      IF(X.LT.0..OR.A.LE.0.)PAUSE
      IF(X.LT.A+1.)THEN
        CALL GSER(GAMSER,A,X,GLN)
        GAMMQ=1.-GAMSER
      ELSE
        CALL GCF(GAMMCF,A,X,GLN)
        GAMMQ=GAMMCF
      ENDIF
      RETURN
      END

      SUBROUTINE GSER(GAMSER,A,X,GLN)
      PARAMETER (ITMAX=100,EPS=3.E-7)
      GLN=GAMMLN(A)
      IF(X.LE.0.)THEN
        IF(X.LT.0.)PAUSE
        GAMSER=0.
        RETURN
      ENDIF
      AP=A
      SUM=1./A
      DEL=SUM
      DO 11 N=1,ITMAX
        AP=AP+1.
        DEL=DEL*X/AP
        SUM=SUM+DEL
        IF(ABS(DEL).LT.ABS(SUM)*EPS)GO TO 1
11    CONTINUE
      PAUSE 'A too large, ITMAX too small'
1     GAMSER=SUM*EXP(-X+A*LOG(X)-GLN)
      RETURN
      END

      SUBROUTINE GCF(GAMMCF,A,X,GLN)
      PARAMETER (ITMAX=100,EPS=3.E-7)
      GLN=GAMMLN(A)
      GOLD=0.
      A0=1.
      A1=X
      B0=0.
      B1=1.
      FAC=1.
      DO 11 N=1,ITMAX
        AN=FLOAT(N)
        ANA=AN-A
        A0=(A1+A0*ANA)*FAC
        B0=(B1+B0*ANA)*FAC
        ANF=AN*FAC
        A1=X*A0+ANF*A1
        B1=X*B0+ANF*B1
        IF(A1.NE.0.)THEN
          FAC=1./A1
          G=B1*FAC
          IF(ABS((G-GOLD)/G).LT.EPS)GO TO 1
          GOLD=G
        ENDIF
11    CONTINUE
      PAUSE 'A too large, ITMAX too small'
1     GAMMCF=EXP(-X+A*ALOG(X)-GLN)*G
      RETURN
      END

      FUNCTION GAMMLN(XX)
      REAL*8 COF(6),STP,HALF,ONE,FPF,X,TMP,SER
      DATA COF,STP/76.18009173D0,-86.50532033D0,24.01409822D0,
     *    -1.231739516D0,.120858003D-2,-.536382D-5,2.50662827465D0/
      DATA HALF,ONE,FPF/0.5D0,1.0D0,5.5D0/
      X=XX-ONE
      TMP=X+FPF
      TMP=(X+HALF)*LOG(TMP)-TMP
      SER=ONE
      DO 11 J=1,6
        X=X+ONE
        SER=SER+COF(J)/X
11    CONTINUE
      GAMMLN=TMP+LOG(STP*SER)
      RETURN
      END
