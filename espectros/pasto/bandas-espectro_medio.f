c as linhas comentada com cccc são substituídas via script
c bandas-spcmedio.sh
C
CCCC	REAL INPUT(NN,4)
CCCC	REAL freqad(NN),suad(NN),svad(NN),swad(NN)
CCCC	REAL HZMEAN(M),SPECUMEAN(M),SPECVMEAN(M),SPECWMEAN(M)
C
COMECANDO A LER O ARQUIVO DE ENTRADA
C
C O ARQUIVO DE ENTRADA NESTE PROGRAMA SAO OS ESPECTROS GERADOS NO 
C PROGRAMA QUE CALCULA OS ESPECTROS
C
c

c
CCCCC   DO 10 I=1,NN
CCCCC      READ (11,*,END=10)(INPUT(I,J),J=1,4)

C10    CONTINUE 
CCCCc	OPEN(UNIT=15,FILE='ESPMEDIO.DAT',STATUS='UNKNOWN')
C
      do i=1,L
	freqad(i)=DATA(i,1)
	suad(i)=DATA(i,2)
	svad(i)=DATA(i,3)
	swad(i)=DATA(i,4)
	enddo
C
CHAMA A SUBROTINA QUE SUAVIZA O ESPECTRO EM M BANDAS
C
	N=L
      CALL BANDAS(freqad,suad,HZMEAN,SPECUMEAN,B,N)
      CALL BANDAS(freqad,sVad,HZMEAN,SPECVMEAN,B,N)
      CALL BANDAS(freqad,sWad,HZMEAN,SPECWMEAN,B,N)
c
C
          DO I=1,B
      WRITE(15,3781)HZMEAN(I),SPECUMEAN(I),SPECVMEAN(I),SPECWMEAN(I)
3781  FORMAT(F12.5,1X,F12.6,1X,F12.6,1X,F12.6)
          ENDDO
C
      CLOSE(11)
      CLOSE(15)
C
      STOP
     	END
C=================================================================================
      SUBROUTINE BANDAS(fqin,spin,fqout,spout,B,N)
      !parameter(n=836,m=22)
      !INTEGER n
      integer nout(n)
      integer ipos
      real fqin(n),spin(n),fqout(n),spout(n)
      real fmin,fmax
      real ratio,xpos
c
c
	fmin=fqin(1)
      fmax=fqin(n)/1.
      nsamp2=n/2.
      nclass=B
      nclass2=0.
c
      do 100 i=1,nclass
      spout(i)=0.
      fqout(i)=0.
	nout(i)=0.
100   continue
c
c
      do 200 i=1,n
      ratio=log10(fqin(i)/fmin)/log10(fmax/fmin)
      xpos=ratio*nclass
      ipos=anint(xpos)
      if(ipos.eq.0)ipos=1
      spout(ipos)=spout(ipos)+spin(i)
      fqout(ipos)=fqout(ipos)+fqin(i)
      nout(ipos)=nout(ipos)+1
c	print*,i,ipos,fqout(ipos),spout(ipos)
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
!	    print*,i,nclass,nclass2,fqout(nclass2),spout(nclass2)
          endif
300   continue
      RETURN
      END 

