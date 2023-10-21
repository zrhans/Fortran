       program leraw
       character filein(1000)*39,lixo*256,label(79)*5	
	character fileout*20
	character filecho*39
       integer ano(100),dia(100),mes(100)
       integer hora(100),minuto(100),segundo(100),julday(100)
       integer ivar(100),hhora(100000),doy(100000),year(100000)
       integer ihora,iminuto
       integer ipos,fpos
       real periodo
       real val(37001,80)
       real stempo,ftempo,etempo
	 real*8 hr(100000)
	 real minu,minut(100000)
c
c  reading the rere2000.input.par
       open(2,file="rere2000p.input.par")
       do 10 i=1,10
          read(2,2000) lixo
2000      format(a256)
10     continue

       do 20 i=1,79
          read(2,2100) label(i) 
2100      format(4x,a5)
20     continue

c
c  Choosing the variables
       print *,">variable list/lista de variaveis:"
       do 30 i=1,16
            ipos=(i-1)*5
            if(i.ne.16) then
               write(*,2200) ((ipos+j),"-",label(ipos+j),j=1,5)
2200           format(5(3x,i2,a1,a5))
            else
               write(*,2300) ((ipos+j),"-",label(ipos+j),j=1,4)
2300           format(4(3x,i2,a1,a5))
            endif
30     continue
       print *,">How many variables/Quantas variaveis?"
       read(*,*) nvar
       print *,">Which ones/Quais variaveis?"
       read(*,*) (ivar(i), i=1,nvar)
       print *,">You chose/Voce escolheu:"
       write(*,2400) (i,"-",label(ivar(i)),i=1,nvar)
2400   format(10(i2,a1,a5,1x))
       
c
c  generating the raw list file:
      call system("del raw.lst") 
      call system("dir /on /b *.rwp > raw.lst")

c
c  now choosing the raw file:
       open(2,file="raw.lst")
       nfile=1
       print *, ">list of raw files/listagem do arquivos brutos:"
100       read(2,1000,end=200) filein(nfile) 
1000      format(a39)
          print *,">file/arquivo ",nfile," : ",filein(nfile)
          nfile=nfile+1
          goto 100
200    close (2)
	 nfile=nfile-1

	do 9999 ifile=1,nfile

	filecho=filein(ifile)
	fileout=filecho(1:20)

c  reading the raw file:

      irec=0
      ntempo=1

      print *,"Espere lendo o arquivo: ",filein(ifile)
      
	open(2,file=filein(ifile))

300   read(2,1100,end=400) ano(ntempo),dia(ntempo),mes(ntempo),
	& hora(ntempo),minuto(ntempo),segundo(ntempo),julday(ntempo)

1100  format(22x,i4,1x,i2,1x,i2,3x,i2,1x,i2,1x,i2,15x,i3)

	minu=minuto(ntempo)

	ntempo=ntempo+1
		
	do 310 i=1,600
		irec=irec+1
		read(2,*,end=400) (val(irec,jrec),jrec=1,79) 
		do 311 j=1,79
			if (val(irec,j).le.-99.) val(irec,j)=val(irec-1,j)
311		continue

		year(irec)=ano(ntempo-1)
		doy(irec)=julday(ntempo-1)
		hhora(irec)=hora(ntempo-1)
		minut(irec)=minu+float(i)/600.
					
      hr(irec)=julday(ntempo-1)+float(hora(ntempo-1))/24.+
	&minut(irec)/1440.
       
310   continue
      goto 300

400   close(2)

	PRINT*,"Arquivo com: ",(IREC-1)
		
c
c choosing the desired time


	PERIODO=(IREC-1)/600

      ntempo=ntempo-1
  	ihora=hora(1)
 	iminuto=minuto(1)
	
c
c positioning at the right index
C       lper=int(periodo)*60*10
	lper=IREC-1


       stempo=float(hora(1))+float(minuto(1))/(60)+
     &             float(segundo(1))/(60*60)

       ftempo=float(hora(ntempo))+float(minuto(ntempo))/(60)+
     &             float(segundo(ntempo))/(60*60)

       etempo=float(ihora)+float(iminuto)/(60)

       ipos=int(irec*(etempo-stempo)/(ftempo-stempo))

       fpos=ipos+lper

       if(fpos .gt. irec) then
          etempo=ftempo-periodo/60
          ipos=irec-lper 
          fpos=irec 
          if(ipos .lt.1) then
               ipos=1
          endif
       endif
       if(ipos .lt. 1) then
          etempo=stempo
          ipos=1 
          fpos=lper 
          if(fpos .gt. irec) then
               fpos=irec
          endif
       endif

c
c saving to the output files
       open(3,file=fileout//".EDDY")
         
       do 500 i=1,lper
            write (3,4000) hr(i),(val(i,ivar(j)), j=1,nvar)
4000        format(f11.6,2x,56(f10.4,2x))
500    continue
       close(3)
	 close(4)
	  

       open(3,file=fileout//".PAR")
       write(3,*) "Arquivo de dados Brutos: ",filein(ifile)
       write(3,*) "Hora Local: ",int(etempo)-4
	 write(3,4210) "Inicio: ",ihora,"h ",iminuto,"m"
       write(3,*) "Numero total de linhas: ",irec-1
       write(3,*) "Variaveis selecionadas: ",nvar
       write(3,4100) (ivar(i),i=1,nvar)
4100   format(80(i2,1x))
       write(3,4200) (label(ivar(i)),i=1,nvar)
       write(3,*) "Arquivo de Saida (10Hz): ",fileout//".EDDY"
       write(3,*) "Arquivo de Saida ( 1Hz): ",fileout//".SLOW"

4200   format(80(a5,1x))
4210	 format(a12,1x,i2,a2,i2,a1)
       close(3)

c
c saving to the output of 1Hz files


       open(2,file=fileout//".SLOW")
         
       do 501 i=1,fpos,10
            write (2,4000) hr(i),(val(i,ivar(j)), j=1,nvar)
501    continue
       close(2)
	 close(3)
	 close(4) 

9999	continue

c****************************************************************
c end of the program!
       stop
       end

