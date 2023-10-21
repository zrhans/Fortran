      program medias
      
      implicit none
      real ntprov, prova, ntlist, lista, med, ntp, ntl
      integer np, nl, aval

      np  =3
      nl  =3
      ntp =0
      ntl =0
      med =0
      
      open(unit=1,file='mediatri.txt')
      
      do aval=1,np

      print *,'Digite a nota da prova',aval
      read  *,prova
      print *,'Digite a not da lista',aval
      read  *,lista
      
      write(1,*)'Prova', aval, '=', prova
      write(1,*)'Lista', aval, '=', lista

      ntp = ntp + prova
      ntl = ntl + lista
      
      med = (ntp/np)*0.8+(ntl/nl)*0.2

 1    enddo
 
      write(1,*)'Media final =',  med

      if(med.GE.7) then
      print *,'Aprovado'
      else
      print *,'Reprovado! :)'
      endif

      stop
      end
      
