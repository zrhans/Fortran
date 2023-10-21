#!/bin/bash
clear
cam="350sec/"
cd $cam
mkdir spcmedio

# funcao para calcular a media dos espectros 
# acordo com a classe de estabilidade
prepara(){
 echo 'vou remover'
 rm -f ../spc.dat
 echo 'vou coratr'
 nome=`echo $x | cut -c1-4`
 echo "Media em $x como: $nome"
  echo "vou copiar $x para ../spc.dat"
 cp $x ../spc.dat
  echo "feito"
  echo `pwd`
  echo "vou descer"  
  cd ..
   echo "estou em `pwd`"
return
}

finaliza(){
 echo "vou finalizar"
 cd $cam
 mv ../medio.dat spcmedio/M$nome.dat
 echo "Arquivo de média: M$nome.dat finalizado. "
return
}


for x in *.spc
do
prepara
./mspc44b.x77
finaliza
# remove F2 dos arquivos de espectro
#  nome=`echo $x | cut -c3-10`
#  echo "$x e $nome"
# mv $x $nome

done
cd ..