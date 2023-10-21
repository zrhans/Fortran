#!/bin/bash
clear
DIALOG=${DIALOG=Xdialog}
if [ ! $DIALOG ]
then
	echo " Xdialog nao encontrado"
	exit 1
fi
	path=`pwd`
	buceta=$1
	graficos="graficos";data="data";spcdia="spcdia";
	spcmes="spcmes";usados="usados";spcmedio="meanspc";
	    
########################################################################
###
# Seleção de ações p o aplicativo      #
########################################################################
###
montafort()
{
cat<<eof1>COCO
      program Calculo_med
      parameter(colunas=32,linhas=$linhas)

      INTEGER i.J
      REAL Sv,Su,Sw,f
      REAL D(linhas,1:colunas)

C     COMECANDO A LER O ARQUIVO DE ENTRADA

      OPEN(UNIT=10,FILE='$arq')
      OPEN(UNIT=11,FILE='O$arq')
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

eof1
echo "compilando ..."
mv COCO COCO.f
 rm adimensional.x77
 g77 -o adimensional.x77 COCO.f

if [ -f adimensional.x77 ]; then
   chmod +x adimensional.x77
   ./adimensional.x77
else
  echo "<font color=red><h3>Erro compilando spcmedio.x77"
  exit 1
fi



return
}






executar()
{
ls ?????-??C?-* ?????-??S?-* >xi
xii=`wc -l xi | awk '{print $1}'`
rm -f xi
step=`expr 100 / $xii`
compteur="0"
step=`expr 100 / $xii`
#step=`expr 100 / 16`
(
echo "$compteur" ; #sleep 1
for x in `ls ?????-??C?-* ?????-??S?-*`
do
arq=$x
echo "XXX"
echo " \\n Executando: $arq";#sleep 1
linhas=`wc -l $x | awk '{print $1}'`
montafort
./adimensional.x77

echo "XXX"
compteur=`expr $compteur + $step`
echo "$compteur" ; #sleep 1
done; 
) | $DIALOG --title "Microm 2006 " --gauge "Execução de Media em \\n Espectros" 20 60 

return
}

menu()
{
c="$1"
datas=`date`
MENU=`$DIALOG --and-widget --stdout --nocancel --title "MENU MICROM 2006" \
--backtitle "Usuário: $USER - $datas" \
--cancel-label "Cancelar" --menu "Escolha a tarefa a executar !" 20 70 4 \
EXECUTAR "Executar $buceta" \
MEDIAS "Calcular espectros medios" \
LIMPEZA "Limpar pasta de trabalho" \
FIM "Sair do menu"`
case $? in
  0)
	case $MENU in
		EXECUTAR)executar;;
		MEDIAS)clear exit 1;;
		LIMPEZA)clear exit 1;;
		FIM)clear exit 1;;
        esac
	;;
  1)
      exit 0
       ;;
 255)
      exit 1
       ;;
esac
return
}

if [ $# -ne 0 ]; then

 echo "";echo "Usage: $0 filename";echo ""
 echo " $USER! Digite o nome do arquivo em fortran para executar."
 echo " Exemplo: $0 programa.x77";echo ""
 else
clear

menu

fi
