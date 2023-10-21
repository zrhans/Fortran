
#!/bin/bash
clear
echo "Hans<- this running @ `date`.">>log
echo "-------------------------------------------------">>log
echo "-------[ Hans<- this running @ `date`. ]---------"
echo "-------------------------------------------------"
#---------------------[ FUNCOES ]----------------------"

montapastas()
{
if [ -d "$path/$fmax" ]; then
  echo "Checando $path/$fmax ...OK" 
  else
  mkdir "$path/$fmax"
  echo "Criando $path/$fmax ...OK" 
fi
return
}

remfile(){
if [ -f $filetoremove ]; then
  echo "existe $filetoremove removendo ...OK"
  rm -f $filetoremove
  else
  echo "nao existe $filetoremove"
#return
fi
return
}

R_script()
{
cat<<endR_script>fmax.r
#------------------------------------------------------------------------
# Desenvolvido para o software R
# Função para obter a frequencia maxima em espectros. para sistema UNIX
# Version : 26/05/2006
# Original: Hans Rogério Zimermann
# Develop : Hans
# Revised : 26/05/2006
#------------------------------------------------------------------------
	options(object.size = 600000000)
#	options(digits=4)
	filein<-"$file"
        dados <-scan(c(filein))    
	dados <- matrix(dados, ncol = 4, byrow = T)
	frequencia <- dados[, 1]
	nsu        <- dados[, 2]
	nsv        <- dados[, 3]
	nsw        <- dados[, 4]

##
	fmaxu<-frequencia[which(nsu==max(nsu))]	
	fmaxv<-frequencia[which(nsv==max(nsv))]	
	fmaxw<-frequencia[which(nsw==max(nsw))]	

## escreve os arquivos limpos (condicionados) em outro diretório
	write(c("$file",fmaxu,fmaxv,fmaxw),"$fmax/fmaximas.doc", ncol=4, append= T, sep=" ")

#save.image()
endR_script
return
}



rm -f fmaximas.doc
for file in M*.spc
do
clear
tail -n10 log
echo "_________________________________________________"
       dir=`echo $file | cut -c1-4`
       nome=`echo $file | cut -c1-19`
       path=`pwd`
       ymd=`echo $file | cut -c1-10`
#cat pasto.r.original > filetxt
#sed s,arquivo,"$relpath$nome",g filetxt > rawpasto.r
echo "file    : $file"
echo "dir     : $dir"
echo "nome    : $nome"
echo "path    : $path"
# removendo arquivos velhos
# definindo nome para criacao de pastas
fmax=$path
montapastas
./remlixo.awk $file > tmp
mv tmp $file
# montando script R
R_script
# execucao do script R
echo "--[ executando R script : $t1 ]--"
R --no-save < fmax.r
echo "--[ executado  R script : $tempo]--"
echo "R: $nome ini/fim: $t1 / $tempo. Total: $elapsed" >>log

done
