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
	filein<-"MFDR14-SCS1-2001-09.spc"
#       filein<-paste("/home/hans/work/dados/22col/2002/",arqu)
 
 

#------------------------------------------------------------------------
## função para gerar o nome do arquiv jpg de acordo com o argumento "var"
## passado para a função gname(var)
#
#         gname=function(var){
#                cam<-"/"
#                nome<-substr(filein,1,10)
#                nome<-paste(cam,var,"_",nome,".png",sep="", collapse=NULL)
#                png(filename=nome)
##                nome<-paste(cam,var,"-",nome,".jpg",sep="", collapse=NULL)
##                jpeg(filename=nome)
#        }
## expressao      gname("gradu")
#                plot(x,Y)
#------------------------------------------------------------------------
 
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
	write(c("frFDR14-SCS1-2001-09",fmaxu,fmaxv,fmaxw),"data-freq/frFDR14-SCS1-2001-09",ncol=4, sep=" ")
## depois daqui utiliza externamente gnuplot p os graficos
#save.image()
