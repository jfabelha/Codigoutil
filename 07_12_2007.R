#Importação
treino<-read.table("Treino.txt",sep=",",na.strings="?")
#Entropia Total
Uns<-sum(treino$V15)
Zeros<-length(treino$V15)-Uns
Entropia.Total<- (-Uns/length(treino$V15))*log((Uns/length(treino$V15)),2)+
(-Zeros/length(treino$V15))*log((Zeros/length(treino$V15)),2)
Entropia.Total

#Ou

Entropia <- function (x) {
		output <- 0
		sumx <- sum(x)
		for (i in x) {
			output1 = (i/sumx)*log2(i/sumx)
			if (is.nan(output1))  
			output1 <- 0
			output <- output - output1
			}
		output
		}

Entropia.Total<-Entropia(c(Uns,Zeros))

#Entropia do Atributo V1
treino$V1
TV1<-table(treino$V1,treino$V15)
P0V1<-sum(TV1[1,])
P1V1<-sum(TV1[2,])
e0v1<-Entropia(TV1[1,])
e1v1<-Entropia(TV1[2,])
EntropiaV1<-(sum(TV1[1,])/length(treino$V15))*Entropia(TV1[1,])+
(sum(TV1[2,])/length(treino$V15))*Entropia(TV1[2,])
#Ganho de Informação do Atributo V1
GanhoInfV1<-Entropia.Total-EntropiaV1
sum(treino$V1)
620-421
Entropia(c(421,199))
#GainRatio de V1
GainRatioV1<-GanhoInfV1/EntropiaV1

#Automatização do Processo
getrank<- function(dados,...) 	
			{
			result<-data.frame()
			attach(dados)
			finalrank<-numeric(0)
	   		(classe<-names(dados)[length(names(dados))])
	   		root.info<-Entropia(table(get(classe)))
	   		atrs<-names(dados)[1:length(names(dados))-1]
	   		for (atr in atrs) {
	   					atr.info<-0
	   					atr.table<-table(get(atr), get(classe))
	   					no.atr.valset<-length(atr.table[,2])
	   					no.atr.vals<-nrow(dados)
						ja<-NULL
	   					for (atr.val in 1: no.atr.valset)
							{
	   						atr.peso <- sum( atr.table[atr.val,]) / no.atr.vals
	   						atr.info1 <- atr.peso * Entropia(atr.table[atr.val,])
	   						atr.info<-atr.info + atr.info1
							ja[atr.val]<-sum( atr.table[atr.val,])
							}
	   					info.gain <- root.info - atr.info
						aj<-Entropia(ja)
						gain.ratio <- info.gain/aj
	   					finalrank<-append( finalrank, gain.ratio, after=length(finalrank))
						}
	   		atrs<-as.vector(atrs)
			finalrank<-as.vector(finalrank)
			nome.col<-names(dados[-length(names(dados))])
			result<-data.frame(finalrank, row.names=(nome.col))
			result}
ana<-getrank(treino)
joao<-getrank(treino)
ja<-cbind(joao,ana)

