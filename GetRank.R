getrank<- function(dados,...) 	
			{
			result<-data.frame()
# Defini��o da fun��o info
			attach(dados)
# Cria vari�vel rank
			finalrank<-numeric(0)
# Capturar vari�vel objectivo (classe)
#assumindo que � a que se encontra mais � direita / �ltima)
	   		(classe<-names(dados)[length(names(dados))])
# Calcula informa��o inicial, sem qualquer split.
	   		root.info<-Entropia(table(get(classe)))
# Capturar os restantes atributos (vari�veis explicativas)
	   		atrs<-names(dados)[1:length(names(dados))-1]
# Calcular para todos os atributos
	   		for (atr in atrs) {
	   					atr.info<-0
	   					atr.table<-table(get(atr), get(classe))
# Calcular para todos os valores do atributo
	   					no.atr.valset<-length(levels(get(atr)))
	   					no.atr.vals<-nrow(dados)
	   					for (atr.val in 1: no.atr.valset)
							{
	   						atr.peso <- sum( atr.table[atr.val,]) / no.atr.vals
	   						atr.info1 <- atr.peso * Entropia(atr.table[atr.val,])
	   						atr.info<-atr.info + atr.info1
							}
# Ganho de informa��o do atributo
	   					info.gain <- root.info - atr.info
	   					finalrank<-append( finalrank, info.gain, after=length(finalrank))
						}

	   		atrs<-as.vector(atrs)
			finalrank<-as.vector(finalrank)

#excluir o nome da coluna de classe dos dados

			nome.col<-names(dados[-length(names(dados))])
			result<-data.frame(finalrank, row.names=(nome.col))
			result}
			