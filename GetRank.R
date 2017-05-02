getrank<- function(dados,...) 	
			{
			result<-data.frame()
# Definição da função info
			attach(dados)
# Cria variável rank
			finalrank<-numeric(0)
# Capturar variável objectivo (classe)
#assumindo que é a que se encontra mais à direita / última)
	   		(classe<-names(dados)[length(names(dados))])
# Calcula informação inicial, sem qualquer split.
	   		root.info<-Entropia(table(get(classe)))
# Capturar os restantes atributos (variáveis explicativas)
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
# Ganho de informação do atributo
	   					info.gain <- root.info - atr.info
	   					finalrank<-append( finalrank, info.gain, after=length(finalrank))
						}

	   		atrs<-as.vector(atrs)
			finalrank<-as.vector(finalrank)

#excluir o nome da coluna de classe dos dados

			nome.col<-names(dados[-length(names(dados))])
			result<-data.frame(finalrank, row.names=(nome.col))
			result}
			