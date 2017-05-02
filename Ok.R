#Variar aleatoriamente produção
Agent.micro.econ(dados2,5,random.prod=TRUE)

#Variar aleatoriamente um dado bem (Vestuário)
dadosAL2<-dados2
aso<-dadosAL2$"Variação da produção"
aso[4]<-c(1,1,1,abs(rnorm(1)/rnorm(1)),abs(rnorm(1)/rnorm(1))
,abs(rnorm(1)/rnorm(1)),1,1,1,1,1,1)
dadosAL2$"Variação da produção"<-aso
Agent.micro.econ(dadosAL2,15)
#aso:
#1  1.00000000
#2  1.00000000
#3  1.00000000
#4  0.42139082
#5  0.45757254
#6  0.01028934
#7  1.00000000
#8  1.00000000
#9  1.00000000
#10 1.00000000
#11 1.00000000
#12 1.00000000

#######################################################################

##Variação das quantidades iniciais de bens (morte semana 2)
#Agente agricula 1 mais rico
#Agente agricula 2 igual
#Agente agricula 3 mais pobre
dadosQI1<-dados2
aso<-dadosQI1$"Quantidades iniciais"
aso[1,3:6]<-c(2000,7,1,8)
aso[3,3:6]<-c(40,0.14,0.0039,0.24)
dadosQI1$"Quantidades iniciais"<-aso
Agent.micro.econ(dadosQI1,5)
Agent.micro.econ(dadosQI1,70)  #Morre o mais fraco de cada sector



##Variação das quantidades iniciais de bens (não morre)
#Agente agricula 1 mais rico
#Agente agricula 2 igual
#Agente agricula 3 mais pobre
dadosQI1<-dados2
aso<-dadosQI1$"Quantidades iniciais"
aso[1,3:6]<-c(2000,7,1,8)
aso[3,3:6]<-c(45,0.19,0.01,0.3)
dadosQI1$"Quantidades iniciais"<-aso
Agent.micro.econ(dadosQI1,5)
Agent.micro.econ(dadosQI1,20)

#Variação das quantidades iniciais de bens e da capacidade de produzir
#Agente agricula 1 mais rico
#Agente agricula 2 igual
#Agente agricula 3 mais pobre
aso<-dadosQI1$"Produção semanal"
aso[1,3]<-100
aso[3,3]<-1
dadosQI1$"Produção semanal"<-aso
Agent.micro.econ(dadosQI1,5)
#Agent.micro.econ(dadosQI1,1000)

#aqui
#Variação das quantidades de produção nos sectores
#Agente 1 produz mais
#Agente 2 igual
#Agente 3 produz pouco
dadosQI1<-dados2
aso<-dadosQI1$"Produção semanal"
aso[1,3]<-100
aso[3,3]<-1
aso[4,4]<-1
aso[6,4]<-0.2
aso[7,5]<-0.1
aso[9,5]<-0.005
aso[10,6]<-2
aso[12,6]<-0.4
dadosQI1$"Produção semanal"<-aso
Agent.micro.econ(dadosQI1,20)

#Variação das quantidades de produção nos sectores
#Agente 1 produz mais
#Agente 2 igual
#Agente 3 produz pouco
dadosQI1<-dados2
aso<-dadosQI1$"Produção semanal"
aso[1,3]<-0.8
aso[3,3]<-0.8
aso[4,4]<-1
aso[6,4]<-0.2
aso[7,5]<-0.1
aso[9,5]<-0.005
aso[10,6]<-2
aso[12,6]<-0.4
dadosQI1$"Produção semanal"<-aso
Agent.micro.econ(dadosQI1,20)
Agent.micro.econ(dadosQI1,45)