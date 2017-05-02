# Quando se corta por quantil, quando o valor do quantil faz parte do corte 
# se faz necessário um ajuste para retirarmos o NA

pc2<-quantile(dados$A2,seq(from=0,to=1,by=1/8),na.rm=FALSE)
pc2[1]<-pc2[1]-0.01
dados$A2<-cut(dados$A2,pc2,labels=FALSE)
sum(table(dados$A2))

#---------------------------------------------------------------------------

pc3<-quantile(dados$A3,seq(from=0,to=1,by=1/8),names=FALSE)
pc3<-pc3-0.1
pc3[9]<-pc3[9]+0.2
dados$A3<-cut(dados$A3,pc3,labels=FALSE)
sum(table(dados$A3))
dados$A3
#---------------------------------------------------------------------------
pc5<-quantile(dados$A5,seq(from=0,to=1,by=1/6),names=FALSE)
pc5<-pc5-1
pc5[7]<-15
dados$A5<-cut(dados$A5,pc5,labels=FALSE)
table(dados$A5)
sum(table(dados$A5))
dados$A5
#--------------------------------------------------------------------------
pc7<-quantile(dados$A7,seq(from=0,to=1,by=1/8),names=FALSE)
pc7<-pc7-0.1
pc7[9]<-pc7[9]+0.2
dados$A7<-cut(dados$A7,pc7,labels=FALSE)
sum(table(dados$A7))
dados$A7
#---------------------------------------------------------------------------
#nestes atributos não foi possivel aplicar diretamente o equal frequency binning
#--------------------
pc10<-quantile(dados$A10,seq(from=0,to=1,by=1/8),names=FALSE)
pc10
pc10<-c(-1,1,3,7,68)
dados$A10<-cut(dados$A10,pc10,labels=FALSE)
sum(table(dados$A10))
#----------------------------------------------------------------------------
pc13<-quantile(dados$A13,seq(from=0,to=1,by=1/8),names=FALSE)
pc13
pc13<-c(-1,80,120,160,200,280,369.25,2001)
dados$A13<-cut(dados$A13,pc13,labels=FALSE)
sum(table(dados$A13))
#-----------------------------------------------------------------------------
pc14<-quantile(dados$A14,seq(from=0,to=1,by=1/8),names=FALSE)
pc14
pc14<-c(0.000,4.000,68.875,395.500,1245.125,100002.000)
dados$A14<-cut(dados$A14,pc14,labels=FALSE)
sum(table(dados$A14))
#----------------------------------------------------------------------------