##Importar Dados
dados<-read.table("dados2.csv", sep=';', header=T)
dados
##Importar libraria
install.packages("u:/desktop/ECD II/carenR_0.1-9-1.zip","u:/desktop/ECD II",repos=NULL)
library(carenR,lib.loc="u:/desktop/ECD II")
.libPaths('u:/desktop/ECD II')

library(carenR)

##Análise Preliminar
#Nº de cores mais utilizada
length(unique(dados$V18))
#Ordenação das cores utilizadas
sort(table(dados$V18),decreasing=T)[1:length(unique(dados$V18))]

#Relação entre cor predominante e extremos das tabelas
table(dados$V18,dados$V30)
table(dados$V18,dados$V29)

#Obtenção das regras
regras<-caren(dados,min.sup=0.2, min.conf=0.8, Bas=T)
length(regras)
regras
ar.pp(regras)
regras[1,]
ar.pp(regras[1,])

regrasimp<-caren(dados,min.sup=0.2, min.conf=0.8,imp=0.01, Bas=T)
regraslif<-caren(dados,min.sup=0.2, min.conf=0.8, Bas=T)

#Confiança
ant.sizes<-apply(as.array(1:nrow(regras)),1,function(i)
length(regras$ant[i][[1]]))
plot(regras$conf,regras$sup,pch=15,col=ant.sizes)

#Nº de Vezes que item aparece numa regra
cf<-apply(as.array(1:nrow(regras)),1,function(i)c(as.character(regras$cons[i]),regras$ant[i][[1]]))
occ.it<-unique(unlist(cf))
m<-matrix(0,nrow(regras),length(occ.it))
colnames(m)<-occ.it
for(i in 1:nrow(m)) 
m[i,cf[i][[1]]]<-1
plot(which(m==1,arr.ind=T),col='green', pch=15)



par(mfrow=c(2,1))
#Regras em relação ao minslip
nrules<-apply(as.array(seq(0.05,1,0.05)),1,function(s) nrow(regras[regras$sup>=s,]))
plot(seq(0.05,1,0.05),nrules,type='l',xlab='min.sup',ylab='nrules',main='Variation of nrules with min.sup')

#Regras em relação à confiança
nrules<-apply(as.array(seq(0.7,1,0.025)),1,function(c) nrow(regras[regras$conf>=c,]))
plot(seq(0.7,1,0.025),nrules,type='l',xlab='min.conf',ylab='nrules',main='Variation of nrules with min.conf')

#Sumário do improvement
summary(regraslif$lift)
nrules<-apply(as.array(seq(0.1,2,0.1)),1,function(l) nrow(regraslif[regraslif$lift>=l,]))
plot(seq(0.1,2,0.1),nrules,type='l',xlab='lift',ylab='nrules',main='Variation of nrules with min.lift')

#conviction
x<-as.numeric(as.character(regraslif$conv))
summary(x)
x[is.na(x)]<-20
regraslif$conv<-x
summary(regras$conv)
nrules<-apply(as.array(seq(1,16,0.2)),1,function(cv) nrow(regraslif[regraslif$conv>=cv,]))
plot(seq(1,16,0.2),nrules,type='l',xlab='conviction',ylab='nrules',main='Variation of nrules with conviction')

#Qui-quadrado
summary(regraslif$chi)

# visualmente
nrules<-apply(as.array(seq(0,61,0.2)),1,function(chi) nrow(regraslif[regraslif$chi>=chi,]))
plot(seq(0,61,0.2),nrules,type='l',xlab='chi^2',ylab='nrules',main='Variation of nrules with chi^2')
lines(c(3.841459,3.841459),c(0,nrow(rls)),col='red',lty='dotted')
