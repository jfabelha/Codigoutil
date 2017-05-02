## Previsão Redes Neuronais

dados<-read.csv('tablea.csv', head=T, sep=",")
library(nnet)
dadosok<-as.data.frame(dados[,7])
t.train.best<-dadosok[1:2006,]
a<-dadosok[2007:2013,]
train.best <- data.frame(embed(diff(t.train.best), dim = 6))
ad <- data.frame(embed(diff(a), dim = 6))
names(train.best)<-names(ad) <- c("dt", "dt.1","dt.2", "dt.3", "dt.4", "dt.5")
Um <- predict(nn, ad)
ad[6]<-ad[5]
ad[5]<-ad[4]
ad[4]<-ad[3]
ad[3]<-ad[2]
ad[2]<-ad[1]
ad[1]<-Um
Dois<-predict(nn, ad)
ad[6]<-ad[5]
ad[5]<-ad[4]
ad[4]<-ad[3]
a[3]<-ad[2]
ad[2]<-ad[1]
ad[1]<-Dois
Tres<-predict(nn, ad)
ad[6]<-ad[5]
ad[5]<-ad[4]
ad[4]<-ad[3]
a[3]<-ad[2]
ad[2]<-ad[1]
ad[1]<-Tres
Quatro<-predict(nn, ad)
ad[6]<-ad[5]
ad[5]<-ad[4]
ad[4]<-ad[3]
a[3]<-ad[2]
ad[2]<-ad[1]
ad[1]<-Quatro
Cinco<-predict(nn, ad)
prev<-cbind(Um,Dois,Tres,Quatro,Cinco)

L<-dadosok[2013,]
previsaoRN<-NULL
nh<-L
for (i in 1:5) {
previsaoRN[i]<-nh+prev[i]
nh<-previsaoRN[i]
}
previsaoRN

## Previsão Árvore de regressão
arvore<-rpart(train.best$dt~., data=train.best)
Um<-predict(arvore,ad)
ad[6]<-ad[5]
ad[5]<-ad[4]
ad[4]<-ad[3]
ad[3]<-ad[2]
ad[2]<-ad[1]
ad[1]<-Um
Dois<-predict(arvore,ad)
ad[6]<-ad[5]
ad[5]<-ad[4]
ad[4]<-ad[3]
a[3]<-ad[2]
ad[2]<-ad[1]
ad[1]<-Dois
Tres<-predict(arvore,ad)
ad[6]<-ad[5]
ad[5]<-ad[4]
ad[4]<-ad[3]
a[3]<-ad[2]
ad[2]<-ad[1]
ad[1]<-Tres
Quatro<-predict(arvore,ad)
ad[6]<-ad[5]
ad[5]<-ad[4]
ad[4]<-ad[3]
a[3]<-ad[2]
ad[2]<-ad[1]
ad[1]<-Quatro
Cinco<-predict(arvore,ad)
prev<-cbind(Um,Dois,Tres,Quatro,Cinco)

L<-dadosok[2013,]
previsaoArv<-NULL
nh<-L
for (i in 1:5) {
previsaoArv[i]<-nh+prev[i]
nh<-previsaoArv[i]
}
previsaoArv

#Previsões AR(3)


### Previsão usando o AR(3) 
dados2000<-read.csv('tablea.csv', head=T, sep=",")
dados1_2000<-dados2000[,-1]
dadosok2000<-as.data.frame(dados1_2000[,1:4])
dadosok<-dadosok2000[,4]
mod1<-ar(dadosok, order.max = 5)
preds.ar3 <- predict(mod1, n.ahead = 5)



