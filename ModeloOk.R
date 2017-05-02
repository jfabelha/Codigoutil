#Importação de dados e livrarias
dados<-read.csv('tablea26.csv', head=T, sep=",")
dados1<-dados[,-1]
dadosok<-as.data.frame(dados1[,1:4])
library(rpart)
library(class)
library(e1071)

## Árvore de regressão 'embeded time delay' considerando importante para a previsão os últimos 5 dias úteis
#Com data set de construção do modelo = t.train.best
dadosok<-as.data.frame(dados[,7])
t.train<-dadosok[1:as.integer((0.7*dim(dadosok)[1])),]
t.modsel<-dadosok[as.integer((0.7*dim(dadosok)[1])):as.integer(
(0.9*dim(dadosok)[1])),]
t.modsel<-t.modsel[-1]
t.test<-dadosok[as.integer((0.9*dim(dadosok)[1])):dim(dadosok)[1],]
t.test<-t.test[-1]
t.train.best<-dadosok[1:as.integer((0.9*dim(dadosok)[1])),]
train <- data.frame(embed(diff(t.train), dim = 6))
names(train) <- c("dt", "dt.1", "dt.2", "dt.3", "dt.4", "dt.5")
head(train)
modsel <- data.frame(embed(diff(t.modsel), dim = 6))
test <- data.frame(embed(diff(t.test), dim = 6))
train.best <- data.frame(embed(diff(t.train.best), dim = 6))
names(modsel) <- names(test) <- names(train.best) <- c("dt", "dt.1","dt.2", "dt.3", "dt.4", "dt.5")
arvoreM<-rpart(train.best$dt~., data=train.best)
previsao1<-predict(arvoreM,test)
mse.arvM<-mean((previsao1-test[,1])^2)

par(mfrow=c(3,1))
ts.plot(t.train, main='t.train')
ts.plot(t.modsel,main='t.modsel')
ts.plot(t.test,main='t.test')


#Previsões Test

c<-NULL
nh<-t.test[6]
for (i in 1:length(prevs[,1])) {
c[i]<-nh+previsao1[i]
nh<-c[i]
}
c[1:5]
t.test[7:11]
mean((c[1:5]-t.test[7:11])^2)


