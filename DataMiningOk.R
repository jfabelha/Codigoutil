#Importação de dados e livrarias
dados<-read.csv('tablea.csv', head=T, sep=",")
dados1<-dados[,-1]
dadosok<-as.data.frame(dados1[,1:4])
library(rpart)
library(class)
library(e1071)

#Divisão dos dados
t.train<-dadosok[1:as.integer((0.7*length(dadosok[,4]))),]
t.modsel<-dadosok[as.integer((0.7*length(dadosok[,4]))):as.integer(
(0.9*length(dadosok[,4]))),]
t.modsel[-1,]
t.test<-dadosok[as.integer((0.9*length(dadosok[,4]))):length(dadosok[,4]),]
t.test[-1,]
t.train.best<-dadosok[1:as.integer((0.9*length(dadosok[,4]))),]


#Capacidade preditiva dos atributos para a previsão do 6º valor
arvore<-rpart(t.train$Close~., t.train)
previsao<-predict(arvore,t.modsel)
erros<-t.modsel[,4]-previsao
plot(arvore,branch=0)
text(arvore)
mse.arv<-mean((previsao-t.modsel[,"Close"])^2)

############################################################################################################
##Árvores de Regressão

dados<-read.csv('tablea.csv', head=T, sep=",")
dados1<-dados[,-1]
dadosok<-as.data.frame(dados1[,1:4])

# Árvore de regressão 'embeded time delay' considerando importante para a previsão os últimos 5 dias úteis

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
arvore<-rpart(train$dt~., data=train)
previsao1<-predict(arvore,modsel)
mse.arv1<-mean((previsao1-modsel[,1])^2)

# Árvore de regressão 'embeded time delay' considerando importante para a previsão o últimos mês

train <- data.frame(embed(diff(t.train), dim = 21))
names(train) <- c("dt", "dt.1", "dt.2", "dt.3", "dt.4", "dt.5",
"dt7", "dt.8", "dt.9", "dt.10", "dt.11", "dt.12","dt.13", "dt.14", "dt.15", 
"dt.16", "dt.17", "dt.18","dt.19", "dt.20", "dt.21")
head(train)

modsel <- data.frame(embed(diff(t.modsel), dim = 21))
test <- data.frame(embed(diff(t.test), dim = 21))
train.best <- data.frame(embed(diff(t.train.best), dim = 21))
names(modsel) <- names(test) <- names(train.best) <- c("dt", "dt.1", "dt.2", "dt.3", "dt.4", "dt.5",
"dt7", "dt.8", "dt.9", "dt.10", "dt.11", "dt.12","dt.13", "dt.14", "dt.15", 
"dt.16", "dt.17", "dt.18","dt.19", "dt.20", "dt.21")
arvore<-rpart(train$dt~., data=train)
previsao2<-predict(arvore,modsel)
mse.arv2<-mean((previsao2-modsel[,1])^2)

# Árvore de regressão 'embeded time delay' considerando importante para a previsão o últimos trimestre
dadosok<-as.data.frame(dados[,7])


train <- data.frame(embed(diff(t.train), dim = 63))

modsel <- data.frame(embed(diff(t.modsel), dim = 63))
test <- data.frame(embed(diff(t.test), dim = 63))
train.best <- data.frame(embed(diff(t.train.best), dim = 63))

arvore<-rpart(train$X1~., data=train)
previsao3<-predict(arvore,modsel)
mse.arv3<-mean((previsao3-modsel[,1])^2)

#######################################################################################

## Redes Neuronais

dados<-read.csv('tablea.csv', head=T, sep=",")
library(nnet)
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

sel <- data.frame(embed(diff(t.modsel), dim = 6))
test <- data.frame(embed(diff(t.test), dim = 6))
train.best <- data.frame(embed(diff(t.train.best), dim = 6))
names(sel) <- names(test) <- names(train.best) <- c("dt", "dt.1","dt.2", "dt.3", "dt.4", "dt.5")

alt <- expand.grid(size = c(10, 20, 30), decay = c(0.01, 0.001, 0.001),
theil = 0)
prevs.ant <- c(train[nrow(train), "dt"], sel[1:(nrow(sel) - 1), "dt"])
for (a in 1:nrow(alt)) {
nn <- nnet(dt ~ ., train, size = alt[a, "size"], decay = alt[a,
"decay"], maxit = 1000, linout = T)
prevs <- predict(nn, sel)
alt[a, "theil"] <- sqrt(sum(((sel[, "dt"] - prevs)/prevs.ant)^2)/sum(((sel[,
"dt"] - prevs.ant)/prevs.ant)^2))
}
best <- which.min(alt[, "theil"])
nn <- nnet(dt ~ ., train.best, size = alt[best, "size"], decay = alt[best,
"decay"], maxit = 1000, linout = T)
prevs.ant <- c(train.best[nrow(train.best), "dt"], test[1:(nrow(test) -
1), "dt"])
prevs <- predict(nn, test)
theil <- sqrt(sum(((test[, "dt"] - prevs)/prevs.ant)^2)/sum(((test[,
"dt"] - prevs.ant)/prevs.ant)^2))

c<-NULL
nh<-t.test[6]
for (i in 1:length(prevs[,1])) {
c[i]<-nh+prevs[i]
nh<-c[i]
}
mse.RNa<-mean((prevs-test[,1])^2)

mean((c[1:5]-t.test[7:11])^2)

c[1]-t.test[7]