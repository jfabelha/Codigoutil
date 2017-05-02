#Importação
treino<-read.table("Treino.txt",sep=",",na.strings="?")
teste<-read.table("Test.txt",sep=",",na.strings="?")

#Importar Rpart
library(rpart)

#Árvore de Decisão
arv1<-rpart(V15~.,treino,method="class",cp=0,,minsplit=0)

# Desenhar árvore
plot(arv1)
text(arv1)

#Prever sucesso de 1º
Pred1<-predict(arv1,treino,type="class")

#Quantos falhou usando os dados todos
table(Pred1,treino$V15)

#Taxa de erro
diag(table(Pred1,treino$V15))
tx.acerto1<-sum(diag(table(Pred1,treino$V15)))/length(treino$V15)
tx.acerto1

#Subdividir os nossos dados treino para podermos avaliar os modelos
amostra1<-sample(1:nrow(treino),as.integer(
0.3*nrow(treino)))
amostra_teste<-treino[amostra1,]
amostra_treino<-treino[-amostra1,]
length(amostra_teste$V2)
length(amostra_treino$V2)

#Árvore de decisão para amostra treino
arv2<-rpart(amostra_treino$V15~., method="class", amostra_treino)
plot(arv2)
text(arv2)

#Prever sucesso de Treino
Pred2<-predict(arv2,amostra_treino[,-15], type="class")

#Quantos falhou usando os dados todos
table(Pred2,amostra_treino$V15)

#Taxa de erro
diag(table(Pred2,amostra_treino$V15))
tx.acerto2<-sum(diag(table(Pred2,amostra_treino$V15)))/length(
amostra_treino$V15)
tx.acerto2

#Prever sucesso de Teste
Pred3<-predict(arv2,amostra_teste[,-15], type="class")

#Quantos falhou usando os dados todos
table(Pred3,amostra_teste$V15)

#Taxa de erro
diag(table(Pred3,amostra_teste$V15))
tx.acerto3<-sum(diag(table(Pred3,amostra_teste$V15)))/length(
amostra_teste$V15)
tx.acerto3


____________________________________________________________________

?naiveBayes
#Importar Naive Bayes - Packages -Load Packages- Classe - e1071
library(class)
library(e1071)

nb<- naiveBayes(V15 ~ ., data = treino,method="class")
nb
Pred_nb<-predict(nb, treino[,-15])
table(Pred_nb,treino$V15)
diag(table(Pred_nb,amostra_teste$V15))
tx.acerto_nb<-sum(diag(table(Pred_nb,treino$V15)))/length(treino$V15)
tx.acerto_nb

treino$V15<-factor(treino$V15, labels = c("0","1"))

