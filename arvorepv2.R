#Importar Rpart
library(rpart)

#Subdividir os nossos dados treino para podermos avaliar os modelos
amostra1<-sample(1:nrow(treino),as.integer(0.1*nrow(treino)))
amostra_teste<-treino[amostra1,]
amostra_treino<-treino[-amostra1,]
length(amostra_teste$V2)
length(amostra_treino$V2)

##Árvore de decisão para amostra_treino
arv2<-rpart(amostra_treino$V15~., method="class", amostra_treino)
plot(arv2)
text(arv2)
plot(arv2, branch=0)
text(arv2)

#Prever sucesso de amostra_treino
Pred2<-predict(arv2,amostra_treino[,-15], type="class")

#Quantos falhou usando os dados todos
table(Pred2,amostra_treino$V15)

#Taxa de acerto amostra_treino
diag(table(Pred2,amostra_treino$V15))
tx.acerto2<-sum(diag(table(Pred2,amostra_treino$V15)))/length(amostra_treino$V15)
tx.acerto2

#Prever sucesso de amostra_teste
Pred3<-predict(arv2,amostra_teste[,-15], type="class")

#Quantos falhou usando os dados todos
table(Pred3,amostra_teste$V15)

#Taxa de acerto amostra_teste
diag(table(Pred3,amostra_teste$V15))
tx.acerto3<-sum(diag(table(Pred3,amostra_teste$V15)))/length(
amostra_teste$V15)
tx.acerto3

