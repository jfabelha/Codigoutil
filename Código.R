dados<-read.table("crx.txt", sep=",", na.strings="?") # Comando para importar os dados para R

attributes(dados) # Fornece o nome das vari�veis e o tipo de objecto pelo qual o dataset est� representado em R

str(dados) # Resumo dos dados

dados[1:100,]  # Amostra dos primeiros 100 registos

head(dados) # Mostra as 6 primeiras inst�ncias do conjunto de dados

tail(dados) # Mostra as �ltimas 6 inst�ncias do conjunto de dados

summary(dados) # Sum�rio dos dados	

by(dados[, -16], dados$V16, summary) # Sum�rio dos dados por classe

summary(dados$V6)/length(dados[,1])*100 # Tabela de frequ�ncias

mean(na.omit(dados[,2]))  # C�lculo da m�dia sendo necess�rio omitir os Missing Values no c�lculo

table(dados$V1,dados$V16)# Tabela de cruzamentos
 	
sum(dados$V3>20) 

which.min(dados$V2)

attach(dados) # Facilita a indexa��o dos dados 

sum(V12=="f")  

table(V1=="a"&V2>12,V16)  

sort(V3)

detach(dados)

cor(na.omit(dados[,sapply(dados, is.numeric)])) 	# Matriz de correla��es para as vari�veis num�ricas

Sumario.Nominais <- function(x) { 	# Fun��o sum�rio para as vari�veis nominais
ckh<-c(0)
for (i in 1:length(dados[1,])) {
ckh[i]<-is.numeric(dados[,i])
}
s <- dados[,ckh==0]
kk<-list(0)
for (i in 1:length(s[1,])) {
kk[i]<-list(summary(s[,i])/length(s[,1])*100)
}
kk
}

Sumario.Nominais(dados)

pairs(dados[,sapply(dados, is.numeric)], main = "Concess�o de       # Scatterplot das vari�veis num�ricas
Cr�dito", pch = 21, bg = c("red","green")[dados$V16])

panel.hist <- function(x, ...) {	# Representa��o gr�fica das vari�veis num�ricas, atrav�s de um Scatterplot e de Histogramas
usr <- par("usr")
on.exit(par(usr))
par(usr = c(usr[1:2], 0, 1.5))
h <- hist(x, plot = FALSE, cex.main = 0.7)
breaks <- h$breaks
nB <- length(breaks)
y <- h$counts
y <- y/max(y)
rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}
pairs(dados[,sapply(dados, is.numeric)], main = "Concess�o de 
Cr�dito",pch = 21, bg = c("red", "green3")[dados$V16],
diag.panel = panel.hist, cex.labels = 1.5)

boxplot(dados$V14[dados$V16=="+"])

hist(dados[,3][dados$V16=="+"],prob = T, xlab = "",main = "Histograma de V3 ")

library(Hmisc) 	# � necess�rios carregar previamente esta livraria para utilizar o comando datadensity

datadensity(dados[,1:6])   #Aplica��o da fun��o para as 6 primeiras vari�veis.
 
library(rpart)	# � necess�rios carregar previamente esta livraria para utilizar o comando rpart
ac <- rpart(dados$V16 ~ ., dados)	# �rvore de decis�o 

plot(ac, margin = 0.1, branch = 0)

text(ac, use.n = T, fancy = F, all = T)
