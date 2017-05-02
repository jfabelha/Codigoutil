dados<-read.table("crx.txt", sep=",", na.strings="?") # Comando para importar os dados para R

attributes(dados) # Fornece o nome das variáveis e o tipo de objecto pelo qual o dataset está representado em R

str(dados) # Resumo dos dados

dados[1:100,]  # Amostra dos primeiros 100 registos

head(dados) # Mostra as 6 primeiras instâncias do conjunto de dados

tail(dados) # Mostra as últimas 6 instâncias do conjunto de dados

summary(dados) # Sumário dos dados	

by(dados[, -16], dados$V16, summary) # Sumário dos dados por classe

summary(dados$V6)/length(dados[,1])*100 # Tabela de frequências

mean(na.omit(dados[,2]))  # Cálculo da média sendo necessário omitir os Missing Values no cálculo

table(dados$V1,dados$V16)# Tabela de cruzamentos
 	
sum(dados$V3>20) 

which.min(dados$V2)

attach(dados) # Facilita a indexação dos dados 

sum(V12=="f")  

table(V1=="a"&V2>12,V16)  

sort(V3)

detach(dados)

cor(na.omit(dados[,sapply(dados, is.numeric)])) 	# Matriz de correlações para as variáveis numéricas

Sumario.Nominais <- function(x) { 	# Função sumário para as variáveis nominais
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

pairs(dados[,sapply(dados, is.numeric)], main = "Concessão de       # Scatterplot das variáveis numéricas
Crédito", pch = 21, bg = c("red","green")[dados$V16])

panel.hist <- function(x, ...) {	# Representação gráfica das variáveis numéricas, através de um Scatterplot e de Histogramas
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
pairs(dados[,sapply(dados, is.numeric)], main = "Concessão de 
Crédito",pch = 21, bg = c("red", "green3")[dados$V16],
diag.panel = panel.hist, cex.labels = 1.5)

boxplot(dados$V14[dados$V16=="+"])

hist(dados[,3][dados$V16=="+"],prob = T, xlab = "",main = "Histograma de V3 ")

library(Hmisc) 	# É necessários carregar previamente esta livraria para utilizar o comando datadensity

datadensity(dados[,1:6])   #Aplicação da função para as 6 primeiras variáveis.
 
library(rpart)	# É necessários carregar previamente esta livraria para utilizar o comando rpart
ac <- rpart(dados$V16 ~ ., dados)	# Árvore de decisão 

plot(ac, margin = 0.1, branch = 0)

text(ac, use.n = T, fancy = F, all = T)
