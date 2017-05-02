#Análise da Série

dados2000<-read.csv('tablea.csv', head=T, sep=",")
dados1_2000<-dados2000[,-1]
dadosok2000<-as.data.frame(dados1_2000[,1:4])
dadosok<-dadosok2000[,4]
par(mfrow=c(3,1))
ts.plot(dadosok,main="Série Temporal 2000 - 2008")
acf(dadosok)
pacf(dadosok)
mod1<-ar(dadosok, order.max = 5)
preds.ar3 <- predict(mod1, n.ahead = 5)
preds.ar3_pred<-preds.ar3$pred
pres.ar3_se<-preds.ar3$se
U<-preds.ar3_pred+1.96*preds.ar3$se
L<-preds.ar3_pred-1.96*preds.ar3$se
ts.plot(dadosok, xlim=c(1980,2010),ylab="Valor de Fecho",xlab="Observações",
type="o",main="Previsões utilizando o modelo AR(3)")
lines(preds.ar3_pred, col="Red",type="o")
lines(U,col="blue",type="o")
lines(L,col="blue",type="o")
abline(v=2005.5,lty="dotted")

par(mfrow=c(3,1))
ts.plot(diff(dadosok,2),main="Série Temporal 2000 - 2008")
acf(diff(dadosok,2))
pacf(diff(dadosok,2))

### Previsão usando o ARIMA(0,0,1)
treino<-dadosok[1:2000]
teste<-dadosok[2001:2005]
treino1<-diff(treino,2)
fit<-arima(treino1,order=c(0,0,1))
tsdiag(fit)
ax<-predict(fit,n.ahead=5)
ts.plot(treino1, xlim=c(1990,2008))
lines(ax$pred, col="red")
x<-ax$pred[1]+(treino[2000]-treino[1999])+treino[2000]
y<-ax$pred[2]+(x-treino[2000])+x
z<-ax$pred[3]+(y-x)+y
u<-ax$pred[4]+(z-y)+z
v<-ax$pred[5]+(u-z)+u
previ<-c(x,y,z,u,v)
previ1<-cbind(x,y,z,u,v)
U1<-previ+1.96*ax$se
L1<-previ-1.96*ax$se
sum((teste-previ)^2)

### Previsão usando o AR(3) 
mod1<-ar(treino, order.max = 5)
preds.ar3 <- predict(mod1, n.ahead = 5)
preds.ar3_pred<-preds.ar3$pred
pres.ar3_se<-preds.ar3$se
U2<-preds.ar3_pred+1.96*preds.ar3$se
L2<-preds.ar3_pred-1.96*preds.ar3$se
ts.plot(treino, xlim=c(1980,2005),ylab="Valor de Fecho",xlab="Observações",
type="o",main="Previsões utilizando o modelo AR(3)")
lines(preds.ar3_pred, col="Red",type="o")
lines(U2,col="blue",type="o")
lines(L2,col="blue",type="o")
abline(v=2000.5,lty="dotted")
sum((preds.ar3_pred-teste)^2) 

