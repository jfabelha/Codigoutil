Dados <- sqlQuery(channel = 1, select * from [ar2$])
serie<-ts(Dados)

plot(serie)

par(mfrow=c(2,1))
acf(serie, lag.max=30)
pacf(serie, lag.max=30)

acf(diff(serie), lag.max=30)
pacf(diff(serie), lag.max=30)


air <- sqlQuery(channel = 2, select * from [Sheet1$])
serie2<-ts(air)
plot(serie2)
lnserie2<-log(serie2)
plot(lnserie2)

par(mfrow=c(2,1))
acf(lnserie2, lag.max=30)
pacf(lnserie2, lag.max=30)

par(mfrow=c(2,1))
acf(diff(lnserie2), lag.max=30)
pacf(diff(lnserie2), lag.max=30)

par(mfrow=c(2,1))
acf(diff(diff(lnserie2),lag=12), lag.max=30)
pacf(diff(diff(lnserie2),lag=12), lag.max=30)
