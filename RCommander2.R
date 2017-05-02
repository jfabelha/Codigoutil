air <- sqlQuery(channel = 2, select * from [Sheet1$])

air <-ts(air, deltat=(1/12))

air.modelo<-arima(air, order=c(0,1,1), seasonal=list(order=c(0,1,1)))

arima(air, order=c(1,1,1), seasonal=list(order=c(0,1,1)))

arima(air, order=c(1,1,1), seasonal=list(order=c(1,1,1)))
?arima

variancias e covaria
air.modelo$var.coef

covariancia
air.modelo$var.coef[1,2]/(air.modelo$var.coef[1,1]*air.modelo$var.coef[2,2])^0.5

residuos<-air.modelo$residuals

plot(residuos)
par(mfrow=c(2,1))
acf(residuos, lag.max=36)
pacf(residuos, lag.max=36)

Teste da fac dos residuos (teste global)

Box.test(residuos,lag=36, type="Ljung-Box")





