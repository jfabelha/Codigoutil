
air <- sqlQuery(channel = 1, select * from [Sheet1$])

lnair <- log(air)

lnair <- ts(lnair, deltat=1/12)

air.modelo <- arima(lnair, order=c(0,1,1), seasonal=list(order=c(0,1,1)))

predict(air.modelo, n.ahead=12)$pred

exp(predict(air.modelo, n.ahead=12)$pred)

exp(predict(air.modelo, n.ahead=12)$pred+1.96*predict(air.modelo, n.ahead=12)$se)
exp(predict(air.modelo, n.ahead=12)$pred-1.96*predict(air.modelo, n.ahead=12)$se)
