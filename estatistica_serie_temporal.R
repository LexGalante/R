#base de dados, nativos no R
AirPassengers
#visualizando os dados limites
start(AirPassengers)
end(AirPassengers)
#plotando o grafico para visualizacao
plot(AirPassengers)
#agragação para visualizar somente evolucao
plot(aggregate(AirPassengers))
#grafico para visualizacao mensal
monthplot(AirPassengers)
#extraindo sub dados
extract <- window(AirPassengers, start=c(1960, 1), end = c(1960,12))
plot(extract)
#visualizando decomposicao
dec = decompose(AirPassengers)
#sazionalidade
plot(dec$seasonal)
#tendendia
plot(dec$trend)
#toda a decomposicao
plot(dec)
#previsao
media_serie = mean(AirPassengers)
media_ultimo_ano = mean(window(AirPassengers, start=c(1960, 1), end = c(1960,12)))
#intalando pacote forecast
install.packages("forecast")
library(forecast)
media_movel <- ma(AirPassengers, order = 12)
previsao <- forecast(media_movel, h=12)
plot(previsao)
X11()
previsao
arima <- auto.arima(AirPassengers)
arima
previsao_arima <- forecast(arima, h=12)
plot(previsao_arima)


