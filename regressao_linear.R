#conjunto de dados cars, está nativo no r
#trata-se da velocidade x distancia de frenagem
#análises exploratorias
dim(cars)
summary(cars)
head(cars)
#correlação
cor(cars)
#criando modelo linear
modelo = lm(speed ~ dist, data = cars)
#visualizando modelo
plot(modelo)
#criando gráfico do modelo
plot(speed ~ dist, data = cars)
#setando a linha do modelo
abline(modelo)
#prevendo um dado
#verificando coeficientes do modelo
modelo$coefficients
#calculando a previsao com uma velocidade de 22
previsao <- modelo$coefficients[1] + (modelo$coefficients[2] * 22)
#prevendo usando a funcao de predicao
predict(modelo, data.frame(dist=22))
predict(modelo, data.frame(dist=50))
#verificando o modelo
summary(modelo)
#informações uteis do modelo
modelo$residuals#valores residuais erros quanto a linha de ajuste
modelo$fitted.values#valor para perfeicao com a linha de previsao
modelo$

