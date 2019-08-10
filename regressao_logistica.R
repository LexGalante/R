#lendo base de dados
eleicao <- read.csv(file.choose(), sep = ";", header = TRUE)
#ajustes na base
fix(eleicao)
#verificando grafico de dispersao
plot(eleicao$DESPESAS, eleicao$SITUACAO)
summary(eleicao)
#verificando correlacao entre dispesas e situacao
cor(eleicao$DESPESAS, eleicao$SITUACAO)
#criando modelo 
modelo = glm(SITUACAO ~ DESPESAS, data=eleicao, family = "binomial")
#sumarizacao do modelo
summary(modelo)
plot(eleicao$DESPESAS, eleicao$SITUACAO, col='red', pch=20)
points(eleicao$DESPESAS, modelo$fitted.values, pch=4)
#preparando previsao
previsao <- read.csv(file.choose(), sep = ";", header = TRUE)
previsao$RESULT = predict(modelo, newdata = previsao, type = "response")
previsao$RESULT
fix(previsao)