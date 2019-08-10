#Para obter uma linha de regressao efetuamos os seguintes calculos
#1 -> Correlaçao
#2 -> Inclinação
#3 -> Interceptação
#4 -> Previsão
#estudos iniciais
colnames(mtcars)
dim(mtcars)
summary(mtcars)
#verificando correlações
cor(mtcars[1:4])
#criando modelo de regressao simples
modelo <- lm(mpg ~ disp, data=mtcars)
#verificando o coeficiente de determinação, correlação ao quadrado
summary(modelo)$r.squared
#verificando o coeficiente de determinação ajustado, correlação ao quadrado
summary(modelo)$adj.r.squared
#gerando grafico do modelo
plot(mpg~disp, data=mtcars)
abline(modelo)
#previsões
predict(modelo, data.frame(disp=200))
#criando modelo regressão multipla
modelo <- lm(mpg ~ disp + hp + cyl, data=mtcars)
#verificando o coeficiente de determinação ajustado, correlação ao quadrado
summary(modelo)$adj.r.squared
#previsao
predict(modelo, data.frame(disp=200, cyl=3, hp=108))
