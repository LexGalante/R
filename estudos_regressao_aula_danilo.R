summary(mtcars)
plot(mpg ~ hp, data = mtcars)
#funcao para evitar digitar mtcars todo momento
attach(mtcars)
#caclulo da covarianca
covaricanca <- cov(mpg,hp)
#correlacao
correlacao <- cor(mpg,hp)
#exibindo dados de covarianca e correlação
sprintf("Covarianca:%f  Correlacao:%f",covaricanca,correlacao)
#histograma de mpg
hist(mpg)
#primeiros dados do dataframe
head(mtcars)
#nomes das colunas
colnames(mtcars)
#multiplas correlações
correlacoes <- cor(mtcars[,1:4])
#verifica se há normalidade
shapiro.test(mpg)$p.value
#verifica se há igualdade nas variancas
fligner.test(mpg ~ hp, data = mtcars)
#criando a regressao linear
modelo.linear <- lm(mpg ~ hp, data = mtcars)
sd(mpg)
summary(modelo.linear)
coefficients(modelo.linear)
modelo.linear$coefficients
#plotar mpg(y) em funcao de hp (x)
plot(mpg ~ hp)
#plotar a reta do modelo
abline(modelo.linear, col = "red")
#plotar os valores estimados de mpg para o modelo
points(modelo.linear$fitted.values~ hp, col = "red", data = mtcars)
#valores estimados para o modelo
fitted.values(modelo.linear)
modelo.linear$fitted.values
#coeficiente de determinação
summary(modelo.linear)$r.squared
#coeficiente de determinacao Ajustado
summary(modelo.linear)$adj.r.squared
#teste hipotese para os desvios, como residuos não seguem uma distribuição normal este modelo não deve ser usado
shapiro.test(modelo.linear$residuals)
hist(modelo.linear$residuals,freq = F)
#novos valores de HP para a predição
novos_hp <- seq(from = 50 , to = 550 , by = 50)
#predicao
predicao <- predict(modelo.linear,data.frame(hp=novos_hp) )
#gráfico com os estimados
plot(predicao ~ novos_hp, col = "red", pch = 15,ylab = "Autonomia mpg", xlab = "Novos Valores de hp", main = "Predição")
#dados observados
points(mpg~hp)
#uma linha horizontal em 0
abline(h=0)
#calcula a média.
media.mpg <- mean(mtcars$mpg)
#faz a modelagem, utiliza-se apenas um paramentro, somente o interceptor
modelo.const <- lm(mpg ~ 1, data = mtcars )
summary(modelo.const)
#visualizar no gráfico
#gráfico com os estimados
plot(predicao ~ novos_hp, col = "red", pch = 15, ylab = "Autonomia mpg", xlab = "Novos Valores de hp", main = "Predição")
#dados observados
points(mpg~hp)
#linha da média
abline(modelo.const, col = "green")
#calculando analise de varianca entre os modelos
anova(modelo.const, modelo.linear,test = "Chisq")
#teste melhor modelo akaike, escolher coluna AIC menor valor
AIC(modelo.const, modelo.linear)
#foi identificado que nem um dos modelos criados é realemento efetico
#tendo em vista que sua acuracia e de mais ou menos 60%
#Por isso vamos verificar possibilidade de criar um modelo de regressão linear multivariada
boxplot(mpg ~ cyl, ylab="Autonomia mpg", xlab="Cilindros", data = mtcars)
#outro grafico
plot(mpg ~ hp , pch =cyl, col = cyl, data = mtcars)
legend(300,32,legend = c("4","6","8"), pch = c(4,6,8),  col = c("blue","pink","grey"), title = "Cilindros")
#verificando a correlação
multi.cor <- cor(mtcars[,c("mpg","hp","cyl")], method = "pearson")
#correlação base
multi.cor
#install.packages("corrplot")
#aqui está uma forma de exibir a correlação
library(corrplot)
corrplot.mixed(multi.cor, upper = "ellipse")
#regressão linear multivariada
modelo.multi <- lm(mpg ~ hp + cyl, data = mtcars)
#aqui temos nossa regressão depois vamos melhorar isso. Por hora apenas vamos deixar assim.
summary(modelo.multi)
#rˆ2 do modelo linear
summary(modelo.linear)$adj.r.squared
#rˆ2 do modelo multi variado
summary(modelo.multi)$adj.r.squared
#coeficiente de determinacao simples
summary(modelo.linear)$r.squared
#aqui é só para ver como sempre há um aumento maior no coeficiente de determinacao e que devemos utilizar o ajustado
summary(modelo.multi)$r.squared
coefficients(modelo.multi)
#grafico do modelo multilinear
plot(mpg ~ hp , pch =cyl, col = cyl, data = mtcars)

points(modelo.multi$fitted.values ~ hp, col = "red", pch = 18, data = mtcars)  
fun.reta <-  function(x, cyl){
  modelo.multi$coefficients[1]+
    modelo.multi$coefficients["cyl"]*cyl + 
    modelo.multi$coefficients["hp"]*x
}
curve(expr = fun.reta(x,4)  , from = 0, to = 500, add = T, col = 4)
curve(expr = fun.reta(x,6)  , from = 0, to = 500, add = T, col = 6)
curve(expr = fun.reta(x,8)  , from = 0, to = 500, add = T, col = 8)
legend(300,32,legend = c("4","6","8"), pch = c(4,6,8),  col = c("blue","pink","grey"), title = "Cilindros")
#Intervalo de confianca
intervalo_confianca <- confint.default(modelo.multi, level = 0.95)
cbind(intervalo_confianca,modelo.multi$coefficients)
#comparando modelos com anova
anova(modelo.const, modelo.linear, modelo.multi,test = "Chisq")


#SELEÇÃO DE ATRIBUTOS
#install.packages("car")
library(car)
scatterplotMatrix(mtcars[ , c("mpg" , "cyl" , "disp","hp" , "wt" ,"qsec")], lwd = 3)
#listando as colunas para escolher as que interessam
colnames(mtcars)
#Escolha um grau de significancia
#modelo linear multi variado
modelo.multi2.0 <- lm(mpg ~ cyl + disp + hp + wt + qsec, data = mtcars)
#observa-se a importancia das variáveis no modelo
summary(modelo.multi2.0)
#função para analise de remoçao de variavel
variavel_menor_backward <- function(modelo, dados, coef = 0.975){
  #modelo <- modelo.multi2
  #summary(modelo)
  #coef <- 0.975
  #dados<- mtcars
  
  #obtém-se os graus de liberdade
  modelo.coef <- summary(modelo)$coefficients;modelo.coef
  if(nrow(summary(modelo)$coefficients) == 2 ){
    return(sprintf("Só há uma variável, nada a remover"))
  }
  gl<-nrow(dados) - length(coefficients(modelo));gl
  
  #calcula o quantil
  quantile  <- qt(coef,gl);quantile
  
  modelo.coef <- modelo.coef[-1,];modelo.coef
  #menor_valor <- min (abs(summary(modelo)$coefficients[, "t value"])) ; menor_valor
  menor_valor <- min (abs(modelo.coef[, "t value"])); menor_valor
  
  #obtém os valores que são menor que o quantil
  nome <- names(which(abs(modelo.coef[,"t value"]) == menor_valor))
  
  if (quantile > menor_valor) {
    return(sprintf("Remova %s, t %f  < quantil: %f", nome,menor_valor,quantile))
  }
  return(sprintf("Remova %s, t %f  < quantil: %f", nome,menor_valor,quantile))
}

variavel_menor_backward(modelo.multi2.0, mtcars)
#nova formula sem qsec
modelo.multi2.1 <- lm(mpg ~ cyl + disp + hp + wt , data = mtcars)
summary(modelo.multi2.1)
#nova formula sem disp
modelo.multi2.2 <- lm(mpg ~ cyl + hp + wt , data = mtcars)
summary(modelo.multi2.2)
#verificando se variaveis que podem sert removidas
variavel_menor_backward(modelo.multi2.2, mtcars)
#nova formula sem hp
modelo.multi2.3 <- lm(mpg ~ cyl + wt , data = mtcars)
summary(modelo.multi2.3)
#nova formula sem cyl
modelo.multi2.4 <- lm(mpg ~  wt , data = mtcars)
summary(modelo.multi2.4)
variavel_menor_backward(modelo.multi2.4, mtcars)
#covarianca
vcov(modelo.multi2.0)
vcov(modelo.multi2.3)
#analisando a diferença entre os modelos
anova(modelo.multi2.0, modelo.multi2.1, modelo.multi2.2, modelo.multi2.3, modelo.multi2.4,test = "Chisq")
AIC(modelo.multi2.0, modelo.multi2.1, modelo.multi2.2, modelo.multi2.3)#escolher com menor valor de erro AIC
#ESTUDO DO MELHOR MODELO
library(hnp)
layout(matrix(c(1,2,3,4),2,2))
#Grafico Normal das Probabilidades dos modelos
hnp(modelo.multi2.0, xlab = 'N(0,1)', ylab = 'Resíduos', main = modelo.multi2.0$call$formula)
#analise de residuos
hnp(modelo.multi2.1, xlab = 'N(0,1)', ylab = 'Resíduos', main = modelo.multi2.1$call$formula)
#analise de residuos
hnp(modelo.multi2.2, xlab = 'N(0,1)', ylab = 'Resíduos', main = modelo.multi2.2$call$formula)
#analise de residuos
hnp(modelo.multi2.3, xlab = 'N(0,1)', ylab = 'Resíduos', main = modelo.multi2.3$call$formula)
#comparativo de resultado AIC x VEROSSIMILHANÇA
analise <- data.frame(
  modelos = c('2.0', '2.1', '2.2', '2.3'),
  aic = c(AIC(modelo.multi2.0), AIC(modelo.multi2.1),AIC(modelo.multi2.2),AIC(modelo.multi2.3)),
  verossimilhança = c(logLik(modelo.multi2.0), logLik(modelo.multi2.1),logLik(modelo.multi2.2),logLik(modelo.multi2.3))
)
#lib para ajudar na seleção de atributos
library(MASS)
modelo.multi.both <- step(modelo.multi2.0, direction = "both")#adiciona atributos e analise a eficacia dele para modelo
modelo.multi.forward <- step(modelo.multi2.0, direction = "forward")#retira atributos
#ponto de alavancagem, instancias que afetam muito o atributo analisado
sort(influence(modelo.multi2.3)$hat)
#A soma das alavancagens é igual ao número de variáveis
sum(influence(modelo.multi2.3)$hat)
#grafico do modelo selecionado
layout(matrix(c(1,2,3,4),2,2))
X11()
plot(modelo.multi2.3)

#REGRESSÃO LINEAR GENERALIZADA - GLM

## BINOMIAL ##
#Análise de insetos mortos
#criando o data frame
insetos <- data.frame(
  dose   = c(0.0, 2.6, 3.8, 5.1, 7.7, 10.2),
  total  = c(49,  50,  48,  46,  49,  50),
  mortos = c(0,   6,   16,  24,  42,  44))
#Nossa variável resposta
insetos$proporcao <- NULL
insetos$proporcao <- insetos$mortos / insetos$total 
#insetos <- cbind(insetos, proporcao)
#limpando
head(insetos)
#Olhar o comportamentomento do nosso Y
hist(insetos$proporcao, freq = F)
#distribuição binomial entre 0 e 1
# Plotando o gráfico da variável resposta por dose
X11()
plot(proporcao ~ dose, data = insetos , xlim = c(0,12), ylim = c(0,1), main = "Insetos Mortos (%) x Dose")
#criando o modelo
# Fazendo a regressão do modelo binomial
modelo.logit <- glm(proporcao ~ dose, family = binomial(link = "logit"), data = insetos)
# Utilizar outra função de ligação probit
modelo.probit <- glm(proporcao ~ dose, family = binomial(link = "probit"), data = insetos)
#aqui os coeficientes de beta
coef(modelo.logit)
#novas dados para estimar a proporção de insetos mortos
new.doses <- seq(0,12, by=0.1)
#estimando os valores
new.prob.logit <- predict(modelo.logit,data.frame(dose=new.doses))
new.prob.probit <- predict(modelo.probit,data.frame(dose=new.doses), type = "response")
#funcao inversa
mu <- function(t) {exp(t)/(1+exp(t))}
#vendo os gráficos
X11()
plot(proporcao ~ dose, data = insetos , xlim = c(0,12), ylim = c(0,1), main = "Insetos Mortos (%) x Dose")
lines(new.doses, mu(new.prob.logit),  col = "blue")
points(insetos$dose, modelo.logit$fitted.values, pch = "*", col = "blue")
lines(new.doses, new.prob.probit, col = "green")
legend(0,1, legend = c("observados","logit","probit"),  col = c("black","blue","green"), pch=c("o","*","line")  )
#comparando os modelos
anova(modelo.logit, modelo.probit , test = "Chisq")
exp(modelo.logit$coefficients[2])
exp(confint.default(modelo.logit))

## POISSON ##
bacterias <- c(175, 108,95,82,71,50,49,31,28,17,16,11)
tempo <- 1:12
plot(bacterias ~ tempo , xlim = c(0,20) , ylim= c(0,180), type = "h", ylab = "Quantidade de Bactérias", xlab = "Unidade de tempo")

modelo <- glm(bacterias ~ tempo, family = poisson)
x <- seq(0:20)
preditos <- predict(modelo, data.frame(tempo = x))

f<- function(l){exp(l)}
points(x , f(preditos), col = "red")
lines(x , f(preditos), col = "red", lty = 2)

preditos <- predict(modelo, data.frame(tempo = x), type = "response", interval = "prediction")

summary(modelo)

int.confianca <- exp(confint.default(modelo))
int.confianca

## GAMA ##
#Direto para o modelo
modelo.g.mpg <- glm(mpg ~ hp ,family = Gamma, data = mtcars)

#Preditor linear multivariado
modelo.g.mpg.2 <- glm(mpg ~ hp + cyl,family = Gamma, data = mtcars)

#Aqui temos a gaussiana inversa, Outra distribuíção para dados assimétricos
modelo.ig.mpg <- glm(mpg ~ hp + cyl,family = inverse.gaussian, data = mtcars)

#valores de predição de 1 até 1000
n.hp <- 1:1000

#vamos apra os gráficos
X11()
plot(mpg ~ hp , data = mtcars, ylim = c(-5,30), xlim=c(0,1000))
lines(n.hp,predict(modelo.linear, newdata = data.frame(hp = n.hp), type = "response"), col = "green")
#points(mtcars$hp,modelo.g.mpg$fitted.values, col = "red", pch = 14)
abline(h = 0)
lines(n.hp,predict(modelo.g.mpg, newdata = data.frame(hp = n.hp), type = "response"), col = "red")
lines(n.hp,predict(modelo.g.mpg.2, newdata = data.frame(hp = n.hp, cyl = 4), type = "response"), col = "blue")
lines(n.hp,predict(modelo.g.mpg.2, newdata = data.frame(hp = n.hp, cyl = 6), type = "response"), col = "blue")
lines(n.hp,predict(modelo.g.mpg.2, newdata = data.frame(hp = n.hp, cyl = 8), type = "response"), col = "blue")

#melhor modelo para problema
analise <- data.frame(
  colnames = c("linear", "gama 1", "gama 2", "I. gaussian"),
  aic = c(AIC( modelo.linear),AIC(modelo.g.mpg),AIC(modelo.g.mpg.2),AIC(modelo.ig.mpg)),
  verossimilhança = c(logLik(modelo.linear),logLik(modelo.g.mpg),logLik(modelo.g.mpg.2),logLik(modelo.ig.mpg)) 
)

anova(modelo.g.mpg,modelo.g.mpg.2,modelo.ig.mpg ,modelo.linear, test = "Chisq")

layout(matrix(c(1,2,3,4),c(2,2)))
hnp(modelo.g.mpg.2, xlab = 'N(0,1)', ylab = 'Resíduos')

plot(modelo.g.mpg.2)
