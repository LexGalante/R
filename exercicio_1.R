#defina A renda media do percapta do estado em relacao a media de escolaridade do estado

#y = renda
#x = escolaridade

#ou seja renda ~ escolaridade
mec <-  data.frame(
  row.names  =      c("RR","AC","PA","TO","MA","SE","BA","AL","SP","ES","SC","PR","GO","DF","AP","RO","AM","PB","RN","PI","PE","CE","RJ","MG","RS","MT","MS"),
  escolaridade = c(5.7, 4.5 ,4.7 ,4.5 ,3.6,4.3 , 4.1 ,3.7 ,6.8 ,5.7 ,6.3 ,6.0 ,5.5 ,8.2 ,6.0 ,4.9 ,5.5 ,3.9 ,4.5 ,3.5 ,4.6 ,4.0 ,7.1 ,5.4 ,6.4,5.4,5.7),
  renda =        c(685,526  ,536, 520 ,343,462 ,460  ,454 ,1076,722 ,814 ,782 ,689 ,1499,683 ,662 ,627 ,423 ,513 ,383 ,517 ,448 ,970 ,681,800,775,731)
)
#veja os gráficos de dispersao
plot(mec$renda ~ mec$escolaridade)
#Analise as correlacoes
correlacao <- cor(mec$escolaridade, mec$renda)
#plote os histogramas de renda e escolaridade
hist(mec$escolaridade)
hist(mec$renda)
#teste de normalidade
shapiro.test(mec$escolaridade)
#regressão linear
modelo.linear <- lm(renda ~ escolaridade, data = mec)
#quais são os pontos com maior alavancagem
boxplot(mec$renda)
sort(influence(modelo.linear)$hat)
#qual o coeficiente de determinacao (r^2)?
coeficiente_determinacao <- summary(modelo.linear)$r.squared
#verifique os resíduos
library(hnp)
hnp(modelo.linear, xlab = 'N(0,1)', ylab = 'Resíduos', main = modelo.linear$call$formula)
#A regressão linear parece ser uma boa escolha? Por que?
#não pois existe um ponto de alavacagem que abrirá uma maior margem de erro
#Qual das distribuíções que estudamos tem uma semelhança com os dados mostrados pelo histograma de renda?
#na minha opnião a distribuição gama seria a mais próxima
#Faça uma glm com essa regressão
modelo.glm <- glm(renda ~ escolaridade,
                  family = Gamma,
                  data = mec)
#Agora estimes os valores de renda para os valores de escolaridade utilizando os dois modelos e plot os gráficos
previsoes_lm <- predict(modelo.linear,
                        data = data.frame(escolaridade=mec$escolaridade))
previsoes_glm <- predict(modelo.glm,
                        data = data.frame(escolaridade=mec$escolaridade),
                        type = "response")
#mostre no mesmo gráfico os valores observados, preditos (modelo 1) em vermelho, e preditos no modelo 2 em verde
#utilize as funçoes plot(), points() e points()
plot(mec$renda ~ mec$escolaridade, main = 'MEC')
#abline(modelo.linear, col = "red")
points(previsoes_lm,  col = 'red')
points(previsoes_glm,  col = 'green')
#Compare os modelos com a funcao AIC e informe qual modelo vc escolhe e por que?
analise <- data.frame(
  colnames = c("linear", "gama"),
  aic = c(AIC(modelo.linear), AIC(modelo.glm)),
  verossimilhança = c(logLik(modelo.linear), logLik(modelo.glm)) 
)
#pela menor valor do teste AIC, maior verossimilhança, pelo estudo do histograma acredito que o modelo
#que mehor atende este problema é o glm com distribuição gama
