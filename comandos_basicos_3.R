x = runif(n=10,min=0,max=10)
y = runif(n=10,min=0,max=10)
# chamando plotagem
plot(x,y)
# abrir em visulizador avançado
# X11()
# adicionando linha media
abline(h=mean(y), col="blue")
abline(v=mean(x), col="red")
title("GRÁFICO TESTE")
# customizando gráfico
for (i in 1:length(x)) {
  if(y[i] > mean(y)){
    segments(x0 = x[i],x1 = x[i], y0 = mean(y),y1 = y[i], col = "blue")
    points(x = x[i],y = y[i], pch = 15)  
  }else{
    arrows(x0 = x[i],x1 = x[i], y0 = mean(y),y1 = y[i], length = .15, col = "red")
  }
}
points(x=x[2], y=y[2])
# curvas
x = seq(from = -1, to = 1 , by = 0.1)
y = x^2
plot(x,y)
curve(x^2, from = -1 , to = 1, add = TRUE, col = "red" )
# histograma
hist(runif(100, min = 0, max = 1))
hist(runif(100, min = 0, max = 1), freq=FALSE)
hist(runif(100, min = 0, max = 1), density=TRUE)
# histrograma com linha de desvio
set.seed(123)
x = rnorm(n = 10000, mean = 0, sd = 1)
hist(x, freq = F)
abline(v = 0, col = "red")
abline(v = 1.96, col = "blue", lty = 3 )
abline(v = -1.96, col = "blue", lty = 3 )
curve(expr = dnorm(x, mean= 0 , sd = 1), add = TRUE, col = "red"  )