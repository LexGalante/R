#preparando dados
tratamento <- read.csv(file.choose(), sep = ";", header = TRUE)
fix(tratamento)
#grafico para visualizar a nova geral
boxplot(tratamento$Horas ~ tratamento$Remedio)
#analise de varianca simples
an <- aov(Horas ~ Remedio, data = tratamento)
summary(an)
#analise de varianca complexa
anc <- aov(Horas ~ Remedio + Sexo, data = tratamento)
summary(anc)
#teste de tukey para identificar aonde existe a variancia
tukey <- TukeyHSD(anc)
plot(tukey)