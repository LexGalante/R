#nome: support vector machine
#tipo: supervisionado
#baseado no teorema de cover(DADOS ESTÃO PROPENSOS A SEREM SEPARADOS LINEARMENTE EM ALTAS DIMENSÕES)
#busca pelo meno ponto de separação que apresentar menos erros, atraves do hiperplano
#mamixização da margem
#utiliza minimos quadrados para auxiliar na busca pelo hiperplano
#ignora outliers
#recebe dados não lineares e os transporta para um plano aonde se possa existir linearidade KERNEL
#utiliza pesos internos como arvore para melhor classificação
#

#carregando a biblioteca e1071
library(e1071)
#carregando dados
dados <- iris
classes <- dados$Species
dados_semclasse <- subset(iris, select = -Species)
#analises iniciais
colnames(dados)
summary(dados)
#construcao do modelo
modelo.svm <- svm(Species ~ ., data = dados)
#plot do modelo
plot.svm(modelo.svm)
previsao <- predict(modelo.svm, dados[1:4])
confusao <- table(dados$Species, previsao)
verdadeiro_positivo <- as.numeric(confusao[1] + confusao[,2][2] + confusao[,3][3])
falso_positivo <- as.numeric(confusao[,2][3] + confusao[,3][2])
acuracia <- (verdadeiro_positivo) / sum(confusao) * 100

