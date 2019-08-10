#instalando pacotes necessários
#e1071
install.packages("e1071", dependencies = TRUE)
library(e1071)
credito <- read.csv("/dados/credit.csv",
                    sep = ",",
                    header = TRUE)
#verificando os dados
head(credito)
dim(credito)
#OBJETIVO: Construir um modelo para prever se um cliente será um bom pagador
#vetor para retira da amostragem da bases
amostra <- sample(2, 1000, replace = TRUE, prob = c(0.7, 0.3))
#separando a base de treino
credito_treino <- credito[amostra==1,]
#separando a base e teste
credito_teste <- credito[amostra==2,]
#criando o modelo
#da lib e1071 vamos utilizar a rede baysiana
#primeiro atributo são os atributos que o modelo irá utilizar . sginifica que serão usados todos
#segundo atributo é base de dados
modelo <- naiveBayes(class ~ . , credito_treino)
#avaliando o desempenho do modelo
teste <- predict(modelo , credito_teste)
#criando a matriz de confusão
confusao <- table(credito_teste$class, teste)
#verificando resultados do modelo
acuracia <- (confusao[1] + confusao[4]) / sum(confusao) * 100
erro <- (confusao[2] + confusao[3]) / sum(confusao) * 100
#excutando modelo com dados de produção
cliente <- read.csv("/dados/novo_credit.csv",
                    sep = ",",
                    header = TRUE)
cliente_previsao <- predict(modelo, cliente)

