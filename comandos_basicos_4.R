#OBJETIVO:indificar atributos mais relevantes para modelo
#necessário pacote e1071
#carregando dados historicos para analise do modelo
credito <- read.csv("/dados/credit.csv",
                    sep = ",",
                    header = TRUE)
#carregando dados historicos para analise do modelo
credito <- read.csv("/dados/Credit.csv",
                    sep = ",",
                    header = TRUE)
#OBJETIVO: Construir um modelo para prever se um cliente será um bom pagador
#vetor para retira da amostragem da bases
amostra <- sample(2, 1000, replace = TRUE, prob = c(0.7, 0.3))
#separando a base de treino
credito_treino <- credito[amostra==1,]
#separando a base e teste
credito_teste <- credito[amostra==2,]
#criando um modelo de support vector machine
modelo <- svm(class ~ ., data = credito_treino)
predicao <- predict(modelo, credito_teste)
confusao <- table(credito_teste$class, predicao)
#verificando resultados do modelo
acuracia <- (confusao[1] + confusao[4]) / sum(confusao) * 100
erro <- (confusao[2] + confusao[3]) / sum(confusao) * 100
#instalando pacote FSelector
install.packages("FSelector")
library(FSelector)
#selecionando os melhores atributos
atributos <- random.forest.importance(class ~ ., credito)
#removendo atributos irrelevantes
modelo <- svm(class ~ checking_status +
                      duration +
                      credit_history +
                      purpose +
                      credit_amount +
                      savings_status,
              data = credito_treino)
predicao1 <- predict(modelo, credito_teste)
confusao1 <- table(credito_teste$class, predicao1)
acuracia1 <- (confusao1[1] + confusao1[4]) / sum(confusao1) * 100
erro1 <- (confusao1[2] + confusao1[3]) / sum(confusao1) * 100

