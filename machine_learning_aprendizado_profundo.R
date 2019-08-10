#instalando pacote necessario
install.packages("h2o")
#inicializacao basica do pacote, este pacote pode trabalhar com vários clusters porem aqui vamos utilizar apenas o basico
h2o.init()
#carregando os dados
treino <- h2O.importFile("dados/digits_train.csv")
teste <- h2O.importFile("dados/digits_test.csv")
#analises iniciais
dim(treino)
heade(treino)
summary(treino)
#fatorizando a classe
treino[,785] <- as.factor(treino[,785])
teste[,785] <- as.factor(teste[,785])
#utilizando a funcao de deep learning
modelo <- h20.deeplearning(x = colnames(treino[,1:784]),#analise
                           y = "C785",#classe
                           training_frame = treino,#base de treino
                           validation_frame = teste,#base de teste
                           distribution = "AUTO",#configuravel quando se trabalha com clusters
                           activation = "RectifierWithDropout",#ver documentacao valor default
                           hidden = c(64,64,64),#analises
                           sparse = TRUE,
                           epochs = 20)
#prevendo um novo digito
previsao <- h20.predict(modelo, newdata=treino[3,1:784])
