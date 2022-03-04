#banco de dados
base = read.csv('credit_data.csv')
#apagando a coluna clientes
base$clientid = NULL
#Substituindo valores inconsistentes pela media
base$age = ifelse(base$age < 0, 40.92, base$age)
#Substituindo valores faltantes pela media
base$age = ifelse(is.na (base$age), mean(base$age, na.rm = TRUE), base$age)
#Escalonamento
base[, 1:3] = scale(base[, 1:3])

#encode da classe
base$default = factor(base$default, levels = c(0,1))

#dividindo dados de treinamentos e os de teste
library(caTools)

set.seed(1)
divisao = sample.split(base$income, SplitRatio = 0.75)
print(divisao)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)

#install.packages('randomForest')
library(randomForest)
#Para evitar respostas diferentes usamos o set.seed(1)
set.seed(1)
#definindo no final da linha de codigo o numero de arvores a serem criadas
classificador = randomForest(x = base_treinamento[-4], y = base_treinamento$default, ntree = 40)
previsoes = predict(classificador, newdata = base_teste[-4])

previsoes

matriz_confusao = table(base_teste[, 4], previsoes)
print(matriz_confusao)

library(caret)
confusionMatrix(matriz_confusao)
#O random forest não funciona com dados faltantes
