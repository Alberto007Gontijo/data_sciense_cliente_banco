base = read.csv('credit_data.csv')

base$clientid = NULL

base$age = ifelse(base$age < 0, 40.92, base$age)

base$age = ifelse(is.na (base$age), mean(base$age, na.rm = TRUE), base$age)

base[, 1:3] = scale(base[, 1:3])

#encode da classe
base$default = factor(base$default, levels = c(0,1))

library(caTools)

set.seed(1)
divisao = sample.split(base$income, SplitRatio = 0.75)
print(divisao)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)

library(rpart)
classificador = rpart(formula = default ~ ., data = base_treinamento)
print(classificador)

library(rpart.plot)
rpart.plot(classificador)
previsoes = predict(classificador, newdata = base_teste[-4], type = 'class')

matriz_confusao = table(base_teste[,4], previsoes)
print(matriz_confusao)
library(caret)
confusionMatrix(matriz_confusao)
