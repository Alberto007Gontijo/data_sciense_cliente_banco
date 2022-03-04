#Leitura da base de dados
base <- read.csv('credit_data.csv')

#excluindo coluna clientid
base$clientid <- NULL

# valores inconsistentes
base$age <- ifelse(base$age < 0, 40.92, base$age)

#tratamento dos valores NA
base$age <- ifelse(is.na(base$age), mean(base$age, na.rm = TRUE), base$age)


#escalonar valores (dar o mesmo peso)
base[, 1:3] <- scale(base[, 1:3])

#install.packages('caTools')
library('caTools')

divisao <- sample.split(base$default, SplitRatio = 0.75)
base_treinamento <- subset(base, divisao == TRUE)
base_teste <- subset(base, divisao == FALSE)

#install.packages('class')
library(class)

previsoes = knn(train = base_treinamento[, -4], test = base_teste[, -4], cl = base_treinamento[, 4], k = 5)
print(previsoes)

matriz_confusao = table(base_teste[, 4], previsoes)
matriz_confusao

library(caret)
confusionMatrix(matriz_confusao)
