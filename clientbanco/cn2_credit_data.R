base <- read.csv('credit_data.csv')
#apagar a coluna inteira cliente
base$clientid <- NULL

#valores inconsistentes
base$age <- ifelse(base$age < 0, 40.92, base$age)

#tratamento dos valores NA
base$age <- ifelse(is.na(base$age), mean(base$age, na.rm = TRUE), base$age)

#escalonar valores (dar o mesmo peso)
#base[, 1:3] <- scale(base[, 1:3])

#install.packages('caTools')
library('caTools')
set.seed(1)
divisao <- sample.split(base$default, SplitRatio = 0.75)
base_treinamento <- subset(base, divisao == TRUE)
base_teste <- subset(base, divisao == FALSE)

#install.packages('RoughSets')
library(RoughSets)

dt_treinamento = SF.asDecisionTable(dataset = base_treinamento, decision.attr = 4)
dt_teste = SF.asDecisionTable(dataset = base_teste, decision.attr = 4)
#transformando numeros em strings
#selecionando o numero de intervalos em 4
intervalos = D.discretization.RST(dt_treinamento, nOfIntervals = 4)

dt_treinamento = SF.applyDecTable(dt_treinamento, intervalos)
dt_teste = SF.applyDecTable(dt_teste, intervalos)

classificador = RI.CN2Rules.RST(dt_treinamento, K = 5)

previsoes = predict(classificador, newdata = dt_teste[-4])
previsoes

library(caret)
matriz_confusao = table(dt_teste[, 4], unlist(previsoes))
print(matriz_confusao)

confusionMatrix(matriz_confusao)
