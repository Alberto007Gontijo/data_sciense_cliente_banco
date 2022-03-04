

base <- read.csv('credit_data.csv')

#apagar a coluna inteira
base$clientid <- NULL

# valores inconsistentes
base$age <- ifelse(base$age < 0, 40.92, base$age)

#tratamento dos valores NA
base$age <- ifelse(is.na(base$age), mean(base$age, na.rm = TRUE), base$age)

#escalonar valores (dar o mesmo peso)
base[, 1:3] <- scale(base[, 1:3])

#install.packages('caTools')
library('caTools')
set.seed(1)
divisao <- sample.split(base$default, SplitRatio = 0.75)
base_treinamento <- subset(base, divisao == TRUE)
base_teste <- subset(base, divisao == FALSE)
