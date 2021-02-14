library(readr)
library(GGally)
library(MASS)
library(class)
library(dplyr)
library(tidyverse)
library(caret)
library(boot)
library(mlbench)
library(modelr)
library(broom)
library(leaps)

#CARREGAR DATASET

setwd("C:/Program Files/R")
forestFires<- read_csv("forest_fires_dataset.csv")

#FIM CARREGAR DATASET
###############################################################################

#CONVERTER MES PARA INT(CATEGORIZAR)
unique(forestFires$month)

forestFires$month[forestFires$month == 'mar'] <- 3      #março
forestFires$month[forestFires$month == 'oct'] <- 10     #outubro
forestFires$month[forestFires$month == 'aug'] <- 8      #agosto
forestFires$month[forestFires$month == 'sep'] <- 9      #setembro
forestFires$month[forestFires$month == 'apr'] <- 4      #abril
forestFires$month[forestFires$month == 'jun'] <- 6      #junho
forestFires$month[forestFires$month == 'jul'] <- 7      #julho
forestFires$month[forestFires$month == 'feb'] <- 2      #fevereiro
forestFires$month[forestFires$month == 'jan'] <- 1      #janeiro
forestFires$month[forestFires$month == 'dec'] <- 12     #dezembro
forestFires$month[forestFires$month == 'may'] <- 5      #maio
forestFires$month[forestFires$month == 'nov'] <- 11     #novembro

forestFires$month <- as.integer(forestFires$month) 

#FIM CONVERTER MES PARA INT
###############################################################################

#CONVERTER DIA SEMANA PARA INT(CATEGORIZAR)
unique(forestFires$day)

forestFires$day[forestFires$day == 'fri'] <- 6  #sexta
forestFires$day[forestFires$day == 'tue'] <- 3  #terça
forestFires$day[forestFires$day == 'sat'] <- 7  #sabado
forestFires$day[forestFires$day == 'sun'] <- 1  #domingo
forestFires$day[forestFires$day == 'mon'] <- 2  #segunda
forestFires$day[forestFires$day == 'wed'] <- 4  #quarta
forestFires$day[forestFires$day == 'thu'] <- 5  #quinta

forestFires$day <- as.integer(forestFires$day) 

#FIM CONVERTER DIA SEMANA PARA INT
##############################################################################

#VER MEDIAS/MAXIMOS/MINIMOS DE DADOS INTERESSANTER

mean(forestFires$area)
max(forestFires$area)
min(forestFires$area)

#FIM MEDIAS/MAXIMOS/MINIMOS
###############################################################################

#VISUALIZAR NUMERO DE INCENDIOS POR MES E DIA DA SEMANA

transform(table(forestFires$month), FreqRel = Freq/517)
transform(table(forestFires$day), FreqRel = Freq/517)

#FIM VISUALIZAR
###############################################################################

#GRAFICOS - SCATTER PLOT

#SCATTER PLOT MONTH/AREA
plot(forestFires$month, forestFires$area, 
     main = "SCATTER PLOT MONTH/AREA",
     xlab = "month",
     ylab = "area")

#SCATTER PLOT DAY/AREA
plot(forestFires$day, forestFires$area, 
     main = "SCATTER PLOT DAY/AREA",
     xlab = "day",
     ylab = "area")

#SCATTER PLOT FFMC/AREA
plot(forestFires$FFMC , forestFires$area, 
     main = "SCATTER PLOT FFMC/AREA",
     xlab = "FMC",
     ylab = "area")

#SCATTER PLOT DMC/AREA
plot(forestFires$DMC , forestFires$area, 
     main = "SCATTER PLOT DMC/AREA",
     xlab = "DMC",
     ylab = "area")

#SCATTER PLOT DC/AREA
plot(forestFires$DC , forestFires$area, 
     main = "SCATTER PLOT DC/AREA",
     xlab = "DC",
     ylab = "area")

#SCATTER PLOT ISI/AREA
plot(forestFires$ISI , forestFires$area, 
     main = "SCATTER PLOT ISI/AREA",
     xlab = "ISI",
     ylab = "area")

#SCATTER PLOT TEMP/AREA
plot(forestFires$temp , forestFires$area, 
     main = "SCATTER PLOT TEMP/AREA",
     xlab = "temp",
     ylab = "area")

#SCATTER PLOT RH/AREA
plot(forestFires$RH , forestFires$area, 
     main = "SCATTER PLOT RH/AREA",
     xlab = "RH",
     ylab = "area")

#SCATTER PLOT WIND/AREA
plot(forestFires$wind , forestFires$area, 
     main = "SCATTER PLOT WIND/AREA",
     xlab = "wind",
     ylab = "area")

#SCATTER PLOT RAIN/AREA
plot(forestFires$rain , forestFires$area, 
     main = "SCATTER PLOT RAIN/AREA",
     xlab = "rain",
     ylab = "area")

#SCATTER PLOT X/Y
plot(forestFires$X , forestFires$Y, 
     main = "SCATTER PLOT X/Y",
     xlab = "X",
     ylab = "Y")

AreaMasacrada <- table(forestFires$Y, forestFires$X)
print(AreaMasacrada)

#FIM GRAFICOS - SCATTER PLOT
###############################################################################

#GRAFICO BOX PLOT

#BOX PLOT X
boxplot(forestFires$X,
        main="BOX PLOT X")

#BOX PLOT Y
boxplot(forestFires$Y,
        main="BOX PLOT X")

#BOX PLOT MONTH
boxplot(forestFires$month, 
        main="BOX PLOT MONTH")

#BOX PLOT DAY
boxplot(forestFires$day, 
        main="BOX PLOT day")

#BOX PLOT FFMC
boxplot(forestFires$FFMC, 
        main="BOX PLOT FFMC")

#BOX PLOT DMC
boxplot(forestFires$DMC, 
        main="BOX PLOT DMC")

#BOX PLOT DC
boxplot(forestFires$DC, 
        main="BOX PLOT DC")

#BOX PLOT ISI
boxplot(forestFires$ISI, 
        main="BOX PLOT ISI")

#BOX PLOT temp
boxplot(forestFires$temp, 
        main="BOX PLOT temp")

#BOX PLOT RH
boxplot(forestFires$RH,
        main="BOX PLOT RH")

#BOX PLOT wind
boxplot(forestFires$wind, 
        main="BOX PLOT wind")

#BOX PLOT rain
boxplot(forestFires$rain, 
        main="BOX PLOT rain")

#BOX PLOT area
boxplot(forestFires$area, 
        main="BOX PLOT area")

#FIM GRAFICOS - BOX PLOT
###############################################################################
#CATEGORIZAR A ÁREA

#0 corresponde a pequeno
forestFires$area[forestFires$area <= 2] <- 0
#1 corresponde a medio
forestFires$area[forestFires$area > 2 & forestFires$area <= 20] <- 1      
#2 corresponde a grande
forestFires$area[forestFires$area > 20] <- 2          

forestFires$area <- as.character(forestFires$area) 

forestFires$area[forestFires$area == '0'] <- 'Incêndio Pequeno'      
forestFires$area[forestFires$area == '1'] <- 'Incêndio Médio'     
forestFires$area[forestFires$area == '2'] <- 'Incêndio Grande'      

#FIM CATEGORIZAR A ÁREA
###############################################################################

#CONTAR INCÊNDIOS POR CLASSIFICAÇÃO

table(forestFires$area)

#FIM CONTAR INCÊNDIOS
###############################################################################

#MATRIZ PLOT

#ENCONTRAR RELAÇÕES INTERESSANTES ENTRE OS DADOS
pairs(forestFires)
ggpairs(forestFires)

#RELAÇÃO DOS DADOS MAIS INTERESSANTES
pairs(~ DC + DMC + month, forestFires)

##FIM MATRIZ PLOT
###############################################################################

#GRAFICO CORRELAÇAO VARIAVEIS

ggcorr(forestFires,method = c("everything", "pearson"))
cor(forestFires)

#FIM GRAFICO CORRELAÇAO VARIAVEIS
###############################################################################

#TESTE HIPOTESES

#H0 = 0 Hipostese Nula -> 
#H1 # 0 Hipotese Alternativa ->


#FIM TESTE HIPOTESES
###############################################################################

#TESTE ANOVA

#Criar amostras da Populaçao
amostra1 <- sample(forestFires$area, 200)
amostra2 <- sample(forestFires$area, 200)
amostra3 <- sample(forestFires$area, 200)

#Calcular media das amostras
mean(amostra1)
mean(amostra2)
mean(amostra3)

#FIM TESTE ANOVA
###############################################################################

#ELIMINAR ATRIBUTOS

set.seed(7)

#Eliminar atributo com correlação
print(forestFires)
forestFires <- forestFires[,c(-7)]
print(forestFires)

#FIM ELIMINAR ATRIBUTOS
###############################################################################

#NORMALIZAÇÃO DOS DADOS

#Função de normalização
normalize <- function(x) 
{
        return ((x - min(x)) / (max(x) - min(x))) 
}

#Aplicar a função normalize no dataset
forestFires_n <- as.data.frame(lapply(forestFires[1:11], normalize))

#FIM NORMALIZAR DADOS
###############################################################################

#TREINAR MODELO

# Definir o seed
set.seed(1) 

# Selecionar 75% dos dados para treinar
forestFire_size <- floor(0.75 * nrow(forestFires[1:12]))
Indice_Treino <- sample(seq_len(nrow(forestFires[1:12])), size = forestFire_size)

# Criar conjuntos de treino e de teste
x_train <- forestFires_n[1:11][Indice_Treino, ]
x_test <- forestFires_n[1:11][-Indice_Treino, ]

y_train <- forestFires$area[Indice_Treino]
y_test <- forestFires$area[-Indice_Treino]

#FIM TREINAR MODELO
################################################################################

#Validação cruzada

set.seed(17)
forestFires$area <- as.factor(forestFires$area)

cv.error.10=rep(0,10)
# MSE calculado
for (i in 1:10){
        glm.fit=glm(area~X+Y+month+day+FFMC+DMC+ISI+temp+RH+wind+rain, 
                    data = forestFires, family=quasibinomial)
        cv.error.10[i]=cv.glm(forestFires,glm.fit,K=10)$delta[1]
}
cv.error.10

#Falta aplicar validação cruzada no projeto!!!!!!!
###############################################################################

#KNN

#Criar o modelo k-nn
set.seed(1)
results_knn <- knn(x_train,x_test,y_train,k=5)
print(results_knn)

#Matriz de confusão
table(y_test,results_knn)
print(table(y_test))

#Accuracy (está nos 75% com k=20)
mean(results_knn==y_test)

#FIM KNN
###############################################################################

#LDA- LINEAR DISCRIMINANT ANALYSIS

lda_model = lda(area~X+Y+month+day+FFMC+DMC+ISI+temp+RH+wind+rain, 
                data = forestFires[Indice_Treino, ])

lda_pred = predict(lda_model, x_test) 
names(lda_pred)
lda_pred_y = lda_pred$class

## compute the confusion matrix 
table(lda_pred_y, y_test)

## compute the misclassification error rate 
mean(lda_pred_y != y_test) 

#FIM LDA
###############################################################################

#QDA- QUADRATIC DISCRIMINANT ANALYSIS

qda_model = qda(area~X+Y+month+day+FFMC+DMC+ISI+temp+RH+wind+rain, 
                data = forestFires[Indice_Treino, ]) 

qda_pred = predict(lda_model, x_test) 
qda_pred_y = qda_pred$class

## compute the confusion matrix
table(qda_pred_y, y_test)

## compute the misclassification error rate 
mean(qda_pred_y != y_test)

#FIM QDA
###############################################################################

#Mulinomial Logistic Regression
#REF É ELIMINAR A QUE NAO NOS INTERESSA TANTO

library(nnet)

forestFires$area <- as.factor(forestFires$area)
levels(forestFires$area)

forestFires$area <- relevel(forestFires$area, ref = "Incêndio Pequeno")
multinom_model = multinom(area~X+Y+month+day+FFMC+DMC+ISI+temp+RH+wind+rain, 
                data = forestFires[Indice_Treino, ])

summary(multinom_model)

z <- summary(multinom_model)$coefficients/summary(multinom_model)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

exp ( coef (multinom_model))

head(pp <- fitted(multinom_model))

#FIM REGRESSAO LOGISTICA
###############################################################################



###Falta AIC/BIC/R^2/Acabar MultinomialRegressao


