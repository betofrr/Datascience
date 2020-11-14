rm(list=ls())
setwd("~") 

##########################
# Machine Learning - KNN #
# 		    			         #
# 			  			         #
##########################

#install.packages(c("class", "gmodels"))
require(class)
require(gmodels)
require(dplyr)
library(ggplot2)
library(tidyverse)
library(dplyr)

dbv <- "C:\\Users\\Beto Felix\\Desktop\\Beto\\SPPC\\Curso Análisis de Datos\\klustera\\v.csv"
dbe <- "C:\\Users\\Beto Felix\\Desktop\\Beto\\SPPC\\Curso Análisis de Datos\\klustera\\e.csv"

## Guardar base de datos en variable
e <- read.csv(paste(dbe, sep="/"), stringsAsFactors = FALSE)
v <- read.csv(paste(dbv, sep="/"), stringsAsFactors = FALSE)

str(e)
e <- e[-1]
str(e)

sapply(e, function(x) sum(is.na(x)))

##e <- ifelse(is.na(e$device_mac), is.na(e$branch_office), is.na(e$month_tz), is.na(e$day_tz), is.na(e$day_of_week_tz), is.na(e$hour_tz), is.na(e$visitor), is.na(e$tiempodeses))


##table(e$visitor)

e$visitor <- factor(e$visitor, levels = c("true", "false"),
                    labels = c("Visitante", "No Visitante"))

round(prop.table(table(e$visitor)) * 100, digits = 1)

summary(e[c("tiempodeses", "day_tz", "hour_tz")])

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

e_n <- as.data.frame(lapply(e[c(4,6,8)], normalize)) #list apply
head(e_n)

##summary(e_n$tiempodeses)
##summary(e_n$day_tz)

# separamos la DB en un set como entrenamiento y otro como prueba
nfilas <- floor(nrow(e_n) * .80)
set.seed(123)
index <- sample(7:nrow(e_n), nfilas) # 80%
e_train <- e_n[1:80000, ] # Obtener solo las muestras
e_test <- e_n[80001:90000, ] # Todo menos las muestras

# Guardamos la clasificaciÃ³n de cada uno (B o M) de la primera columna
e_train_labels <- e[1:80000, 7]
e_test_labels <- e[80001:90000, 7]
str(e_train_labels)


e_test_pred <- knn(train = e_train, test = e_test, cl = e_train_labels, k = 30)

## ----------- Evaluamos los resultados del modelo 
# Creamos una tabla para compara predicciones vs real
CrossTable(x = e_test_labels, y = e_test_pred, prop.r=FALSE, prop.chisq = FALSE)
summary(e_test_pred)

v$visitor <- e_test_pred
summary(v$visitor)
## ------------------- Base de datos v-------------------------##
str(v)
v <- v[-1]
str(v)

summary(v[c("tiempodeses", "day_tz", "hour_tz")])

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

v_n <- as.data.frame(lapply(v[c(2,5,6)], normalize)) #list apply
head(v_n)

##summary(e_n$tiempodeses)
##summary(e_n$day_tz)

# separamos la DB en un set como entrenamiento y otro como prueba
##nfilas <- floor(nrow(v_n) * .80)
set.seed(123)
##index <- sample(1:nrow(v_n), nfilas) # 80%
v_train <- v_n[1:80000, ] # Obtener solo las muestras
v_test <- v_n[80001:90000, ] # Todo menos las muestras


v_test_pred <- knn(train = v_train, test = v_test, cl = e_train_labels, k = 65)


CrossTable(x = e_test_labels, y = v_test_pred, prop.r=FALSE, prop.chisq = FALSE)

v$visitor <- e_test_pred
summary(v$visitor)
round(prop.table(table(v$visitor)) * 100, digits = 1)

##------------------- Solución de Preguntas y gráficas -------------------------


##------------------ ¿Cuál es el mes con mayor número de visitantes? -----------

mes <- v %>%
  filter(visitor=="Visitante") %>%
  group_by(month_tz) %>%
  count() %>%
  mutate(total=n)
##ungroup()

# b) Graficar datos
gr <- ggplot(mes, aes(x=month_tz, y=total)) +
  geom_col(fill="seagreen")+labs(title = "Visitantes por Mes", x="Mes", y="No. de Visitantes")
gr


##------------------ ¿Cuál es el día de la semana con mayor número de visitantes?------------

dia <- v %>%
  filter(visitor=="Visitante") %>%
  group_by(day_of_week_tz) %>%
  count() %>%
  mutate(total=n)
  ##ungroup()

# b) Graficar datos
gr <- ggplot(dia, aes(x=day_of_week_tz, y=total)) +
  geom_point(color="#0570F1")+ labs(title = "Visitantes por Día",x="Días de la semana",y="Visitantes")+
  theme_light()
gr


##-------------- ¿Cuál es la franquicia con mayor número de visitantes? -------------

branch <- v %>%
  filter(visitor=="Visitante") %>%
  group_by(branch_office) %>%
  count() %>%
  mutate(total=n)
##ungroup()

branch

# b) Graficar datos
gr <- ggplot(branch, aes(x=branch_office, y=total)) +
  geom_col(fill="royalblue")+labs(title = "Visitantes por Franquicia", x="Franquicia", y="No. de Visitantes")
gr
