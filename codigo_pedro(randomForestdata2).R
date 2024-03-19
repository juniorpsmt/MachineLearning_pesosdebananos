### Diccionario de datos

#Variable y tipo

#Peso: es el peso de cada racimo de plátano, medido en lb. (Tipo: continua)


### Cargar datos
library(readxl)
library(tidyverse)
library(readr)
library(dplyr)
library(ggcorrplot)
library(skimr)


file.choose()
data_tesis_pedro <- read_excel("F:\\Modulos Maestria\\Tesis\\Datos de Tesis\\tesis 2022-20220323T002110Z-001\\tesis 2022\\consulta racimos(2).xlsx")

View(data_tesis_pedro)
#para verificar las diferentes variables que tipo de datos es aplicamos la funcion summary
summary(data_tesis_pedro)
#prepracion y manejo de variables

str(data_tesis_pedro)
#cambiar el punto pr la coma y transformar a numerico
data_tesis_pedro$peso <- as.numeric(gsub(",", ".",data_tesis_pedro$peso))
data_tesis_pedro$manos <- as.numeric(gsub(",", ".",data_tesis_pedro$manos))
#data_tesis_pedro$longitud_dedos  <- as.numeric(gsub(",", ".",data_tesis_pedro$longitud_dedos ))
#data_tesis_pedro$empacadora  <- as.numeric(data_tesis_pedro$empacadora)
#data_tesis_pedro$fecha  <- as.numeric(data_tesis_pedro$fecha)

#data_tesis_pedro[,-2]
#drop <- names(data_tesis_pedro) %in% c("fecha")
#data_tesis_pedro[,!drop]

#esto sirve para cambiar el nombre de una columna 

data_tesis_pedro = data_tesis_pedro %>% rename(deschive_f2 = `deschive F/2`,
                                               deschive_f3 = `deschive F/3`)


#se escoge las variables en la cual se tenemos que trabajar para este caso de estudio
#se excluye variable que no son importantes como fecha mes año

data_tesis_pedro = data_tesis_pedro %>% select(peso,
                                               palanca,
                                              calibracion_superior,
                                              deschive_f3, 
                                              deschive_f2,
                                              edad,
                                              lote,
                                            
                                              deschante,
                                               selección,
                                               fertilizacion,riego,herbicidas,fumigacion,manos)
                                                    #deshoje

####### Revisión de datos en blanco

table(is.na(data_tesis_pedro$peso))
table(is.na(data_tesis_pedro$manos))
table(is.na(data_tesis_pedro$calibracion_superior))
table(is.na(data_tesis_pedro$palanca))
table(is.na(data_tesis_pedro$peso))
table(is.na(data_tesis_pedro$edad))
table(is.na(data_tesis_pedro$deschive_f2))
table(is.na(data_tesis_pedro$deschive_f3))
table(is.na(data_tesis_pedro$deschante))
table(is.na(data_tesis_pedro$selección))
table(is.na(data_tesis_pedro$fertilizacion))
table(is.na(data_tesis_pedro$riego))
table(is.na(data_tesis_pedro$herbicidas))
table(is.na(data_tesis_pedro$fumigacion))

#table(is.na(data_tesis_pedro$deshoje))


#esto sirve para correlacionar todas las variables con la variable dependiente y ver cuanto porcentaje esta relacionada


cor(data_tesis_pedro$peso, data_tesis_pedro$manos)
cor(data_tesis_pedro$peso, data_tesis_pedro$calibracion_superior)
cor(data_tesis_pedro$peso, data_tesis_pedro$palanca)
cor(data_tesis_pedro$peso, data_tesis_pedro$lote)
cor(data_tesis_pedro$peso, data_tesis_pedro$peso)
cor(data_tesis_pedro$peso, data_tesis_pedro$edad)
cor(data_tesis_pedro$peso, data_tesis_pedro$deschive_f2)
cor(data_tesis_pedro$peso, data_tesis_pedro$deschive_f3)
cor(data_tesis_pedro$peso, data_tesis_pedro$deschante)
cor(data_tesis_pedro$peso, data_tesis_pedro$selección)
cor(data_tesis_pedro$peso, data_tesis_pedro$fertilizacion)
cor(data_tesis_pedro$peso, data_tesis_pedro$riego)
cor(data_tesis_pedro$peso, data_tesis_pedro$herbicidas)
cor(data_tesis_pedro$peso, data_tesis_pedro$fumigacion)


#cor(data_tesis_pedro$peso, data_tesis_pedro$deshoje)

#grafico de correlacion entre peso y manos

plot(data_tesis_pedro$peso, data_tesis_pedro$manos, pch = 19, col = "lightblue")
abline(lm(data_tesis_pedro$manos~ data_tesis_pedro$peso), col = "red", lwd = 3)
text(paste("Correlación:", round(cor(data_tesis_pedro$peso, data_tesis_pedro$manos), 2)), x = 25, y = 95)

#grafico de correlacion entre peso y fumigacion

plot(data_tesis_pedro$peso, data_tesis_pedro$fumigacion, pch = 19, col = "lightblue")
abline(lm(data_tesis_pedro$fumigacion~ data_tesis_pedro$peso), col = "red", lwd = 3)
text(paste("Correlación:", round(cor(data_tesis_pedro$peso, data_tesis_pedro$fumigacion), 2)), x = 25, y = 95)


#grafico de correlacion entre peso y herbicidas

plot(data_tesis_pedro$peso, data_tesis_pedro$herbicidas, pch = 19, col = "lightblue")
abline(lm(data_tesis_pedro$herbicidas~ data_tesis_pedro$peso), col = "red", lwd = 3)
text(paste("Correlación:", round(cor(data_tesis_pedro$peso, data_tesis_pedro$herbicidas), 2)), x = 25, y = 95)


#grafico de correlacion entre peso y deschive_f2

plot(data_tesis_pedro$peso, data_tesis_pedro$deschive_f2, pch = 19, col = "lightblue")
abline(lm(data_tesis_pedro$deschive_f2~ data_tesis_pedro$peso), col = "red", lwd = 3)
text(paste("Correlación:", round(cor(data_tesis_pedro$peso, data_tesis_pedro$deschive_f2), 2)), x = 25, y = 95)


#grafico de correlacion entre peso y calibracion_superior

plot(data_tesis_pedro$peso, data_tesis_pedro$calibracion_superior, pch = 19, col = "lightblue")
abline(lm(data_tesis_pedro$calibracion_superior~ data_tesis_pedro$peso), col = "red", lwd = 3)
text(paste("Correlación:", round(cor(data_tesis_pedro$peso, data_tesis_pedro$calibracion_superior), 2)), x = 25, y = 95)


#grafico de correlacion entre peso y riego

plot(data_tesis_pedro$peso, data_tesis_pedro$riego, pch = 19, col = "lightblue")
abline(lm(data_tesis_pedro$riego~ data_tesis_pedro$peso), col = "red", lwd = 3)
text(paste("Correlación:", round(cor(data_tesis_pedro$peso, data_tesis_pedro$riego), 2)), x = 25, y = 95)

#grafico de correlacion entre peso y seleccion

plot(data_tesis_pedro$peso, data_tesis_pedro$selección, pch = 19, col = "lightblue")
abline(lm(data_tesis_pedro$selección~ data_tesis_pedro$peso), col = "red", lwd = 3)
text(paste("Correlación:", round(cor(data_tesis_pedro$peso, data_tesis_pedro$selección), 2)), x = 25, y = 95)




# esta es la que funciona MATRIZ DE CORRELACION
#ESTA MATRIZ SE DEMORA EN CARGAR OJO

library(ggcorrplot)

corr <- round(cor(data_tesis_pedro), 1)
corr
ggcorrplot(corr) +
  ggtitle("Correlograma del conjunto ") +
  theme_minimal()



#este es de otra forma circulo
ggcorrplot(corr, method = 'circle') +
  ggtitle("Correlograma del conjunto ") +
  theme_minimal()

ggcorrplot(corr, method = 'circle', type = 'lower') +
  ggtitle("Correlograma del conjunto ") +
  theme_minimal()

ggcorrplot(corr, method = 'circle', type = 'lower', lab = TRUE) +
  ggtitle("Correlograma del conjunto ") +
  theme_minimal() +
  theme(legend.position="none")




### Análisis exploratorio de datos

str(data_tesis_pedro)
summary(data_tesis_pedro)


####### verificacion de datoa atipicos Datos atípicos varaible dependiente

boxplot(data_tesis_pedro$peso, border = c("blue"),horizontal = TRUE)

# esto solo es una compracion de histogramas  de pesos con manos
boxplot(data_tesis_pedro$peso ~ data_tesis_pedro$manos,
        horizontal = TRUE,border = c("blue", "green"))

hist(data_tesis_pedro$peso, main = "Histograma de Pesos")
mean(data_tesis_pedro$peso)
median(data_tesis_pedro$peso)
IQR(data_tesis_pedro$peso)
quantile(data_tesis_pedro$peso)

### division de datos  en mejor rango
#81.708 esel percentil 75 se suma con IQR (23.71) * 1.5 = 35.56
#57.992 es el percentil 50 se resta 


81.708 + 1.5 * IQR(data_tesis_pedro$peso)

57.992 - 1.5 * IQR(data_tesis_pedro$peso)

#nuevos datos con mejores pesos por eso sacamos el rango intercuantilico
nuevos_datos = data_tesis_pedro[data_tesis_pedro$peso >= 22 &
                                  data_tesis_pedro$peso <= 117,]


#verificacion del comportaiento de la variable con la nueva parametri¿zacion
str(nuevos_datos)

boxplot(nuevos_datos$peso, border = c("blue"),horizontal = TRUE)
hist(nuevos_datos$peso,border = c("blue"))
mean(nuevos_datos$peso)
median(nuevos_datos$peso)
IQR(nuevos_datos$peso)
quantile(nuevos_datos$peso)



# esto es para nuevamente tener las caracteristicas delos datos
library(skimr)

skim(nuevos_datos)


####hasta aqui me quede ojojooj 31/08/2022
### Modelamiento
# se escoge la mejores variable


library(dplyr)

data_modelamiento = nuevos_datos %>% select( edad, 
                                            deschive_f2,
                                            deschante,
                                            selección,
                                            fertilizacion,
                                            riego,
                                            herbicidas,
                                            fumigacion,
                                            peso)
                                            #edad
                                            #manos,calibracion_superior, deshoje,
str(data_modelamiento)
summary(nuevos_datos)

### División test train

#sample es una funcion que tiene ra función interesante es sample, que obtiene una muestra 
#aleatoria de un vector de elementos. Por defecto, la muestra es sin reemplazo y todos los
#elementos tienen la misma probabilidad de salir, pero hay parámetros para modificar este comportamiento por defecto.

#sample(se pone la cantidad en dividir , la data ,  )
#pone en doa grupos los 1 y los 2  y los divide en la probabilidad de 70 % y 30%
#
ind = sample(2,nrow(data_modelamiento),replace = TRUE,prob = c(0.7,0.3))

View(ind)
summary(ind)


train = data_modelamiento[ind ==1,]
View(train)
test = data_modelamiento[ind==2,]
View(test)

y_train = train[,9] #esto es solo para XGBOOST

x_test = test[,-9] #variables independientes menos peso
View(x_test)
y_test = test[,9] # variable dependiente solo peso
View(y_test)

library(randomForest)

modelo_rf = randomForest(peso ~.,
                         data = train,
                         ntree =100
                         )

plot(modelo_rf)
#par(mar = c(1,1,1,1))
#plot(x = modelo_rf, type = "proportional")
#text(x = modelo_rf, splits = TRUE,pretty = 0, cex = 0.8, col = "firebrick")
View(modelo_rf)
summary(modelo_rf)
#FUNCION PARA EXTARAER PARA MEDICION DE IMPORTNACIA DE VARAIABLES PRODUCIODAS POR RANDOM FOREST
importance(modelo_rf)

#esto solamente es como prueba ojojo
#library(Boruta)
#boruta.model <- Boruta(peso~., data = test, doTrace = 0)
#print(boruta.model)
#boruta_significativas <- names(boruta.model$finalDecision[boruta.model$finalDecision %in% "Confirmed"])  
#print(boruta_significativas)
# ESTO ES  LA CREACION DE UN VECTOR DE PREDICCION BASADO EN EL PROPIO CONJUNTO DE ENTRENAMIENTO 
# QUE CON ESTE VECTOR PODREMOS VISUALIZAR LA CURVA DE AJUSTES DE LOS DATOS



#prediccion del modelo
View(x_test)
#resultado
View(y_pred_rf)
y_pred_rf = predict(modelo_rf,x_test)

#grafico de lo predicho con los datos real de prueba de peso
#diagrama de dispersion

plot(y_pred_rf, y_test$peso)
plot(y_pred_rf, y_test$peso,main = "Estimacion vs Real",
   col = "blue")



#Medicion del Modelo

#RMSE

#Cuanto mayor sea el RMSE, mayor será la diferencia entre los 
#valores predichos y observados lo que significa que peor se ajusta un modelo 
#de regresión a los datos. Por el contrario, cuanto más pequeño sea
#el RMSE, mejor podrá un modelo ajustar los datos.

mse = mean((y_pred_rf - y_test$peso )^2)
mse
rmse = sqrt(mean(( y_pred_rf - y_test$peso )^2))
rmse
mae = mean(abs(y_pred_rf - y_test$peso ))
mae
mape = mean(abs((y_test$peso - y_pred_rf)/y_test$peso))*100
mape
r2 = 1 - sum((y_test$peso - y_pred_rf)^2)/sum((y_test$peso -  mean(y_test$peso))^2)
r2

#comparacion nada mas

View(y_pred_rf[2])
y_pred_rf[2]
x_test[1,]
y_test[2,]


### Gráfica de errores
#estos graficos son solo para  pruebas
#test$pred = y_pred_rf
#View(test)
#comparacion de lo real de manos con lo predicho de peso
plot(y_pred_rf, test$calibracion_superior)
#comparacion del peos real con la calibfracion real
plot(test$calibracion_superior,y_test$peso )
#real fesrtilizacion con peso
plot(test$fertilizacion,y_test$peso )
# estimado peso con la fertilizacion
plot(test$fertilizacion,y_pred_rf)
#comparacion de lo real de manos con lo real de peso
plot(y_test$peso, test$manos)



#### XGBoost

library(xgboost)
library(caret)
library(car)

#PARA REALIZAR EL MODELO SE NECESITA QUE NUESTRO DATA SET SE CONVIERTA EN MATRIX CON DATA.MATRIX
modelo_xgb = xgboost(data = data.matrix(train[,-9]),
                     #eval_metric = "rmse",
                     label = y_train$peso,
                     objective = "reg:linear",
                     reg_lambda = 0.5,
                     nrounds = 50)
                     #watchlist = list(train1=data.matrix(train[,-9]), test1=data.matrix(test[,-9]))
                              

#para imprimir el modelo los detalles
print(modelo_xgb)


### esto es para  prueba es para evaluar el modelo de entrenamiento
grafico=data.frame(modelo_xgb$evaluation_log) 
plot(grafico$iter, grafico$train, col = 'blue') 
lines(grafico$iter, grafico$train, col = 'red')

#esta curva estaa a prueba no funca
plot(grafico$iter, grafico$train, col = 'blue') 
lines(grafico$iter, y_train$peso, col = 'red')


# ver la importancia de la variable en el modelo
xgb.importance(colnames(data.matrix(train[,-9])), model = modelo_xgb)

importance=xgb.importance(feature_names = colnames(data.matrix(train[,-9])),model= modelo_xgb)
xgb.plot.importance(importance_matrix = importance,rel_to_first = T)



#prediccion de pesos 
y_pred_xgb = predict(modelo_xgb,data.matrix(test[,-9]))

y_pred_xgb


 # comprartivo de lo estimado vs real

plot(y_pred_xgb,test$peso,main = "Estimacion vs Real",
     col = "blue")

#METRICAS 

rmse_xgb = sqrt(mean((y_pred_xgb - y_test$peso)^2))
rmse_xgb

mse_xgb = mean((y_pred_xgb - y_test$peso)^2)
mse_xgb

mae_xgb = mean(abs(y_pred_xgb - y_test$peso))
mae_xgb

mape_xgb = mean(abs((y_test$peso - y_pred_xgb)/y_test$peso))*100
mape_xgb

y_pred_xgb






### Dependencia parcial
### ESTO ES PARA EXPLICAR Y OBJETIVO GENERAL
library(pdp)
library(ggplot2)
library(gbm)
importance(modelo_rf)

fumigacion_parcial = partial(modelo_rf, pred.var = c('fumigacion'), chull = TRUE)
autoplot(fumigacion_parcial, contour = TRUE)

fertilizacion_parcial = partial(modelo_rf, pred.var = c('fertilizacion'), chull = TRUE)
autoplot(fertilizacion_parcial, contour = TRUE)

deschive_parcial = partial(modelo_rf, pred.var = c('deschive_f2'), chull = TRUE)
autoplot(deschive_parcial, contour = TRUE)

ggplot(data = fumigacion_parcial, aes(x = fumigacion, y = yhat))+ geom_line()
dev.off()


### Dependencia parcialXGBOOST
### ESTO ES PARA EXPLICAR Y OBJETIVO GENERAL
library(pdp)
library(ggplot2)
library(magrittr)
library(ggfortify)

####Este es el correcto

a=partial(modelo_xgb, pred.var = "fumigacion", train = data.matrix(test[,-9]))
autoplot(a,contour = TRUE, colour = "red")   


b=partial(modelo_xgb, pred.var = "fertilizacion", train = data.matrix(test[,-9]))
autoplot(b,contour = TRUE, colour = "red")

c=partial(modelo_xgb, pred.var = "deschive_f2",  train = data.matrix(test[,-9]))
autoplot(c,contour = TRUE, colour = "red")

c=partial(modelo_xgb, pred.var = "edad", train = data.matrix(test[,-9]))
autoplot(c,contour = TRUE, colour = "red")







