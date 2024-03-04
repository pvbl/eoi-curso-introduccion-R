# Script de R con conceptos básicos
# Usaremos el paquete Iris. El objetivo es clasificar flores de iris de tres tipos distintos en función de la longitud y anchura 
# de sus sépalos y pétalos. 
# Este conjunto se encuentra en la librería datasets del paquete base de R. Para ver la lista de datasets incluidos en el paquete:
install.packages("gridExtra")
install.packages("ggplot2")
install.packages('caret', dependencies=TRUE)
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("randomForest")
#install.packages("dlplyr")


library(help = "datasets")

library(rpart)
library(rpart.plot)
library(gridExtra)
library(grid)
library("ggplot2")
library(randomForest)
library(dplyr)
library(caret)
# Para cargar un conjunto de datos que ya está en R basta con usar la función data:
data(iris)
# veamos los primero datos
head(iris)

# veamos un resumen de la estructura de los datos
str(iris)

# veamos un resumen estadístico de los datos
summary(iris)

# veamos el numero de files y columnas
nrow(iris)
length(iris)
# Estudio de las variables

#Queremos evaluar las características, detectar errores, tratar los posibles NA’s o ‘missing values’,
#tipos y codificación de las variables…
# vamos a utilzar la librería dlplyr para hacer el Exploratory Data Analysis, para ello instalamos y cargamos


## con dlplyr vamos a seleccionar determinadas columnas y sobre ellas hacer un head mediante pipes
iris %>% 
  select("Species","Petal.Width","Petal.Length") %>%
  head()

# dividimos por clase
## setosa y virginica usando la libreria dlplyr
setosa<-iris %>% 
  filter(Species == "setosa")

virginica <- filter(iris, Species == "virginica")


## veriscolor utilizando la forma standard
versicolor <- iris[iris$Species=="versicolor",]


## utilziamos la funcion mutate para introducir una nueva columna que defina el area 
## el septal.width > 0.5 * sepal.length y llamamos a la columna greater.half
iris <- mutate(iris, area=iris$Petal.Length*iris$Petal.Width)
tail(iris)

## podemos ordenar el data frame con la funcion arange
head(arrange(iris,desc(Petal.Length)))



## evaluacion de valores nulos por columnas
sapply(iris, function(x) sum(is.na(x)))

# hace grupby
group_by(iris, Species) %>%
  summarise(meanSL = mean(Sepal.Length), meanSW = mean(Sepal.Width), 
            meanPL = mean(Petal.Length), meanPW = mean(Petal.Width))

# ejercicio 
# crea una nueva columna que muestre que flores cumplen Sepal.Width > 0.5 * Sepal.Length
# evalua cuantas flores cumplen esa condicion de las 150




# visualizacion simple de iris
plot(iris)

# veamos Sepal Width vs Length
plot(iris$Sepal.Width, iris$Sepal.Length,xlab='Width',ylab="Length",col="red", pch= 13)
title("Width vs Length")

## Ejercicio, haz una grafica similar a la anterior pero entre Sepal Length y Sepal Width



# veamos histograma de Sepal Width
hist(iris$Sepal.Width)



# Hagamos una inspeccion visual de los datos
## para ello definimos que vamos a hacer graficos de 2 en 2  
par(mfrow=c(2,2))

#Histogramas
for (i in 1:4){
  # extramos la variable
  variable<-iris[,i]
  # definimos un intervalos de X entre min,max y con 15 valores de longitud
  hist(setosa[,i], col=2,breaks=seq(min(variable),max(variable),length.out = 15),main=colnames(iris)[i])
  hist(versicolor[,i], col=3, breaks=seq(min(variable),max(variable),length.out = 15), add=TRUE)
  hist(virginica[,i], col=4, breaks=seq(min(variable),max(variable),length.out = 15), add=TRUE)
}

# Veamos el mismo resultado usando la libreria gglplot

# Sepal length 
HisSl <- ggplot(data=iris, aes(x=Sepal.Length))+
  geom_histogram(binwidth=0.2, color="black", aes(fill=Species)) + 
  xlab("Sepal Length (cm)") +  
  ylab("Frequency") + 
  theme(legend.position="none")+
  ggtitle("Histogram of Sepal Length")+
  geom_vline(data=iris, aes(xintercept = mean(Sepal.Length)),linetype="dashed",color="grey")


# Sepal width
HistSw <- ggplot(data=iris, aes(x=Sepal.Width)) +
  geom_histogram(binwidth=0.2, color="black", aes(fill=Species)) + 
  xlab("Sepal Width (cm)") +  
  ylab("Frequency") + 
  theme(legend.position="none")+
  ggtitle("Histogram of Sepal Width")+
  geom_vline(data=iris, aes(xintercept = mean(Sepal.Width)),linetype="dashed",color="grey")


# Petal length
HistPl <- ggplot(data=iris, aes(x=Petal.Length))+
  geom_histogram(binwidth=0.2, color="black", aes(fill=Species)) + 
  xlab("Petal Length (cm)") +  
  ylab("Frequency") + 
  theme(legend.position="none")+
  ggtitle("Histogram of Petal Length")+
  geom_vline(data=iris, aes(xintercept = mean(Petal.Length)),
             linetype="dashed",color="grey")


# Ejecicio, haz lo mismo con Petal Width con nombre HistPl

# Petal width


# Plot all visualizations
grid.arrange(HisSl + ggtitle(""),
             HistSw + ggtitle(""),
             HistPl + ggtitle(""),
             #HistPw  + ggtitle(""),
             nrow = 2,
             top = textGrob("Iris Frequency Histogram", 
                            gp=gpar(fontsize=15))
)


#Diagramas de cajas
par(mfrow=c(1,4))
for (i in 1:4){
  variable<-iris[,i]
  boxplot(cbind(setosa[,i],versicolor[,i], virginica[,i]),main=colnames(iris)[i])
}
## diagrama de cajas con featurePlot de caret
featurePlot(x = iris[, 1:4], 
            y = iris$Species, 
            plot = "box", 
            scales = list(y = list(relation="free"),
                          x = list(rot = 90)),  
            layout = c(4,1 ))

# Podemos evaluar la correlacion entre las variables
#con findCorrelation podemos determinar las columnas candidatas a la eliminación, con correlaciones mayores cutoff

Corr<-cor(iris[,1:4])
cols<-findCorrelation(Corr, cutoff = .9) 
cols



#  preProcess de caret encapsula  transformaciones de los datos previas a la aplicación de una regla de clasificación.
# como son el escalado
# -center, scale: para estandarizar las variables.
# -BoxCox: transformaciones para conseguir normalidad.
# -pca: cálculo de componentes principales.

# Estandarizacion
parametros <- preProcess(iris, method=c('center','scale'))
iris.scaled <- predict(parametros, iris)
summary(iris.scaled)

# calcula las dos primeras componentes principales
parametros <- preProcess(iris, method=c('pca'), pcaComp = 2)
iris.pca <- predict(parametros, iris)
head(iris.pca)

# --------- kNN CLASSIFIER------

## probamos con un modelo de kNN
valores <- expand.grid(k = 3)             # fijamos k=3 
ajustes <- trainControl(method='none')   # no es necesario seleccionar k óptimo
res.knn.iris <- train(x=iris[,-5], y=iris$Species,
                      method = 'knn',
                      tuneGrid = valores,
                      trControl = ajustes)

predicciones <- predict(res.knn.iris, iris)
confusionMatrix(predicciones, iris$Species)

# --------- DECISION TREES CLASSIFIER------

## uso de arboles de decisión
set.seed(2313)
ind<-sample(2,nrow(iris),replace=TRUE,prob=c(0.75,0.25))

train<-iris[ind==1,-1]
test<-iris[ind==2,-1]
model_rpart<-rpart(Species ~ .,train,method="class",control = rpart.control(cp = 0))
test$pred<-predict(model_rpart,test,type="class")
summary(model_rpart)
# creamos matriz de confusion
table(test$Species,test$pred)
# evaluamos precision
mean(test$Species==test$pred)
# ploteamos el modelo
rpart.plot(model_rpart, type = 1)


# --------- RANDOM FOREST CLASSIFIER------

set.seed(2222)

model_rf<-randomForest(Species ~ .,train)
test$pred_rf<-predict(model_rf,test)

table(test$Species,test$pred_rf)
mean(test$Species==test$pred_rf)




## probamos clusterizacion 



# cogemos los 4 primeros atributos
trainingData <- iris[,1:4]
# 2d plot con los otros atributos para ver la clusterizacion
plotData <- iris[,c(1,3)]

# ponemos el numero de clusters a utilizar 
numberOfClusters <- 3

# --------- k-means CLUSTER------

model <- kmeans(trainingData, numberOfClusters)
plot(plotData, col=model$cluster, main="K-Means")
points(model$centers[, c(1,3)], col=1:3, pch=8, cex=2)


# --------- Hierarchical CLUSTER------

distance <- dist(trainingData, method="euclidean") 
hc <- hclust(distance, method = "ward.D2")
hc2 <- cutree(hc, numberOfClusters)
plot(plotData, col=hc2, main="Hierarchical")
plot(hc)
