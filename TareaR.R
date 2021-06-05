library(questionr)
library(car)
library(caret)
library(Hmisc)
library(readxl)
library(corrplot)
library(ggplot2)
library(tidyr)
library(dplyr)
library(OneR)
par(mar=c(7, 9, 6.1, 2.1))
library("dplyr")

source("C:/Users/JoseCarlos/Desktop/EjemploRMineria/FuncionesMineriaAida.R")
datos <- read_excel("C:/Users/JoseCarlos/Desktop/Minería de datos/DatosEleccionesEuropeas2019.xlsx")

#### Limpieza de datos:

"
  _____                                  _   __        
 |  __ \                                (_) /_/        
 | |  | | ___ _ __  _   _ _ __ __ _  ___ _  ___  _ __  
 | |  | |/ _ \ '_ \| | | | '__/ _` |/ __| |/ _ \| '_ \ 
 | |__| |  __/ |_) | |_| | | | (_| | (__| | (_) | | | |
 |_____/ \___| .__/ \__,_|_|  \__,_|\___|_|\___/|_| |_|
             | |                                       
             |_|                                       
"

# 1. Asignar variables categóricas como factores
datos[,c(12,13,32)] <- lapply(datos[,c(12,13,32)], as.factor)

# 2. Variables numéricas toman más de 10 valores.
sapply(Filter(is.numeric, datos), function(x) length(unique(x)))

# Análisis datos:
summary(datos)

"
Tratamiento de datos atípicos:
"

#Valores fuera de rango de la variable porcentaje de desempleo
datos$UnemploymentPtge<-replace(datos$UnemploymentPtge, which((datos$UnemploymentPtge < 0)|(datos$UnemploymentPtge>100)), NA)

#Valores fuera de rango de la variable porcentaje de autónomos
datos$AutonomosPtge<-replace(datos$AutonomosPtge, which((datos$AutonomosPtge < 0)|(datos$AutonomosPtge>100)), NA)

#datos$varObjCont <- datos$PSOE / datos$VotosEmitidos * 100
#datos$varObjBin <- ifelse(datos$PartidoMasVotado == "PSOE" | datos$PartidoMasVotado == "PP", 1, 0)

varObjCont<- datos$PSOE / datos$VotosEmitidos * 100
varObjBin <- ifelse(datos$PartidoMasVotado == "PSOE" | datos$PartidoMasVotado == "PP", 1, 0)
input <- as.data.frame(datos[,-(1:12)])
#row.names(input)<-datos$ID

# Eliminación de outliners.
for (i in names(which(sapply(input, class)=="numeric"))){
  outliers(paste0("input$",i))
}

"
Gestión de datos ausentes
"

input$prop_missings <- rowMeans(is.na(input))
summary(input$prop_missings)

(prop_missingsVars<-colMeans(is.na(input)))

input[,as.vector(which(sapply(input, class)=="numeric"))]<-
  sapply(Filter(is.numeric, input),function(x) impute(x,"random"))
input[,as.vector(which(sapply(input, class)=="factor"))]<-
  sapply(Filter(is.factor, input),function(x) impute(x,"random"))
# Se cambia el tipo de factor a character al imputar, así que hay que corregirlo
input[,as.vector(which(sapply(input, class)=="character"))] <-
  lapply(input[,as.vector(which(sapply(input, class)=="character"))] , as.factor)

length(unique(input$prop_missings))

input$prop_missings<-as.factor(input$prop_missings)
freq(input$prop_missings)

input$prop_missings<-car::recode(input$prop_missings, "c(0.0689655172413793,0.103448275862069,0.137931034482759,0.206896551724138,0.241379310344828,0.344827586206897	)='>0.068';c(0.0344827586206897)='0.034'")
freq(input$prop_missings)

mosaico(input$PartidoCCAA,varObjBin,"PartidoCCAA")
mosaico(input$CCAA,varObjBin,"CCAA")

dispersion(Filter(is.numeric, input),varObjCont)

corrplot.mixed(cor(data.frame(varObjCont,Filter(is.numeric, input)),
                   use="pairwise", method="pearson"),
               lower = "number", upper = "ellipse",lower.col = "black", number.cex = .9)


par(mar=c(9, 5.1, 4.1, 2.1)) #Para ajustar los márgenes del gráfico
input$aleatorio<-runif(nrow(input))
input$aleatorio2<-runif(nrow(input))
varObjBin<-as.factor(varObjBin)
graficoVcramer(input,varObjBin) 



"
   _____      _               _   __              _                        _       _     _           
  / ____|    | |             (_) /_/             | |                      (_)     | |   | |          
 | (___   ___| | ___  ___ ___ _  ___  _ __     __| | ___  __   ____ _ _ __ _  __ _| |__ | | ___  ___ 
  \___ \ / _ \ |/ _ \/ __/ __| |/ _ \| '_ \   / _` |/ _ \ \ \ / / _` | '__| |/ _` | '_ \| |/ _ \/ __|
  ____) |  __/ |  __/ (_| (__| | (_) | | | | | (_| |  __/  \ V / (_| | |  | | (_| | |_) | |  __/\__ \
 |_____/ \___|_|\___|\___\___|_|\___/|_| |_|  \__,_|\___|   \_/ \__,_|_|  |_|\__,_|_.__/|_|\___||___/
"

datosNuevos<-input
TransfCont<-Transf_Auto(Filter(is.numeric, datosNuevos[,-c(31,32)]),varObjCont)
names(TransfCont)

discCont<-droplevels(optbin(data.frame(Filter(is.numeric, datosNuevos), bin(varObjCont,nbins=5,method = "content"))))[,-(ncol(Filter(is.numeric, datosNuevos))+1)]

names(discCont)<-paste("disc", names(discCont), sep = "_")

apply(discCont,2,freq)


datos_todocont<-data.frame(varObjCont,datosNuevos,TransfCont)
names(datos_todocont)

colnames(datos_todocont)

set.seed(12345)
trainIndex <- createDataPartition(datos_todocont$varObjCont, p=0.8, list=FALSE)
data_train <- datos_todocont[trainIndex,]
data_test <- datos_todocont[-trainIndex,]



modeloManual<-lm(varObjCont~.,data=data_train)
Rsq(modeloManual,"varObjCont",data_train)

Rsq(modeloManual,"varObjCont",data_test)

modeloManual$rank


null<-lm(varObjCont~1, data=data_train) #Modelo minimo
full<-lm(varObjCont~., data=data_train[,c(1:30)])
modeloStepAIC<-step(null, scope=list(lower=null, upper=full), direction="both",trace=F)

Rsq(modeloStepAIC,"varObjCont",data_test)
modeloStepAIC$rank


modeloBackAIC<-step(full, scope=list(lower=null, upper=full), direction="backward",
                    trace=F)
Rsq(modeloBackAIC,"varObjCont",data_test)
modeloBackAIC$rank

modeloStepBIC<-step(null, scope=list(lower=null, upper=full), direction="both",
                    k=log(nrow(data_train)),trace=F)
Rsq(modeloStepBIC,"varObjCont",data_test)
modeloStepBIC$rank


modeloBackBIC<-step(full, scope=list(lower=null, upper=full), direction="backward",
                    k=log(nrow(data_train)),trace=F)
Rsq(modeloBackBIC,"varObjCont",data_test)
modeloBackBIC$rank


"
. Selección de variables con las input originales e interacciones:
"

fullInt<-lm(varObjCont~.^2, data=data_train[,c(1:30)])
modeloStepAIC_int<-step(null, scope=list(lower=null, upper=fullInt), direction="both",
                        trace=F)
Rsq(modeloStepAIC_int,"varObjCont",data_test)


"

  __  __           _      _         _      _                  _ 
 |  \/  |         | |    | |       | |    (_)                | |
 | \  / | ___   __| | ___| | ___   | |     _ _ __   ___  __ _| |
 | |\/| |/ _ \ / _` |/ _ \ |/ _ \  | |    | | '_ \ / _ \/ _` | |
 | |  | | (_) | (_| |  __/ | (_) | | |____| | | | |  __/ (_| | |
 |_|  |_|\___/ \__,_|\___|_|\___/  |______|_|_| |_|\___|\__,_|_|
"

colnames(input)


datosModeloLineal<-data.frame(varObjCont,datosNuevos)


colnames(datosModeloLineal)

set.seed(12345)
trainIndex <- createDataPartition(datosModeloLineal$varObjCont, p=0.8, list=FALSE)
data_train <- datosModeloLineal[trainIndex,]
data_test <- datosModeloLineal[-trainIndex,]

modelo1<-lm(varObjCont~.,data=data_train)
summary(modelo1)


Rsq(modelo1,"varObjCont",data_train)
Rsq(modelo1,"varObjCont",data_test)

importanciaVariables(modelo1)

modelo2<-lm(varObjCont~Clasificacion+Azucar+Etiqueta+CalifProductor+pH+Acidez+Alcohol+
              CloruroSodico,data=data_train)
Rsq(modelo2,"varObjCont",data_train)


"
  __  __           _      _         _             __    _   _           
 |  \/  |         | |    | |       | |           /_/   | | (_)          
 | \  / | ___   __| | ___| | ___   | | ___   __ _ _ ___| |_ _  ___ ___  
 | |\/| |/ _ \ / _` |/ _ \ |/ _ \  | |/ _ \ / _` | / __| __| |/ __/ _ \ 
 | |  | | (_) | (_| |  __/ | (_) | | | (_) | (_| | \__ \ |_| | (_| (_) |
 |_|  |_|\___/ \__,_|\___|_|\___/  |_|\___/ \__, |_|___/\__|_|\___\___/ 
                                             __/ |                      
                                            |___/                      

"

datosModeloLogistico<-data.frame(varObjBin,datosNuevos)

set.seed(12345)
trainIndex <- createDataPartition(datosModeloLogistico$varObjBin, p=0.8, list=FALSE)
data_train <- datosModeloLogistico[trainIndex,]
data_test <- datosModeloLogistico[-trainIndex,]

modeloInicial<-glm(varObjBin~.,data=datosModeloLogistico,family=binomial)
pseudoR2(modeloInicial,data_train,"varObjBin")

pseudoR2(modeloInicial,data_test,"varObjBin")

modeloInicial$rank

importanciaVariablesLog(modeloInicial)


"

# Análisis datos:
summary(datos)

# Tratamiento de datos atípicos:

varObjCont<-datos$PSOE / datos$VotosEmitidos * 100

input<-as.data.frame(datos)
row.names(input)<-datos$ID

for (i in names(which(sapply(input, class)==\"numeric\"))){
  outliers(paste0("input$",i))
}

# Gestión de datos ausentes
input$prop_missings<-rowMeans(is.na(input))
summary(input$prop_missings)



########################

library(caret)
library(car)
par(mar=c(7, 9, 4.1, 2.1))

datos$varObjCont<- datos$PSOE / datos$VotosEmitidos * 100

set.seed(12345)
trainIndex <- createDataPartition(datos$varObjCont, p=0.8, list=FALSE)
data_train <- datos[trainIndex,]
data_test <- datos[-trainIndex,]

#nums <- select_if(input, is.numeric)
#round(cor(x = nums, method = pearson), 3)
#my_data2 <- nums[, c(38, 4, 1, 2, 3)]
#round(cor(x = my_data2, method = pearson), 3)
#my_data2 <- nums[, c(38, 1:37)]

modelo <- lm(formula = varObjCont ~VotosEmitidos + Abstenciones+Blancos+Nulos+Cs+PP+PSOE+VOX+Podemos+Otros+Censo+Population+PartidoMasVotado, data = datos)
summary(modelo)
importanciaVariables(modelo)

step(object = modelo, direction = both, trace = 1)
"

