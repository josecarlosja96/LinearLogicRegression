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


# Dibujamos el histograma de los valores de las variables.
hist(datos$AutonomosPtge,xlab="Porcentaje Autonomos",main="")
hist(datos$UnemploymentPtge,xlab="Porcentaje Desempleo",main="")


"
Correcion de errores detectados.
"

#Valores fuera de rango de la variable porcentaje de desempleo
datos$UnemploymentPtge<-replace(datos$UnemploymentPtge, which((datos$UnemploymentPtge < 0)|(datos$UnemploymentPtge>100)), NA)

#Valores fuera de rango de la variable porcentaje de autónomos
datos$AutonomosPtge<-replace(datos$AutonomosPtge, which((datos$AutonomosPtge < 0)|(datos$AutonomosPtge>100)), NA)


"
Tratamiento de datos atipicos
"

varObjCont<- datos$PSOE / datos$VotosEmitidos * 100
varObjBin <- ifelse(datos$PartidoMasVotado == "PSOE" | datos$PartidoMasVotado == "PP", 1, 0)
input <- as.data.frame(datos[,-(1:12)])

# Eliminación de outliners.
for (i in names(which(sapply(input, class)=="numeric"))){
  outliers(paste0("input$",i))
}

"
. Gestión de datos ausentes
"
"
Creamos una nueva variable que contabiliza la proporción de variables ausentes que tiene cada observación:
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


# Verificamos que la variable introducida prop_missings tiene menos de 10 valores diferentes y la convertimos en factor. 
length(unique(input$prop_missings))
input$prop_missings<-as.factor(input$prop_missings)

# Reajustamos los valores de la variable.
freq(input$prop_missings)
input$prop_missings<-car::recode(input$prop_missings, "c(0.0689655172413793,0.103448275862069,0.137931034482759,0.206896551724138,0.241379310344828,0.344827586206897	)='>0.068';c(0.0344827586206897)='0.034'")
freq(input$prop_missings)

"
. Análisis de las relaciones entre variables
"

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
graficoVcramer(input,varObjCont) 



"
   _____      _               _   __              _                        _       _     _           
  / ____|    | |             (_) /_/             | |                      (_)     | |   | |          
 | (___   ___| | ___  ___ ___ _  ___  _ __     __| | ___  __   ____ _ _ __ _  __ _| |__ | | ___  ___ 
  \___ \ / _ \ |/ _ \/ __/ __| |/ _ \| '_ \   / _` |/ _ \ \ \ / / _` | '__| |/ _` | '_ \| |/ _ \/ __|
  ____) |  __/ |  __/ (_| (__| | (_) | | | | | (_| |  __/  \ V / (_| | |  | | (_| | |_) | |  __/\__ \
 |_____/ \___|_|\___|\___\___|_|\___/|_| |_|  \__,_|\___|   \_/ \__,_|_|  |_|\__,_|_.__/|_|\___||___/
"


# Creo una nueva variable a partir de los datos limpios anteriormente.
datosNuevos<-input

# Ejecutamos la funcion Transf_Auto, que selecciona la transformación que maximiza el coeficiente de correlación lineal con la variable
# objetivo. Además eliminamos las variables aleatorias creadas anteriormente.

TransfCont<-Transf_Auto(Filter(is.numeric, datosNuevos[,-c(31,32)]),varObjCont)
names(TransfCont)

discCont<-droplevels(optbin(data.frame(Filter(is.numeric, datosNuevos[,-c(31,32)]), bin(varObjCont,nbins=5,method = "content"))))[,-(ncol(Filter(is.numeric, datosNuevos[,-c(31,32)]))+1)]

names(discCont)<-paste("disc", names(discCont), sep = "_")

# Agrupar aquellas categorías cuya frecuencia sea baja:
  
apply(discCont,2,freq)

aggregate(varObjCont, by=list(discCont$disc_Age_under19_Ptge), mean)

discCont$disc_Age_under19_Ptge<-car::recode(discCont$disc_Age_under19_Ptge,
                                        "c('(12.5,13.4]','(13.4,14]')='(12.5,14]'")

discCont$disc_Age_over65_Ptge<-car::recode(discCont$disc_Age_over65_Ptge,
                                        "c('(28.3,29.4]','(29.4,30.8]')='(28.3,30.8]'")

discCont$disc_ForeignersPtge<-car::recode(discCont$disc_ForeignersPtge,
                                           "c('(5.41,6.09]','(6.09,6.35]')='(5.41,6.35]'")

discCont$disc_totalEmpresas<-car::recode(discCont$disc_totalEmpresas,
                                          "c('(306,434]','(434,487]')='(306,487]'")

discCont$disc_IndustriaPtge<-car::recode(discCont$disc_IndustriaPtge,
                                         "c('(2.51,3.23]','(3.23,3.83]','(3.83,4.35]')='(2.51,4.35]'")

discCont$disc_ConstruccionPtge<-car::recode(discCont$disc_ConstruccionPtge,
                                         "c('(5.31,6.19]','(6.19,7.46]')='(5.31,7.46]'")

discCont$disc_ServiciosPtge<-car::recode(discCont$disc_ServiciosPtge,
                                         "c('(10.3,11.4]','(11.4,12.9]','(12.9,13.9]')='(10.3,13.9]'")

discCont$disc_PersonasInmueble<-car::recode(discCont$disc_PersonasInmueble,
                                            "c('(1.21,1.27]','(1.27,1.31]')='(1.21,1.31]'")

discCont$disc_WomenUnemploymentPtge<-car::recode(discCont$disc_WomenUnemploymentPtge,
                                            "c('(50.6,53.4]','(53.4,53.7]')='(50.6,53.7]'")

discCont$disc_UnemployMore40_Ptge<-car::recode(discCont$disc_UnemployMore40_Ptge,
                                            "c('(50.6,53.4]','(53.4,53.7]')='(50.6,53.7]'")

discCont$disc_ServicesUnemploymentPtge<-car::recode(discCont$disc_ServicesUnemploymentPtge,
                                               "c('(61.8,62.2]','(62.2,63.9]')='(61.8,63.9]'")

"Por último, unimos en un mismo dataFrame la variable objetivo y las variables input originales, transformadas
y las discretizadas:"

datos_todocont<-data.frame(varObjCont,datosNuevos,TransfCont,discCont)
names(datos_todocont)


"
  __  __           _      _             _      _                  _           
 |  \/  |         | |    | |           | |    (_)                | |          
 | \  / | ___   __| | ___| | ___  ___  | |     _ _ __   ___  __ _| | ___  ___ 
 | |\/| |/ _ \ / _` |/ _ \ |/ _ \/ __| | |    | | '_ \ / _ \/ _` | |/ _ \/ __|
 | |  | | (_) | (_| |  __/ | (_) \__ \ | |____| | | | |  __/ (_| | |  __/\__ \
 |_|  |_|\___/ \__,_|\___|_|\___/|___/ |______|_|_| |_|\___|\__,_|_|\___||___/
"

set.seed(12345)
trainIndex <- createDataPartition(datos_todocont$varObjCont, p=0.8, list=FALSE)
data_train <- datos_todocont[trainIndex,]
data_test <- datos_todocont[-trainIndex,]

modelo1<-lm(varObjCont~.,data=data_train)
Rsq(modelo1,"varObjCont",data_train)
Rsq(modelo1,"varObjCont",data_test)

modelo1$rank

"
modeloStepAIC
"

null<-lm(varObjCont~1, data=data_train) #Modelo minimo
full<-lm(varObjCont~., data=data_train[,c(1:30)])
modeloStepAIC<-step(null, scope=list(lower=null, upper=full), direction="both",trace=F)

Rsq(modeloStepAIC,"varObjCont",data_test)
modeloStepAIC$rank

"
modeloBackAIC
"

modeloBackAIC<-step(full, scope=list(lower=null, upper=full), direction="backward",
                    trace=F)
Rsq(modeloBackAIC,"varObjCont",data_test)
modeloBackAIC$rank

"
modeloStepBIC
"

modeloStepBIC<-step(null, scope=list(lower=null, upper=full), direction="both",
                    k=log(nrow(data_train)),trace=F)
Rsq(modeloStepBIC,"varObjCont",data_test)
modeloStepBIC$rank

"
modeloBackBIC
"

modeloBackBIC<-step(full, scope=list(lower=null, upper=full), direction="backward",
                    k=log(nrow(data_train)),trace=F)
Rsq(modeloBackBIC,"varObjCont",data_test)
modeloBackBIC$rank


"
. Selección de variables con las input originales e interacciones:
"


#! modeloStepAIC_int -> No llega a funcionar
#fullInt<-lm(varObjCont~.^2, data=data_train[,c(1:30)])
#modeloStepAIC_int<-step(null, scope=list(lower=null, upper=fullInt), direction="both",trace=F)
#Rsq(modeloStepAIC_int,"varObjCont",data_test)


"
Puede que no funcione
modeloStepBIC_int<-step(null, scope=list(lower=null, upper=fullInt), direction=\"both\",
                        k=log(nrow(data_train)),trace=F)
Rsq(modeloStepBIC_int,\"varObjCont\",data_test)
modeloStepBIC_int$rank
"

"
modeloStepAIC_trans
"

fullT<-lm(varObjCont~., data=data_train[,c(1:30)])
modeloStepAIC_trans<-step(null, scope=list(lower=null, upper=fullT), direction="both"                          ,trace=F)
Rsq(modeloStepAIC_trans,"varObjCont",data_test)
modeloStepAIC_trans$rank


"
modeloStepBIC_trans
"
modeloStepBIC_trans<-step(null, scope=list(lower=null, upper=fullT), direction="both",
                          k=log(nrow(data_train)),trace=F)
Rsq(modeloStepBIC_trans,"varObjCont",data_test)
modeloStepBIC_trans$rank


"
modeloStepAIC_todo
"
fulltodo<-lm(varObjCont~., data=data_train)
modeloStepAIC_todo<-step(null, scope=list(lower=null, upper=fulltodo), direction="both"
                         ,trace=F)
Rsq(modeloStepAIC_todo,"varObjCont",data_test)
modeloStepAIC_todo$rank


"
modeloStepBIC_todo
"
modeloStepBIC_todo<-step(null, scope=list(lower=null, upper=fulltodo), direction="both",
                         k=log(nrow(data_train)),trace=F)
Rsq(modeloStepBIC_todo,"varObjCont",data_test)
modeloStepBIC_todo$rank


"
modeloStepBIC_todoInt
"
fullIntT<-lm(varObjCont~.^2, data=data_train)
modeloStepBIC_todoInt<-step(null, scope=list(lower=null, upper=fullIntT), direction="both",
                            k=log(nrow(data_train)),trace=F)
Rsq(modeloStepBIC_todoInt,"varObjCont",data_test)
modeloStepBIC_todoInt$rank


"
Comparación de modelos de regresión lineal
"


modelos<-list(modelo1,modeloStepAIC,modeloStepBIC,
              modeloStepAIC_trans,modeloStepBIC_trans,modeloStepAIC_todo,modeloStepBIC_todo,
              modeloStepBIC_todoInt) #incluir los modelos que se desee comparar
sapply(modelos,function(x) x$rank)
sapply(modelos,function(x) Rsq(x,"varObjCont",data_test))
sapply(modelos,function(x) Rsq(x,"varObjCont",data_train))

total<-c()
formulaModelos<-sapply(modelos,formula)
for (i in 1:length(modelos)){
  set.seed(1712)
  vcr<-train(as.formula(formulaModelos[[i]]), data = data_train,
             method = "lm",
             trControl = trainControl(method="repeatedcv", number=5, repeats=20,
                                      returnResamp="all")
  )
  total<-rbind(total,cbind(vcr$resample[,1:2],
                           modelo=rep(paste("Modelo", ifelse(i<10,paste0("0",i),i)),
                                      nrow(vcr$resample))))
}
boxplot(Rsquared~modelo,data=total,main="R-Square")
axis(3, at=1:length(modelos), labels=sapply(modelos,function(x) x$rank), cex.axis=1)

aggregate(Rsquared~modelo, data = total, function(x) c(mean(x), sd(x)))
coef(modeloStepBIC_todo)
importanciaVariables(modeloStepBIC_todo)



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
colnames(datosModeloLogistico[,-c(31,32)])

datosModeloLogistico<-datosNuevos

colnames(datosNuevos)

Transfbin<-Transf_Auto(Filter(is.numeric, datosModeloLogistico[,-c(31,32)]),varObjBin)
names(Transfbin)


discbin<-droplevels(optbin(data.frame(Filter(is.numeric,datosModeloLogistico[,-c(31,32)]),varObjBin)))[,
          -(ncol(Filter(is.numeric, datosModeloLogistico[,-c(31,32)]))+1)]

apply(discbin,2,freq) #todas están bien representadas

datos_todobin<-data.frame(varObjBin,datosModeloLogistico,Transfbin,discbin)

set.seed(12345)
trainIndex <- createDataPartition(varObjBin, p=0.8, list=FALSE)
data_train <- datos_todobin[trainIndex,]
data_test <- datos_todobin[-trainIndex,]

colnames(data_train)

modeloInicial<-glm(varObjBin~.,data=data_train[1:31],family=binomial)
pseudoR2(modeloInicial,data_train,"varObjBin")
pseudoR2(modeloInicial,data_test,"varObjBin")

modeloInicial$rank

importanciaVariables(modeloInicial)
Log(modeloInicial)

null<-glm(varObjBin~1,data=data_train,family=binomial)

#Variables input originales e interacciones
full<-glm(varObjBin~.,data=data_train[,1:30],family=binomial)

#Variables input originales e interacciones
fullInt<-glm(varObjBin~.^2,data=data_train[,1:30],family=binomial)

#Variables input originales y transformadas

# fullT<-glm(varObjBin~.,data=data_train[,1:21],family=binomial)

#Variables input originales, transformadas y discretizadas
fullTodo<-glm(varObjBin~.,data=data_train,family=binomial)

#Variables input originales, transformadas, discretizadas e interacciones
fullTodoInt<-glm(varObjBin~.^2,data=data_train,family=binomial)

modeloStepBIC<-step(null, scope=list(lower=null, upper=full), direction="both",
                    k=log(nrow(data_train)),trace=F)

modeloStepBIC_int<-step(null, scope=list(lower=null, upper=fullInt), direction="both",
                        k=log(nrow(data_train)),trace=F)

#modeloStepBIC_transf<-step(null, scope=list(lower=null, upper=fullT), direction="both",
#                           k=log(nrow(data_train)),trace=F)

modeloStepBIC_todo<-step(null, scope=list(lower=null, upper=fullTodo), direction="both",
                         k=log(nrow(data_train)),trace=F)

modeloStepBIC_todoInt<-step(null, scope=list(lower=null, upper=fullTodoInt),
                            direction="both",k=log(nrow(data_train)),trace=F)

modelos<-list(modeloStepBIC,
              modeloStepBIC_int,
              #modeloStepBIC_transf,
              modeloStepBIC_todo,modeloStepBIC_todoInt)

sapply(modelos,function(x) pseudoR2(x,data_test,"varObjBin"))

