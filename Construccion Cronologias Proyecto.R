##CONSTRUCCIÓN DE CRONOLOGÍAS

# Empezar con espacio de trabajo en blanco
rm(list=ls())

#Intalar paquete
install.packages("dplR")

# Cargar paquete
library(dplR)

# Direccionar R a la carpeta de trabajo
getwd()
setwd("/Users/oscardiaz/Desktop")

# Importar archivos *.rwl
Picea.rwl <- read.rwl('eco.rwl')

#Información del archivo *rwl
rwl.report(Picea.rwl)
dim(Picea.rwl)
colnames(Picea.rwl)
rownames(Picea.rwl)
class(Picea.rwl)
summary(Picea.rwl)

#Obtener serie promedio por año
(promPicea <- rowMeans(Picea.rwl, na.rm=TRUE))
promPicea.df <- as.data.frame(promPicea)
plot(promPicea.df[,1], type = "l")
plot(x=c(1844:2020),y=promPicea.df[,1],type="l",ylab="RWI",xlab="year",col="red")

#Estadísticos *.rwl
Picea.stats <- rwl.stats(Picea.rwl)

#Graficar *.rwl
seg.plot(Picea.rwl)
spag.plot(Picea.rwl)
spag.plot(Picea.rwl, zfac = 0.3)

#Estandarización interactiva
Picea.i <- i.detrend(Picea.rwl)

#Estandarización método negativo exponencial
Picea.rwi <- detrend(Picea.rwl, method= "ModNegExp",pos.slope = TRUE)

#Estadísticos *.rwi
rwi.stats(Picea.rwi)

#Especificar un identificador para árbol y core para cada serie
Picea.ids <- read.ids(Picea.rwi, stc = c(4,3,1))

#Construir cronología
Picea.crn <- chron(Picea.rwi, prefix = "UPC", biweight = TRUE, prewhiten = TRUE)
head (Picea.crn)
tail (Picea.crn)
#Graficar crono
crn.plot(Picea.crn)

