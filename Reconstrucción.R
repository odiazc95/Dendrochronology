##CONSTRUCCI?N DE CRONOLOG?AS

# Empezar con espacio de trabajo en blanco
rm(list=ls())

#Intalar paquete
install.packages("dplR")

# Cargar paquete
library(dplR)

# Direccionar R a la carpeta de trabajo
#getwd()
#setwd('C:/Users/hp/Desktop/Carrillo')

# Importar archivos *.rwl
Picea.rwl <- read.rwl('LMA.RWL')
#Informaci?n del archivo *rwl
rwl.report(Picea.rwl)
dim(Picea.rwl)
colnames(Picea.rwl)

rownames(Picea.rwl)
class(Picea.rwl)
summary(Picea.rwl)

#Obtener serie promedio por a?o
(promPicea <- rowMeans(Picea.rwl, na.rm=TRUE))
promPicea.df <- as.data.frame(promPicea)
plot(promPicea.df[,1], type = "l")
plot(x=c(1844:2020),y=promPicea.df[,1],type="l",ylab="RWI",xlab="year",col="red")

#Estad?sticos *.rwl
Picea.stats <- rwl.stats(Picea.rwl)

#Graficar *.rwl
seg.plot(Picea.rwl)
spag.plot(Picea.rwl)
spag.plot(Picea.rwl, zfac = 0.3)

#Estandarizaci?n interactiva
#ECO.i <- i.detrend(ECO.rwl)

#Estandarizaci?n m?todo negativo exponencial
Picea.rwi <- detrend(Picea.rwl, method= "ModNegExp",pos.slope = TRUE)

#Estad?sticos *.rwi
rwi.stats(Picea.rwi)

#Especificar un identificador para ?rbol y core para cada serie
Picea.ids <- read.ids(Picea.rwi, stc = c(3,2,1))


#Construir cronolog?a
Picea.crn <- chron(Picea.rwi, prefix = "LMA", biweight = TRUE, prewhiten = TRUE)
head (Picea.crn)
tail (Picea.crn)

def.par <- par(no.readonly = TRUE)
eps.cut <- 0.85
foo <- rwi.stats.running(Picea.rwi,Picea.ids,
                         window.length = 53)
yrs <- as.numeric(rownames(Picea.crn))
bar <- data.frame(yrs = c(min(yrs), foo$mid.year, max(yrs)),
                  eps = c(NA, foo$eps, NA))
par(mar = c(2, 2, 2, 2), mgp = c(1.1, 0.1, 0), tcl = 0.25,
    mfcol=c(2, 1), xaxs='i')
plot(yrs, Picea.crn[, 1], type = "n", xlab = "Year",
     ylab = "RWI", axes=FALSE)
cutoff <- max(bar$yrs[bar$eps < eps.cut], na.rm = TRUE)    
xx <- c(500, 500, cutoff, cutoff)
yy <- c(-1, 3, 3, -1)
polygon(xx, yy, col = "grey80")
abline(h = 1, lwd = 1.5)
lines(yrs, Picea.crn[, 1], col = "grey50")
lines(yrs, ffcsaps(Picea.crn[, 1], nyrs = 32), col = "red",
      lwd = 2)
axis(1); axis(2); axis(3);
par(new = TRUE)
plot(bar$yrs, bar$eps, type = "b", xlab = "", ylab = "",
     axes = FALSE, pch = 20, col = "blue")
axis(4, at = pretty(foo$eps))
mtext("EPS", side = 4, line = 1.1)
box()

##Esta gr?fica se realiza despu?s del "corte". La cronolog?a es reconstruida
##considerando s?lo el periodo mejor replicado.
yr.mask <- yrs > cutoff
yrs2 <- yrs[yr.mask]
Picea.crn2 <- chron(Picea.rwi[yr.mask, ], prefix = "JOY", biweight = TRUE, prewhiten = TRUE)
plot(yrs2, Picea.crn2[, 1], type = "n",
     xlab = "Year", ylab = "RWI", axes=FALSE)
abline(h = 1, lwd = 1.5)
lines(yrs2, Picea.crn2[, 1], col = "grey50")
lines(yrs2, ffcsaps(Picea.crn2[, 1], nyrs = 32),
      col = "red", lwd = 2)
axis(1); axis(2); axis(3); axis(4)
box()
par(def.par)

crn.plot(Picea.crn2, add.spline = TRUE, nyrs = 32,
         crn.line.col='grey10',spline.line.col='red',
         samp.depth.col='grey70',
         samp.depth.border.col='grey80',
         crn.lwd=1,spline.lwd=1.5,
         abline.pos=1,abline.col='black',
         abline.lty=1,abline.lwd=1,
         xlab="Year",ylab="Index")


library(treeclim)
temp_max <-read.table("TM.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)#leo la tabla con datos de temperatura
temp_min <-read.table("tmin.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE) 

month <- rep(seq(from = 1, to = 12),length(temp_max[,1]))
year <- rep(1946:2015, each= 12)

data.T <- as.vector(t(temp_max[,-1]))
data.t <- as.vector(t(temp_min[,-1]))

datosclima <- data.frame(cbind(year,month,data.T,data.t))

colnames(datosclima) <- c("Year", "Month", "Temp Max", "Temp Min")
head(datosclima)

dccINAD <- dcc(JOY.crn[,-1], datosclima, method = "correlation")

plot(dccINAD)