#DM En Ciencia y Tencología
#TP Hyades 2018-II


rm(list=ls())
setwd("E:/UBA/2018-II/DM en Ciencia y Tecnología/Hyades")
library("corrplot")


symbad <- read.csv(file="datasets/Symbad.csv", header=TRUE, sep=";")
hipparcos <- read.csv(file="datasets/hipparcos.csv", header=TRUE, sep=";")
tycho <- read.csv(file="datasets/Tycho.csv", header=TRUE, sep=";")


#symbad as.numeric
symbad$RA <- as.numeric(symbad$RA)
symbad$RA_J2000 <- as.numeric(symbad$RA_J2000)
symbad$DE <- as.numeric(symbad$DE)
symbad$DE_J2000 <- as.numeric(symbad$DE_J2000)
#Hipparcos as.numeric
hipparcos$RA_J2000 <- as.numeric(hipparcos$RA_J2000)
hipparcos$DE_J2000 <- as.numeric(hipparcos$DE_J2000)
hipparcos$Plx <- as.numeric(hipparcos$Plx)
hipparcos$pmRA <- as.numeric(hipparcos$pmRA)
hipparcos$pmDE <- as.numeric(hipparcos$pmDE)
hipparcos$Vmag <- as.numeric(hipparcos$Vmag)
hipparcos$B_V <- as.numeric(hipparcos$B_V)
#Tycho as.numeric
tycho$RA_J2000_24 <- as.numeric(tycho$RA_J2000_24)
tycho$DE_J2000 <- as.numeric(tycho$DE_J2000)
tycho$pmRA <- as.numeric(tycho$pmRA)
tycho$pmDE <- as.numeric(tycho$pmDE)
tycho$BT <- as.numeric(tycho$BT)
tycho$VT <- as.numeric(tycho$VT)
tycho$V <- as.numeric(tycho$V)
tycho$B.V <- as.numeric(tycho$B.V)
tycho$Plx <- as.numeric(tycho$Plx)


#Numeric Symbad
symbad_numeric = data.frame(symbad[,5:8])
#Numeric Hipparcos
hipparcos_numeric = data.frame(hipparcos[,2:8])
#Numeric Symbad
tycho_numeric = data.frame(tycho[,5:15])

#Clearing missing values variables from tycho
tycho_numeric$HD <- NULL
tycho_numeric$HIP <- NULL

#Realiza el análisis de componentes principales
sym.pca.cov = prcomp(symbad_numeric, center = TRUE, scale. = FALSE)
hip.pca.cov = prcomp(hipparcos_numeric, center = TRUE, scale. = FALSE)
tyc.pca.cov = prcomp(tycho_numeric, center = TRUE, scale. = FALSE)


#Realiza un resumen de las variabilidades explicadas por las componentes principales
summary(sym.pca.cov)
summary(hip.pca.cov)
summary(tyc.pca.cov)


#Calcula la matriz de correlación
matrizCorrelacionSymbad = cor(symbad_numeric)
matrizCorrelacionHipparcos = cor(hipparcos_numeric)
matrizCorrelacionTycho = cor(tycho_numeric)

#Genera el correlograma
corrplot.mixed(matrizCorrelacionSymbad, lower = "number", upper = "shade", addshade = "all")
corrplot.mixed(matrizCorrelacionHipparcos, lower = "number", upper = "shade", addshade = "all")
corrplot.mixed(matrizCorrelacionTycho, lower = "number", upper = "shade", addshade = "all")






