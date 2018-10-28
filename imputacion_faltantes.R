#DM En Ciencia y Tencología
#TP Hyades 2018-II
#Imputacion de valores faltantes Data Set Tycho


rm(list=ls())
setwd("E:/UBA/2018-II/DM en Ciencia y Tecnología/Hyades")
library("corrplot")

tycho <- read.csv(file="datasets/Tycho.csv", header=TRUE, sep=";")
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

#Numeric Tycho
tycho_numeric = data.frame(tycho[,5:15])
#Clearing missing values variables from tycho
tycho_numeric$HD <- NULL
tycho_numeric$HIP <- NULL



#Imputación de valores faltantes
tycho_numeric$Plx[tycho_numeric$Plx == 1] <- NA
colSums(is.na(tycho))
tychoNonPlx = tycho[!is.na(tycho$Plx),]
tychoNonPlx$HD <- NULL
tychoNonPlx$HIP <- NULL
colSums(is.na(tychoNonPlx))

tychoNonPlxScaled <-scale(tychoNonPlx[, 5:13])
pam.tychoNonPlx <- pam(tychoNonPlxScaled, 3)
fviz_cluster(pam.tychoNonPlx,
             #palette = c("#00AFBB", "#FC4E07"), # color palette
             ellipse.type = "t", # Concentration ellipse
             geom = c("point"),
             #repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)


library("gridextra")
install.packages("gridextra")

Tycho_membros <- read.csv("datasets/Tycho_membros.csv")
Tycho_membros$tieneplx <- as.factor(is.na(Tycho_membros$Plx))

grid.arrange(
  ggplot(Tycho_membros, aes(x = tieneplx, y = RA_J2000_24)) + geom_boxplot(),
  ggplot(Tycho_membros, aes(x = tieneplx, y = DE_J2000)) + geom_boxplot(),
  ggplot(Tycho_membros, aes(x = tieneplx, y = pmRA)) + geom_boxplot(),
  ggplot(Tycho_membros, aes(x = tieneplx, y = pmDE)) + geom_boxplot(),
  ggplot(Tycho_membros, aes(x = tieneplx, y = BT)) + geom_boxplot(),
  ggplot(Tycho_membros, aes(x = tieneplx, y = VT)) + geom_boxplot(),
  ggplot(Tycho_membros, aes(x = tieneplx, y = V)) + geom_boxplot(),
  ggplot(Tycho_membros, aes(x = tieneplx, y = B.V)) + geom_boxplot()
)


ggplot(Tycho_membros, aes(x = tieneplx, y = BT)) + geom_boxplot()
ggplot(Tycho_membros, aes(x = tieneplx, y = VT)) + geom_boxplot()
ggplot(Tycho_membros, aes(x = tieneplx, y = V)) + geom_boxplot()

wilcox.test(V ~ tieneplx, data = Tycho_membros)













