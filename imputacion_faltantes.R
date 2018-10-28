#DM En Ciencia y Tencología
#TP Hyades 2018-II
#Imputacion de valores faltantes Data Set Tycho


rm(list=ls())
setwd("E:/UBA/2018-II/DM en Ciencia y Tecnología/Hyades")
library("ggplot2")
#library("GGally")
library("cluster")
library("factoextra")



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
#tycho_numeric$HIP <- NULL



#Imputación de valores faltantes
tycho_numeric$Plx[tycho_numeric$Plx == 1] <- NA
colSums(is.na(tycho_numeric))

tychoNonPlx = tycho_numeric[is.na(tycho_numeric$Plx),]
tychoPlx = tycho_numeric[!is.na(tycho_numeric$Plx),]

colSums(is.na(tychoNonPlx))

tychoPlx$Plx[is.na(tychoPlx$Plx)] <- 0
tychoPlx$HIP[is.na(tychoPlx$HIP)] <- 0

tychoNonPlx$Plx[is.na(tychoNonPlx$Plx)] <- 0
tychoNonPlx$HIP[is.na(tychoNonPlx$HIP)] <- 0


tychoNonPlxScaled <-scale(tychoNonPlx[, 1:9])
tychoPlxScaled <-scale(tychoPlx[, 1:9])


pam.tychoNonPlx <- pam(tychoNonPlxScaled, 3)
pam.tychoPlx <- pam(tychoPlxScaled, 3)

clustersNonPlx <- fviz_cluster(pam.tychoNonPlx,
             #palette = c("#00AFBB", "#FC4E07"), # color palette
             ellipse.type = "t", # Concentration ellipse
             geom = c("point"),
             #repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)


clustersPlx <- fviz_cluster(pam.tychoPlx,
             #palette = c("#00AFBB", "#FC4E07"), # color palette
             ellipse.type = "t", # Concentration ellipse
             geom = c("point"),
             #repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)


dataNoPlx <- clustersNonPlx[[1]]
dataPlx <- clustersPlx[[1]]

clusterNoPlx <- dataNoPlx[,5]
clusterPlx <- dataPlx[,5]


tychoNonPlx$cluster <- clustersNoPlx
tychoPlx$cluster <- clustersPlx



###******************************************************************
#tychoPlx[tychoPlx$cluster == 1 ,]

cluster1 <- tychoPlx[tychoPlx$cluster == 1 ,]
cluster2 <- tychoPlx[tychoPlx$cluster == 2 ,]
cluster3 <- tychoPlx[tychoPlx$cluster == 3 ,]

plx1 <- mean(cluster1$Plx)
plx2 <- mean(cluster2$Plx)
plx3 <- mean(cluster3$Plx)

tychoNonPlx$Plx[tychoNonPlx$cluster == 1] <- plx1
tychoNonPlx$Plx[tychoNonPlx$cluster == 2] <- plx2
tychoNonPlx$Plx[tychoNonPlx$cluster == 3] <- plx3


tychoNonPlx$HIP <-NULL
tychoPlx$HIP <-NULL


tychoNonPlxScaled <-scale(tychoNonPlx[, 1:9])
pam.tychoNonPlx <- pam(tychoNonPlxScaled, 3)
clustersNonPlx <- fviz_cluster(pam.tychoNonPlx,
                               #palette = c("#00AFBB", "#FC4E07"), # color palette
                               ellipse.type = "t", # Concentration ellipse
                               geom = c("point"),
                               #repel = TRUE, # Avoid label overplotting (slow)
                               ggtheme = theme_classic()
)


tychoPlxScaled <-scale(tychoPlx[, 1:9])
pam.tychoPlx <- pam(tychoPlxScaled, 3)
clustersPlx <- fviz_cluster(pam.tychoPlx,
                               #palette = c("#00AFBB", "#FC4E07"), # color palette
                               ellipse.type = "t", # Concentration ellipse
                               geom = c("point"),
                               #repel = TRUE, # Avoid label overplotting (slow)
                               ggtheme = theme_classic()
)



fulltycho = rbind(tychoPlx, tychoNonPlx)
fullTychoScaled <-scale(fulltycho[, 1:9])
pam.fullTycho <- pam(fullTychoScaled, 8)
clustersFullTycho <- fviz_cluster(pam.fullTycho,
                               #palette = c("#00AFBB", "#FC4E07"), # color palette
                               ellipse.type = "t", # Concentration ellipse
                               geom = c("point"),
                               #repel = TRUE, # Avoid label overplotting (slow)
                               ggtheme = theme_classic()
)





library("gridextra")
install.packages("gridextra")

library("ggplot2")


library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

Tycho_membros <- read.csv("datasets/Tycho_membros.csv")
Tycho_membros$tieneplx <- as.factor(is.na(Tycho_membros$Plx))

p1 <- ggplot(Tycho_membros, aes(x = tieneplx, y = RA_J2000_24)) + geom_boxplot()
p2 <- ggplot(Tycho_membros, aes(x = tieneplx, y = DE_J2000)) + geom_boxplot()
p3 <- ggplot(Tycho_membros, aes(x = tieneplx, y = pmRA)) + geom_boxplot()
p4 <- ggplot(Tycho_membros, aes(x = tieneplx, y = pmDE)) + geom_boxplot()
p5 <- ggplot(Tycho_membros, aes(x = tieneplx, y = BT)) + geom_boxplot()
p6 <- ggplot(Tycho_membros, aes(x = tieneplx, y = VT)) + geom_boxplot()
p7 <- ggplot(Tycho_membros, aes(x = tieneplx, y = V)) + geom_boxplot()
p8 <- ggplot(Tycho_membros, aes(x = tieneplx, y = B.V)) + geom_boxplot()

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, nrow = 4)



ggplot(Tycho_membros, aes(x = tieneplx, y = BT)) + geom_boxplot()
ggplot(Tycho_membros, aes(x = tieneplx, y = VT)) + geom_boxplot()
ggplot(Tycho_membros, aes(x = tieneplx, y = V)) + geom_boxplot()

wilcox.test(BT ~ tieneplx, data = Tycho_membros)
wilcox.test(VT ~ tieneplx, data = Tycho_membros)
wilcox.test(V ~ tieneplx, data = Tycho_membros)














