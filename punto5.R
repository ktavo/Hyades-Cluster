library("FactoMineR")
library("factoextra")
library("cluster")

#source('3DPlotFunctionsFactoMiner.r')
library("PerformanceAnalytics")
chart.Correlation(hipparcos_numeric, histogram = TRUE)
install.packages('ggcorrplot', dependencies=TRUE, repos='http://cran.rstudio.com/')
library("ggcorrplot")


corr <- round(cor(hipparcos_numeric[, -c(1,9)]), 1)
ggcorrplot(corr, hc.order = TRUE, lab = TRUE)

library(ggplot2)
plotmatrix(hip2[, - c(1, 9)])
library(GGally)
ggpairs(hipparcos_numeric[, - c(1, 9)], aes(alpha = 0.4))

hyadeship = read.csv("datasets/hyades_en_pdf_hyparcos.csv", sep = ";")
table(hyadeship$MIEMBRO)

hip2 <- read.csv("datasets/data.csv")
hip2$miembro <- as.factor(hip2$miembro)

hiparscaled <- scale(hip2[, 2:8])

rownames(hiparscaled) <- hip2$HIP

res.pca <- PCA(hiparscaled, graph = FALSE)

fviz_pca_ind(res.pca, geom = "point", col.ind = hip2$miembro)

#contribucion de variables
fviz_pca_var(res.pca, alpha.var = "contrib", select.var = list(contrib = 10)) +
    theme_minimal()

#here

#variables  con "cluster" de miembros y no miembros
fviz_pca_biplot(res.pca, label = "var", alpha.var = "contrib", habillage = hip2$miembro, repel = T)

#miemberos y no miembros cluster
fviz_pca_ind(res.pca, label = "none", habillage = hip2$miembro, addEllipses = TRUE, ellipse.level = 0.95) + theme_minimal()

#rgl.viewpoint(zoom = .5)
#PCABIPlot3D(res.pca)

#kmeans = 2 , hyades y no hyades?
kmeansresult <- kmeans(hiparscaled, 2, nstart = 25)
kmeansresult8 <- kmeans(hiparscaled, 8, nstart = 25)


#####este arma las 4 metricas
fviz_nbclust(hiparscaled, kmeans, method = "wss") + geom_vline(xintercept = 9, linetype = 2)
fviz_nbclust(hiparscaled, kmeans, method = "silhouette")
fviz_nbclust(hiparscaled, pam, method = "wss") + geom_vline(xintercept = 9, linetype = 2)
fviz_nbclust(hiparscaled, pam, method = "silhouette")


fviz_cluster(kmeansresult8, data = hiparscaled,
#palette = c("#00AFBB", "#FC4E07"), # color palette
			 #ellipse.type = "t", # Concentration ellipse
			geom = c("point"),
#repel = TRUE, # Avoid label overplotting (slow)
			 ggtheme = theme_classic()
			 )


ggplot(as.data.frame(hiparscaled), aes(x = Vmag, y = B.V, color = as.factor(kmeansresult8$cluster))) +
    geom_point() + geom_rug()

#print(kmeansresult)
kmeansresult$size
kmeansresult$cluster <- as.factor(kmeansresult$cluster)

fviz_pca_biplot(res.pca, axes = c(1, 2), label = "var", alpha.var = "contrib", habillage = kmeansresult$cluster, repel = T)
fviz_pca_biplot(res.pca, axes = c(1, 2), label = "var", alpha.var = "contrib", habillage = hip2$miembro, repel = T, addEllipses = TRUE, ellipse.level = 0.95)

fviz_pca_biplot(res.pca, axes = c(2, 3), label = "var", alpha.var = "contrib", habillage = kmeansresult$cluster, repel = T)
fviz_pca_biplot(res.pca, axes = c(2, 3), label = "var", alpha.var = "contrib", habillage = hip2$miembro, repel = T, addEllipses = TRUE, ellipse.level = 0.95)

fviz_pca_biplot(res.pca, axes = c(1, 3), label = "var", alpha.var = "contrib", habillage = kmeansresult$cluster, repel = T)
fviz_pca_biplot(res.pca, axes = c(1, 3), label = "var", alpha.var = "contrib", habillage = hip2$miembro, repel = T, addEllipses = TRUE, ellipse.level = 0.95)


#optima cantidad de k 
fviz_nbclust(hiparscaled, pam, method = "silhouette") +   theme_classic()
#8 segun pam medido por silhouette

pam.res <- pam(hiparscaled, 8)
pam.resk2 <- pam(hiparscaled, 2)

#pam k = 8
fviz_cluster(pam.res,
			 #palette = c("#00AFBB", "#FC4E07"), # color palette
			 ellipse.type = "t", # Concentration ellipse
			geom = c("point"),
			 #repel = TRUE, # Avoid label overplotting (slow)
			 ggtheme = theme_classic()
			 )

#pam k = 2
fviz_cluster(pam.resk2,
			palette = c("#00AFBB", "#FC4E07"), # color palette
			 ellipse.type = "t", # Concentration ellipse
			geom = c("point"),
			#repel = TRUE, # Avoid label overplotting (slow)
			 ggtheme = theme_classic()
			 )


fviz_silhouette(silhouette(pam.resk2))


#PARA HACERRRRRRRRRRRRR
#kmeans de 2 a 8k
#pam de 8 y despues con los otros mejores, probar con manhattan o euclidea
#comparar los modelos validando con las 282, silhoute, minimos cuadrados, con las 50



#opcional dbscan
library("fpc")
db <- fpc::dbscan(hiparscaled, eps = 2)
print(db)
fviz_cluster(db, hiparscaled, geom = "point")

dbscan::kNNdistplot(hiparscaled, k = 5)
abline(h = 2, lty = 2)


##optima cantidad de clusters, 30 metricas calculadas
library("NbClust")
nb <- NbClust(hiparscaled, distance = "euclidean", min.nc = 2,
		max.nc = 10, method = "complete", index = "all")
print(nb)
fviz_nbclust(nb) + theme_minimal()