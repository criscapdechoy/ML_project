library("matrixStats")
library("ggplot2")
library("gridExtra")
library("ggpubr")
library("DMwR")
library("plsdepot")
library("FactoMineR")
library("ggplot2")
library("ggpubr")
library ("cclust")
install.packages("fpc")
library("fpc")


set.seed(123456)

data <- read.table("Russet_ineqdata_noNA.txt", sep="\t")
n <- nrow(data); varnames <- colnames(data)
p <- ncol(data); indnames <- rownames(data)
# 1  sup variables and individuals
var.sup <- which(varnames == "demo")
ind.sup <- which(indnames == "Cuba")
pca.fm <- FactoMineR::PCA(data, quali.sup=var.sup, ind.sup=ind.sup, ncp=10, graph=TRUE)


# 2  interpret the first two obtained factors?
#The dimdesc() function allows to describe the dimensions. 
#We can describe the dimensions given by the variables by writing:
dimdesc(pca.fm, axes=c(1,2)) #use the graphs from pcs

# 3  Decide the number of significant dimensions that you retain 
#(by subtracting the average eigenvalue and represent the new obtained eigenvalues in a new screeplot). 

eigenvalues <- pca.fm$eig[,c("eigenvalue")];
print(eigenvalues)

kaiser.rule.lim <- mean(eigenvalues)
sign.comp <- which(eigenvalues > kaiser.rule.lim) #we have 3 significant dimentions
sign.comp

ggplot_screeplot <- function(eigenvalues, sign.comp) {
  p <- length(eigenvalues)
  ggplot() + geom_hline(yintercept=sign.comp, col="red") +
    geom_point(aes(1:p,eigenvalues), col="blue", size=2.8)  +
    geom_line(aes(1:p,eigenvalues), col="blue", size=0.8) + customtheme(tsize=12) + 
    ggtitle("Scree Plot") + xlab("Component Number") + ylab("Eigenvalue")
}

  g1 <- ggplot_screeplot(eigenvalues, sign.comp)
 
  print(ggarrange(g1,ncol=3,widths=c(4,4,2.5)))


# 4  Perform a hierarchical clustering with the significant factors, 
#decide the number of final classes to obtain and perform a consolidation operation of the clustering. 

Psi <- pca.fm$ind$coord[,sign.comp]
d <- dist(Psi, method="euclidean")
h.clust <- hclust(d, method="ward.D2")
rect.hclust(h.clust, k=4)
par(mfrow=c(1,2), mar=c(1,4,2,1))
plot(h.clust)
barplot(h.clust$height)
#define number of classes
library(factoextra)
num.clust <- 4
ind.clusters <- cutree(h.clust, k=num.clust)
ind.clusters
table(ind.clusters) #classes and nr of elements for each

da = list(data=Psi, cluster=ind.clusters)
da
plot <-fviz_cluster(da,ggtheme = theme_minimal())
Psi.sup <- as.data.frame(pca.fm$ind.sup$coord)[,sign.comp]
clust.cent <- aggregate(as.data.frame(Psi),list(ind.clusters),mean)[,sign.comp+1]
clust.cent
Bss <- sum(rowSums(clust.cent^2)*as.numeric(table(ind.clusters)))
Tss <- sum(rowSums(Psi^2))
Ib <- 100*Bss/Tss
print(paste0("Inertia between clusters is ", round(Ib, digits=3), "%"))

# perform a consolidation operation of the clustering

k.means <- kmeans(Psi, centers=clust.cent)
Bss <- sum(rowSums(k.means$centers^2)*k.means$size) # = k.means$betweenss
Wss <- sum(k.means$withinss)                        # = k.means$tot.withinss
Ib <- 100*Bss/(Bss+Wss)
Ib
print(paste0("Inertia between clusters is ", round(Ib, digits=3), "%"))
ind.clusters <- k.means$cluster
ind.clusters

p1 <- fviz_cluster(k.means, data=Psi, k = num.clust, rect = TRUE, main = "After consilidation",ggtheme = theme_minimal())
p1

library(gridExtra)
grid.arrange(p1, plot)

#  5  Compute the Calinski-Harabassz index 

before_conso_index =  round(calinhara(da$data,da$cluster),digits=2)
before_conso_index
 
  
after_conso_index = round(calinhara(Psi,k.means$cluster),digits=2)
after_conso_index


#CH.3 <- clustIndex(Psi,int.clust, index="calinski")
CH_after= (Bss/Wss)
CH_after

CH_before=Bss/(Bss+Tss)
CH_before
#calculate it with and without kmeans


#the result is improved with calinski-Harabassz


# 6   Using the function catdes interpret and name the obtained clusters
#and represent them in the first factorial display. 
desc <- catdes(cbind(as.factor(k.means$cluster),data[-ind.sup,]),1)
desc$quanti

#  representation of clusters in first factorial plane

kmeans.cent <- k.means$centers
kmeans.cent

first_factorial <- fviz_cluster(k.means, data=Psi, k = num.clust, rect = TRUE, main = "Hclust ward",ggtheme = theme_minimal())
first_factorial

grid.arrange(p1, first_factorial)
#  assign cuba to a defined cluster

dist <- apply(clust.cent, 1, function(center) {
  sum((Psi.sup - center)^2)
}); print(dist)




