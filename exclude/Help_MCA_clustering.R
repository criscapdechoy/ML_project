
Quetal_raw <- read.table("PCA_quetaltecaen.txt", header = TRUE, sep="\t", dec = ".")
Quetal = data.frame(row.names = Quetal_raw[,1], Quetal_raw[2:9])
Quetal

# Q2 PERFORM A CA ON THIS DATA

install.packages("FactoMineR")
library(FactoMineR)
CA.quetal <- CA(Quetal)
CA.quetal$eig

plot( CA.quetal$eig[,1], type="b")
abline(h=mean(CA.quetal$eig[,1]), lty=2)

mean(CA.quetal$eig[,1])
sig.dimensions <- CA.quetal$eig[,1][CA.quetal$eig[,1]>mean(CA.quetal$eig[,1])]
sig.dimensions

# Q3 COMPUTE THE CONTRIBUTION OF EACH CELL TO THE TOTAL INERTIA

X = as.matrix(Quetal)

freq = X/sum(X)
freq

(fi. = rowSums(freq))
(f.j = colSums(freq))

cell.inertia <- matrix(nrow=8, ncol=8)
for(i in 1:dim(cell.inertia)[1])
{
  for(j in 1:dim(cell.inertia)[2])
  {
    cell.inertia[i,j] <- (freq[i,j]-(fi.[i]*f.j[j]))/(sqrt(fi.[i]*f.j[j]))
    
  }
}

round(cell.inertia, 5)

#Matrix of residual

In <- matrix(nrow=8, ncol=8)
for(i in 1:dim(In)[1])
{
  for(j in 1:dim(In)[2])
  {
    In[i,j] <- ((freq[i,j]-(fi.[i]*f.j[j]))^2)/(sqrt(fi.[i]*f.j[j]))
    
  }
}

In

#Contribution of the diagonal
(contrib.diag <- sum(diag(In))/sum(In))

# Q4 NULLIFY THE INFLUENCE OF THE DIAGONAL

n = sum(In)

#TODO : boucle
#First interation

diag.1 <- 0
for (i in 1:8){
  diag.1[i] = fi.[i] * f.j[i]
}
print(diag.1)

freq.1 = freq
diag(freq.1)
diag(freq.1) <- diag.1
diag(freq.1)

(fi..1 = rowSums(freq.1))
(f.j.1 = colSums(freq.1))

In.1 = X
In.1 <- matrix(nrow=8, ncol=8)

for (i in 1:8){
  for (j in 1:8){
    In.1[i,j] = ((freq.1[i,j] - fi..1[i] * f.j.1[j])^2)/(fi..1[i] * f.j.1[j])
  }
}
In.1

(contrib.diag.1 <- sum(diag(In.1))/sum(In.1))

#Second iteration

diag.2 <- 0
for (i in 1:8){
  diag.2[i] = fi..1[i] * f.j.1[i]
}
print(diag.2)

freq.2 = freq
diag(freq.2)
diag(freq.2) <- diag.2
diag(freq.2)

(fi..2 = rowSums(freq.2))
(f.j.2 = colSums(freq.2))

In.2 = X
In.2 <- matrix(nrow=8, ncol=8)

for (i in 1:8){
  for (j in 1:8){
    In.2[i,j] = ((freq.2[i,j] - fi..2[i] * f.j.2[j])^2)/(fi..2[i] * f.j.2[j])
  }
}
In.2

(contrib.diag.2 <- sum(diag(In.2))/sum(In.2))

#Third iteration

diag.3 <- 0
for (i in 1:8){
  diag.3[i] = fi..2[i] * f.j.2[i]
}
print(diag.3)

freq.3 = freq
diag(freq.3)
diag(freq.3) <- diag.3
diag(freq.3)

(fi..3 = rowSums(freq.3))
(f.j.3 = colSums(freq.3))

In.3 = X
In.3 <- matrix(nrow=8, ncol=8)

for (i in 1:8){
  for (j in 1:8){
    In.3[i,j] = ((freq.3[i,j] - fi..3[i] * f.j.3[j])^2)/(fi..3[i] * f.j.3[j])
  }
}
In.3

(contrib.diag.3 <- sum(diag(In.3))/sum(In.3)) 

diag(freq)
diag.1 
diag.2  
diag.3 


# Q5 PERFORM A CA WITH THE UPDATED DIAGONAL


CA.quetal.updated <- CA(freq.2)
CA.quetal.updated$eig
plot(CA.quetal.updated$eig[,1], type="b")


###################################  MCA   ##########################################

mca.data= read.csv("mca_car.csv", header = TRUE, quote = "\'", sep = ";" )
row.names(mca.data) = mca.data[,1]
mca.data = mca.data[c(2:20)]

MCA.Data = MCA(mca.data, quanti.sup = c(17), quali.sup = c(18), graph = FALSE)
plot(MCA.Data, cex=0.7)

dimdesc(MCA.Data)
###significant components

average.mca<- mean(MCA.Data$eig[,1])
lmb <- MCA.Data$eig[,1][MCA.Data$eig[,1]>average.mca]
plot(MCA.Data$eig[,1] - average.mca, type = "o", xlab = 'Principal Components', 
     ylab = 'Eigenvalues', main = 'Plot (subtracting avg eigenvalue)', col = 'blue')
cumsum(100*lmb/sum(lmb))
### hc clustering

library(factoextra)
Psi <- MCA.Data$ind$coord[,1:5]
d <- dist(Psi, method = "euclidean")
hc <- hclust(d, method = "ward.D2")

fviz_dend(hc, cex = 0.5, k = 5, main = "dendrogam")

##consolidation operation

library(ggthemes)
ind.clusters = cutree(hc, 5)
table(ind.clusters)

da = list(data=Psi, cluster=ind.clusters)

centroids = aggregate(Psi, list(ind.clusters), mean)

clust.cent <- aggregate(Psi,list(ind.clusters),mean)[,2:(nd+1)]
Bss <- sum(rowSums(clust.cent^2)*as.numeric(table(ind.clusters)))
Tss <- sum(rowSums(Psi^2))
Ib <- 100*Bss/Tss
print(paste0("Inertia between clusters is ", round(Ib, digits=3), "%"))

k5 <- kmeans(Psi,centers=clust.cent)
k5

###catdes

catdes(cbind(as.factor(k5$cluster), mca.data),1)

plot(Psi[,1],Psi[,2],type="n",main="Clustering with consolidation operation")
text(Psi[,1],Psi[,2],col=k5$cluster,cex = 0.6)
abline(h=0,v=0,col="gray")
legend("bottomleft",c("c1","c2","c3","c4", "c5"),pch=20,col=c(1:5))
