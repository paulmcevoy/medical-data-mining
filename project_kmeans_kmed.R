dat<-read.table(url("http://mathsci.ucd.ie/~brendan/data/Physio.txt"))



dat <- na.omit(dat)

datmat <- do.call(cbind, dat)

act_vals <- dat[,c(37)]

act_vals <- as.numeric(act_vals)
act_vals = act_vals -1
act_vals[act_vals==0] <- 3



#act_vals <- revalue(act_vals, c(2="1", 3="2", 1="3"))

datmat <- as.matrix(datmat)

X <- datmat[,-(37)]


#fit <- kmeans(X, centers = 2, nstart = 20)

# Choosing k

K <- 10
SSvec <- rep(NA, K)

for (k in 2:10)
{
	SSvec[k] <- kmeans(X, centers = k, nstart = 20 )$tot.withinss
}

plot(SSvec)


# Inspect the results further
library(cluster)

d <- dist(X)^2
#sil <- silhouette(fit$cluster, d)

Sil_width <- rep(NA, K)


for (k in 2:10)
{
  Sil_width[k] <- pam(d,k=k, metric = "binary")$silinfo$avg.width
  print(pam(d,k=k, metric = "manhattan")$silinfo$avg.width)
  #Sil_width[k] <- pam(d,k=k, metric = "manhattan")$silinfo$avg.width
  
}

fitkmed3 <- pam(d,k=3, metric = "binary")
sil3 <- silhouette(fitkmed3$cluster,d)

fitkmed2 <- pam(d,k=2, metric = "binary")
sil2 <- silhouette(fitkmed2$cluster,d)

pdf("sil3_plot.pdf",width=11,height=8)
plot(sil3, main="Silhouette width (K=3)", col = c("red", "blue", "green"))# with cluster-wise coloring
dev.off()


pdf("sil2_plot.pdf",width=11,height=8)
plot(sil2, main="Silhouette width (K=2)", col = c("red", "blue"))# with cluster-wise coloring
dev.off()


pdf("sil_width_plot.pdf",width=11,height=8)
plot(1:10, Sil_width, main="Silhouette width vs K", xlab="K value", ylab="Silhouette Width")
dev.off()

pdf("sq_err.pdf",width=11,height=8)
plot(1:10, SSvec, main="Total Sum of Squares vs K", xlab="K value", ylab="Sq Error")
dev.off()

pdf("clus_plot2.pdf",width=11,height=8)
clusplot(datmat,fitkmed2$cluster, main="Plot of k=2 Clusters", color=TRUE, shade=TRUE, labels=2)
dev.off()

pdf("clus_plot3.pdf",width=11,height=8)
clusplot(datmat,fitkmed3$cluster, main="Plot of k=3 Clusters", color=TRUE, shade=TRUE, labels=2)
dev.off()

clust_assign <- fitkmed3$clustering
res_tab <- table(act_vals, clust_assign)
library(caret) 
conf <- confusionMatrix(res_tab)


# Using k-medoids
d <- dist(X, method = "binary")
fit2 <- pam(d, k = 3)

X[fit2$medoids,]
# Compare results
#table(fit$cluster, fit2$clustering)