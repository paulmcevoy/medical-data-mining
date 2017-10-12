dat<-read.table(url("http://mathsci.ucd.ie/~brendan/data/Physio.txt"))



dat <- na.omit(dat)

datmat <- do.call(cbind, dat)

act_vals <- dat[,c(37)]

act_vals <- as.numeric(act_vals)
act_vals = act_vals -1
act_vals[act_vals==0] <- 3



#act_vals <- revalue(act_vals, c(2="1", 3="2", 1="3"))

datmat <- as.matrix(datmat)

datmat <- datmat[,-(37)]


d_sil <- dist(datmat, method = "binary")
d <- dist(datmat, method = "binary")



library(cluster)
library(fpc)
fitkmed <- pam(d,k=4)
sil <- silhouette(fitkmed$cluster,d)

pdf("sil_plot.pdf",width=11,height=8)
plot(sil, main="Silhouette width (K=3)", col = c("red", "blue", "green"))# with cluster-wise coloring
dev.off()

clust_assign <- fitkmed$clustering
res_tab <- table(act_vals, clust_assign)
library(caret) 
conf <- confusionMatrix(res_tab)

K <- 10
SSvec <- rep(NA, K)
Sil_width <- rep(NA, K)
for (k in 2:10)
{
  SSvec[k] <- kmeans(d, centers = k, nstart = 10 )$tot.withinss
#  #Sil_width[k] <- kmeans(d, centers = k, nstart = 20 )$silinfo$avg.width
  Sil_width[k] <- pam(d,k=k, metric = "manhattan")$silinfo$avg.width
}


pdf("clus_plot.pdf",width=11,height=8)
clusplot(datmat,fitkmed$cluster, main="Plot of k=3 Clusters", color=TRUE, shade=TRUE, labels=2)
dev.off()
pdf("sil_width_plot.pdf",width=11,height=8)
plot(1:10, Sil_width, main="Silhouette width vs K", xlab="K value", ylab="Silhouette Width")
dev.off()
pdf("sq_err.pdf",width=11,height=8)
plot(1:10, SSvec, main="Squared Errors vs K", xlab="K value", ylab="Silhouette Width")
dev.off()
