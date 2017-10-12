# Load the adabag library
set.seed(123)

library(adabag)



dat_raw<-read.table(url("http://mathsci.ucd.ie/~brendan/data/Physio.txt"))

dat <- na.omit(dat_raw)

dat <- data.frame(lapply(dat, as.character), stringsAsFactors=FALSE)

dat$assigned.labels[dat$assigned.labels == "Nociceptive"] <- "NP"
dat$assigned.labels[dat$assigned.labels == "Peripheral Neuropathic"] <- "PN"
dat$assigned.labels[dat$assigned.labels == "Central Neuropathic"] <- "CN"

dat <- data.frame(lapply(dat, as.factor))
# Implement Bootstrap Aggregating
fit.b <- bagging(assigned.labels~.,data=dat)
pred.b <- predict(fit.b,newdata=dat,type="class")$class

# Fit a classification tree (same data)
fit.r <- rpart(assigned.labels~.,data=dat)
pred.r <- predict(fit.r,newdata=dat,type="class")

# Fit a classifier to only the training data
plot(as.party(fit.r))

# Compare performance
tab.b <- table(dat$assigned.labels,pred.b)
tab.r <- table(dat$assigned.labels,pred.r)



# Work out the accuracy
acc.b <- sum(diag(tab.b))/sum(tab.b)
acc.r <- sum(diag(tab.r))/sum(tab.r)

# Store the results
#res_tvt[iter,1] <- acc.r
#res_tvt[iter,2] <- acc.l


# Plot examples of 9 trees
library(partykit)
par(mfrow=c(3,3))
for (j in 1:3)
{
	plot(fit.b$trees[[j]],main=paste("Tree ",j,sep=""))
	text(fit.b$trees[[j]],use.n=TRUE,xpd=TRUE,col="blue")
}
par(mfrow=c(1,1))

# Load the randomForest package
library(randomForest)

# Implement the random forest algorithm
fit.rf <- randomForest(assigned.labels~.,data=dat, ntree=500)

# Examine the results
fit.rf

# Let's look at variable importance
pdf("plot_var_imp.pdf",width=11,height=8)
varImpPlot(fit.rf)
dev.off()
