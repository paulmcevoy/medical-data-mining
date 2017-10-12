set.seed(123)

library(adabag)



dat_raw<-read.table(url("http://mathsci.ucd.ie/~brendan/data/Physio.txt"))

dat <- na.omit(dat_raw)

dat <- data.frame(lapply(dat, as.character), stringsAsFactors=FALSE)

dat$assigned.labels[dat$assigned.labels == "Nociceptive"] <- "0"
dat$assigned.labels[dat$assigned.labels == "Peripheral Neuropathic"] <- "0"
dat$assigned.labels[dat$assigned.labels == "Central Neuropathic"] <- "1"

dat <- data.frame(lapply(dat, as.factor))

# Load the randomForest package
library(randomForest)

# Implement the random forest algorithm
fit.rf <- randomForest(assigned.labels~.,data=dat, ntree=500)

# Let's look at variable importance
pdf("plot_var_imp_cn.pdf",width=11,height=8)
varImpPlot(fit.rf)
dev.off()
order(-fit.rf$importance)
summary(fit.rf$importance)


fit<-glm(assigned.labels~X13+X28+X2+X11+X26+X4+X31+X10+X6+X23,data=dat,family="binomial")

summary(fit)

#fit<-glm(assigned.labels~X13+X11+X31+X2+X6,data=dat,family="binomial")

summary(fit)

pred <- predict(fit,type='response',newdata=dat)
pred_vals <- ifelse(pred > 0.5,1,0)
act_vals <- dat$assigned.labels


#pred <- pred[pred<0.5] <- 0

# Look at table for the validation data only (rows=truth, cols=prediction)

res_tab <- table(act_vals, pred_vals)
res_tab

# Using the rms library
library(rms)

#fit1<-lrm(low~age+lwt+race+smoke+ptl+ht+ui+ftv,data=birthwt,x=TRUE,y=TRUE)

residuals(fit,type="pearson")

# Performance
library(xtable)
pred<-predict(fit,type="response")
tau<-1.0
l<-(pred>=tau)*1
l<-factor(l,levels=c(0,1))
tab<-table(dat$assigned.labels,l)

xtable(tab)

tauv<-seq(0,1,by=0.001)

res<-matrix(NA,length(tauv),8)
for (j in 1:length(tauv))
{
  tau<-tauv[j]	
  l<-(pred>=tau)*1
  l<-factor(l,levels=c(0,1))
  tab<-table(dat$assigned.labels,l)
  Sens<-tab[2,2]/(tab[2,2]+tab[2,1])
  Spec<-tab[1,1]/(tab[1,1]+tab[1,2])
  PPV<-tab[2,2]/(tab[2,2]+tab[1,2])
  NPV<-tab[1,1]/(tab[1,1]+tab[2,1])
  Acc<-(tab[2,2]+tab[1,1])/sum(tab)
  FDR<-1-PPV
  FPR<-1-Spec
  
  res[j,]<-c(tau,Sens,Spec,PPV,NPV,Acc,FDR,FPR)
}
res<-data.frame(res)
colnames(res)<-c("tau","Sensitivity","Specificity","PPV","NPV","Accuracy","FDR","FPR")

xtable(res)

plot(res$FPR,res$Sens,xlab="False Positive Rate (FPR)",ylab="True Positive Rate (TPR)",type="s")
abline(a=0,b=1,lty=3)
points(c(0,0,1),c(0,1,1),lty=2,col="red",type="l")

library(ROCR)
predobj<-prediction(pred,dat$assigned.labels)
AUC <- performance(predobj,"auc")

perf <- performance(predobj,"tpr","fpr")
plot(perf)

#plot(tauv,res$Sens+res$Spec,xlab=expression(tau),ylab="Sensitivity+Specificity",pch=3)

j<-which.max(res$Sens+res$Spec)
tauv[j]
res[j,]
AUC

pdf("roc_plot_cn.pdf",width=11,height=8)
plot(res$FPR,res$Sens,xlab="False Positive Rate (FPR)",ylab="True Positive Rate (TPR)",type="s")
abline(a=0,b=1,lty=3)
points(res$FPR[j],res$Sens[j],pch=3,col="red",cex=3)
dev.off()
