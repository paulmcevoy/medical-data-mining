dat_raw<-read.table(url("http://mathsci.ucd.ie/~brendan/data/Physio.txt"))

library(arules)
library(ggplot2)
library(plyr)


dat <- na.omit(dat_raw)
#colnames(dat) <- c("recent_pain", "assoc_trauma", "nerve_injury_history", "disp_pain", "intermitt", "burning", "localised", "referred", "widespread", "mech_nature", "mech_movement", "disproportionate", "spont", "dyesthesias", "high_sev", "neurological_symp", "night_pain", "NSAIDS", "rapid_resolve", "pain_persist", "history_failed", "psychosocial", "functional", "Antalgic", "Consistent", "compress_neural_tissue", "non_mechanical", "pos_neurological", "local_palpation", "Diffuse", "allodynia", "hyperalgesia", "hyperpathia", "palpation_neural_tissues", "identification_psychosocial", "class")



datmat <- do.call(cbind, dat)

datmat <- as.matrix(datmat)
datmat <- datmat[,-(37)]
dat_sum <- dat[,-(37)]

summary(dat_sum)

cols <- colSums(datmat)
cols
min(cols)


datmat <- sapply(as.data.frame(datmat), as.logical)


datmat_tran <- as(datmat,"transactions")
summary(datmat_tran)



fit <- apriori(datmat_tran, parameter = list(supp = 0.01, conf = 0.0, minlen = 1, maxlen = 1))
fit <- apriori(datmat_tran, parameter = list(supp = 0.6, conf = 0.9, minlen = 2, maxlen = 2))

fit <- sort(fit, by = "lift")

inspect(fit)

fit <- apriori(datmat_tran, parameter = list(supp = 0.60, conf = 0.9))

fit <- sort(fit, by = "lift")

inspect(fit)

# Do another analysis to look at the relationships graphically.

library(arulesViz)
fit2 <- apriori(datmat_tran, parameter = list(supp = 0.2, conf = 0.9, minlen = 2, maxlen = 2))
fit2 <- sort(fit2, by = "lift")
inspect(fit2)
#fit2 <- subset(fit2, lift>1.25)
plot(fit2, method="matrix", measure="lift")

#fit2 <- apriori(datmat_tran, parameter = list(supp = 0.55, conf = 0.9,maxlen = 2,minlen = 2))

#plot(fit2, measure=c("support", "confidence"), shading="lift", interactive=TRUE)


pdf("arules_fit.pdf",width=11,height=8)
plot(fit2)
dev.off()

pdf("arules_fit_group.pdf",width=11,height=8)
plot(fit2, method = "grouped")
dev.off()

pdf("arules_fit_graph.pdf",width=11,height=8)
plot(fit2, method = "graph")
dev.off()

