
#Ricorda di settare la tua working directory da Session!
rm(list=ls())
graphics.off()

library(sf)
library(tidyverse)
library(e1071)
library(rgl)
library(misc3d)

# Importazione dataset
dat <- st_read("datonite.shp")
dat <- st_drop_geometry(dat)
dat <- dat[which(dat$osm_surf != "unk"),]
dat <- dat[order(dat$osm_surf),]
rownames(dat) <- 1:nrow(dat)
categories <- dat[,4]
dat_num <- dat[,-(1:6)]

# PCA
boxplot(dat_num)
ds <- data.frame(scale(dat_num))
boxplot(ds)

pc <- princomp(ds, scores = TRUE)
summary(pc)
load <- pc$loadings

#Osserviamo graficamente la percentuale di varianza spiegata
#dalle varie componenti
layout(matrix(c(2,3,1,3),2,byrow=T))
plot(pc, las=2, main='Principal components')
barplot(sapply(ds,sd)^2, las=2, main='Original Variables', ylab='Variances', ylim=c(0,1))
plot(cumsum(pc$sd^2)/sum(pc$sd^2), type='b', axes=F, xlab='number of components', 
     ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(ds),labels=1:ncol(ds),las=2)

#Varianza spiegata
# To obtain the rows of the summary:
# standard deviation of the components
pc$sd
# proportion of variance explained by each PC
pc$sd^2/sum(pc$sd^2)
# cumulative proportion of explained variance
cumsum(pc$sd^2)/sum(pc$sd^2)

#Rappresentiamo i loadings delle componenti principali, per 
#poterli interpretare
#x11()
par(mfrow = c(5,1))
for(i in 1:5) barplot(load[,i], ylim = c(-1, 1))

dat_pc <- data.frame(pc$scores[,1:5])
#plot3d(dat_pc[,1:3], col = ifelse(categories=='paved','gold','blue'))

set.seed(19)
err = rep(1000, 21)

library(class)

for (k in 1:30) {
  d.knn <- knn.cv(train = dat_pc, cl = categories, k = k)
  
  errorCV <- (d.knn != categories)
  err[k]   <- sum(errorCV)/length(categories)
}
min(err)
kbest=which.min(err)  
kbest 
best <- knn.cv(train = dat_pc, cl = categories, k = kbest)
errorCV <- (best != categories)
err_fin  <- sum(errorCV)/length(categories)
err_fin

