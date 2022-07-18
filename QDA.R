#Ricorda di settare la tua working directory da Session!
#setwd("C:/Users/markh/Desktop/Università/Maputo")

rm(list=ls())
graphics.off()
load("~/RHome/AppliedStat/Lab 5 - Box-Cox + Multivariate CR/mcshapiro.test.RData")

library(MASS)
library(class)
library(sf)
library(tidyverse)
library(e1071)
library(rgl)
library(misc3d)

# Importazione dataset
dat <- st_read("lance.shp")
dat <- st_drop_geometry(dat)
dat <- dat[-c(4287, 1218, 4368, 3337, 3325, 3990),]
dat <- dat[which(dat$osm_surf != "unk"),]
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
# cumulative proportion of explained variance
cumsum(pc$sd^2)/sum(pc$sd^2)

#Rappresentiamo i loadings delle componenti principali, per 
#poterli interpretare
#x11()
par(mfrow = c(5,1))
for(i in 1:5) barplot(load[,i], ylim = c(-1, 1))

dat_pc <- data.frame(pc$scores[,1:5])
#plot3d(dat_pc[,1:3], col = ifelse(categories=='paved','gold','forestgreen'))

set.seed(10094)
sam = sample(2558,2300)
train = dat_pc[sam,]
test = dat_pc[-sam,]
cat_train = categories[sam]
cat_test = categories[-sam]
rownames(train) = 1:2300
rownames(test) = 1:257

# QDA
qda.d <- qda(train, cat_train)
qda.d
Qda.d <- predict(qda.d, test)

t <- table(class.true=cat_test, class.assigned=Qda.d$class)
t
accuracy <- (t[1,1]+t[2,2])/257
accuracy
