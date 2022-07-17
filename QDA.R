
#Ricorda di settare la tua working directory da Session!
#setwd("C:/Users/markh/Desktop/Università/Maputo")

rm(list=ls())
graphics.off()

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

#######################################################################################
###PER ORA O FAI CON 2 E RIESCI A PLOTTARE, O FAI CON TUTTE E OTTIENI UN OTTIMO APER###
#######################################################################################

############# QDA ###################
###--------------------------
#### Assumptions:
###------------------
# 1) if L=i, X.i ~ N(mu.i, sigma.i^2), i=A,B
# 2) c(A|B)=c(B|A) (equal misclassification costs)

qda.iris <- qda(dat_pc, categories)
qda.iris
Qda.iris <- predict(qda.iris, dat_pc)

# 1) APER (without priors)
table(class.true=categories, class.assigned=Qda.iris$class)
errorsq <- (Qda.iris$class != categories)
APERq   <- sum(errorsq)/length(categories)
APERq

# Remark: correct only if we estimate the priors through the sample frequencies!

# 2) AER L1OCV (without priors)
QdaCV.iris <- qda(dat_pc, categories, CV=T)
table(class.true=categories, class.assignedCV=QdaCV.iris$class)
errorsqCV <- (QdaCV.iris$class != categories)
AERqCV   <- sum(errorsqCV)/length(categories)
AERqCV

