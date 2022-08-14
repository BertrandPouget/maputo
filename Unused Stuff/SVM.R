
#Ricorda di settare la tua working directory da Session!

library(sf)
library(tidyverse)
library(e1071)
library(rgl)
library(misc3d)

#Creiamo il dataset senza geometry

dat <- st_read("datonite.shp")
dat <- st_drop_geometry(dat)
dat <- dat[which(dat$osm_surf != "unk"),]
dat <- dat[order(dat$osm_surf),]
rownames(dat) <- 1:nrow(dat)
categories <- dat[,4]

#Creiamo e plottiamo il dataset numerico

keeps <- c("rmed","rvar","osm_surf")
dat_num <- dat[keeps]
attach(dat)
plot(scale(dat_num[,1:2]), col = ifelse(osm_surf=="paved","red","blue"))
detach(dat)

ran=sample(2558,1500,replace = F)



svmfit <- svm(factor(osm_surf)~., data=dat_num[ran,] , kernel ='radial', cost =10, scale =T,type="C-classification")
summary(svmfit)
svm.pred <- predict(svmfit, dat_num[-ran,])

plot(svmfit, dat_num, col =c('salmon', 'light blue'), pch=19)

tab <- table(pred = svm.pred, true = dat$osm_surf[-ran])
classification_error <- 1- sum(svm.pred == dat$osm_surf[-ran])/length(svm.pred)
tab

