
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

keeps <- c("rmean","rmed","rvar","rmax","rmin","gmean","gmed","gvar","gmax","gmin","bmean","bmed","bvar","bmax","bmin","osm_surf")
dat_num <- dat[keeps]

set.seed(19)
err = rep(1000, 21)

library(class)

for (k in 1:30) {
  d.knn <- knn.cv(train = dat_num[,1:15], cl = categories, k = k)
  
  errorCV <- (d.knn != categories)
  err[k]   <- sum(errorCV)/length(categories)
}
min(err)
kbest=which.min(err)  
kbest 
best <- knn.cv(train = dat_num[,1:15], cl = categories, k = kbest)
errorCV <- (best != categories)
err_fin  <- sum(errorCV)/length(categories)
err_fin

