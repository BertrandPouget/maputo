library(sf)
library(tidyverse)
library(rgl)

#Creazione dataset senza geometry
dat <- st_read("datonite.shp")
dat <- st_drop_geometry(dat)
dat <- dat[which(dat$osm_surf != "unk"),]
dat <- dat[order(dat$osm_surf),]
rownames(dat) <- 1:nrow(dat)
categories <- dat[,3]

#Creazione dataset numerico
keeps <- c("rmean","rvar","rmax")
dat_num <- dat[keeps]
attach(dat)
open3d()
plot3d(dat_num, col = ifelse(osm_surf=="paved","red","blue"))
pairs(dat_num)
detach(dat)

#Definizione distanza
dat.e <- dist(dat_num, method = "euclidean")
#x11()
#image(1:2558,1:2558,as.matrix(dat.e), main='metrics: Euclidean', asp=1, xlab='i', ylab='j')

#Definizione clusters
dat.ec <- hclust(dat.e, method = "ward.D2")

plot(dat.ec, main='euclidean-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(dat.ec, k=2)

cluster.ec <- cutree(dat.ec, k=2)
cluster.ec

open3d()
plot3d(dat_num, col=cluster.ec+3, pch=19)
table(label.true = categories, label.cluster = cluster.ec)
