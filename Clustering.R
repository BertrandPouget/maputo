
#Ricorda di settare la tua working directory da Session!

library(sf)
library(tidyverse)
library(rgl)

#Creiamo il dataset senza geometry

dat <- st_read("datonite.shp")
dat <- st_drop_geometry(dat)
dat <- dat[which(dat$osm_surf != "unk"),]
dat <- dat[order(dat$osm_surf),]
rownames(dat) <- 1:nrow(dat)
categories <- dat[,4]

#Creiamo e plottiamo il dataset numerico, sia con tutte le
#strade, sia unicamente con le pavimentate

keeps <- c("rmed","gmed","bmed")
dat_num <- dat[keeps]
attach(dat)
open3d()
plot3d(dat_num, col = ifelse(osm_surf=="paved","gold","blue"))
pairs(dat_num)
detach(dat)

dat <- dat[which(dat$osm_surf == "paved"),]
rownames(dat) <- 1:nrow(dat)
dat_num <- dat[keeps]
attach(dat)
open3d()
plot3d(dat_num, col = ifelse(osm_surf=="paved","gold","blue"))
pairs(dat_num)
detach(dat)

#La seconda direzione principale sembra interessante...
#PCA

boxplot(scale(x=dat_num,center = T, scale=F), las=2, col='gold')
pc.dat <- princomp(dat_num, scores=T)
summary(pc.dat)
load <- pc.dat$loadings

#Rappresentiamo i loadings delle componenti principali, per 
#poterli interpretare

x11()
par(mfrow = c(3,1))
for(i in 1:3) barplot(load[,i], ylim = c(-1, 1))

#Osserviamo graficamente la percentuale di varianza spiegata
#dalle varie componenti

x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
plot(pc.dat, las=2, main='Principal components')
barplot(sapply(dat_num,sd)^2, las=2, main='Original Variables', ylab='Variances')
plot(cumsum(pc.dat$sd^2)/sum(pc.dat$sd^2), type='b', axes=F, xlab='number of components', 
     ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(dat_num),labels=1:ncol(dat_num),las=2)

x11()
biplot(pc.dat)

scores <- pc.dat$scores

#Plottiamo gli scores, eventualmente con colori che evidenzino
#le variabili categoriche di interesse

colo = ifelse(dat$osm_surf=='paved','gold','blue')
x11()
plot(scores[,1:2], col=colo, pch=20)

#Procediamo con il clustering

#Definiamo la distanza ed i clusters

dat.e <- dist(scores[,1:2], method = "canberra")
#x11()
#image(1:732,1:732,as.matrix(dat.e), main='metrics: Euclidean', asp=1, xlab='i', ylab='j')
dat.ec <- hclust(dat.e, method = "average")

#Plottiamo il dendrogramma con la suddivisione tramite rettangoli,
#calcoliamo i clusters e plottiamo

plot(dat.ec, main='euclidean-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(dat.ec, k=4)

cluster.ec <- cutree(dat.ec, k=4)
cluster.ec

plot(scores[,1:2], col=cluster.ec+1, pch=19)
#table(label.true = categories, label.cluster = cluster.ec)




