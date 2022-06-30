
#Ricorda di settare la tua working directory da Session!

library(sf)
library(tidyverse)
library(rgl)

#Creazione dataset senza geometry
dat <- st_read("datonite.shp")
dat <- st_drop_geometry(dat)
dat <- dat[which(dat$osm_surf != "unk"),]
dat <- dat[order(dat$osm_surf),]
rownames(dat) <- 1:nrow(dat)
categories <- dat[,4]

#Creazione dataset numerico
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

# We perform the PCA on original data
pc.dat <- princomp(dat_num, scores=T)
pc.dat
summary(pc.dat)


# loadings (recall: coefficients of the linear combination of the original 
#           variables that defines each principal component)

load <- pc.dat$loadings
load



# graphical representation of the loadings of the first six principal components
x11()
par(mfrow = c(3,1))
for(i in 1:3) barplot(load[,i], ylim = c(-1, 1))


# Explained variance
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

# scores
scores <- pc.dat$scores

# Let's use the categorical variables to further interpret the results
colo = ifelse(dat$osm_surf=='paved','gold','blue')

x11()
plot(scores[,1:2], col=colo, pch=20)

######################################################################à

#Definizione distanza
dat.e <- dist(scores[,1:2], method = "canberra")
#x11()
#image(1:732,1:732,as.matrix(dat.e), main='metrics: Euclidean', asp=1, xlab='i', ylab='j')

#Definizione clusters
dat.ec <- hclust(dat.e, method = "average")

plot(dat.ec, main='euclidean-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(dat.ec, k=4)

cluster.ec <- cutree(dat.ec, k=4)
cluster.ec

plot(scores[,1:2], col=cluster.ec+1, pch=19)
table(label.true = categories, label.cluster = cluster.ec)




