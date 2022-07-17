###--------------------###
###    GEOSTATISTICS   ###
###--------------------###

## Clear the workspace
rm(list=ls())
graphics.off()

## Load spatial packages
library(sp)           ## Data management
library(lattice)      ## Data management
library(geoR)         ## Geostatistics
library(gstat)        ## Geostatistics
library(sf)

#setwd("C:/Users/giuli/OneDrive/Desktop/Maputo")
setwd("C:/Users/markh/Desktop/Università/Maputo")

d=st_read("lance.shp")

#Creiamo e abitiamo i vettori delle medie
mean_x = rep(0,5116)
mean_y = rep(0,5116)
dist = rep(0,5116)
attach(d)

cityhall=c(3626100,-2995123)

for(i in 1:5116)
{
  g = st_geometry(d)[[i]]
  mat = matrix(unlist(g), ncol = 2, nrow = length(unlist(g))/2)
  mean_x[i] = mean(mat[,1])
  mean_y[i] = mean(mat[,2])
  dist[i] = sqrt((cityhall[1]-mean_x[i])^2+(cityhall[2]-mean_y[i])^2)
}
rm(g,mat,i)

#Sostituiamo la colonna della geometria con le medie calcolate
#e togliamo i dati "sporchi"
d = st_drop_geometry(d)
d = cbind.data.frame(d,mean_x,mean_y,dist)
d = d[-c(4287, 1218, 4368, 3337, 3325, 3990),c(2:4,22:24)]
rm(mean_x,mean_y,dist)
dk = d[which(osm_surf!='unk'),]
detach(d)
pav = ifelse(dk$osm_surf == 'paved', 1, 0)
dk$osm_surf = pav

#############################################################
##############                                 ##############
######   EXPLORATORY ANALYSIS & VARIOGRAM ESTIMATION  #######
##############                                 ##############
#############################################################

## Define the sample coordinates
coordinates(d) <- c('mean_x','mean_y')
coordinates(dk) <- c('mean_x','mean_y')

## Estimating Spatial Correlation ##
##     Variogram Analysis         ##
##--------------------------------##
# CLASS
v <- variogram(osm_surf ~ dist, dk)

plot(v,pch=19)

# try reasonable initial values
v.fit <- fit.variogram(v, vgm(0.2, "Sph", 6000, 0.05))
plot(v, v.fit, pch = 19)

# MODEL
g.no <- gstat(formula = osm_surf ~ dist, data = dk, model = v.fit)

# PREDICTION
x1 = data.frame(
  mean_x=3629324,
  mean_y=-2992720,
  dist=4020.822)
coordinates(x1) <- c("mean_x","mean_y")
pr <- predict(g.no, x1)$var1.pred
pr