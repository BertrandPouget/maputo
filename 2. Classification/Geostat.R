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

setwd("C:/Users/giuli/OneDrive/Desktop/Maputo")
#setwd("C:/Users/markh/Desktop/Università/Maputo")

set.seed(10094)
d = st_read("lance.shp")

# Creiamo e abitiamo i vettori delle medie
mean_x = rep(0,5116)
mean_y = rep(0,5116)
dist = rep(0,5116)

cityhall = c(3626100,-2995123)

for(i in 1:5116)
{
  g = st_geometry(d)[[i]]
  mat = matrix(unlist(g), ncol = 2, nrow = length(unlist(g))/2)
  mean_x[i] = mean(mat[,1])
  mean_y[i] = mean(mat[,2])
  dist[i] = sqrt((cityhall[1]-mean_x[i])^2+(cityhall[2]-mean_y[i])^2)
}
rm(g,mat,i)
dist=dist/1000
# Sostituiamo la colonna della geometria con le medie calcolate
# e togliamo i dati "sporchi"
d = st_drop_geometry(d)
d = cbind.data.frame(d,mean_x,mean_y,dist)
d = d[-c(4287, 1218, 4368, 3337, 3325, 3990),c(2:4,7:8,22:24)]
rm(mean_x,mean_y,dist)
dk = d[which(d$osm_surf!='unk'),]
sam = sample(2558,2300)
train = dk[sam,]
test = dk[-sam,]
rownames(train) = 1:2300
rownames(test) = 1:257
pav = ifelse(train$osm_surf == 'paved', 1, 0)
train$osm_surf = pav
pav = ifelse(test$osm_surf == 'paved', 1, 0)
test$osm_surf = pav
rm(pav)

# Define the sample coordinates
coordinates(train) <- c('mean_x','mean_y')

# Variogram
v <- variogram(osm_surf ~ dist + osm_typo + rmean + rvar, train)
plot(v,pch=19)

# try reasonable initial values
v.fit <- fit.variogram(v, vgm(0.1, "Sph", 5000, 0))
plot(v, v.fit, pch = 19)

# Model
g.no <- gstat(formula = osm_surf ~ dist + osm_typo + rmean +
                rvar, data = train, model = v.fit)

# Prediction
x = NULL
threshold = c(0.2,0.35,0.5)
for (j in 1:length(threshold)) {
  for(i in 1:257)
  {
    x1 = data.frame(
    osm_typo = test[i,2],
    mean_x = test[i,6],
    mean_y = test[i,7],
    dist = test[i,8],
    rmean = test[i,4],
    rvar = test[i,5])
    coordinates(x1) <- c("mean_x","mean_y")
    pr <- predict(g.no, x1, BLUE = TRUE)$var1.pred
   print(i)
   x = c(x, ifelse(pr > threshold[j], 1, 0))
  }
  t <- table(class.true=test$osm_surf, class.assigned=x)
  rownames(t) = c("unpaved", "paved")
  colnames(t) = c("unpaved", "paved")
  t <- t[c(2,1),c(2,1)]
  t
  accuracy <- (t[1,1]+t[2,2])/257
  accuracy
}



