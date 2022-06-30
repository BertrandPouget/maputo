
#Ricorda di settare la tua working directory da Session!

rm(list=ls())

library(sf)
library(sp)

data = st_read("datonite.shp")

mean_x = rep(0,5116)
mean_y = rep(0,5116)

attach(data)

for(i in 1:5116)
{
  g = st_geometry(data)[[i]]
  mat = matrix(unlist(g), ncol = 2, nrow = length(unlist(g))/2)
  mean_x[i] = mean(mat[,1])
  mean_y[i] = mean(mat[,2])
}

data = st_drop_geometry(data)

data = cbind.data.frame(data,mean_x,mean_y)
data=data[-c(4287, 1218, 4368, 3337, 3325, 3990),]
detach(data)
#View(data)
rm(mean_x,mean_y)

## Plottiamo i centri strade

attach(data)
colo = ifelse(osm_surf=='unk','black',ifelse(osm_surf=='paved','gold','blue'))
plot(mean_x,mean_y,col=colo,pch=20)
detach(data)


