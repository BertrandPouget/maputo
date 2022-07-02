
#Ricorda di settare la tua working directory da Session!

rm(list=ls())

library(sf)
library(sp)
library(ggplot2)
library(rgdal)
library(rgl)
library(raster)

data = st_read("datonite.shp")
data = data[order(as.numeric(data$id)),]

#Importiamo un esempio di immagine e plottiamola
#(Vai nella directory di rater_mask_datonite!)

img = brick("img__1573.tif")
plotRGB(img)
names(img) = c("redBand","greenBand", "blueBand", "null")

#Dividiamo il dataset in due parti per motivi computazionali

dat = data.frame(values(img))
attach(dat)
len=dim(dat)[1]/2
dat1 = dat[1:len,]
dat2 = dat[-c(1:len),]
detach(dat)

#Definiamo distanza e clusters e plottiamo

dat.e <- dist(dat1[,1:3], method = "euclidean")
dat.ec <- hclust(dat.e, method = "complete")
cluster.ec1 <- cutree(dat.ec, k=3)

dat.e <- dist(dat2[,1:3], method = "euclidean")
dat.ec <- hclust(dat.e, method = "complete")
cluster.ec2 <- cutree(dat.ec, k=3)

cluster.ec <- c(cluster.ec1,cluster.ec2)

plot3d(dat[,1:3], col=cluster.ec+1, pch=19)

#Coloriamo i pixel delle strade a seconda del cluster d'appartenenza

for(i in 1:dim(values(img))[1])
  if(sum(values(img)[i,])!=0)
  {
    if(cluster.ec[i]==1)
    {
      values(img)[i,1]=0
      values(img)[i,2]=0
      values(img)[i,3]=0
    }
    else
    {
      values(img)[i,1]=values(img)[i,1]
      values(img)[i,2]=values(img)[i,2]
      values(img)[i,3]=values(img)[i,3]
    }
  }
        
plotRGB(img)
