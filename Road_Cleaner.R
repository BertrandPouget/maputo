
#Ricorda di settare la tua working directory da Session!

rm(list=ls())
library(sf)
library(sp)
library(ggplot2)
library(rgdal)
library(sp)
library(raster)
data = st_read("datonite.shp")
data = data[order(as.numeric(data$id)),]

head(data[which(data$osm_surf=="paved"),])
head(data[which(data$osm_surf=="unpaved"),])

#Importiamo un esempio di immagine e plottiamola

img_paved = brick("img__1573.tif")
plotRGB(img_paved);

#Visualizziamo i vari colori separatamente

names(img_paved) = c("redBand","greenBand", "blueBand", "null")
plot(img_paved, col=gray(1:100/100)) 

#Plottiamo gli istogrammi (di tutta l'immagine)

{
  par(mfrow=c(1,3))
  hist(img_paved$redBand, xlab="", main='red values', col='red')
  hist(img_paved$greenBand, xlab="", main='green values', col='green')
  hist(img_paved$blueBand, xlab="", main='blue values', col='blue')
  par(mfrow=c(1,1))
}

#Rimuoviamo i pixel neri e riplottiamo gli istogrammi

val_paved = data.frame(values(img_paved)[which(apply(values(img_paved),1,sum)!=0),])
{
  par(mfrow=c(1,3))
  hist(val_paved$redBand, xlab="", main='red values', col='red')
  hist(val_paved$greenBand, xlab="", main='green values', col='green')
  hist(val_paved$blueBand, xlab="", main='blue values', col='blue')
  par(mfrow=c(1,1))
}

graphics.off()

#Plottiamo i valori dei tre colori

attach(val_paved)
plot3d(redBand, greenBand, blueBand)
detach(val_paved)

#Facciamo clustering sui pixels

val_tot = data.frame(values(img_paved))
attach(val_tot)

#Definiamo distanza e clusters

dat.e <- dist(val_tot[,1:3], method = "euclidean")
dat.ec <- hclust(dat.e, method = "complete")

#Plottiamo il dendrogramma con la suddivisione tramite rettangoli,
#calcoliamo i clusters e plottiamo

plot(dat.ec, main='euclidean-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(dat.ec, k=2)

cluster.ec <- cutree(dat.ec, k=2)
cluster.ec

plot3d(val_tot[,1:3], col=cluster.ec+1, pch=19)

#Coloriamo i pixel delle strade a seconda del cluster d'appartenenza

for(i in 1:dim(values(img_paved))[1])
  if(sum(values(img_paved)[i,])!=0)
  {
    if(cluster.ec[i]==1)
    {
      values(img_paved)[i,1]=255
      values(img_paved)[i,2]=0
      values(img_paved)[i,3]=0
    }
    else if(cluster.ec[i]==2)
    {
      values(img_paved)[i,1]=0
      values(img_paved)[i,2]=0
      values(img_paved)[i,3]=255
    }
  }
        
x11()
plotRGB(img_paved)
