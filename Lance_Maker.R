# DIRECTORY MAPUTO
# setwd("C:/Users/markh/Desktop/Università/Maputo")
rm(list=ls())
graphics.off()

library(sf)
library(raster)
library(sp)
library(ggplot2)
library(rgdal)
library(rgl)

datini=st_read("datini.shp")

# Creiamo e aggiungiamo le colonne relative ai dati di interesse

rmean=rvar=rmed=rmin=rmax=gmean=gvar=gmed=gmin=gmax=bmean=bvar=bmed=bmin=bmax=rep(0,dim(datini)[1])
lance=cbind(datini,rmean,rvar,rmed,rmin,rmax,gmean,gvar,gmed,gmin,gmax,bmean,bvar,bmed,bmin,bmax)

# DIRECTORY RATER_MASK_DATONITE
# setwd("C:/Users/markh/Desktop/Università/Maputo/rater_mask_datonite")

# Abitiamo le colonne e creiamo il nuvo dataset
for(i in 1:dim(datini)[1])
{
  img = brick(lance$image[i])
  
  dat = data.frame(values(img))
  dat_col=dat[which(values(img)[,4]!=0),]
  
  dat.e <- dist(dat_col[,1:3], method = "euclidean")
  dat.ec <- hclust(dat.e, method = "complete")
  cluster.ec <- cutree(dat.ec, k=4)
  dat_col=cbind.data.frame(dat_col,cluster.ec)
  
  
  clus=rep(0,dim(dat)[1])
  j=0
  for(ii in row.names(dat_col))
  {
    j=j+1
    clus[as.numeric(ii)]=dat_col$cluster.ec[j]
  }
  
  dat=cbind.data.frame(dat,clus)
  
  k=dat_col[which(dat_col[,1]==min(dat_col[,1])),5]
  
  checki=intersect(unique(which(dat$clus==k)), which(values(img)[,4]==255))
  
  
  for(iii in checki) #se � del cluster 1, va eliminato
  {
    values(img)[iii,]=c(0,0,0,0)
  }
  
  # CALCOLO VARIABILI
  names(img) = c("r","g", "b", "null")
  colori = data.frame(values(img)[which(apply(values(img),1,sum)!=0),])
  lance$rmean[i]=mean(colori$r)
  lance$rvar[i]=var(colori$r)
  lance$rmed[i]=median(colori$r)
  lance$rmin[i]=min(colori$r)
  lance$rmax[i]=max(colori$r)
  
  lance$gmean[i]=mean(colori$g)
  lance$gvar[i]=var(colori$g)
  lance$gmed[i]=median(colori$g)
  lance$gmin[i]=min(colori$g)
  lance$gmax[i]=max(colori$g)
  
  lance$bmean[i]=mean(colori$b)
  lance$bvar[i]=var(colori$b)
  lance$bmed[i]=median(colori$b)
  lance$bmin[i]=min(colori$b)
  lance$bmax[i]=max(colori$b)
}

# DIRECTORY MAPUTO
# setwd("C:/Users/markh/Desktop/Università/Maputo")
st_write(lance,"lance.shp")
