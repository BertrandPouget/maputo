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

  # ROAD CLEANING
  # Dividiamo il dataset in due parti per motivi computazionali
  dat = data.frame(values(img))
  attach(dat)
  len=dim(dat)[1]/2
  dat1 = dat[1:len,]
  dat2 = dat[-c(1:len),]
  detach(dat)
  
  # Definiamo distanza e clusters
  dat.e <- dist(dat1[,1:3], method = "euclidean")
  dat.ec <- hclust(dat.e, method = "complete")
  cluster.ec1 <- cutree(dat.ec, k=3)
  
  dat.e <- dist(dat2[,1:3], method = "euclidean")
  dat.ec <- hclust(dat.e, method = "complete")
  cluster.ec2 <- cutree(dat.ec, k=3)
  
  cluster.ec <- c(cluster.ec1,cluster.ec2)
  
  for(j in 1:dim(values(img))[1])
    if(sum(values(img)[j,])!=0)
    {
      if(cluster.ec[j]==1)
      {
        values(img)[j,1]=0
        values(img)[j,2]=0
        values(img)[j,3]=0
        values(img)[j,4]=0
      }
      else
      {
        values(img)[j,1]=values(img)[j,1]
        values(img)[j,2]=values(img)[j,2]
        values(img)[j,3]=values(img)[j,3]
        values(img)[j,4]=values(img)[j,4]
      }
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
