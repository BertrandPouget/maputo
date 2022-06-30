
#Ricorda di settare la tua working directory da Session!

library(sf)
library(raster)
datini=st_read("datini.shp")

#Creiamo e aggiungiamo le colonne relative ai dati di interesse

rmean=rvar=rmed=rmin=rmax=gmean=gvar=gmed=gmin=gmax=bmean=bvar=bmed=bmin=bmax=rep(0,dim(datini)[1])
datonite=cbind(datini,rmean,rvar,rmed,rmin,rmax,gmean,gvar,gmed,gmin,gmax,bmean,bvar,bmed,bmin,bmax)

#Abitiamo le colonne e creiamo il nuvo dataset

for(i in 1:dim(datini)[1])
{
  img = brick(datonite$image[i])
  names(img) = c("r","g", "b", "null")
  colori = data.frame(values(img)[which(apply(values(img),1,sum)!=0),])
  datonite$rmean[i]=mean(colori$r)
  datonite$rvar[i]=var(colori$r)
  datonite$rmed[i]=median(colori$r)
  datonite$rmin[i]=min(colori$r)
  datonite$rmax[i]=max(colori$r)
  
  datonite$gmean[i]=mean(colori$g)
  datonite$gvar[i]=var(colori$g)
  datonite$gmed[i]=median(colori$g)
  datonite$gmin[i]=min(colori$g)
  datonite$gmax[i]=max(colori$g)
  
  datonite$bmean[i]=mean(colori$b)
  datonite$bvar[i]=var(colori$b)
  datonite$bmed[i]=median(colori$b)
  datonite$bmin[i]=min(colori$b)
  datonite$bmax[i]=max(colori$b)
}

st_write(datonite,"datonite.shp")
