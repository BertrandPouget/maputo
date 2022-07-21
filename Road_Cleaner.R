
#Ricorda di settare la tua working directory da Session!

rm(list=ls())
graphics.off()

library(sf)
library(sp)
library(ggplot2)
library(rgdal)
library(rgl)
library(raster)
library(ramify)

#Importiamo un esempio di immagine e plottiamola
#(Vai nella directory di rater_mask_datonite!)

img = brick("img__1573.tif")
plotRGB(img)
names(img) = c("redBand","greenBand", "blueBand", "null")

dat = data.frame(values(img))
dat_col=dat[which(values(img)[,4]!=0),]

dat.e <- dist(dat_col[,1:3], method = "euclidean")
dat.ec <- hclust(dat.e, method = "complete")
cluster.ec <- cutree(dat.ec, k=4)
dat_col=cbind.data.frame(dat_col,cluster.ec)

x11()
par(mfrow=c(2,1), bg = "cornsilk1")
plot(dat.ec, main = "Dendrogram", hang = -0.1, xlab = "", labels = F, cex = 0.6, sub = "")
# with k cluster
rect.hclust(dat.ec, k = 4)

clus=rep(0,dim(dat)[1])
j=0
for(i in row.names(dat_col))
{
  j=j+1
  clus[as.numeric(i)]=dat_col$cluster.ec[j]
}

dat=cbind.data.frame(dat,clus)

#Dividiamo il dataset in due parti per motivi computazionali

# attach(dat)
# len=dim(dat)[1]/2
# dat1 = dat[1:len,]
# dat2 = dat[-c(1:len),]
# detach(dat)
# 
# #Definiamo distanza e clusters e plottiamo
# 
# dat.e <- dist(dat1[,1:3], method = "euclidean")
# dat.ec <- hclust(dat.e, method = "complete")
# cluster.ec1 <- cutree(dat.ec, k=3)
# cluster.ec1[which(cluster.ec1==argmax(t(table(cluster.ec1))))]=4
# 
# dat.e <- dist(dat2[,1:3], method = "euclidean")
# dat.ec <- hclust(dat.e, method = "complete")
# cluster.ec2 <- cutree(dat.ec, k=3)
# cluster.ec2[which(cluster.ec2==argmax(t(table(cluster.ec2))))]=4
# 
# cluster.ec <- c(cluster.ec1,cluster.ec2)

plot3d(dat_col[,1:3], col=cluster.ec+1, pch=19)

k=dat_col[which(dat_col[,1]==min(dat_col[,1])),5]

checki=intersect(unique(which(dat$clus==k)), which(values(img)[,4]==255))


for(i in checki) #se è del cluster 1, va eliminato
{
  values(img)[i,1]=0
  values(img)[i,2]=0
  values(img)[i,3]=0
  values(img)[i,4]=0
}

plotRGB(img)



# TEST modifica una strada

# colori = data.frame(values(img)[which(apply(values(img),1,sum)!=0),])
# data$rmean[which(data$image=="img__1573.tif")]=mean(colori$redBand)
# data$rvar[which(data$image=="img__1573.tif")]=var(colori$redBand)
# data$rmed[which(data$image=="img__1573.tif")]=median(colori$redBand)
# data$rmin[which(data$image=="img__1573.tif")]=min(colori$redBand)
# data$rmax[which(data$image=="img__1573.tif")]=max(colori$redBand)
# 
# data$gmean[which(data$image=="img__1573.tif")]=mean(colori$greenBand)
# data$gvar[which(data$image=="img__1573.tif")]=var(colori$greenBand)
# data$gmed[which(data$image=="img__1573.tif")]=median(colori$greenBand)
# data$gmin[which(data$image=="img__1573.tif")]=min(colori$greenBand)
# data$gmax[which(data$image=="img__1573.tif")]=max(colori$greenBand)
# 
# data$bmean[which(data$image=="img__1573.tif")]=mean(colori$blueBand)
# data$bvar[which(data$image=="img__1573.tif")]=var(colori$blueBand)
# data$bmed[which(data$image=="img__1573.tif")]=median(colori$blueBand)
# data$bmin[which(data$image=="img__1573.tif")]=min(colori$blueBand)
# data$bmax[which(data$image=="img__1573.tif")]=max(colori$blueBand)
