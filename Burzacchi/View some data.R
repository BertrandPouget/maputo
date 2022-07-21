
#Ricorda di settare la tua working directory da Session!

rm(list=ls())

library(sf)
library(sp)
library(ggplot2)


# Shapefiles -------------------------------------------------------------------

# Shapefile con i segmenti di strada
data = st_read("datonite.shp")
data = data[order(as.numeric(data$id)),]
data

N_streets = dim(data)[1]

summary(data$Length)


cbind( "n"=table(data$osm_typo),
       "perc"=round(table(data$osm_typo)/N_streets*100,2) )
# nb: "unk"-> "unclassified": it is a type of road!


cbind( "n"=table(data$osm_surf),
       "perc"=round(table(data$osm_surf)/N_streets*100,2) )
# nb: "unk"-> "unknown": road surface is not known


# Greater Maputo area:
st_bbox(data) 
(st_bbox(data)[3]-st_bbox(data)[1])*(st_bbox(data)[4]-st_bbox(data)[2]) # area
sum(data$Length)*1e-03 # km


# plot di alcuni segmenti:
i_asp = which(data$osm_surf=="paved")
i_unp = which(data$osm_surf=="unpaved")
i_unk = which(data$osm_surf=="unk")

ttt = character(dim(data)[1])
ttt[i_unk]= "1. unknown"; ttt[i_unp]="2. unpaved"; ttt[i_asp]="3. paved"

windows();  ggplot() + 
  geom_sf(data = data, aes(color=ttt,fill=ttt))+
  scale_fill_manual(values=c("darkgray", "forestgreen", "brown"))+
  scale_color_manual(values=c("darkgray", "forestgreen", "brown"))+
  labs(fill= "Pavement surface")+
  coord_sf() +
  theme(panel.grid.major = element_line(color = gray(.9), linetype=3, size=0.2), 
        panel.background = element_rect(fill="cornsilk1"))+
  guides(color=FALSE)




# Images  -----------------------------------------------------------------

library(rgdal)
library(sp)
library(raster)


head(data[which(data$osm_surf=="paved"),])
head(data[which(data$osm_surf=="unpaved"),])


# paved and unpaved
setwd("C:/Users/markh/Desktop/Università/Maputo/rater_mask_datonite")
img_paved = brick("img__7.tif")
img_unpaved = brick("img__129.tif")



# full pictures:
par(mfrow=c(2,1)); plotRGB(img_paved); plotRGB(img_unpaved)


# layer by layer:
names(img_paved) = names(img_unpaved) = c("redBand","greenBand", "blueBand", "null")
plot(img_paved, col=gray(1:100/100)) 


View(values(img_paved))

# histogram of pixel components: red, green, blue
{
  par(mfrow=c(2,3))
  hist(img_paved$redBand, xlab="", main='red values', col='red')
  hist(img_paved$greenBand, xlab="", main='green values', col='green')
  hist(img_paved$blueBand, xlab="", main='blue values', col='blue')
  hist(img_unpaved$redBand, xlab="", main='red values', col='red')
  hist(img_unpaved$greenBand, xlab="", main='green values', col='green')
  hist(img_unpaved$blueBand, xlab="", main='blue values', col='blue')
  par(mfrow=c(1,1))
}

val_paved = data.frame(values(img_paved)[which(apply(values(img_paved),1,sum)!=0),])
val_unpaved = data.frame(values(img_unpaved)[which(apply(values(img_unpaved),1,sum)!=0),])

{
  par(mfrow=c(2,3))
  hist(val_paved$redBand, xlab="", main='red values', col='red')
  hist(val_paved$greenBand, xlab="", main='green values', col='green')
  hist(val_paved$blueBand, xlab="", main='blue values', col='blue')
  hist(val_unpaved$redBand, xlab="", main='red values', col='red')
  hist(val_unpaved$greenBand, xlab="", main='green values', col='green')
  hist(val_unpaved$blueBand, xlab="", main='blue values', col='blue')
  par(mfrow=c(1,1))
}

graphics.off()


