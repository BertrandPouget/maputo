
#Ricorda di settare la tua working directory da Session!

library(sf)
library(tidyverse)
library(e1071)
library(rgl)
library(misc3d)

#Creiamo il dataset senza geometry

dat <- st_read("datonite.shp")
dat <- st_drop_geometry(dat)
dat <- dat[which(dat$osm_surf != "unk"),]
dat <- dat[order(dat$osm_surf),]
rownames(dat) <- 1:nrow(dat)
categories <- dat[,4]

#Creiamo e plottiamo il dataset numerico

keeps <- c("rmed","rvar","rmax","gmed","gvar","gmax","bmed","bvar","bmax","osm_surf")
dat_num <- dat[keeps]
attach(dat)
plot(scale(dat_num[,1:2]), col = ifelse(osm_surf=="paved","red","blue"))
detach(dat)

#######################################################################################
###PER ORA O FAI CON 2 E RIESCI A PLOTTARE, O FAI CON TUTTE E OTTIENI UN OTTIMO APER###
#######################################################################################

############# QDA ###################
###--------------------------
#### Assumptions:
###------------------
# 1) if L=i, X.i ~ N(mu.i, sigma.i^2), i=A,B
# 2) c(A|B)=c(B|A) (equal misclassification costs)

qda.iris <- qda(dat_num[,1:9], dat_num$osm_surf)
qda.iris
Qda.iris <- predict(qda.iris, dat_num[,1:9])

# 1) APER (without priors)
table(class.true=dat_num$osm_surf, class.assigned=Qda.iris$class)
errorsq <- (Qda.iris$class != dat_num$osm_surf)
APERq   <- sum(errorsq)/length(dat_num$osm_surf)
APERq
# Remark: correct only if we estimate the priors through the sample frequencies!

# 2) AER L1OCV (without priors)
QdaCV.iris <- qda(dat_num[,1:9], dat_num$osm_surf, CV=T)
table(class.true=dat_num$osm_surf, class.assignedCV=QdaCV.iris$class)
errorsqCV <- (QdaCV.iris$class != dat_num$osm_surf)
AERqCV   <- sum(errorsqCV)/length(dat_num$osm_surf)

# Plot partition
x11()
plot(dat_num[,1:2], main='Plot', pch=20)
points(dat_num[which(dat_num$osm_surf=="paved"),1:2], col='red', pch=20)
points(dat_num[which(dat_num$osm_surf=="unpaved"),1:2], col='green', pch=20)
legend("topright", legend=levels(dat_num$osm_surf), fill=c('red','green'), cex=.7)

points(qda.iris$means, pch=4,col=c('red','green') , lwd=2, cex=1.5)
x  <- seq(min(dat_num[,1]), max(dat_num[,1]), length=200)
y  <- seq(min(dat_num[,2]), max(dat_num[,2]), length=200)
xy <- expand.grid(Sepal.Length=x, Sepal.Width=y) #### CHANGE THIS ONE

z  <- predict(qda.iris, xy)$post  # these are P_i*f_i(x,y)  
z1 <- z[,1] - pmax(z[,2])  # P_1*f_1(x,y)-max{P_j*f_j(x,y)}  
z2 <- z[,2] - pmax(z[,1])  # P_2*f_2(x,y)-max{P_j*f_j(x,y)}    

# Plot the contour line of level (levels=0) of z1, z2, z3: 
# P_i*f_i(x,y)-max{P_j*f_j(x,y)}=0 i.e., boundary between R.i and R.j 
# where j realizes the max.
contour(x, y, matrix(z1, 200), levels=0, drawlabels=F, add=T)  
contour(x, y, matrix(z2, 200), levels=0, drawlabels=F, add=T)
