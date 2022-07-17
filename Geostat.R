###--------------------###
###    GEOSTATISTICS   ###
###--------------------###

## Clear the workspace
rm(list=ls())

## Load spatial packages

library(sp)           ## Data management
library(lattice)      ## Data management
library(geoR)         ## Geostatistics
library(gstat)        ## Geostatistics

setwd("C:/Users/giuli/OneDrive/Desktop/Maputo")

library(sf)
library(sp)

d=st_read("lance.shp")


#Creiamo e abitiamo i vettori delle medie

mean_x = rep(0,5116)
mean_y = rep(0,5116)
dist=rep(0,5116)
attach(d)

cityhall=c(3626100,-2995123)

for(i in 1:5116)
{
  g = st_geometry(d)[[i]]
  mat = matrix(unlist(g), ncol = 2, nrow = length(unlist(g))/2)
  mean_x[i] = mean(mat[,1])
  mean_y[i] = mean(mat[,2])
  dist[i] = sqrt((cityhall[1]-mean_x[i])^2+(cityhall[2]-mean_y[i])^2)
}
rm(g,mat,i)
#Sostituiamo la colonna della geometria con le medie calcolate
#e togliamo i dati "sporchi"

d = st_drop_geometry(d)
d = cbind.data.frame(d,mean_x,mean_y,dist)
d=d[-c(4287, 1218, 4368, 3337, 3325, 3990),]
detach(d)
rm(mean_x,mean_y,dist)

d=d[,c(2:4,22:24)]



#############################################################
##############                                 ##############
######   EXPLORATORY ANALYSIS & VARIOGRAM ESTIMATION  #######
##############                                 ##############
#############################################################

## Define the sample coordinates
coordinates(d) <- c('mean_x','mean_y')

# bubble plot(obj,zcol,...)
# key.space=location of the key
bubble(d,'Length',maxsize=1)

dev.off()



## Exploratory Analysis ##
##----------------------##

# histogram of zinc variable
hist(Length, breaks=16, col="grey", main='Histogram of Ln', prob = TRUE, xlab = 'Ln')
# highly skewed, transform to the log
hist(log(Length), breaks=16, col="grey", main='Histogram of log(Zn)', prob = TRUE, xlab = 'log(Zn)')

# scatterplot of log(zinc) with respect to distance from the river 
xyplot(Length ~ dist, as.data.frame(d))

# Negative correlation: lower distance from the river => higher level of zinc
dev.off()


## Estimating Spatial Correlation ##
##     Variogram Analysis         ##
##--------------------------------##

# sample variogram (binned estimator)
svgm <- variogram(log(zinc) ~ 1, d)
plot(svgm, main = 'Sample Variogram',pch=19)

dev.off()

# N.B.  the notation "~ 1" stands for a single constant predictor
#       (hp: spatially constant mean)

# default decisions:
# direction dependence, cutoff, lag width

# the following
plot(variogram(log(zinc) ~ 1, d),pch=19)
# automatically decides to ignore direction: point pairs are merged on the
# basis of distance to compute the empirical variogram

plot(variogram(log(zinc) ~ 1, d, alpha = c(0, 45, 90, 135)),pch=19)
# point pairs whose separation vector has a given direction are used in each
# panel (not too many directions otherwise noise will increase)
# Note: zonal anisotropy

# cutoff distance: maximum distance up to which point pairs are considered
#                  (default = bbox diagonal / 3)
# lag width: width of distance intervals over which point pairs are averaged
#            in bins (default = cutoff distance / 15)

plot(variogram(log(zinc) ~ 1, d, cutoff = 1000, width = 1000/15),pch=19)

# intervals can have different widths: to fix varying widths use the argument
# boudaries
plot(variogram(log(zinc) ~ 1, d, boundaries = c(0,200,seq(400,1500,100))),pch=19)
# useful for data sets that have much information on short distance variability

plot(variogram(log(zinc) ~ 1, d, cutoff = 1000, width = 1000/5),pch=19)


## Variogram modeling
##-------------------

# list of parametric isotropic variogram models
vgm()

# in gstat, valid variogram models are constructed by using one or
# combination of two or more basic variogram models
# first argument of the function 'vgm' is partial sill,
# then the desired model, then range, and finally nugget: 
# vgm(sill, model, range, nugget)

# some examples...
vgm(1, "Sph", 300)
vgm(1, "Sph", 300, 0.5)

# one can also add two or more models
v1 <- vgm(1, "Sph", 300, 0.5)
v2 <- vgm(0.8, "Sph", 800, add.to = v1)
v2

# this is only measurement error
vgm(0.5, "Nug", 0)

## weighted least squares fitting a variogram model to the sample variogram
## STEPS:
## 1) choose a suitable model
## 2) choose suitable initial values for partial sill, range & nugget
## 3) fit the model using one of the possible fitting criteria

v <- variogram(log(zinc) ~ 1, d)

plot(v,pch=19)
# Linear behavior near the origin, growth not very fast 
# Recall: both spherical and exponential model have a linear behavior near the
#         origin but exponential model has a faster growth than the spherical one
# => we fit a spherical model

# try reasonable initial values
fit.variogram(v, vgm(1, "Sph", 800, 1))

# try unreasonable initial values
fit.variogram(v, vgm(1, "Sph", 10, 1))
# due to high non linearity in the minimization problem,
# starting from unreasonable initial values might cause fail to converge

# plot of the final fit
v <- variogram(log(zinc) ~ 1, d)
v.fit <- fit.variogram(v, vgm(1, "Sph", 800, 1))
plot(v, v.fit, pch = 19)

# fitting method: non linear regression with minimization of weighted
# sum of squares error. final value of the minimum
attr(v.fit, 'SSErr')
# how can we choose weights? argument fit.method in fit.variogram
# fit.method = 1 : w = N_j
# fit.method = 2 : w = N_j/gamma(h_j)^2
# fit.method = 6 : w = 1
# fit.method = 7 : w = N_j/h_j^2

# one can also keep one of the parameters fixed, and fit only the others.
# this is common for the nugget parameter, which may be hard to infer from data
# when sample locations are regularly spread. Information may be derived from
# measurement error characteristics for a specific device.

# ex: fix the nugget variance to the value 0.06
fit.variogram(v, vgm(1, "Sph", 800, 0.06), fit.sills = c(FALSE, TRUE))
# the range parameters can be fixed using argument fit.ranges

## maximum likelihood fitting of variogram models
## - does not need the sample variogram
## - can be performed through restricted maximum likelihood

fit.variogram.reml(log(zinc)~1, d, model=vgm(0.6, "Sph", 800, 0.06))
v.fit


## modeling anisotropy*
v.dir <- variogram(log(zinc)~1,d,alpha=(0:3)*45) 
v.anis <- vgm(.6, "Sph", 1600, .05, anis=c(45, 0.3))

print(plot(v.dir, v.anis, pch=19))

#############################################################
##############                                 ##############
######          SPATIAL PREDICTION & KRIGING          #######
##############                                 ##############
#############################################################

## Stationary Univariate Spatial Prediction (Ordinary Kriging)
##-------------------------------------------------------------

## Prediction at a single new location 
s0.new=data.frame(x=179180, y=330100) # UTM coordinates
coordinates(s0.new)=c('x','y')

# plot all together
image(meuse.grid, col = "lightgrey")
plot(meuse.sr, col = "grey", add = TRUE)
plot(d, add = TRUE)
plot(s0.new, add = TRUE, col='red', lwd = 2)
title('meuse river geostatistical data')

# Create a gstat object setting a spherical (residual) variogram
# gstat(g.obj, id, formula, data, model, set,...)
g.tr <- gstat(formula = log(zinc) ~ 1, data = d, model = v.fit)

## ordinary kriging
# Make the ordinary kriging prediction with the function: 
# predict(obj, grid, BLUE=FALSE)
# this gives the prediction of Y(s_0):
predict(g.tr, s0.new)
# variance > 0 (as expected)

# Estimate the mean:use the argument 'BLUE'
predict(g.tr, s0.new, BLUE = TRUE)
# this gives the estimate of the mean
# (trend component) under gls

## consider a location where I OBSERVE data
# this gives the prediction of Y(s_0)
# in the first location (zero variance!)
d[1,]
predict(g.tr, d[1,])

# this gives the estimate of the mean
# (drift component) under gls
predict(g.tr, d[1,], BLUE = TRUE)

# prediction over the entire grid
lz.ok <- predict(g.tr, meuse.grid, BLUE = FALSE)

spplot(lz.ok)

## Non-stationary Univariate Spatial Prediction (Universal Kriging)
##-----------------------------------------------------------------
# the hypothesis of spatially constant mean may be too restrictive!
# we now use as covariate the square root of the distance from the river Meuse

# to fit the variogram on the residuals, one should take into account 
# the spatial dependence while estimating the trend component by using GLS

# Create a gstat object setting a spherical (residual) variogram
# gstat(g.obj, id, formula, data, model, set,...)
meuse.gstat <- gstat(id = 'zinc', formula = log(zinc) ~ sqrt(dist),
                     data = d, nmax = 50, model=v.fit, set = list(gls=1))
meuse.gstat

# Estimate the variogram from GLS residuals:
?variogram.gstat
v.gls<-variogram(meuse.gstat)
plot(v.gls)

v.gls.fit <- fit.variogram(v.gls, vgm(1, "Sph", 800, 1))
plot(v.gls, v.gls.fit, pch = 19)

# Update gstat object with variogram model
meuse.gstat <- gstat(id = 'zinc', formula = log(zinc) ~ sqrt(dist),
                     data = d, nmax = 50, model=v.gls.fit, set = list(gls=1))

## universal kriging:
## I have to define the covariate in s_0
s0.vec <- as.vector(slot(s0.new,'coords'))
# distance to the river: calculate the distance between s0 and each point of
# the river, then select the minimum
s0.dist <- min(rowSums(scale(meuse.riv,s0.vec)^2)) 
s0.new <- as.data.frame(c(s0.new,s0.dist))
names(s0.new) <- c('x','y','dist')
coordinates(s0.new) <- c('x','y')
s0.new <- as(s0.new, 'SpatialPointsDataFrame')
s0.new

# Function "predict" uses the residual variogram stored in the gstat
# object to make the prediction
predict(meuse.gstat, s0.new)
# variance > 0 (as expected)

# this gives the estimate of x(s_0)'*beta
# (trend component) under gls
predict(meuse.gstat, s0.new, BLUE = TRUE)

# prediction over the entire grid
lz.uk <- predict(meuse.gstat, meuse.grid, BLUE=FALSE)

# estimate of the mean over the entire grid
lz.uk.BLUE <- predict(meuse.gstat, meuse.grid, BLUE=TRUE)

spplot(lz.ok[,1], main = 'Ordinary Kriging, gstat')

spplot(lz.uk[,1], main = 'Universal Kriging, gstat')

spplot(lz.uk.BLUE[,1], main = 'Universal Kriging - drift , gstat')

# Is the drift important to explain the variability of the response variable?
# Let's compare the variogram of the data and of the residuals:
plot(v$dist,v$gamma,xlab='distance',ylab='semivariance',pch=19,col='skyblue1',ylim=c(0,0.8))
curve(v.f.est(x, C0=v.fit[2,2]+v.fit[1,2], cov.pars=rbind(c(v.fit[2,2], v.fit[2,3]),c(v.fit[1,2], v.fit[1,3])), cov.model = c("spherical","pure.nugget")), from = 0.0001, to = 1600,
      xlab = "distance", ylab = expression(gamma(h)),
      main = "Variogram model",add=TRUE,col='skyblue1',lwd=2, ylim=c(0,110))
points(v.gls$dist,v.gls$gamma,xlab='distance',ylab='semivariance',pch=19,col='steelblue',ylim=c(0,0.8))
curve(v.f.est(x, C0=v.gls.fit[2,2]+v.gls.fit[1,2], 
              cov.pars=rbind(c(v.gls.fit[2,2], v.gls.fit[2,3]),c(v.gls.fit[1,2], v.gls.fit[1,3])), cov.model = c("spherical","pure.nugget")), from = 0.0001, to = 1600,
      xlab = "distance", ylab = expression(gamma(h)),
      main = "Variogram model",add=TRUE,col='steelblue',lwd=2, ylim=c(0,110))

graphics.off()

#############################################################
##############                                 ##############
######           EXERCIZES FROM PAST EXAMS            #######
##############                                 ##############
#############################################################

# One of the most relevant consequences of the eruption of volcan 
# Eyjafjoll (in Iceland), in 2010, is the contamination by fluoride. 
# The latter is due to the deposit on the ground of the ash released
# in the atmosphere during the eruption.
# The file "fluoruro.txt" reports the coordinates of 50 measurement sites
# s_i, i=1,...,50, the corresponding concentrations of fluoride (ppm) F(s_i)
# and the distance D.s_i of each site s_i from the crater of the volcano.
# Denoting by delta a zero-mean, second-order stationary and isotropic random
# field:
# a) Estimate two empirical variograms, assuming the following models:
#    F(s_i)=beta_0+delta(s_i) and 
#    F(s_i)=beta_0+beta_1*D.s_i+delta(s_i). 
#    Choose the most appropriate model for the observations.
# b) Fit to the empirical variogram at point (a), a Gaussian model
#    without nugget, via weighted least squares. Use as initial parameters: 
#    sill=100, range=0.02. Report the estimates of sill and range.
# c) Fit to the empirical variogram chosen at point (a), a spherical model
#    without nugget, via weighted least squares. Report the estimates of sill 
#    and range.
# d) Compare the variograms estimated at points (b) and (c), with the empirical
#    variogram at point (a). Given that the ash deposition is known to be
#    a very regular phenomenon, which variogram model is the most appropriate?
# e) Based on model (d), estimate the concentration of fluoride due to the eruption
#    in the city of Raufarhofn (s0 = (0.3; 0.24), D.s0 = 0.1970) 
# f) Based on model (d), estimate the concentration of fluoride at the same
#    location, due to a possible new eruption of equivalent intensity, independent 
#    of that of 2010.

#  SOLUTION
#_____________

# Data import
data=read.table('fluoruro.txt')
names(data)[3]='f'
attach(data)
coordinates(data)=c('X','Y')

# a) Estimate two empirical variograms, assuming the following models:
#    F(s_i)=beta_0+delta(s_i) and 
#    F(s_i)=beta_0+beta_1*D.s_i+delta(s_i). 
#    Choose the most appropriate model for the observations.

v=variogram(f ~ 1, data=data)
plot(v,pch=19)

v.t=variogram(f ~ D, data=data)
plot(v.t,pch=19)

# b) Fit to the empirical variogram at point (a), a Gaussian model
#    without nugget, via weighted least squares. Use as initial parameters: 
#    sill=100, range=0.02. Report the estimates of sill and range.
v.fit2 <- fit.variogram(v.t, vgm(100, "Gau", 0.02))
plot(v.t, v.fit2, pch = 3)
v.fit2

# c) Fit to the empirical variogram chosen at point (a), a spherical model
#    without nugget, via weighted least squares. Report the estimates of sill 
#    and range.
v.fit1 <- fit.variogram(v.t, vgm(100, "Sph", 0.08))
plot(v.t, v.fit1, pch = 3)
v.fit1

# d) Compare the variograms estimated at points (b) and (c), with the empirical
#    variogram at point (a). Given that the ash deposition is known to be
#    a very regular phenomenon, which variogram model is the most appropriate?

# We choose v.fit2

# e) Based on model (d), estimate the concentration of fluoride due to the eruption
#    in the city of Raufarhofn (s0 = (0.3; 0.24), D.s0 = 0.1970) 

g.t <- gstat(formula = f ~ D, data = data, model = v.fit2)

D.s0=0.1970
s0=as.data.frame(matrix(c(0.3,0.24,D.s0),1,3))
names(s0)=c('X','Y','D')
coordinates(s0)=c('X','Y')

predict(g.t, s0, BLUE = FALSE)

# f) Based on model (d), estimate the concentration of fluoride at the same
#    location, due to a possible new eruption of equivalent intensity, independent 
#    of that of 2010.

predict(g.t, s0, BLUE = TRUE)

detach(data)

#________________________________________________________________________________
#
# The file radioville.txt reports the information on 158 control units
# in the area around the nuclear power plant of Radioville.
# At each site, available data consist of: radioactivity levels [Bq],
# longitude [°N], latitude [°W] and type of soil [urban/vegetation].
# Denoting by s_i the i-th site, by R the radioactivity level,
# by eps a weakly stationary random field and by D a dummy 
# urban/vegetation:

# a) estimate the parameters of the linear model 
#    R(s_i) = beta_0 + beta_1 D(s_i) + eps(s_i) 
#    assuming for eps a spherical variogram without nugget, estimated
#    via weighted least squares;
# b) estimate the parameters of the linear model 
#    R(s_i) = beta_0 + beta_1 D(s_i) + eps(s_i) 
#    assuming for eps a spherical variogram with nugget, estimated
#    via weighted least squares;
# c) choose the best variogram model by comparing the fitted model 
#    with the corresponding empirical variograms (report qualitative
#    plots and the estimated variogram parameters)
# d) on the basis of model (c), predict the radioactivity level at the
#    parking lot of the shopping centre of Radioville (lon = 78.59, 
#    lat = 35.34), and in the park of Radioville (lon = 77.6, 
#    lat = 34.99);
# e) estimate variance of prediction error at the same locations
#    as at point d).

#  SOLUTION
#_____________

data <- read.table('radioville.txt',header=TRUE)
attach(data)

# create dummy: 0 = urban, 1 = vegetation
DUMMY <- rep(0,length(D))
DUMMY[which(D=='V')] <- 1
data <- data.frame(cbind(Bq,Long,Lat,DUMMY))
names(data) <- c('Bq','Long','Lat','D')
coordinates(data) <- c('Long','Lat')

## point a)
## fitting a variogram without nugget

v <- variogram(Bq ~ D, data = data)
plot(v)
v.fit1 <- fit.variogram(v, vgm(1, "Sph", 0.5))
plot(v, v.fit1, pch = 3)
v.fit1

# coefficient of the linear model: 
# it sufficies to estimate the drift at two locations where we have observations,
# with D=U and D=V
# data[1,] = urbane
# data[6,] = vegetation
g.tr <- gstat(formula = Bq ~ D, data = data, model = v.fit1)
predict(g.tr, data[1,], BLUE = TRUE)
predict(g.tr, data[6,], BLUE = TRUE)

## point b)
## fitting a variogram with nugget

v <- variogram(Bq ~ D, data = data)
plot(v)
v.fit2 <- fit.variogram(v, vgm(0.6, "Sph", 0.5, 0.1))
plot(v, v.fit2, pch = 3)
v.fit2

# it sufficies to estimate the drift at two locations where we have observations,
# with D=U and D=V
# data[1,] = urbane
# data[6,] = vegetation
g.tr <- gstat(formula = Bq ~ D, data = data, model = v.fit2)
predict(g.tr, data[1,], BLUE = TRUE)
predict(g.tr, data[6,], BLUE = TRUE)


## point d)
## predict at 2 new locations: we use model 1 (without nugget)
g.tr <- gstat(formula = Bq ~ D, data = data, model = v.fit1)

# urbane : 78.59,35.34
s0.new <- as.data.frame(matrix(c(78.59,35.34,0),1,3))
names(s0.new) <- c('lon','lat','D')
coordinates(s0.new) <- c('lon','lat')
predict(g.tr, s0.new)

# vegetation : 77.69,34.99
s0.new <- as.data.frame(matrix(c(77.69,34.99,1),1,3))
names(s0.new) <- c('lon','lat','D')
coordinates(s0.new) <- c('lon','lat')
predict(g.tr, s0.new)



