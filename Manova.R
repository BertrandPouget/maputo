library(sf)
library(tidyverse)



#_______________________________________________________________________________
##### One-way MANOVA
##### (p=6, g=2)
#####---------------

dat <- st_read("datonite.shp")
dat <- dat[which(dat$osm_surf != "unk"),]
dat <- dat[order(dat$osm_surf),]
rownames(dat) <- 1:nrow(dat)

attach(dat)
dat_num <- cbind.data.frame(rmean, rvar, rmed)
detach(dat)

categories <- factor(dat$osm_surf, labels=c('paved', 'unpaved'))

i1 <- which(categories=='paved')
i2 <- which(categories=='unpaved')

par(mfrow=c(1,3))
boxplot(dat_num$rm~categories, col = rainbow(2))
boxplot(dat_num$gm~categories, col = rainbow(2))
boxplot(dat_num$bm~categories, col = rainbow(2))

n1 <- length(i1)
n2 <- length(i2)
n  <- n1+n2

g  <- length(levels(categories))
p  <- 2

fit <- manova(as.matrix(dat_num) ~ categories)
summary.manova(fit,test="Wilks")

summary.aov(fit)

### Via Bonferroni
alpha <- 0.05
k <- p*g*(g-1)/2
qT <- qt(1-alpha/(2*k), n-g)

W <- summary.manova(fit)$SS$Residuals
m  <- sapply(dat_num,mean)         # estimates mu
m1 <- sapply(dat_num[i1,],mean)    # estimates mu.1=mu+tau.1
m2 <- sapply(dat_num[i2,],mean)    # estimates mu.2=mu+tau.2

inf12 <- m1-m2 - qT * sqrt( diag(W)/(n-g) * (1/n1+1/n2) )
sup12 <- m1-m2 + qT * sqrt( diag(W)/(n-g) * (1/n1+1/n2) )

CI <- list(paved_unpaved=cbind(inf12, sup12))
CI

mg <- rbind(m1,m2)
sp.name <- c('rmean','rvar','rmed')
for(k in 1:3){
  plot(c(1,g*(g-1)/2),ylim=c(-4,4), xlim=c(1,3), pch='', 
       xlab='pairs treat', ylab=paste('CI tau',k), 
       main=paste('CI tau',sp.name[k]))
  lines (c(1,1), c(CI[[1]][k,1],CI[[1]][k,2])); 
  points(1, mg[1,k]-mg[2,k], pch=16); 
  points(1, CI[[1]][k,1], col=rainbow(g)[2], pch=16); 
  points(1, CI[[1]][k,2], col=rainbow(g)[1], pch=16);  
  abline(h=0)
}

dev.off()

