library(raster)

dataroot<-"C:/Users/schafstall/Desktop/R iLand/Input script/" # wd
r0<-raster(paste0(dataroot,"kerneltest0_alb.asc"))

par(cex=0.5)

xmin <- 4000
xmax <- 6000
ymin <- 4000
ymax <- 6000

plot(r0, xlim = c(xmin, xmax), ylim = c(ymin, ymax), xlab="distance in m", ylab="distance in m", main = "Asian longhorn beetle kernel: exp(-0.1478*sqrt(x)")

#########################

dataroot<-"C:/Users/schafstall/Desktop/R iLand/Input script/" # wd
r0<-raster(paste0(dataroot,"kerneltest_3.asc"))

par(cex=0.5)


plot(r0, xlab="distance in m", ylab="distance in m", main = "Modified inverse 2Dt: ''exp(-x*x/(2*3.14159*500*500))', x-max = 1500")




