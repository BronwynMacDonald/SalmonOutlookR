#https://gist.github.com/scrogster/7fc5b7597b63585a00b6


library(png)
library(RCurl)
#I got these free png silhouettes of red fox and rabbit from phylopic.org
foxurl<-"http://phylopic.org/assets/images/submissions/51b1b6e4-129d-41a6-bbbd-c3fab459c25f.1024.png"
raburl<-"http://phylopic.org/assets/images/submissions/1e15411c-5394-4a9d-a209-76c8ac0c331d.1024.png"
fox_logo <-  readPNG(getURLContent(foxurl))
rab_logo <-  readPNG(getURLContent(raburl))

#utility function for embedding png images at specified fractional sizes in R plots
#places the logo centred on a specified fraction of the the usr space,
#and sizes appropriately (respects aspect ratio)
logoing_func<-function(logo, x, y, size){
  dims<-dim(logo)[1:2] #number of x-y pixels for the logo (aspect ratio)
  AR<-dims[1]/dims[2]
  par(usr=c(0, 1, 0, 1))
  rasterImage(logo, x-(size/2), y-(AR*size/2), x+(size/2), y+(AR*size/2), interpolate=TRUE)
}

#Demo: a time-series plot of fake fox and rabbit abundance data with phylopic logos overlaid
pdf("fox_plot.pdf", width=6, height=10)
layout(matrix(1:2, nrow=2))
set.seed(122)
plot(y=50*cumprod(exp(rnorm(30, 0.05, 0.1))), x=1981:2010, xlab="Time", pch=16,
     ylab="Fox abundance", las=1, col="tomato", lwd=2, type="o", ylim=c(0, 150))
#adding a fox silhouette logo near the bottom righthand corner
logoing_func(fox_logo, x=0.10, y=0.90, size=0.15)
title(main="Index of fox abundance, 1981-2010")
plot(y=50*cumprod(exp(rnorm(30, -0.05, 0.2))), x=1981:2010, xlab="Time", pch=17,
     ylab="Rabbit abundance", las=1, col="orange", lwd=2, type="o", ylim=c(0, 150))
#adding a fox silhouette logo near the bottom righthand corner
logoing_func(rab_logo, x=0.10, y=0.90, size=0.15)
title(main="Index of rabbit abundance, 1981-2010")
dev.off()
