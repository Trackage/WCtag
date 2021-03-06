
library(SGAT)
library(WCtag)

data(ElephantSeal2)
data(ElephantSeal2calib)

dc <- ElephantSeal2calib
d <- ElephantSeal2

deploy <- c(158.950, -54.5)
recapt <- deploy
calib <- wc(wcparams(gen = "Splash"), offset = 70)
cfun <- with(dc, approxfun(zenith, light, rule = 2))
eks <- c(80, 85, 93, 98)
with(head(d, 1000L), 
     {
       plot(time, light)
       sol <- solar(time)
       zen <- zenith(sol, deploy[1L], deploy[2L] )
       
       lines(time, cfun(zen), col = "red")
       abline(h = cfun(eks), col = "red")
       
       lines(time, calib(zen))
       abline(h = calib(eks), col = "black")
      
       })


## empirical method
calib <- cfun

lonlim <- c(120, 220)
latlim  <- c(-70, -45)


library(rworldxtra)
data(countriesHigh)
wmap <- countriesHigh
## subset(countriesHigh, SOVEREIGNT %in% c("Australia", "New Zealand", "Antarctica"))

land.mask <- function(poly, xlim, ylim, dim = c(180, 120), land = TRUE) {
  r <- raster(nrows = dim[2L], ncols = dim[1L],
              xmn = xlim[1L], xmx = xlim[2L],
              ymn= ylim[1L], ymx = ylim[2L],
              crs = projection(poly))
  r <- rasterize(poly, r)
  r <- as.matrix(is.na(r))[nrow(r):1L, ]
  if(land) r <- !r
  xbin <- seq(xlim[1L], xlim[2L], length=ncol(r) + 1L)
  ybin <- seq(ylim[1L], ylim[2L], length=nrow(r) + 1L)
  
  function(p, returnmask = FALSE) {
    if (returnmask) return(r)
    r[cbind(.bincode(p[,2L], ybin),.bincode(p[,1L], xbin))]
  }
}

is.sea <- land.mask(wmap,  xlim = lonlim, ylim = latlim, land = FALSE)

log.prior <- function(p)  {
  f <- p[,1L] >= lonlim[1L] & p[,1L] <= lonlim[2L] & p[,2L] >= latlim[1L] & p[,2L] <= latlim[2L]
  f <- f & is.sea(p)
  
  ifelse(f | is.na(f), 0, -1000)
}


library(raadtools)
topo <- crop(readtopo("etopo2",  lon180 = FALSE), extent(lonlim, latlim))

topo[topo > 0] <- 0

topo <- aggregate(topo, fact = 4, fun = mean)

plot(expand.grid(lonlim, latlim), type = "n", asp = 1.064)
plot(topo, add = TRUE, col = grey(seq(0, 1, length = 100)))



plot(wmap, add = TRUE, col = "grey")
points(deploy[1], deploy[2])


## what distribution of speeds?
speeds <- seq(0, 14, length = 1e3)
## by the time we get to 3.6 km/h it tails off fast
pag <- parameters.gamma(3, 2) ## mean, sd speed in km/hm
plot(speeds, dgamma(speeds, pag[1], pag[2], log = TRUE));abline(v = 2)

# d2 <- rbind(d2[1,], d2, d2[nrow(d2), ])
# d2$gmt[1] <- d2$gmt[1] - 12 * 3600
# d2$light[1] <- 0
# d2$segment[1] <- 0
# d2$gmt[nrow(d2)] <- d2$gmt[nrow(d2)] + 12 * 3600
# d2$light[nrow(d2)] <- 0
# d2$segment[nrow(d2)] <- max(d2$segment) + 1
# d2$segment <- unclass(factor(d2$segment))

model <- curve.model(d$time, d$light, d$segment, calib,
                     logp.x = log.prior,
                     logp.z = log.prior,
                     alpha=c(18, 26),
                     #                     beta=c(1.05, 0.05),
                     ##  beta = c(1.02, 0.04),
                     beta = pag,
                     fixedx = c(TRUE, rep(FALSE, max(d$segment) - 2L), TRUE))



nx <- 60L
ny <- 45L

grid <- list(x = seq(lonlim[1L], lonlim[2L], length = nx),
             ## encourage it to stay north for initialization
             y = seq(-60, latlim[2L], length = ny),
             z = array(0, c(nx, ny, length(model$time))))
for (i in seq_along(grid$x)) {
  for (j in seq_along(grid$y)) {
    grid$z[i,j,] <- model$logpx(cbind(rep(grid$x[i], length(model$time)), grid$y[j], -10))
  }
}
allxy <- as.matrix(expand.grid(grid$x, grid$y))

x0 <- matrix(0, dim(grid$z)[3], 3)
for (i in 2:(nrow(x0)-1)) {
  x0[i,1:2] <-   allxy[which.max(grid$z[,,i-1]  + grid$z[,,i] + grid$z[,,i+1]), 1:2] 
}


## the first and last is known
x0[1L, c(1L, 2L) ] <- deploy
x0[nrow(x0), c(1, 2)] <- recapt

model$x0 <- x0


x.proposal <- mvnorm(S=diag(c(0.005,0.005, 0.05)),n=nrow(x0))
z.proposal <- mvnorm(S=diag(c(0.005,0.005)),n=nrow(x0)-1)

fit <- estelle.metropolis(model,x.proposal,z.proposal,iters=20,thin=20, x0 = x0, z0 = (x0[-nrow(x0),1:2]+ x0[-1,1:2])/2)



lastx <- chain.last(chain.collapse(fit$x))
lastz <- chain.last(chain.collapse(fit$z))
plot(lastx, type = "l")
test <- sum((model$fixedx | log.prior(lastx) > -1000) & c(TRUE, (log.prior(lastz) == 0)))

while(test < nrow(x0)) {
  ##while((!all(model$fixedx | log.prior(lastx) == 0)) | !all(log.prior(lastz) == 0)) {
  fit <- estelle.metropolis(model,x.proposal,z.proposal,iters=40,thin=20, x0 = lastx, z0 = lastz, verbose = FALSE)
  
  lastx <- chain.last(chain.collapse(fit$x))
  lastz <- chain.last(chain.collapse(fit$z))
  
  lines(lastx)
  
  test <- sum((model$fixedx | log.prior(lastx) > -1000) & c(TRUE, (log.prior(lastz) == 0)))
  print(sprintf("%i of %i pass", test, nrow(x0)))
}


for (i in 1:3) {
  ## Tune proposals based on previous run
  x.proposal <- mvnorm(chain.cov(fit$x),s=0.3)
  z.proposal <- mvnorm(chain.cov(fit$z),s=0.3)
  fit <- estelle.metropolis(model,x.proposal,z.proposal,
                            x0 = lastx, z0 = lastz,
                            iters=100,thin=20)
  
  lastx <- chain.last(chain.collapse(fit$x))
  lastz <- chain.last(chain.collapse(fit$z))
  
  xm <- apply(chain.collapse(fit$x), 1:2, mean)
  plot(expand.grid(lonlim, latlim), type = "n", asp = 1/cos(mean(latlim) * pi/180))
  plot(topo, add = TRUE, col = grey(seq(0, 1, length = 100)))
  
  plot(wmap, add = TRUE, col = "grey")
  points(deploy[1], deploy[2])
  
  lines(xm)
  
}


pz <- Pimage(fit, type = "intermediate")
p <- Pimage(fit, grid = crop(readsst(), extent(pz[1]), snap = "out"), type = "intermediate")
## functions for merging global datasets with pimage
## .aligned(p[], global)
sst <- crop(readsst(), extent(pz[1]), snap = "out")

plot(sst)
points(xyFromCell(sst, SGAT:::cn.pimg(p[[1]])), pch = ".")

## plot(extent(trim(p[i], value = 0)), add = TRUE)


