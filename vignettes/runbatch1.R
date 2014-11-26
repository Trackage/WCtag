library(SGAT)
load("runbatch1.Rdata")
for (i in 1:5) {
  ## Tune proposals based on previous run
  x.proposal <- mvnorm(chain.cov(fit$x),s=0.3)
  z.proposal <- mvnorm(chain.cov(fit$z),s=0.3)
  fit <- estelle.metropolis(model,x.proposal,z.proposal,
                            x0 = lastx, z0 = lastz,
                            iters=3000,thin=20)
  
  lastx <- chain.last(chain.collapse(fit$x))
  lastz <- chain.last(chain.collapse(fit$z))
  
  xm <- apply(chain.collapse(fit$x), 1:2, mean)
  plot(expand.grid(lonlim, latlim), type = "n", asp = 1/cos(mean(latlim) * pi/180))
  plot(topo, add = TRUE, col = grey(seq(0, 1, length = 100)))
  
  plot(wmap, add = TRUE, col = "grey")
  points(deploy[1], deploy[2])
  
  lines(xm)
  
}

x.proposal <- mvnorm(chain.cov(fit$x),s=0.3)
z.proposal <- mvnorm(chain.cov(fit$z),s=0.3)
fit <- estelle.metropolis(model,x.proposal,z.proposal,
                          x0 = lastx, z0 = lastz,
                          iters=5000,thin=20)

lastx <- chain.last(chain.collapse(fit$x))
lastz <- chain.last(chain.collapse(fit$z))

px <- Pimage(fit, proj = "+proj=lcc +lon_0=150 +lat+0=-58 +lat_1=-40 +lat_2=-60")
pz <- Pimage(fit, type = "intermediate", proj = projection(px[1]))

save.image("savebatch1.Rdata")
