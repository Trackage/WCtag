library(raadtools)
xm <- xm[,1:2]
ucurr <- readcurr(fit$model$time, xylim = extent(xm) + 5, lon180 = FALSE, uonly = TRUE)
vcurr <- readcurr(fit$model$time, xylim = extent(xm) + 5, lon180 = FALSE, vonly = TRUE)

plotcurrents <- function(ni = 1, ext = NULL, scale = 2000, ...) {
  crds <- coordinates(ucurr)
  plot(sqrt(ucurr[[ni]]^2 + vcurr[[ni]]^2), ...)
  x1 <- crds[,1]
  y1 <- crds[,2]
  x2 <- crds[,1] + values(ucurr[[ni]]) * scale
  y2 <- crds[,2] + values(vcurr[[ni]]) * scale
  op <- options(warn = -1)
  arrows(x1, y1, x2, y2, length = 0.03)
  options(op)
  invisible(NULL)
}
for (i in seq_along(fit$model$time)) {
  plotcurrents(findInterval(fit$model$time[i], getZ(ucurr)) + 1, col = sst.pal(50), scale = 0.65)
  lines(xm[1:i,], col = "white", lwd = 2)
  scan("",1)
}



plotcurrents(date = as.Date("2000-01-05"), ext = xylim, scale = 2100)
xm <- location.mean(fit$x)
pxm <- coordinates(spTransform(SpatialPoints(xm, CRS("+proj=longlat +datum=WGS84")), CRS(projection(ice))))
plot(ice[[1]])
lines(pxm)
for(i in fit$model$time) {plot(ice[[findInterval(fit$model$time[i], getZ(ice))]]);lines(pxm[1:i,])}
findInterval(fit$model$time[i], getZ(ice))
i
for(i in seq_along(fit$model$time)) {plot(ice[[findInterval(fit$model$time[i], getZ(ice))]]);lines(pxm[1:i,])}
findInterval(fit$model$time[i], getZ(ice))
for(i in seq_along(fit$model$time)) {plot(ice[[findInterval(fit$model$time[i], getZ(ice)) + 1]]);lines(pxm[1:i,])}
ssh <- readssh(fit$model$time, lon180 = FALSE, xylim = extent(xm))
plot(ssh[[1]], col = sst.pal(100))
lines(xm)
for(i in seq_along(fit$model$time)) {plot(ssh[[findInterval(fit$model$time[i], getZ(ssh)) + 1]]);lines(xm[1:i,])}
for(i in seq_along(fit$model$time)) {plot(ssh[[findInterval(fit$model$time[i], getZ(ssh)) + 1]], col = sst.pal(50));lines(xm[1:i,])}
curr <- readcurr(fit$model$time, xylim = extent(xm), lon180 = FALSE)
ucurr <- readcurr(fit$model$time, xylim = extent(xm), lon180 = FALSE, uonly = TRUE)
vcurr <- readcurr(fit$model$time, xylim = extent(xm), lon180 = FALSE, vonly = TRUE)
plot(sqrt(ucurr[[1]]^2 + vcurr[[1]]^2), col = sst.pal(50))
for (i in seq_along(fit$model$time)) {plot(sqrt(ucurr[[findInterval(fit$model$time[i], getZ(ssh)) + 1]]^2 + vcurr[[findInterval(fit$model$time[i], getZ(ssh)) + 1]]^2), col = sst.pal(50)); lines(xm[1:i,])}
for (i in seq_along(fit$model$time)) {plot(sqrt(ucurr[[findInterval(fit$model$time[i], getZ(ssh)) + 1]]^2 + vcurr[[findInterval(fit$model$time[i], getZ(ssh)) + 1]]^2), col = sst.pal(50)); lines(xm[1:i,])}
