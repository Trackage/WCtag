
# library(readr)
# d <- read_csv("file.csv", skip = 4)
# d$gmt <- as.POSIXct(strptime(d$Date, "%d-%b-%Y %H:%M:%S"), tz = "GMT")
# library(tidync)

# b <- read_wc_nc("file.nc")
# b2 <- disaggregate(b, fact = 8, method = "bilinear")
# for (i in seq_len(nrow(d))) {
#   Sys.sleep(0.05)
#   plot(log(b[[i]]), addfun = function() {
#     lines(d$`Most Likely Longitude`[1:i], d$`Most Likely Latitude`[1:i])
#     contour(b2[[i]], add = TRUE)
#     })
# }
read_wc_nc <- function(file) {
  lon <- (tidync::tidync(file) %>% activate("longitude") %>% hyper_slice())[[1]]
  lat <- rev((tidync::tidync(file) %>% activate("latitude") %>% hyper_slice())[[1]])
   a <- (tidync("42951-3.nc") %>% hyper_filter() %>% hyper_slice())[[1]]
  
   raster::setExtent(raster::brick(a, crs = "+proj=longlat +datum=WGS84", transpose = TRUE), raster::extent(range(lon), range(lat)))
}
