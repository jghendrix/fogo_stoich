## Tidying up data post-extraction of landscape covariates

stack_rasters <- function(r1, r2, r3, r4, r5, r6, r7, r8) {

  # sdss, lc, ndvi, dem, slope, aspect, tpi, terrain
  r1 <- rast(r1)
  r2 <- rast(r2)
  r3 <- rast(r3)
  r4 <- rast(r4)
  r5 <- rast(r5)
  r6 <- rast(r6)
  r7 <- rast(r7)
  r8 <- rast(r8)
  
  layers <- c(r1, r2)
  layers <- c(layers, r3)
  layers <- c(layers, r4)
  layers <- c(layers, r5)
  layers <- c(layers, r6)
  layers <- c(layers, r7)
  layers <- c(layers, r8)
  
  return(layers)
  writeRaster(layers, 'output/rasters_merged.tif')
  
  
  # How can we bring in distance_to_coast? need to raster-ise that somehow
  # from https://stackoverflow.com/questions/77614574/how-do-i-create-a-raster-of-distance-to-nearest-feature-using-the-terra-sf-r-pac
 # s1 <- rbind(c(0,3),c(0,4),c(1,5),c(2,5))
#  s2 <- rbind(c(0.2,3), c(0.2,4), c(1,4.8), c(2,4.8))
#  s3 <- rbind(c(0,4.4), c(0.6,5))
  
 # v <- vect(list(s1,s2,s3), "lines", crs="local") |> aggregate()
  
#  r <- rast(v, res = 0.01)
 # r <- rasterize(v, r)
  #d <- distance(r)
  
  #plot(d)
  #lines(v, col="blue", lwd=2, xpd=TRUE)  
  
}
