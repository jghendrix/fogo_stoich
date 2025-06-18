## Tidying up data post-extraction of landscape covariates

stack_rasters <- function(r1, r2, r3, r4, r5, r6, r7, r8, r9) {

  # sdss, lc, ndvi, dem, slope, aspect, tpi, terrain
  r1 <- rast(r1)
  r2 <- rast(r2)
  r3 <- rast(r3)
  r4 <- rast(r4)
  r5 <- rast(r5)
  r6 <- rast(r6)
  r7 <- rast(r7)
  r8 <- rast(r8)
  r9 <- rast(r9)
  
    
  layers <- c(r1, r2)
  layers <- c(layers, r3)
  layers <- c(layers, r4)
  layers <- c(layers, r5)
  layers <- c(layers, r6)
  layers <- c(layers, r7)
  layers <- c(layers, r8)
  layers <- c(layers, r9)
  
  return(layers)
  writeRaster(layers, 'output/rasters_merged.tif')
  
}
