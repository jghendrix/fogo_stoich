## Tidying up data post-extraction of landscape covariates

stack_rasters <- function(r1, sdss_legend, r2, cfs_legend, r3, r4, r5, r6, r7, r8, r9) {

  # Convert all rasters to SpatRaster for terra:
  # sdss, cfs, ndvi, dem, slope, aspect, tpi, terrain, dist_to_coast
  r1 <- rast(r1)
  levels(r1) <- sdss_legend
  names(r1) <- "sdss_lc"
  r2 <- rast(cfs)
  levels(r2) <- cfs_legend
  names(r2) <- "cfs_lc"
  r3 <- rast(r3)
  r4 <- rast(r4)
  r5 <- rast(r5)
  r6 <- rast(r6)
  r7 <- rast(r7)
  r8 <- rast(r8)
  r9 <- rast(r9)
  
  # stack all these raster into a single layer to use as our predictive surface
  layers <- c(r1, r2)
  layers <- c(layers, r3)
  layers <- c(layers, r4)
  layers <- c(layers, r5)
  layers <- c(layers, r6)
  layers <- c(layers, r7)
  layers <- c(layers, r8)
  layers <- c(layers, r9)
  
  # use this stack with our model
  
  mod1 <- lm(percent_N ~ sdss_lc + cfs_lc + dist_to_coast + ndvi +
               dem + slope + cos(aspect) + TPI + terrain, 
             data = sp_prep)
  #sp_prep %<>% mutate(ndvi = 10000*ndvi)
  
  summary(mod1)
  
  r_pred <- terra::predict(layers, mod1, na.rm = TRUE)
  
  pred_df <- as.data.frame(r_pred, xy = TRUE)
  pred_df %<>% rename(`percent N` = lyr1)

  ggplot(pred_df) +
    geom_raster(aes(x = x, y = y, fill = `percent N`)) +
    geom_polygon(data = coast, aes(x = x, y = y)) +
    coord_cartesian(ylim = c(5505000, 5515000)) +
    scale_fill_viridis(option = "B", discrete = FALSE) +
    theme_bw() #+
    theme(legend.title = "Percent N")

}
