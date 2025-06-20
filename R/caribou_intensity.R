## Intensity of use of caribou, as a raster... how?

caribou_intensity <- function(DT) {
  
  DT <- read_csv('output/all_Fogo_caribou_locs_20June2025.csv')
  
  locs <- DT %>% dplyr::select(x = x_long, y = y_lat)
  sf_obj <- st_as_sf(locs, 
                     coords = c("x", "y"),
                     crs = "+proj=longlat +datum=WGS84")
  
  reproj <- st_transform(sf_obj, "EPSG:32621")
  
  empty_kernel_grid <- raster(ext = extent(reproj), resolution = 25, crs = st_crs(reproj))
  
  kernel_density <- rasterize(coordinates(as_Spatial(reproj)), empty_kernel_grid, fun = 'count', background = 0)
  
  plot(kernel_density)
  
  kernel_terra <- rast(kernel_density)
 
 
  kernel_crop <- terra::crop(kernel_terra, sdss)
  
   
  terra::writeRaster(kernel_terra, "output/kernel.tif", filetype = "GTiff")
  
  
  plot(kernel_crop)
  
  
  
  
  DT %<>% filter(month %in% c(11, 12, 1, 2, 3, 4)) #only using winter data? b/c that's when they're depositing? Or do we want just all data ever
  
  # all years? or only some of them
  
  mod <- lm(as.formula(paste(response, 
                             "~ sdss_lc + cfs_lc + dist_to_coast + ndvi +
                             dem + slope + cos(aspect) + TPI + terrain")), 
            data = DT)
}
