## Intensity of use of caribou, as a raster... how?

caribou_intensity <- function(DT) {
  
  DT <- read_csv('output/all_Fogo_caribou_locs_20June2025.csv')
  
  winter <- DT %>% filter(month %in% c(11, 12, 1, 2, 3, 4))
  summer <- DT %>% filter(month %in% c(6, 7, 8))
  
  w_locs <- winter %>% dplyr::select(x = x_long, y = y_lat)
  sf_w <- st_as_sf(w_locs, 
                     coords = c("x", "y"),
                     crs = "+proj=longlat +datum=WGS84")
  
  w_reproj <- st_transform(sf_w, "EPSG:32621")
  
  empty_kernel_grid <- raster(ext = extent(w_reproj), resolution = 25, crs = st_crs(w_reproj))
  
  winter_kernel <- rasterize(coordinates(as_Spatial(w_reproj)), empty_kernel_grid, fun = 'count', background = 0)
  
  plot(winter_kernel)
  
  winter_terra <- rast(winter_kernel)
 # kernel_crop <- terra::crop(kernel_terra, sdss)
  terra::writeRaster(kernel_terra, "output/winter_kernel.tif", filetype = "GTiff")
  
  # now the same, but for summer data
  
  s_locs <- summer %>% dplyr::select(x = x_long, y = y_lat)
  sf_s <- st_as_sf(s_locs, 
                   coords = c("x", "y"),
                   crs = "+proj=longlat +datum=WGS84")
  
  s_reproj <- st_transform(sf_s, "EPSG:32621")
  
  empty_kernel_grid <- raster(ext = extent(s_reproj), resolution = 25, crs = st_crs(s_reproj))
  
  summer_kernel <- rasterize(coordinates(as_Spatial(s_reproj)), empty_kernel_grid, fun = 'count', background = 0)
  
  plot(summer_kernel)
  
  summer_terra <- rast(summer_kernel)
  # kernel_crop <- terra::crop(kernel_terra, sdss)
  terra::writeRaster(summer_terra, "output/summer_kernel.tif", filetype = "GTiff")
  
  ## Comparing these kernels to plant data
   winter_df <- as.data.frame(winter_terra, xy = TRUE) %>%
     rename(winter_density = layer) %>%
     mutate(winter = ifelse(winter_density > 5, 5, winter_density))
     
    summer_df <- as.data.frame(summer_terra, xy = TRUE) %>%
     rename(summer_density = layer) %>%
      mutate(summer = ifelse(summer_density > 5, 5, summer_density))
   
    scols <- c("0" = "white",
              "1" = "#FFE697",
              "2" = "#FFD755",
              "3" = "#FEC200",
              "4" = "#C89800",
              "5" = "#8C6B00")
    
    wcols <- c("0" = "white", 
               "1" = "#C0DFEE", 
               "2" = "#80BFDE",
               "3" = "#419ECD",
               "4" = "#277297",
               "5" = "#164156")
 
  ggplot(summer_df) +
    geom_raster(aes(x = x, y = y, fill = as.factor(summer))) +
    coord_cartesian(ylim = c(5505000, 5515000),
                    xlim = c(692100, 716550)) +
    xlab("") +
    ylab("") +
    scale_fill_manual(values = scols) +
    ggtitle("Summer (Jun-Aug) density of caribou") +
    guides(fill = "none") +
    theme_bw() 
  
  ggsave('graphics/summer_caribou_density.png',
         height = 4,
         width = 8)
  
  
  
  ggplot(winter_df) +
    geom_raster(aes(x = x, y = y, fill = as.factor(winter))) +
    coord_cartesian(ylim = c(5505000, 5515000),
                    xlim = c(692100, 716550)) +
    xlab("") +
    ylab("") +
    scale_fill_manual(values = wcols) +
    ggtitle("Winter (Nov-Mar) density of caribou") +
    guides(fill = "none") +
    theme_bw() 
  
  ggsave('graphics/winter_caribou_density.png',
         height = 4,
         width = 8)
  
  
  
  
  # all years? or only some of them
  
  mod <- lm(as.formula(paste(response, 
                             "~ sdss_lc + cfs_lc + dist_to_coast + ndvi +
                             dem + slope + cos(aspect) + TPI + terrain")), 
            data = DT)
}
