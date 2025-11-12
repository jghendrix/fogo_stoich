# Bringing together all the rasters and predicting across the surface

## I KNOW THIS LOOKS LIKE WAY TOO MUCH FOR ONE FUNCTION
# but when I try to generate the stacked rasters separately, and then bring them into the model + prediction function, it keeps breaking. doing it all at once works, somehow

predicted_single <- function(df, r1, sdss_legend, r2, cfs_legend, r3, r4, r5, r6, r7, r8, r9) {

  
  df %<>%
    mutate(sdss_lc = factor(sdss_lc),
           cfs_lc = factor(cfs_lc))

  sdss_legend1 <- sdss_legend %>% dplyr::filter(label %in% df$sdss_lc) %>%
    mutate(label = factor(label)) %>%
    rename(sdss_lc = label)
  cfs_legend1 <- cfs_legend %>% dplyr::filter(label %in% df$cfs_lc) %>%
    mutate(label = factor(label)) %>%
    rename(cfs_lc = label)
  
  # Convert all rasters to SpatRaster for terra:
  # sdss, cfs, ndvi, dem, slope, aspect, tpi, terrain, dist_to_coast
  r1 <- rast(r1)
  levels(r1) <- sdss_legend1
  names(r1) <- "sdss_lc"
  r2 <- rast(r2)
  levels(r2) <- cfs_legend1
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
  
  # Explanatory model: PROPORTION DATA ------
  
  df %<>% mutate(N = percent_N/100)
  
  if(unique(df$species) == "Dwarf_birch") {
  
  mod <- betareg(N ~ sdss_lc + dist_to_coast + ndvi +
               dem + slope + cos(aspect) + TPI, 
             data = df)
  
  # apply the model to the stacked rasters
  
  r_pred <- terra::predict(layers, mod, na.rm = TRUE)
  
  pred_df <- as.data.frame(r_pred, xy = TRUE) %>%
    mutate(percent_N = 100*lyr1) %>%
    dplyr::select(-c(lyr1))
  
  # How to deal with outliers??? Censor everything to 6%, the max observed N
  pred_df %<>% mutate(percent_N = ifelse(percent_N > 6, 6, percent_N))
  
  
  g <- ggplot(pred_df) +
    # geom_sf(data = coast) +
    geom_raster(aes(x = x, y = y, fill = percent_N)) +
    coord_cartesian(ylim = c(5505000, 5515000)) +
    xlab("") +
    ylab("") +
    scale_fill_viridis(option = "D", discrete = FALSE) +
    ggtitle(paste0("Predicted %N for Dwarf_birch")) +
    theme_bw() 
  
  ggsave(paste0('graphics/stoich_layers/percent_N_Dwarf_birch.png'),
         g,
         height = 5,
         width = 10)
  
  pred_df %<>%
    rename_with(~paste("N_dwarf_birch"),
                .cols = percent_N)
  }
  
  else{
    
    mod <- betareg(N ~ cfs_lc + ndvi + dist_to_coast +
                     TPI + terrain, 
                   data = df)
    
  # apply the model to the stacked rasters
  
  r_pred <- terra::predict(layers, mod, na.rm = TRUE)
  
    pred_df <- as.data.frame(r_pred, xy = TRUE) %>%
     mutate(percent_N = 100*lyr1) %>%
      dplyr::select(-c(lyr1))
      
    # How to deal with outliers??? Censor everything to 6%, the max observed N
    pred_df %<>% mutate(percent_N = ifelse(percent_N > 6, 6, percent_N))
    
    
 g <- ggplot(pred_df) +
   # geom_sf(data = coast) +
    geom_raster(aes(x = x, y = y, fill = percent_N)) +
    coord_cartesian(ylim = c(5505000, 5515000)) +
    xlab("") +
    ylab("") +
    scale_fill_viridis(option = "D", discrete = FALSE) +
    ggtitle(paste0("Predicted %N for graminoid_spp.")) +
    theme_bw() 

  ggsave(paste0('graphics/stoich_layers/percent_N_graminoid_spp.png'),
                g,
                height = 5,
                width = 10)
  
  pred_df %<>%
    rename_with(~paste("N_graminoid_spp"),
                .cols = percent_N)
  
  return(pred_df)
  }
}

