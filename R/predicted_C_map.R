# Bringing together all the rasters and predicting across the surface

## I KNOW THIS LOOKS LIKE WAY TOO MUCH FOR ONE FUNCTION
# but when I try to generate the stacked rasters separately, and then bring them into the model + prediction function, it keeps breaking. doing it all at once works, somehow

predicted_C_map <- function(df, sp_key, r1, sdss_legend, r2, cfs_legend, r3, r4, r5, r6, r7, r8, r9) {

  df %<>% dplyr::filter(species == sp_key$species) %>%
    mutate(sdss_lc = factor(sdss_lc),
           cfs_lc = factor(cfs_lc)) %>%
    filter(percent_C < 100 & percent_C > 10)

  
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
  
  df %<>% mutate(C = percent_C/100)
  
  mod <- betareg(C ~ cfs_lc + sdss_lc + dist_to_coast + ndvi +
               dem + slope + cos(aspect) + TPI + terrain, 
             data = df)
  
  summary(mod)
  # This model works for most species, but for Dwarf_birch need to remove cfs_lc
  
  
# apply the model to the stacked rasters
  
  r_pred <- terra::predict(layers, mod, na.rm = TRUE)
  
    pred_df <- as.data.frame(r_pred, xy = TRUE) %>%
     mutate(percent_C = 100*lyr1) %>%
      dplyr::select(-c(lyr1))
      
    # How to deal with outliers??? Censor everything to 6%, the max observed N
   # pred_df %<>% mutate(percent_C = ifelse(percent_N > 6, 6, percent_N))
    
    
    
 g <- ggplot(pred_df) +
   # geom_sf(data = coast) +
    geom_raster(aes(x = x, y = y, fill = percent_C)) +
    coord_cartesian(ylim = c(5503000, 5515000)) +
    xlab("") +
    ylab("") +
    scale_fill_viridis(option = "D", discrete = FALSE) +
    ggtitle(paste0("Predicted %C for ", sp_key$species)) +
    theme_bw() 

  ggsave(paste0('graphics/stoich_layers/percent_C_', sp_key$species, '.png'),
                g,
                height = 5,
                width = 10)
  
  pred_df %<>%
    rename_with(~paste("percent_C_", sp_key$species),
                .cols = percent_C)
  
  return(pred_df)
}

