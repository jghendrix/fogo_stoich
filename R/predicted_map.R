# Bringing together all the rasters and predicting across the surface

## I KNOW THIS LOOKS LIKE WAY TOO MUCH FOR ONE FUNCTION
# but when I try to generate the stacked rasters separately, and then bring them into the model + prediction function, it keeps breaking. doing it all at once works, somehow

predicted_map <- function(df, sp_key, r1, sdss_legend, r2, cfs_legend, r3, r4, r5, r6, r7, r8, r9) {

  df %<>% dplyr::filter(species == sp_key$species) %>%
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
  
  mod <- lm(percent_N ~ sdss_lc + cfs_lc + dist_to_coast + ndvi +
               dem + slope + cos(aspect) + TPI + terrain, 
             data = df)
  
  summary(mod)
  
  r_pred <- terra::predict(layers, mod, na.rm = TRUE)
  
    pred_df <- as.data.frame(r_pred, xy = TRUE) %>%
    rename(Percent_N = lyr1) %>%
### INTERIM SOLUTION TO OUTLIER N VALUES
      # highest observed %N was 5.66 for alder
      # set 6 as the maximum value, and 0 as the minimum
      mutate(Percent_N = ifelse(Percent_N < 0, 0, Percent_N),
             Percent_N = ifelse(Percent_N > 6, 6, Percent_N))
  
 g <- ggplot(pred_df) +
   # geom_sf(data = coast) +
    geom_raster(aes(x = x, y = y, fill = Percent_N)) +
    coord_cartesian(ylim = c(5505000, 5515000)) +
    xlab("") +
    ylab("") +
    scale_fill_viridis(option = "D", discrete = FALSE) +
    ggtitle(paste0("Predicted %N for ", sp_key$species)) +
    theme_bw() 

  ggsave(paste0('graphics/restricted_predicted_N_', sp_key$species, '.png'),
                g,
                height = 4,
                width = 10)
  # some plants have negative predicted %N... do we just reset those to 0?
  
  pred_df %<>%
    rename_with(~paste("Percent_N_", sp_key$species),
                .cols = Percent_N)
  
  return(pred_df)
}

