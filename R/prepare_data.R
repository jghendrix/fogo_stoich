## Tidying up data post-extraction of landscape covariates

prepare_data <- function(DT, raster) {

  raster %<>% 
    rename(ndvi = 3,
           elevation = 4,
           slope_deg = 5,
           aspect_deg = 6,
           TPI = 7,
           rugged = 8) %>%
    mutate(ndvi = ndvi/10000)
  
  DT1 <- left_join(DT, raster, by = c("x", "y"))
  
  DT1 %<>% rename(dist_to_road = dist_to_feat_line) %>%
    mutate(NC_ratio = ifelse(NC_ratio > 1, NC_ratio, (NC_ratio)^-1)) %>%
  # realized my NC and Dara's CN were getting mixed up
    dplyr::rename(CN_ratio = NC_ratio) %>%
    mutate(site = as.factor(site),
           lc = as.factor(lc),
           species = as.factor(species))
  
  DT1 %<>%
    group_by(species) %>%
    mutate(aspect = aspect_deg*pi/180) %>%
    mutate(aspect = ifelse(is.na(aspect_deg), mean(aspect, na.rm = T), aspect))
  
  return(DT1)  
  
}
