## Tidying up data post-extraction of landscape covariates

prepare_data <- function(DT, raster) {

  raster %<>% 
    rename(ndvi = 5,
           elevation = 6,
           slope = 7,
           aspect = 8,
           TPI = 9,
           rugged = 10) %>%
    mutate(ndvi = ndvi/10000,
           # Merge rock/rubble into barrens
           pt_lc = ifelse(pt_lc == "32", "33", pt_lc),
           lc_description = ifelse(pt_lc == "32" | pt_lc == "33",
                                   "Barrens", lc_description))
  
  DT1 <- left_join(DT, raster, by = c("x", "y"))
  
  DT1 %<>% rename(dist_to_road = dist_to_feat_line) %>%
    mutate(NC_ratio = ifelse(NC_ratio > 1, NC_ratio, (NC_ratio)^-1)) %>%
  # realized my NC and Dara's CN were getting mixed up
    dplyr::rename(CN_ratio = NC_ratio) %>%
    mutate(site = as.factor(site),
           pt_lc = as.factor(pt_lc),
           lc_description = as.factor(lc_description),
           species = as.factor(species))
  
  DT1 %<>%
  group_by(species) %>%
    mutate(aspect = ifelse(is.na(aspect), mean(aspect, na.rm = T), aspect))
  
  
  
  return(DT1)  
  
}
