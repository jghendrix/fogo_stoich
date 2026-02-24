## Tidying up data post-extraction of landscape covariates

prepare_data <- function(DT, raster) {

  raster %<>% 
    rename(ndvi = 8,
           dem = 9,
           slope = 10,
           aspect = 11,
           TPI = 12,
           terrain = 13)
  DT1 <- left_join(DT, raster, by = c("x", "y"))
  
  DT1 %<>% mutate(NC_ratio = ifelse(NC_ratio > 1, NC_ratio, (NC_ratio)^-1)) %>%
  # fixing some erroneous ratios
    dplyr::rename(CN_ratio = NC_ratio) %>%
    mutate(site = as.factor(site),
           sdss_lc = as.factor(sdss_lc),
           cfs_lc = as.factor(cfs_lc),
           species = as.factor(species))
  
  DT1 %<>%
  group_by(species) %>%
    mutate(aspect = ifelse(is.na(aspect), mean(aspect, na.rm = T), aspect),
           date = lubridate::as_date(date))
  
  return(DT1)  
  
}
