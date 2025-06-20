
predictions_by_cell <- function(df){

  # mapping across all the species, the predictions are rbind() together into 53 million rows, with a bunch of NAs in every column except the focal species. Ideally we could avoid a 53 million row df and just cbind() the mapped targets but I don't know how to do that
  # so this is my very inefficient workaround
  
  df1 <- df %>% group_by(x,y) %>%
  summarise(N_crowberry = mean(`Percent_N_ Crowberry`, na.rm = T),
            N_Kalmia = mean(`Percent_N_ Kalmia`, na.rm = T),
            N_lingonberry = mean(`Percent_N_ Lingonberry`, na.rm = T),
            N_Cladonia = mean(`Percent_N_ Cladonia`, na.rm = T),
            N_blueberry = mean(`Percent_N_ Blueberry`, na.rm = T),
            N_alder = mean(`Percent_N_ Alder`, na.rm = T),
            N_dwarf_birch = mean(`Percent_N_ Dwarf_birch`, na.rm = T),
            N_deergrass = mean(`Percent_N_ Deergrass`, na.rm = T),
            N_graminoid_spp = mean(`Percent_N_ graminoid_spp.`, na.rm = T),
            N_black_spruce = mean(`Percent_N_ Black_spruce`, na.rm = T))

}