
predictions_by_cell <- function(df){

  # mapping across all the species, the predictions are rbind() together into 53 million rows, with a bunch of NAs in every column except the focal species. Ideally we could avoid a 53 million row df and just cbind() the mapped targets but I don't know how to do that
  # so this is my very inefficient workaround
  
  df1 <- df %>% group_by(x,y) %>%
  summarise(N_crowberry = mean(3, na.rm = T),
            N_Kalmia = mean(4, na.rm = T),
            N_lingonberry = mean(5, na.rm = T),
            N_Cladonia = mean(6, na.rm = T),
            N_blueberry = mean(7, na.rm = T),
            N_alder = mean(8, na.rm = T),
            N_dwarf_birch = mean(9, na.rm = T),
            N_deergrass = mean(10, na.rm = T),
            N_graminoid_spp = mean(11, na.rm = T),
            N_black_spruce = mean(12, na.rm = T))

}