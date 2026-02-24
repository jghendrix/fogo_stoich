
ratio_predictions_by_cell <- function(df, birch){

  # mapping across all the species, the predictions are rbind() together into 53 million rows, with a bunch of NAs in every column except the focal species. Ideally we could avoid a 53 million row df and just cbind() the mapped targets but I don't know how to do that
  # so this is my very inefficient workaround
  
  df1 <- df %>% group_by(x,y) %>%
  summarise(CN_crowberry = mean(`CN_ Crowberry`, na.rm = T),
            CN_Kalmia = mean(`CN_ Kalmia`, na.rm = T),
            CN_lingonberry = mean(`CN_ Lingonberry`, na.rm = T),
            CN_Cladonia = mean(`CN_ Cladonia`, na.rm = T),
            CN_blueberry = mean(`CN_ Blueberry`, na.rm = T),
            CN_alder = mean(`CN_ Alder`, na.rm = T),
            CN_deergrass = mean(`CN_ Deergrass`, na.rm = T))

  df2 <- left_join(df1, birch, by = c("x", "y"))
  
  df2$CN_crowberry <- gsub("NaN", NA, df2$CN_crowberry)
  df2$CN_Kalmia <- gsub("NaN", NA, df2$CN_Kalmia)
  df2$CN_lingonberry <- gsub("NaN", NA, df2$CN_lingonberry)
  df2$CN_Cladonia <- gsub("NaN", NA, df2$CN_Cladonia)
  df2$CN_blueberry <- gsub("NaN", NA, df2$CN_blueberry)
  df2$CN_alder <- gsub("NaN", NA, df2$CN_alder)
  df2$CN_dwarf_birch <- gsub("NaN", NA, df2$CN_dwarf_birch)    
  df2$CN_deergrass <- gsub("NaN", NA, df2$CN_deergrass)

  df2 %<>% mutate_if(is.character, as.numeric)
}