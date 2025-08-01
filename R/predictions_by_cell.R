
predictions_by_cell <- function(df, birch, gram){

  # mapping across all the species, the predictions are rbind() together into 53 million rows, with a bunch of NAs in every column except the focal species. Ideally we could avoid a 53 million row df and just cbind() the mapped targets but I don't know how to do that
  # so this is my very inefficient workaround
  
  df1 <- df %>% group_by(x,y) %>%
  summarise(N_crowberry = mean(`percent_N_ Crowberry`, na.rm = T),
            N_Kalmia = mean(`percent_N_ Kalmia`, na.rm = T),
            N_lingonberry = mean(`percent_N_ Lingonberry`, na.rm = T),
            N_Cladonia = mean(`percent_N_ Cladonia`, na.rm = T),
            N_blueberry = mean(`percent_N_ Blueberry`, na.rm = T),
            N_alder = mean(`percent_N_ Alder`, na.rm = T),
            N_deergrass = mean(`percent_N_ Deergrass`, na.rm = T))

  df2 <- left_join(df1, birch, by = c("x", "y"))
  df2 <- left_join(df2, gram, by = c("x", "y"))
  
  df2$N_crowberry <- gsub("NaN", NA, df2$N_crowberry)
  df2$N_Kalmia <- gsub("NaN", NA, df2$N_Kalmia)
  df2$N_lingonberry <- gsub("NaN", NA, df2$N_lingonberry)
  df2$N_Cladonia <- gsub("NaN", NA, df2$N_Cladonia)
  df2$N_blueberry <- gsub("NaN", NA, df2$N_blueberry)
  df2$N_alder <- gsub("NaN", NA, df2$N_alder)
  df2$N_dwarf_birch <- gsub("NaN", NA, df2$N_dwarf_birch)    
  df2$N_deergrass <- gsub("NaN", NA, df2$N_deergrass)
  df2$N_graminoid_spp <- gsub("NaN", NA, df2$N_graminoid_spp)

  df2 %<>% mutate_if(is.character, as.numeric)
}