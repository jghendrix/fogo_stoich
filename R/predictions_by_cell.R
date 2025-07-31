
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

  df2 <- left_join(df1, N_birch, by = c("x", "y"))
  df2 <- left_join(df2, N_gram, by = c("x", "y"))
  
  df1$N_crowberry <- gsub("NaN", NA, df1$N_crowberry)
  df1$N_Kalmia <- gsub("NaN", NA, df1$N_Kalmia)
  df1$N_lingonberry <- gsub("NaN", NA, df1$N_lingonberry)
  df1$N_Cladonia <- gsub("NaN", NA, df1$N_Cladonia)
  df1$N_blueberry <- gsub("NaN", NA, df1$N_blueberry)
  df1$N_alder <- gsub("NaN", NA, df1$N_alder)
  df1$N_dwarf_birch <- gsub("NaN", NA, df1$N_dwarf_birch)
  df1$N_deergrass <- gsub("NaN", NA, df1$N_deergrass)
  df1$N_graminoid_spp <- gsub("NaN", NA, df1$N_graminoid_spp)

  df1 %<>% mutate_if(is.character, as.numeric) #%>%
   # mutate(N_vascular = mean(N_crowberry, N_Kalmia, N_lingonberry, N_blueberry, N_alder, N_dwarf_birch, N_deergrass, N_graminoid_spp, N_black_spruce, na.rm = T))
  
}