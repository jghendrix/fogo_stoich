# Taking the non-lichen species and averaging them, first as raw data, and then as a weighted average

vascular_average <- function(DT, R2) {

  # there are NaN values where i just want NAs...
  for(j in names(DT)) { 
    set(DT, which(is.nan(DT[[j]])), j, NA) }
  
  sum <- DT %>% 
    group_by(x, y) %>%
    mutate(percent_N = mean(N_crowberry, N_Kalmia, N_lingonberry, N_blueberry, N_alder, N_dwarf_birch, N_deergrass, N_graminoid_spp, N_black_spruce, na.rm = T))
  
 ggplot(sum) +
    geom_raster(aes(x = x, y = y, fill = percent_N)) +
    coord_cartesian(ylim = c(5505000, 5515000)) +
    xlab("") +
    ylab("") +
    scale_fill_viridis(option = "D", discrete = FALSE) +
    ggtitle("Average vascular %N") +
    theme_bw() 

 # it's skewed by some outlier high points... almost nothing is above 2, censor it there?
 sum %<>% mutate(`Percent N` = ifelse(percent_N > 1.5, 1.5, percent_N))
 
 ggplot(sum) +
   geom_raster(aes(x = x, y = y, fill = `Percent N`)) +
   coord_cartesian(ylim = c(5505000, 5515000)) +
   xlab("") +
   ylab("") +
   scale_fill_viridis(option = "D", discrete = FALSE) +
   ggtitle("Average vascular %N") +
   theme_bw()
 
  ggsave('graphics/average_predicted_vascular_N.png',
         height = 4,
         width = 10)
  
  }
