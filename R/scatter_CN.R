scatter_CN <- function(DT){
  
  # all samples
  
  DT %<>% filter(!species %in% c("Moss", "graminoid_spp.", "Black_spruce"))
  
  DT$species <- relevel(DT$species, ref = "Cladonia")
  
  ggplot(DT, aes(x = percent_C, y = percent_N, colour = species)) +
    geom_point(alpha = 0.5) +
    scale_colour_viridis(discrete = TRUE, option = "B") +
    theme_bw() +
    xlab("Carbon (% dry mass)") +
    ylab("Nitrogen (% dry mass)") +
    xlim(c(35, 65))
  ggsave('graphics/allsamplesCN.png',
         height = 5,
         width = 8)
  
  sp_sum <- DT %>% group_by(species) %>%
    summarise(n = n(),
              xN = mean(percent_N, na.rm = T),
              sdN = sd(percent_N, na.rm = T),
              seN = sdN/sqrt(n),
              xC = mean(percent_C, na.rm = T),
              sdC = sd(percent_C, na.rm = T),
              seC = sdC/sqrt(n))
  
  return(sp_sum)

}