scatter_CN <- function(DT){
  
  # all samples, removing the odd species that aren't relevant
  
  DT %<>% filter(!species %in% c("Moss", "graminoid_spp.", "Black_spruce"))
  DT$species <- gsub("_", " ", DT$species)
  DT$species <- gsub("Kalmia", "Sheep laurel", DT$species)
  # Put lichen as first level of factor for easier visualization
  DT$species <- as.factor(DT$species)
  DT$species <- relevel(DT$species, ref = "Cladonia")
  
  # Make Cladonia black, relative to the rest of them
  
 fig <- ggplot(subset(DT, species != "Cladonia"), aes(x = percent_C, y = percent_N, colour = species)) +
    geom_point(alpha = 0.5) +
    scale_colour_viridis(discrete = TRUE, option = "C", end = 0.9) +
    theme_bw() +
    xlab("Carbon (% dry mass)") +
    ylab("Nitrogen (% dry mass)") +
    xlim(c(35, 65))

ggplot(DT, aes(x = percent_C, y = percent_N, colour = species)) +
   geom_point(alpha = 0.5) +
   scale_colour_manual(values = c("black", "#0D0887FF", "#5601A4FF", "#900DA4FF", "#BF3984FF", "#E16462FF", "#F89441FF", "#FCCE25FF")) +
   theme_bw() +
   xlab("Carbon (% dry mass)") +
   ylab("Nitrogen (% dry mass)") +
   xlim(c(35, 65))  
 
scales::viridis_pal(option = "C", end = 0.9)(7) 
# "#0D0887FF" "#5601A4FF" "#900DA4FF" "#BF3984FF" "#E16462FF" "#F89441FF" "#FCCE25FF" 
 
 ggsave('graphics/allsamplesCN.png',
         height = 6,
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