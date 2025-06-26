scatter_CN <- function(DT, winter){
  
  # all samples
  
  forage$species <- relevel(forage$species, ref = "Cladonia")
  
  ggplot(forage, aes(x = percent_C, y = percent_N, colour = species)) +
    geom_point(alpha = 0.5) +
    theme_bw() +
    xlab("Percent C dry mass") +
    ylab("Percent N dry mass") +
    xlim(c(35, 60))
  ggsave('graphics/allsppCN.png',
         height = 6,
         width = 8)
  
  DT <- forage %>% filter(run_number == "Dara")
  
  ggplot(DT, aes(x = percent_C, y = percent_N, colour = species)) +
    geom_point(alpha = 0.6) +
    theme_bw() +
    xlab("Percent C dry mass") +
    ylab("Percent N dry mass") +
    xlim(c(40, 80)) +
    ylim(c(0, 5)) +
    ggtitle("Summer C:N content")
  
  ggsave('graphics/summerCN.png',
         height = 6,
         width = 8)
  
  winter %<>% filter(species != "moss") %>%
    mutate(species = as.factor(species))
  winter$species <- relevel(winter$species, ref = "Cladonia")
  
  ggplot(winter, aes(x = percent_C, y = percent_N, colour = species)) +
    geom_point(alpha = 0.6) + 
    theme_bw() +
    xlim(c(40, 80)) +
    ylim(c(0, 5)) +
    xlab("Winter %C") +
    ylab("Winter %N") +
    ggtitle("Winter C:N content")
  
  ggsave('graphics/winterCN.png',
         height = 6,
         width = 8)
  
  
}