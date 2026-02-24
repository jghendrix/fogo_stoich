winter_comp <- function(DT, winter){
  
  # Subset to the sites where we returned overwinter and the species we collected during winter
  DT %<>% filter(run_number == "Dara" &
                   !species %in% c("Moss", "graminoid_spp.", "Black_spruce"))
  
  DT$species <- relevel(DT$species, ref = "Cladonia")
  
  summer <- DT %>% filter(!is.na(percent_N)) %>%
    group_by(species) %>%
    summarise(number = n(),
              N = mean(percent_N, na.rm = T),
              Nse = sd(percent_N)/sqrt(number),
              C = mean(percent_C, na.rm = T),
              Cse = sd(percent_C)/sqrt(number),
              ratio = mean(percent_C/percent_N, na.rm = T),
              rse = sd(percent_C/percent_N)/sqrt(number)
              )
  
 # winter C & N averages -----
  winter %<>% filter(species != "moss") %>%
    mutate(species = as.factor(species))
  winter$species <- relevel(winter$species, ref = "Cladonia")
 
  wint_sum <- winter %>% filter(!is.na(percent_N)) %>%
    group_by(species) %>%
    summarise(number = n(),
              N = mean(percent_N, na.rm = T),
              Nse = sd(percent_N)/sqrt(number),
              C = mean(percent_C, na.rm = T),
              Cse = sd(percent_C)/sqrt(number),
              ratio = mean(percent_C/percent_N, na.rm = T),
              rse = sd(percent_C/percent_N)/sqrt(number)
    )
  
  ggplot(wint_sum, aes(x = C, xmin = C - Cse, xmax = C + Cse,
                     y = N, ymin = N - Nse, ymax = N + Nse,
                     colour = species)) +
    geom_point() +
    geom_errorbar(width = 0.2) +
    geom_errorbarh(height = 0.02) +
    scale_colour_viridis(discrete = TRUE, option = "B", end = 0.9) +
    theme_bw() +
    xlab("Percent C dry mass") +
    ylab("Percent N dry mass") +
    #xlim(c(40, 80)) +
    #ylim(c(0, 5)) +
    ggtitle("Winter C:N content")
  
  ggsave('graphics/seasonal/winter_avg_CN.png',
         height = 4,
         width = 6)
  
    # seasonal comparison -----
  
  wint_sum %<>% mutate(season = 1)
  summer %<>% mutate(season = 0)
  
  seas <- rbind(wint_sum, summer)
  
  ggplot(seas, aes(x = season, y = N, 
                   ymin = N - Nse, ymax = N + Nse,
                   colour = species)) +
    geom_point() +
    geom_line() +
    geom_errorbar(width = 0.05) +
    scale_colour_viridis(discrete = TRUE, option = "B", end = 0.9) +
    theme_bw() +
    xlab("") +
    ylab("Nitrogen (% dry mass)") +
    theme(axis.ticks.x = element_blank(),
          ) +
    scale_x_continuous(breaks = seq(0,1,1),
                       labels = c("summer", "winter"))
    
  ggsave('graphics/seasonal/change_in_N_seasonal.png',
         height = 4,
         width = 6)
  
  # change in Carbon by season
  ggplot(seas, aes(x = season, y = C, 
                   ymin = C - Cse, ymax = C + Cse,
                   colour = species)) +
    geom_point() +
    geom_line() +
    geom_errorbar(width = 0.05) +
    scale_colour_viridis(discrete = TRUE, option = "B", end = 0.9) +
    theme_bw() +
    xlab("") +
    ylab("Carbon (% dry mass)") +
    theme(axis.ticks.x = element_blank(),
    ) +
    scale_x_continuous(breaks = seq(0,1,1),
                       labels = c("summer", "winter"))
  
  ggsave('graphics/seasonal/change_in_C_seasonal.png',
         height = 4,
         width = 6)
  
  # change in ratio by season --------
  ggplot(seas, aes(x = season, y = ratio, 
                   ymin = ratio - rse, ymax = ratio + rse,
                   colour = species)) +
    geom_point() +
    geom_line() +
    geom_errorbar(width = 0.05) +
    scale_colour_viridis(discrete = TRUE, option = "B", end = 0.9) +
    theme_bw() +
    xlab("") +
    ylab("Carbon:Nitrogen") +
    theme(axis.ticks.x = element_blank(),
    ) +
    scale_x_continuous(breaks = seq(0,1,1),
                       labels = c("summer", "winter"))
  
  ggsave('graphics/seasonal/change_in_ratio_seasonal.png',
         height = 4,
         width = 6)
  
}