## Comparison summer 2022 and winter 2025 composition at subset of n = 10 sites
library(tidyverse)
library(magrittr)
library(lmerTest)
library(viridis)

summer <- read.csv('output/sp_prep.csv')
sN <- summer %>%
  select(c(site, species, percent_N)) %>%
  mutate(season = "summer")

winter <- read.csv('input/winter_2025_prepped.csv')
wN <- winter %>% 
  select(c(site, species, percent_N)) %>%
  mutate(season = "winter")

# unfortunately the sites don't line up b/c winter is just Broadleaf.1, but summer has Broadleaf.1.1, Broadleaf.1.2, etc.

sN[c('hab', 'loc', 'subsite')] <- str_split_fixed(sN$site,pattern = "[.]", n = 3)
sN %<>% mutate(site = paste0(hab, ".", loc)) %>%
  select(1:4)
# we only want to include the same 10 sites we revisited in winter
sNrestr <- sN %>% filter(site %in% c(wN$site))
comp <- rbind(sNrestr, wN)
unique(comp$species)

clad <- comp %>% filter(species == "Cladonia")
ggplot(clad, aes(x = season, y = percent_N, colour = site)) +
  geom_jitter() +
  scale_colour_viridis(discrete = TRUE, option = 'B', end = 0.9) +
  theme_bw() +
  labs(x = "", y = "%N in Cladonia")

ggsave(filename = "graphics/Clad_N_by_season.png",
       height = 1600,
       width = 2000,
       units = "px")

# Looks pretty consistent tbh

summary(lmer(percent_N ~ season + (1|site), data = clad))
# No change in Cladonia %N from summer to winter within sites
summary(lm(percent_N ~ season, data = clad))
# No change at all overall in the population ranges either

# Is rank consistency maintained?
ggplot(clad, aes(x = site, y = percent_N, colour = season)) +
  geom_boxplot() +
  theme_bw()

# Maybe??? How would we test this?
means <- comp %>% group_by(site, species, season) %>%
  summarise(N = mean(percent_N, na.rm = T),
            sd = sd(percent_N, na.rm = T),
            count = n())

cm <- means %>% filter(species == "Cladonia")

cS <- cm %>% filter(season == "summer") %>% arrange(N)
cS$order <- seq(c(1:10))

cW <- cm %>% filter(season == "winter") %>% arrange(N)
cW$order <- seq(c(1:10))

ranks <- rbind(cS, cW)
  
cm$site <- factor(cm$site, levels = c(cS$site))
  
  
ggplot(cm, aes(x = site, y = N, 
               ymin = N - sd, ymax = N + sd, 
               colour = season)) +
  geom_point(position = position_dodge(width = 0.3)) +
  geom_errorbar(width = 0.3, position = position_dodge()) +
  scale_colour_viridis(discrete = TRUE, begin = 0.75, end = 0.1, option = "C") +
  theme_bw() +
  labs(x = "", y = "%N in Cladonia")
ggsave(filename = "graphics/seasonal_Cladonia_comparison.png",
       height = 1500,
       width = 2500,
       units = "px")

# There doesn't actually seem to be that much of a relationship in rank order between our summer and winter samples
# I.e. the "best" site in summer isn't necessarily where you'll find the "best" lichen in winter. but we're seeing selection for lichen N during winter using our summer distributions
