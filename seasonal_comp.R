## Comparison summer 2022 and winter 2025 composition at subset of n = 10 sites
library(tidyverse)
library(magrittr)
library(lmerTest)
library(viridis)

summer <- read.csv('output/sp_prep.csv')
sN <- summer %>%
  select(c(site, species, percent_N, percent_C)) %>%
  mutate(season = "summer")

winter <- read.csv('input/winter_2025_prepped.csv')
wN <- winter %>% 
  select(c(site, species, percent_N, percent_C)) %>%
  mutate(season = "winter")

# unfortunately the sites don't line up b/c winter is just Broadleaf.1, but summer has Broadleaf.1.1, Broadleaf.1.2, etc.

sN[c('hab', 'loc', 'subsite')] <- str_split_fixed(sN$site,pattern = "[.]", n = 3)
sN %<>% mutate(site = paste0(hab, ".", loc)) %>%
  select(1:5)
# we only want to include the same 10 sites we revisited in winter
sNrestr <- sN %>% filter(site %in% c(wN$site))
comp <- rbind(sNrestr, wN)
unique(comp$species)

# get rid of moss, remove missing values, calculate CN
comp %<>% dplyr::filter(species != "moss",
                        !is.na(percent_N)) %>%
  mutate(CN = percent_C/percent_N)

comp$species <- as.factor(comp$species)
comp$species <- relevel(comp$species, ref = "Cladonia")
saveRDS(comp, 'output/season_comp.Rds')

# Plot out changes in N by species

comp_sum <- comp %>% group_by(species, season) %>%
  summarise(n = n(), 
            mean_N = mean(percent_N), seN = sd(percent_N)/sqrt(n),
            mean_C = mean(percent_C), seC = sd(percent_C)/sqrt(n),
            mean_CN = mean(CN), seCN = sd(CN)/sqrt(n))

ggplot(comp_sum, aes(x = season, y = mean_N, 
                     ymin = mean_N - seN, ymax = mean_N + seN,
                     colour = species)) +
  geom_point() +
  geom_errorbar(width = 0.1) +
  scale_colour_viridis(discrete = TRUE, option = "B", end = 0.9) +
  theme_bw() +
  xlab("") +
  ylab("Nitrogen (% dry mass)")

# to get line between, need to make summer-winter a numerical

comp_sum %<>% mutate(season_bin = ifelse(season == "summer", 0 , 1))
labels <- c("summer", "winter")

# Figure: change in N over seasons
ggplot(comp_sum, aes(x = season_bin, y = mean_N, 
                     ymin = mean_N - seN, ymax = mean_N + seN,
                     colour = species)) +
  geom_point(size = 2) +
  geom_errorbar(width = 0.05) +
  geom_line() +
  scale_colour_viridis(discrete = TRUE, option = "B", end = 0.9) +
  theme_bw() +
  xlab("") +
  scale_x_continuous(label = labels, breaks = c(0, 1)) +
  ylab("Nitrogen (% dry mass)")

ggsave(filename = 'graphics/seasonal/seasonal_N.png',
       height = 1200,
       width = 1600,
       units = "px")

# Figure: change in C over seasons
ggplot(comp_sum, aes(x = season_bin, y = mean_C, 
                     ymin = mean_C - seC, ymax = mean_C + seC,
                     colour = species)) +
  geom_point(size = 2) +
  geom_errorbar(width = 0.05) +
  geom_line() +
  scale_colour_viridis(discrete = TRUE, option = "B", end = 0.9) +
  theme_bw() +
  xlab("") +
  scale_x_continuous(label = labels, breaks = c(0, 1)) +
  ylab("Carbon (% dry mass)")

ggsave(filename = 'graphics/seasonal/seasonal_C.png',
       height = 1200,
       width = 1600,
       units = "px")


# Figure: change in C:N ratio over seasons
ggplot(comp_sum, aes(x = season_bin, y = mean_CN, 
                     ymin = mean_CN - seCN, ymax = mean_CN + seCN,
                     colour = species)) +
  geom_point(size = 2) +
  geom_errorbar(width = 0.05) +
  geom_line() +
  scale_colour_viridis(discrete = TRUE, option = "B", end = 0.9) +
  theme_bw() +
  xlab("") +
  scale_x_continuous(label = labels, breaks = c(0, 1)) +
  ylab("Carbon:Nitrogen ratio")

ggsave(filename = 'graphics/seasonal/seasonal_CN_ratio.png',
       height = 1200,
       width = 1600,
       units = "px")


# Statistical models for each of these figures: are any of these changes significant?
for(i in unique(comp$species)){
  
 sp <- comp %>% filter(species == i)
 
 print(i)
 print(summary(lm(percent_N ~ season, data = sp)))
 print(summary(lm(percent_C ~ season, data = sp)))
 print(summary(lm(CN ~ season, data = sp)))
  
}



# Cladonia specific analyses

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

# How do Kristy's control sites compare in the following year? ----
exp_data <- read.csv("input/Ferraro__BiogeochemicalBoomerang_Data_2024.csv") %>%
  filter(treat == "C", time %in% c("Year1", "Year2")) %>%
  select(c(1, 3, 38, 39, 47, 48, 56, 57, 74, 75, 109, 110))

exp_data %<>% mutate(comm_prop_N = N_stock_forage_gm2/tot_forage_biomass_gm2,
                     year = ifelse(time == "Year1", 1, 2))

# Is rank order maintained from year 1 to year 2 among these sites?

#exp_wide <- exp_data %>% pivot_wider(names_from = lichen, values_from = lichen_precN)

y1 <- exp_data %>% filter(time == "Year1") %>%
  rename(lichen_N1 = 3,
         crow_N1 = 5,
         Kalmia_N1 = 7,
         lingon_N1 = 9) %>%
  select(-c(time))

y2 <- exp_data %>% filter(time == "Year2") %>%
  rename(lichen_N2 = 3,
         crow_N2 = 5,
         Kalmia_N2 = 7,
         lingon_N2 = 9) %>%
  select(-c(time))


exp_wide <- left_join(y1, y2, by = "block")
ggplot(exp_wide, aes(x = lichen_N1, y = lichen_N2)) + geom_point()

cor.test(exp_wide$lichen_N1, exp_wide$lichen_N2, method = 'spearman')
# rho = 0.1329, p = 0.5764 ; no evidence of correlation across years
cor.test(exp_wide$crow_N1, exp_wide$crow_N2, method = 'spearman')
# rho = -0.1321, p = 0.6389 ; no evidence of correlation across years (maybe negative correlation, at that?)
cor.test(exp_wide$Kalmia_N1, exp_wide$Kalmia_N2, method = 'spearman')
# rho = 0.5308, p = 0.01745 ; good Kalmia continues to be good Kalmia!
cor.test(exp_wide$lingon_N1, exp_wide$lingon_N2, method = 'spearman')
# rho = 0.1654, p = 0.4842 ; no evidence of correlation across years

long <- exp_data %>%
  rename(Cladonia = 3,
         crowberry = 5,
         Kalmia_leaf = 7,
         lingonberry = 9) %>%
  dplyr::select(-c(4, 6, 8, 10:12)) %>%
  pivot_longer(!block & !time, names_to = "species", values_to = "N") %>%
  pivot_wider(names_from = time, values_from = N)


ggplot(long, aes(x = Year1, y = Year2, colour = species)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_colour_viridis(discrete = TRUE, option = "B", end = 0.8) +
  labs(x = "% nitrogen in year 1", y = "% nitrogen in year 2") +
  theme_bw()
ggsave('graphics/seasonal/Kristy_y1-y2.png',
       height = 1600,
       width = 2400,
       units = "px")

# Collate all N and C data
fogo_data <- read.csv("input/Ferraro__BiogeochemicalBoomerang_Data_2024.csv")
Kristy <- fogo_data %>% filter(treat == "C") %>%
  select(-c(4:35)) %>%
  select(-c(2, 4, 5, 8:14, 17:23, 26:41, 44:83))

kN <- Kristy %>% 
  select(c(1, 2, 3, 5, 7, 9)) %>% 
  rename(Cladonia = 3, Crowberry = 4, Kalmia = 5, Lingonberry = 6) %>%
  pivot_longer(!time & !block, names_to = "species", values_to = "N")

kC <- Kristy %>% 
  select(c(1, 2, 4, 6, 8, 10)) %>% 
  rename(Cladonia = 3, Crowberry = 4, Kalmia = 5, Lingonberry = 6) %>%
  pivot_longer(!time & !block, names_to = "species", values_to = "C")

k <- left_join(kN, kC, by = c("time", "block", "species")) %>%
  select(-c(block)) %>% 
  filter(time != "Baseline") %>%
  mutate(time = ifelse(time == "Year2", "F-2023", "F-2022"))

jgh <- read.csv('input/cleaned_all_stoich.csv') %>%
  filter(species %in% c(k$species)) %>%
  select(c(6, 9, 10)) %>%
  mutate(time = "this study") %>%
  rename(N = 2, C = 3)

allyears <- rbind(jgh, k)
allyears$time <- as.factor(allyears$time)
allyears$time <- relevel(allyears$time, ref = "this study")

allyears %<>% filter(C > 25 & C < 80)

ggplot(allyears, aes(x = C, y = N, colour = time, facet = species)) +
  geom_point(shape = c(1)) +
  #scale_shape_manual(values = c(1, 3, 15)) +
  scale_colour_manual(values = c("grey", "darkgreen", "darkorange")) +
  labs(x = "Percent C", y = "Percent N") +
  theme_bw() +
  facet_wrap(species ~ .)

ggsave('graphics/seasonal/all_data_comp_nobaseline.png',
       height = 1600,
       width = 2400,
       units = "px")

Nsum <- allyears %>% group_by(time, species) %>%
  summarise(count = n(), 
            mean_N = mean(N),
            se = sd(N, na.rm = TRUE)/sqrt(count),
            CI = 1.96*se,
            LCI = mean_N - CI,
            UCI = mean_N + CI
            )

summary(lm(N ~ time, data = subset(allyears, species == "Cladonia")))
summary(lm(N ~ time, data = subset(allyears, species == "Crowberry")))
summary(lm(N ~ time, data = subset(allyears, species == "Kalmia")))
summary(lm(N ~ time, data = subset(allyears, species == "Lingonberry")))
