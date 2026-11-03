# Vascular species contributions to community %N
species_N <- read_csv('output/species_N_by_pixel.csv')

# nitrogen content of each species, for each cell in the StDM (7.8 million 5x5m cells)


xs <- unique(species_N$x) %>% sort()
ys <- unique(species_N$y) %>% sort()

# take every 5th obs, starting at 3rd (to be centred)
xrnd <- xs[-c(1,2)]
x5 <- xrnd[seq(1, length(xrnd), 5)]

yrnd <- ys[-c(1,2)]
y5 <- yrnd[seq(1, length(yrnd), 5)]

x25 <- rep(x5, each = 5)[-c(4809,4810)] #the last two rows are bonus
y25 <- rep(y5, each = 5)

ycon <- data.frame(ys, y25) #%>% mutate(diff = y25 - ys)
#hist(ycon$diff)
xcon <- data.frame(xs, x25) #%>% mutate(diff = x25 - xs)
#hist(xcon$diff)
# both of these worked great, phew

species_N_25 <- left_join(species_N, xcon, by = c("x" = "xs"))
species_N_25 <- left_join(species_N_25, ycon, by = c("y" = "ys"))

species_N_agg <- species_N_25 %>% group_by(x25, y25) %>%
  summarise(N_Cladonia = mean(N_Cladonia, na.rm = T),
            N_alder = mean(N_alder, na.rm = T),
            N_blueberry = mean(N_blueberry, na.rm = T),
            N_crowberry = mean(N_crowberry, na.rm = T),
            N_deergrass = mean(N_deergrass, na.rm = T),
            N_dwarf_birch = mean(N_dwarf_birch, na.rm = T),
            N_Kalmia = mean(N_Kalmia, na.rm = T),
            N_lingonberry = mean(N_lingonberry, na.rm = T),
            N_vascular = mean(N_vascular, na.rm = T)
  )

write.csv(species_N_agg, 'output/species_N_25m.csv')


spN <- read.csv('output/species_N_25m.csv')
spN %<>% mutate(
  Omit_al = rowMeans(spN[6:11], na.rm = T),
  Omit_bl = rowMeans(spN[c(5,7:11)], na.rm = T),
  Omit_cr = rowMeans(spN[c(5:6,8:11)], na.rm = T),
  Omit_de = rowMeans(spN[c(5:7,9:11)], na.rm = T),
  Omit_dw = rowMeans(spN[c(5:8,10,11)], na.rm = T),
  Omit_Ka = rowMeans(spN[c(5:9, 11)], na.rm = T),
  Omit_li = rowMeans(spN[5:10], na.rm = T)
)



sum <- spN %>% dplyr::select(-c(1:11))

# There are 12 rows for Omit_Kalmia that are NAs, every other column has the same length n = 362531, O_K n = 362519
sum_sp <- sum %>% summarize(
  All = mean(N_vascular, na.rm = T),
  Alder = mean(Omit_al),
  Blueberry = mean(Omit_bl),
  Crowberry = mean(Omit_cr),
  Deergrass = mean(Omit_de),
  Dwarf_birch = mean(Omit_dw),
  Kalmia = mean(Omit_Ka, na.rm = T),
  Lingonberry = mean(Omit_li),
  all_sd = sd(N_vascular, na.rm = T),
  Oa_sd = sd(Omit_al),
  Ob_sd = sd(Omit_bl),
  Oc_sd = sd(Omit_cr),
  Ode_sd = sd(Omit_de),
  Odw_sd = sd(Omit_dw),
  OK_sd = sd(Omit_Ka, na.rm = T),
  Ol_sd = sd(Omit_li)
)

means <- sum_sp %>% dplyr::select(c(1:8)) %>%
  pivot_longer(everything(), 
               names_to = "Species omitted") %>%
  rename(Percent_N = value)

sds <- sum_sp %>% dplyr::select(c(9:16)) %>%
  pivot_longer(everything()) %>%
  dplyr::select(sd = value)

avg <- cbind(means, sds) %>%
  mutate(CI = ifelse(`Species omitted` != "Kalmia", 1.96*sd/sqrt(362531), 1.96*sd/sqrt(362519)))


ggplot(avg, aes(x = `Species omitted`, y = Percent_N, ymin = Percent_N - sd, ymax = Percent_N + sd)) +
  geom_point() +
  geom_errorbar() +
  labs(y = "Average percent nitrogen content") +
  theme_bw()

# can we plot the overall average as a ribbon/shaded area?

spp <- avg %>% dplyr::filter(`Species omitted` != "All")
# mean = 2.350976
2.350976 +0.5172203
#ymin = 1.83357756  
# ymax = 2.868196

ggplot(spp, aes(x = `Species omitted`, y = Percent_N, ymin = Percent_N - sd, ymax = Percent_N + sd)) +
  annotate(geom = "rect", xmin = 0, xmax = 8,
           ymin = 1.83357756, ymax = 2.868196, 
           alpha = 0.3, fill = "grey") +
  geom_hline(yintercept = 2.350976, colour = "darkgrey") +
  geom_point() +
  geom_errorbar(width = 0.5) +
  labs(y = "Average percent nitrogen content") +
  theme_bw()

ggsave(filename = "graphics/community_N_spp_omitted.png",
       height = 1400,
       width = 1800,
       units = "px")
  
# Take the difference per pixel and then average the differences?
sum_diff <- sum %>% mutate(
  Alder_diff = Omit_al - N_vascular,
  Blue_diff = Omit_bl - N_vascular,
  Crow_diff = Omit_cr - N_vascular,
  Deer_diff = Omit_de - N_vascular,
  Dwarf_diff = Omit_dw - N_vascular,
  Kalmia_diff = Omit_Ka - N_vascular,
  Lingon_diff = Omit_li - N_vascular
  ) %>%
  summarise(
    Alder = mean(Alder_diff),
    Blueberry = mean(Blue_diff),
    Crowberry = mean(Crow_diff),
    Deergrass = mean(Deer_diff),
    Dwarf_birch = mean(Dwarf_diff),
    Kalmia  = mean(Kalmia_diff, na.rm = T),
    Lingonberry = mean(Lingon_diff),
    
    sd_Alder = sd(Alder_diff),
    sd_Blueberry = sd(Blue_diff),
    sd_Crowberry = sd(Crow_diff),
    sd_Deergrass = sd(Deer_diff),
    sd_Dwarf_birch = sd(Dwarf_diff),
    sd_Kalmia  = sd(Kalmia_diff, na.rm = T),
    sd_Lingonberry = sd(Lingon_diff)
  )


dmeans <- sum_diff %>% dplyr::select(c(1:7)) %>%
  pivot_longer(everything(), 
               names_to = "Species omitted") %>%
  rename(`Change in %N` = value)

sds <- sum_diff %>% dplyr::select(c(8:14)) %>%
  pivot_longer(everything()) %>%
  dplyr::select(sd = value)

avg_diff <- cbind(dmeans, sds) %>%
  mutate(CI = ifelse(`Species omitted` != "Kalmia", 1.96*sd/sqrt(362531), 1.96*sd/sqrt(362519)))


ggplot(avg_diff, aes(x = `Species omitted`, y = `Change in %N`, ymin = `Change in %N` - sd, ymax = `Change in %N` + sd)) +
  geom_hline(yintercept = 0, colour = "darkgrey") +
  geom_point() +
  geom_errorbar(width = 0.5) +
  ylab("Difference in community %N when omitted") +
  theme_bw()

ggsave(filename = "graphics/diff_in_community_N_spp_omitted.png",
       height = 1400,
       width = 1800,
       units = "px")
