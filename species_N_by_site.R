## Calculating pairwise correlations between species % nitrogen content at each sampling site
tar_load(stoich)

stoichnoNA <- stoich %>% mutate(ssps = paste0(site, species, subsample)) %>%
  filter(!duplicated(ssps))

# Need to average within each location - don't want to compare Kalmia.1 to Cladonia.1, Kalmia.2 vs. Cladonia.2 within each sampling plot
spavg <- stoichnoNA %>% group_by(site, species) %>%
  summarise(N = mean(percent_N))

# Now pivot wide by site
sp_wide <- spavg %>% pivot_wider(names_from = species, values_from = N)
sp <- sp_wide %>% ungroup() %>% dplyr::select(-c(site, Moss, graminoid_spp., Black_spruce))
matrix <- as.data.frame(cor(sp, use = "pairwise.complete.obs"))
write.csv(matrix, 'output/site_N_pairwise_correlations.csv')

# How many sites does each correlation represent?
spavg %<>% filter(!species %in% c("Moss", "graminoid_spp.", "Black_spruce"))

splist <- c("Cladonia", "Crowberry", "Kalmia", "Lingonberry", "Alder", "Blueberry", "Deergrass", "Dwarf_birch")

for(i in splist) {
  
  for (j in splist) {
    
    if(i == j) {
    print(NA) }
    else {
      overlap <- spavg %>% filter(species == i | species == j) %>%
        group_by(site) %>%
        summarise(n = n()) %>%
        filter(n == 2)
    
      print(paste0(i, j, length(overlap$site)))
    }
  }
}
