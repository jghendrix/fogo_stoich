# getting a wide version of stoich sampling data to get correlations between species from our observed data

tar_load(stoich)

spN <- stoich %>% dplyr::select(site, subsample, species, percent_N)
# there are 106 duplicates in here though... like Mixed.1 has 2x Kalmia.1, 2x Kalmia.2 ... how did that happen???

stoichnoNA <- stoich %>% mutate(ssps = paste0(site, species, subsample)) %>%
  filter(!duplicated(ssps))
# 1312, not 1419 now
spN <- stoichnoNA %>% dplyr::select(site, subsample, species, percent_N)

# Now pivot wide by site?
sp_wide <- spN %>% pivot_wider(id_cols = c(site, subsample), names_from = species, values_from = percent_N)


write.csv(sp_wide, 'output/site-wise_species_N.csv')

spp <- sp_wide %>% dplyr::select(-c(site, subsample, Moss, graminoid_spp., Black_spruce))

# Get correlation matrix
site_matrix <- as.data.frame(cor(spp, use = "pairwise.complete.obs"))
write.csv(site_matrix, 'output/site_species_pairwise_correlations_percentN.csv')

# These correlations are correlated subsample 1 Kalmia with subsample 1 Cladonia, 2 and 2, etc. from the same site... that's nonsense. We have to take the average per species per site and do across sites, I suppose
spavg <- stoichnoNA %>% group_by(site, species) %>%
  summarise(N = mean(percent_N))

# Now pivot wide by site?
sp_wide <- spavg %>% pivot_wider(names_from = species, values_from = N)
sp <- sp_wide %>% ungroup() %>% dplyr::select(-c(site, Moss, graminoid_spp., Black_spruce))
matrix <- as.data.frame(cor(sp, use = "pairwise.complete.obs"))
write.csv(matrix, 'output/site_N_pairwise_correlations.csv')

# How many sites does each correlation represent though?
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
