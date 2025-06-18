## Tidying up data post-extraction of landscape covariates

habitat_by_sp <- function(DT) {

  DT <- data_cleaned
  
  DT %<>% filter(species != "Moss") %>%
    mutate(species = factor(species)) %>%
  group_by(species, site, sdss_desc, lc_description) %>%
    summarise(n = 1) %>%
    ungroup()
  
  combos <- DT %>% group_by(species, sdss_desc, lc_description) %>%
    summarise(n = sum(n)) %>%
    ungroup()
  
  all_sp <- DT %>%
    filter(!duplicated(species)) %>%
    dplyr::select(c(species))
  all_lc <- DT %>%
    filter(!duplicated(lc_description)) %>%
    dplyr::select(c(lc_description))
  all_sdss <- DT %>%
    filter(!duplicated(sdss_desc)) %>%
    dplyr::select(c(sdss_desc))
  
  permute <- cross_join(all_sp, all_lc) %>%
    cross_join(all_sdss) %>%
    left_join(combos, by = c("species", "sdss_desc", "lc_description")) %>%
    mutate(n = ifelse(is.na(n), 0, n)) %>%
    rename(SDSS = sdss_desc,
           CFS = lc_description)
  
  absence <- permute %>% filter(n == 0)
  presence <- permute %>% filter(n > 0)
  # absence = unique combinations of land cover classes where that species was never observed
  # e.g. Alder would grow in a CFS = "Wetland" pixel if SDSS called it "ConiferScrub", but otherwise, every other CFS "Wetland" pixel did not contain alder.
  
  dist <- presence %>% ungroup() %>% 
    group_by(species) %>% 
    summarise(habitats = length(n),
              prop = habitats/35)
  # but is it actually out of 35???
  
  hab_sampled <- DT %>% group_by(lc_description, sdss_desc) %>%
    summarise(seen = "yes") %>%
    rename(SDSS = sdss_desc,
           CFS = lc_description) %>%
    ungroup()
  # we only visited 23 combinations of these habitat classes
  # there are 35 theoretically possible, we have no idea how many of those exist elsewhere across Fogo Island
  
  permute <- left_join(permute, hab_sampled, by = c("SDSS", "CFS")) %>%
    mutate(seen = ifelse(is.na(seen), "no", "yes"))
  
  ggplot(subset(presence, species == "Alder"),
         aes(x = SDSS, y = CFS, size = n)) +
    geom_point() +
    geom_point(data = subset(permute, seen == "no"), aes(x = SDSS, y = CFS), colour = "red", shape = 4, size = 4, show.legend = FALSE) +
    ggtitle("Alder occurrence") +
    theme_bw()
  }
