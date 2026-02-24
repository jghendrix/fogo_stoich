## Model of stoichiometry based on landscape covariates 

modelling <- function(DT, response) {

  DT %<>% filter(species != "Moss")
  
  mod <- lm(as.formula(paste(response, 
                               "~ sdss_lc + cfs_lc + dist_to_coast + ndvi +
                             dem + slope + cos(aspect) + TPI + terrain")), 
              data = DT)
}