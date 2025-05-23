## Model of stoichiometry based on landscape covariates 

modelling <- function(DT, response) {

  DT %<>% filter(species != "Moss")
  
  mod <- lm(as.formula(paste(response, 
                               "~ pt_lc + pt_sdss + dist_to_coast + ndvi +
                             elevation + slope + cos(aspect) + TPI + rugged")), 
              data = DT)
}
