## Model of stoichiometry based on landscape covariates 

modelling <- function(DT, response) {

  mod <- lm(as.formula(paste(response, 
                               "~ lc + lc_description + dist_to_coast + ndvi + elevation +
                               slope + cos(aspect) + TPI + rugged")), 
              data = DT)
}
