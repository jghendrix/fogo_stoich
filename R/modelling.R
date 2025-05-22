## Model of stoichiometry based on landscape covariates 

modelling <- function(DT, response) {

  mod <- lm(as.formula(paste(response, 
                               "~ lc + dist_to_coast + ndvi + elevation +
                               slope_deg + cos(aspect) + TPI + rugged")), 
              data = DT)
}
