## Model of stoichiometry based on landscape covariates 

modelling <- function(DT, response) {

  DT %<>% filter(species != "Moss")
  
  mod <- lm(as.formula(paste(response, 
                               "~ sdss_lc + cfs_lc + dist_to_coast + ndvi +
                             dem + slope + cos(aspect) + TPI + terrain + pt_kernel")), 
              data = DT)
}

# adding count of caribou fixes in 25m x 25m cell does not do anything explanatory in the model
# this is all caribou fixes ever, though... perhaps if restricted to just winter, a different story?
# or can we calculate an intensity of use that isn't quite so zero-centred?

# adding count of caribou fixes in 25m x 25m cell does not do anything explanatory in the model
# this is all caribou fixes ever, though... perhaps if restricted to just winter, a different story?
# or can we calculate an intensity of use that isn't quite so zero-centred?

## REMOVE CARIBOU FOR NOW SO WE CAN USE AS PREDICTOR IN ISSA
