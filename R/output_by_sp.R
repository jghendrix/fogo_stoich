#' @title Model output by species
#' @export
#' @author Jack G Hendrix
output_by_sp <- function(model, sp_key) {

	DT <- as.data.table(
		tidy(model) %>%
			mutate(species = sp_key$species) %>%
		  filter(term != "(Intercept)")
	)
	
	return(DT)
	
}
  