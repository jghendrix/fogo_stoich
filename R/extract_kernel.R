## Extracting kernel density for a given point

extract_kernel <- function(DT, crs, kernel) {

	#sites <- DT %>% filter(!duplicated(paste0(x, y))) %>%
	 # dplyr::select(c(x, y))
  
  sites <- setDT(DT)

  pt <- c('x', 'y')
  
  if (st_crs(kernel) != crs) {
    kernel <- raster::projectRaster(kernel, crs = crs$wkt, method = 'ngb')
  }
  
	extract_pt(sites, kernel, pt)
		
  return(sites)
}