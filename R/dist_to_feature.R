dist_to_feature <- function(DT, crs, feature) {

  DT %<>% dplyr::filter(!is.na(x) & !is.na(y))
	setDT(DT)

	start <- c('x', 'y')

	feat_line <- st_geometry(obj = feature) %>% 
	  st_cast(to = 'LINESTRING') 
	
	extract_distance_to(DT, feat_line, start, crs)

}
