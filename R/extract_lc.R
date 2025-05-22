## Extracting land cover of given point

extract_lc <- function(DT, crs, rast, legend) {

	sites <- DT %>% filter(!duplicated(paste0(x,y))) %>%
	  dplyr::select(c(x, y))
  
  setDT(sites)

  pt <- c('x', 'y')
  
  if (st_crs(rast) != crs) {
    rast <- raster::projectRaster(lc, crs = crs$wkt, method = 'ngb')
  }

		extract_pt(sites, lc, pt)
		sites[legend, lc_description := label, on = .(pt_r = class)]

			}
