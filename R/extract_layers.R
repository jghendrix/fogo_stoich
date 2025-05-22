#' @title Extract layers
#' @export
#' @author Alec L. Robitaille, Julie W. Turner
extract_layers <- function(DT, crs, lc, legend, burn_prep) {

	setDT(DT)

	start <- c('x1_', 'y1_')
	end <- c('x2_', 'y2_')

	if (st_crs(lc) != crs) {
		lc <- raster::projectRaster(lc, crs = crs$wkt, method = 'ngb')
	}

	extract_pt(DT, lc, end)
	DT[legend, lc_description := label, on = .(pt_lc = class)]

	# It will only give me one of the two distances, whichever is listed second. Overwriting itself, I imagine? For the time being let's just focus on newer burns to which they are more likely to respond
}
