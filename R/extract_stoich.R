## Extracting land cover of given point

extract_stoich <- function(DT, crs, stoich) {

  sites <- setDT(DT)

  if (st_crs(stoich) != crs) {
    stoich <- raster::projectRaster(stoich, crs = crs$wkt, method = 'ngb')
  }

		extract_pt(sites, stoich, pt)
	#	sites[legend, lc_description := label, on = .(pt_lc = class)]

			}
