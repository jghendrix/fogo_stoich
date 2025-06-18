# === Targets: Fogo stoich mapping ----------------------
# Jack G Hendrix
# 24 February 2025

# Source ------------------------------------------------------------------
targets::tar_source('R')

# Options -----------------------------------------------------------------
tar_option_set(format = 'qs')

# Data --------------------------------------------------------------------
# Path to stoich samples data
stoich_path <- file.path('input', 'cleaned_all_stoich.csv')

# Path to land cover rasters
# lc = Canadian Forest service, sdss = provincial product
lc_path <- file.path('input', 'CFS_5m.tif')
sdss_path <- file.path('input', 'SDSS_of_Fogo-5m.tif')
legend_path <- file.path('input', 'cfs_legend.csv')
sdss_legend_path <- file.path('input', 'legend_sdss.csv')

# Path to landscape covariates
dem_path <- file.path('input', 'dem.tif')
ndvi_path <- file.path('input', 'ndvi_5m.tif')
slope_path <- file.path('input', 'slope.tif')
aspect_path <- file.path('input', 'aspect_rad.tif')
TPI_path <- file.path('input', 'TPI.tif')
terrain_path <- file.path('input', 'terrain.tif')

# rasterized distance to coast
dist_to_coast_path <- file.path('input', 'dist_to_coast_4999.tif')

# Variables ---------------------------------------------------------------
bb <- c(
	xmin = -54.3533,
	ymin = 49.5194,
	xmax = -53.954220,
	ymax = 49.763834
)
epsg <- 32621
crs <- st_crs(epsg)
crs_sp <- CRS(crs$wkt)



id_col <- 'Animal_ID'
datetime_col <- 'DATETIME'
x_col <- 'Longitude'
y_col <- 'Lat'
tz <- 'America/St_Johns'

# Split by: within which column or set of columns (eg. c(id, yr))
#  do we want to split our analysis?
split_by <- id_col
seasonal_split <- "season"

# Targets: data -----------------------------------------------------------
targets_data <- c(
	tar_file_read(
		stoich,
		stoich_path,
		fread(!!.x)
	),

	tar_file_read(
		lc,
		lc_path,
		raster(!!.x)
	),

	tar_file_read(
		legend,
		legend_path,
		fread(!!.x)
	),
	
	tar_file_read(
	  sdss,
	  sdss_path,
	  raster(!!.x)
	),
	
	tar_file_read(
	  sdss_legend,
	  sdss_legend_path,
	  fread(!!.x)
	),
	
	tar_file_read(
	  dem,
	  dem_path,
	  raster(!!.x)
	),
	
	tar_file_read(
	  ndvi,
	  ndvi_path,
	  raster(!!.x)
	),
	
	tar_file_read(
	  slope,
	  slope_path,
	  raster(!!.x)
	),
	tar_file_read(
	  aspect,
	  aspect_path,
	  raster(!!.x)
	),
	tar_file_read(
	  TPI,
	  TPI_path,
	  raster(!!.x)
	),
	tar_file_read(
	  terrain,
	  terrain_path,
	  raster(!!.x)
	),
	
	tar_target(
		coast,
		get_coastline(bb, crs)
		),

	tar_target(
		roads,
		get_roads(bb, crs)
	),
	tar_target(
		water,
		get_lakes(bb, crs)
	)
	
)



# Targets: extract --------------------------------------------------------
targets_extract <- c(

tar_target(
    sites,
    unique_sites(
      stoich
      )
    ),

tar_target(
	sites_to_coast,
	dist_to_feature(
		sites,
		crs,
		coast
	)
),

tar_target(
  renamed_coast,
  sites_to_coast %>% dplyr::rename(dist_to_coast = dist_to_feat_line)
),

tar_target(
	sites_to_roads,
	dist_to_feature(
		renamed_coast,
		crs,
		roads
	)
),

tar_target(
  sdss_sites,
  extract_lc(sites_to_roads, 
             crs, 
             sdss, 
             sdss_legend)
),

tar_target(
  renamed_sdss,
  sdss_sites %>% dplyr::rename(pt_sdss = pt_lc, sdss_desc = lc_description)
),

tar_target(
  cfs_sites,
  extract_lc(renamed_sdss, 
             crs, 
             lc, 
             legend)
  ),

tar_target(
  raster_ext,
  extract_raster(
    cfs_sites,
    crs,
    ndvi,
    dem,
    slope,
    aspect,
    TPI,
    terrain
  )
),

tar_target(
  data_cleaned,
  prepare_data(stoich, raster_ext)
)
)

# Targets: explanatory models -------------------------------------------------------

targets_models <- c(
  
  tar_target(
    sp_prep,
    as.data.table(subset(data_cleaned, species != "Moss"))[, tar_group := .GRP, by = c('species')], iteration = 'group'
    ),
    
    tar_target(
      sp_key,
      unique(sp_prep[, .SD, .SDcols = c("species", 'tar_group')])
    ),
    
  tar_target(
    N_models,
    modelling(sp_prep, "percent_N"),
    map(sp_prep)
  ),
  
  tar_target(
    N_sum,
    summarise_model(N_models, sp_prep),
    map(N_models, sp_prep)
  ),
  
 tar_target(
  N_coef,
    output_by_sp(N_models, sp_key),
    pattern = map(N_models, sp_key)
  ),
  
  tar_target(
    CN_models,
    modelling(sp_prep, "CN_ratio"),
    map(sp_prep)
  ),
  
  tar_target(
    CN_sum,
    summarise_model(CN_models, sp_prep),
    map(CN_models, sp_prep)
  ),
 
 tar_target(
   CN_coef,
   output_by_sp(CN_models, sp_key),
   pattern = map(CN_models, sp_key)
 )
)

# Targets: predicting N across rest of island ---------------------------
targets_predict <- c(

  tar_file_read(
    dist_coast,
    dist_to_coast_path,
    raster(!!.x)
  ),
  
  tar_target(
    raster_stack,
    stack_rasters(sdss, 
                  lc, 
                  ndvi, 
                  dem, 
                  slope, 
                  aspect, 
                  TPI,
                  terrain,
                  dist_coast)
  ),
  
  tar_target(
    spp_dist,
    habitat_by_sp(data_cleaned)
  )
  
  ## Spatial prediction from model using the stacked rasters??
  
)
# Targets: all ------------------------------------------------------------
# Automatically grab and combine all the "targets_*" lists above
lapply(grep('targets', ls(), value = TRUE), get)
