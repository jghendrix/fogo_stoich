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

# Path to land cover raster
lc_path <- file.path('input', 'Hermosilla_2022_land_cover.tif')
legend_path <- file.path('input', 'cfs_legend.csv')

# Path to landscape covariates
dem_path <- file.path('input', 'Fogo_DEM.tif')
ndvi_path <- file.path('input', 'weekly2022_13-19Jun_NDVI.tif')
slope_path <- file.path('input', 'Fogo_slope.tif')
aspect_path <- file.path('input', 'Fogo_aspect.tif')
TPI_path <- file.path('input', 'Fogo_TPI.tif')
terrain_path <- file.path('input', 'Fogo_terrain_ruggedness.tif')

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
	sites_to_coast,
	dist_to_feature(
		stoich,
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
  ndvi_extract,
  extract_lc(
    sites_to_roads,
    crs,
    ndvi,
    legend
  )
),

tar_target(
  raster_ext,
  extract_raster(
    sites_to_roads,
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
  prepare_data(sites_to_roads, raster_ext)
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

# Targets: all ------------------------------------------------------------
# Automatically grab and combine all the "targets_*" lists above
lapply(grep('targets', ls(), value = TRUE), get)
