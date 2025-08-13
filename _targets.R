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
winter_path <- file.path('input', 'winter_2025_prepped.csv')

# Path to land cover rasters
# lc = Canadian Forest service, sdss = provincial product
cfs_path <- file.path('input', 'cfs.tif')
sdss_path <- file.path('input', 'sdss.tif')
legend_path <- file.path('input', 'cfs_legend.csv')
sdss_legend_path <- file.path('input', 'trimmed_legend_sdss.csv')

# Path to landscape covariates
dem_path <- file.path('input', 'dem.tif')
ndvi_path <- file.path('input', 'ndvi.tif')
slope_path <- file.path('input', 'slope.tif')
aspect_path <- file.path('input', 'aspect.tif')
TPI_path <- file.path('input', 'TPI.tif')
terrain_path <- file.path('input', 'terrain.tif')
kernel_path <- file.path('input', 'kernel.tif')

# rasterized distance to coast
dist_to_coast_path <- file.path('input', 'dist_to_coast.tif')

# Caribou data path
gps_path <- file.path('output', 'all_Fogo_caribou_locs_20June2025.csv')
id_path <- file.path('output', 'collar-animal-ids.csv')

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
datetime_col <- 'datetime'
x_col <- 'x_long'
y_col <- 'y_lat'
tz <- 'America/St_Johns'

# Split by: within which column or set of columns (eg. c(id, yr))
#  do we want to split our analysis?
split_by <- id_col
seasonal_split <- "season"

# For iSSA tracks:
# Resampling rate
rate <- minutes(120)
# Tolerance
tolerance <- minutes(5)
# Number of random steps
n_random_steps <- 10


# Targets: data -----------------------------------------------------------
targets_data <- c(
	tar_file_read(
		stoich,
		stoich_path,
		fread(!!.x)
	),
	
	tar_file_read(
	  winter,
	  winter_path,
	  fread(!!.x)
	),
	
	tar_file_read(
	  caribou,
	  gps_path,
	  fread(!!.x)
	),
	
	tar_file_read(
	  ids,
	  id_path,
	  fread(!!.x)
	),
	
	tar_file_read(
		cfs,
		cfs_path,
		raster(!!.x)
	),

	tar_file_read(
		cfs_legend,
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
	tar_file_read(
	  kernel,
	  kernel_path,
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



# Targets: collect stoich predictors --------------------------------------------------------
targets_collect <- c(

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
  sdss_sites %>% dplyr::rename(sdss_number = pt_lc, sdss_lc = lc_description)
),

tar_target(
  cfs_sites,
  extract_lc(renamed_sdss, 
             crs, 
             cfs, 
             cfs_legend)
  ),

tar_target(
  renamed_cfs,
  cfs_sites %>% dplyr::rename(cfs_number = pt_lc, cfs_lc = lc_description)
),

tar_target(
  raster_ext,
  extract_raster(
    renamed_cfs,
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
  kerneled,
  extract_kernel(
    raster_ext,
    crs,
    kernel)
),

tar_target(
  data_cleaned,
  prepare_data(stoich, kerneled)
)
)

# Targets: explanatory models -------------------------------------------------------

targets_models <- c(
  
  tar_target(
    sp_prep,
    as.data.table(subset(data_cleaned, species != "Moss" &
                                       species != "Black_spruce" &
                                       species != "graminoid_spp." &
                                       species != "Dwarf_birch"))
    [, tar_group := .GRP, by = c('species')], iteration = 'group'
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
    dist_to_coast,
    dist_to_coast_path,
    raster(!!.x)
  ),
  # We can't apply the same predictive model to graminoid or birch, it breaks
  # separate these out and run separately?
  
  tar_target(
    N_predictions,
    predicted_map(sp_prep,
                  sp_key, 
                  sdss, 
                  sdss_legend,
                  cfs, 
                  cfs_legend,
                  ndvi, 
                  dem, 
                  slope, 
                  aspect, 
                  TPI,
                  terrain,
                  dist_to_coast),
  pattern = map(sp_prep, sp_key)
  ),
  
  tar_target(
    gram_prep,
    as.data.table(subset(data_cleaned, species == "graminoid_spp."))
  ),
  
  tar_target(
    birch_prep,
    as.data.table(subset(data_cleaned, species == "Dwarf_birch"))
  ),
  
  tar_target(
    N_birch,
    predicted_single(birch_prep,
                  sdss, 
                  sdss_legend,
                  cfs, 
                  cfs_legend,
                  ndvi, 
                  dem, 
                  slope, 
                  aspect, 
                  TPI,
                  terrain,
                  dist_to_coast)
  ),
  
  tar_target(
    N_gram,
    predicted_single(gram_prep,
                     sdss, 
                     sdss_legend,
                     cfs, 
                     cfs_legend,
                     ndvi, 
                     dem, 
                     slope, 
                     aspect, 
                     TPI,
                     terrain,
                     dist_to_coast)
  ),
  
  tar_target(
      N_by_cell,
      predictions_by_cell(N_predictions, N_birch, N_gram)
    ),
  
  tar_target(
    N_raster,
    rasterise(N_by_cell)
  ),
  
  tar_target(
    CN_predictions,
    ratio_predicted_map(sp_prep,
                  sp_key, 
                  sdss, 
                  sdss_legend,
                  cfs, 
                  cfs_legend,
                  ndvi, 
                  dem, 
                  slope, 
                  aspect, 
                  TPI,
                  terrain,
                  dist_to_coast),
    pattern = map(sp_prep, sp_key)
  ),
  
  ## Birch and graminoid all failed to run for C:N ratio, exact same as %N
  tar_target(
    CN_birch,
    ratio_predicted_single(birch_prep,
                     sdss, 
                     sdss_legend,
                     cfs, 
                     cfs_legend,
                     ndvi, 
                     dem, 
                     slope, 
                     aspect, 
                     TPI,
                     terrain,
                     dist_to_coast)
  ),
  
  tar_target(
    CN_gram,
    ratio_predicted_single(gram_prep,
                     sdss, 
                     sdss_legend,
                     cfs, 
                     cfs_legend,
                     ndvi, 
                     dem, 
                     slope, 
                     aspect, 
                     TPI,
                     terrain,
                     dist_to_coast)
  ),
  
  tar_target(
    CN_by_cell,
    ratio_predictions_by_cell(CN_predictions, CN_birch, CN_gram)
  ),
  
  tar_target(
    CN_raster,
    rasterise(CN_by_cell)
  )
  
)

# Targets: all ------------------------------------------------------------
# Automatically grab and combine all the "targets_*" lists above
lapply(grep('targets', ls(), value = TRUE), get)
