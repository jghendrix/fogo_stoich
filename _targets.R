# === Targets: Fogo StDM----------------------
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

# rasterized distance to coast layer
dist_to_coast_path <- file.path('input', 'dist_to_coast.tif')


# Variables ---------------------------------------------------------------
bb <- c(
	xmin = -54.3533,
	ymin = 49.5194,
	xmax = -53.954220,
	ymax = 49.763834
)

epsg <- 32621
crs <- st_crs(epsg)

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
	
	tar_target(
	  coast,
	  get_coastline(bb, crs))
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
  sdss_sites,
  extract_lc(renamed_coast, 
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
))

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
  )
)

# Targets: predicting N across rest of island ---------------------------
targets_predict <- c(

  # distance to coast needs to be a raster to project across
  tar_file_read(
    dist_to_coast,
    dist_to_coast_path,
    raster(!!.x)
  ),
  
  # We can't apply the same predictive model to dwarf_birch, it fails to convege
  # so look at the rest of the species separately and then combine at end
  
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
      N_by_cell,
      predictions_by_cell(N_predictions, N_birch)
    ),
  
  tar_target(
    N_raster,
    rasterise(N_by_cell, "N")
  )
)
  
  
  ### For supplement: Ratio models ----
targets_supplement <- c(
   
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
    CN_by_cell,
    ratio_predictions_by_cell(CN_predictions, CN_birch)
  ),
  
  tar_target(
    CN_raster,
    rasterise(CN_by_cell, "ratio")
  ),
  
  ### For supplement: Carbon alone models ----
  
  tar_target(
    C_models,
    modelling(sp_prep, "percent_C"),
    map(sp_prep)
  ),
  
  tar_target(
    C_sum,
    summarise_model(C_models, sp_prep),
    map(C_models, sp_prep)
  ),
  
  tar_target(
    C_coef,
    output_by_sp(C_models, sp_key),
    pattern = map(C_models, sp_key)
  ),
  
tar_target(
  C_predictions,
  predicted_C_map(sp_prep,
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
  C_birch,
  predicted_single_C(birch_prep,
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
)
)
# Targets: all ------------------------------------------------------------
# Automatically grab and combine all the "targets_*" lists above
lapply(grep('targets', ls(), value = TRUE), get)
