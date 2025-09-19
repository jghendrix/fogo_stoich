
rasterise <- function(df, response){


  if(response == "N"){
 df$N_vascular <- rowMeans(subset(df, select = c(3:5, 7:10)), na.rm = T)
  
  write.csv(df, 'output/species_N_by_pixel.csv') 
  
  cladonia <- df %>% dplyr::select(c(x, y, N_Cladonia))
  vascular <- df %>% dplyr::select(c(x, y, N_vascular))
  clad <- rasterFromXYZ(cladonia)
  vasc <- rasterFromXYZ(vascular)
  
  terra::writeRaster(clad, "output/Cladonia_N.tif", filetype = "GTiff")
  terra::writeRaster(vasc, "output/Vascular_N.tif", filetype = "GTiff")
  
  }
  
  else{
    df$CN_vascular <- rowMeans(subset(df, select = c(3:5, 7:10)), na.rm = T)
    
    write.csv(df, 'output/species_CN_ratio_by_pixel.csv') 
    
    cladonia <- df %>% dplyr::select(c(x, y, CN_Cladonia))
    vascular <- df %>% dplyr::select(c(x, y, CN_vascular))
    clad <- rasterFromXYZ(cladonia)
    vasc <- rasterFromXYZ(vascular)
    
    terra::writeRaster(clad, "output/Cladonia_CN.tif", filetype = "GTiff")
    terra::writeRaster(vasc, "output/Vascular_CN.tif", filetype = "GTiff")
    
  }
    
  }