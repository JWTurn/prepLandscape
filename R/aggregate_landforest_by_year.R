#' @title make grid proportion layers from yearly CA_forest
#' @export
#' @author Julie W. Turner


aggregate_landforest <- function(path, trast, startyr, endyr, crs, where2save){
  yrs <- startyr:endyr
  ls_loc <- c(file.path(path, paste0('CA_forest_VLCE2_', yrs), 
                        paste0('CA_forest_VLCE2_', yrs, '.tif')))
  names(ls_loc) <- as.character(yrs)
  
  # if it doesn't exist, create where saving
  dir.create(where2save)

  foreach(rr = 1:length(ls_loc)) %do% {
    land.full <- rast(ls_loc[[rr]])
    if(st_crs(land.full) != st_crs(crs)){
      land.proj <- project(land.full, crs, method = 'near')
      land.full <- land.proj
    }
    
    land <- crop(land.full, trast)
    print('land projected and cropped')
    
    
    
    
    water <- land == 20
    names(water) <- "water"
    water01 <- classify(water,cbind(from = c(TRUE, FALSE), to = c(1, 0)))
    wateragg <- resample(water01, trast, method ='average')
    writeRaster(wateragg, file.path(where2save, paste0('prop_water_', yrs[[rr]], '.tif')))
    print("water")
    
    gc()
    snow <- land == 31
    names(snow) <- "snow"
    snow01 <- classify(snow,cbind(from = c(TRUE, FALSE), to = c(1, 0)))
    snowagg <- resample(snow01, trast, method ='average')
    writeRaster(snowagg, file.path(where2save, paste0('prop_snow_', yrs[[rr]], '.tif')))
    print("snow")
    
    gc()
    rock <- land == 32
    names(rock) <- "rock"
    rock01 <- classify(rock,cbind(from = c(TRUE, FALSE), to = c(1, 0)))
    rockagg <- resample(rock01, trast, method ='average')
    writeRaster(rockagg, file.path(where2save, paste0('prop_rock_', yrs[[rr]], '.tif')))
    print("rock")
    
    gc()
    barrenland <- land == 33
    names(barrenland) <- "barrenland" # exposed_barren_land in CA_forest, but keeping for consistency with landsat
    barrenland01 <- classify(barrenland,cbind(from = c(TRUE, FALSE), to = c(1, 0)))
    barrenlandagg <- resample(barrenland01, trast, method ='average')
    writeRaster(barrenlandagg, file.path(where2save, paste0('prop_barrenland_', yrs[[rr]], '.tif')))
    print("barrenland")
    
    gc()
    bryoids <- land == 40
    names(bryoids) <- "bryoids"
    bryoids01 <- classify(bryoids,cbind(from = c(TRUE, FALSE), to = c(1, 0)))
    bryoidsagg <- resample(bryoids01, trast, method ='average')
    writeRaster(bryoidsagg, file.path(where2save, paste0('prop_bryoids_', yrs[[rr]], '.tif')))
    print("bryoids")
    
    gc()
    shrub <- land == 50
    names(shrub) <- "shrub"
    shrub01 <- classify(shrub,cbind(from = c(TRUE, FALSE), to = c(1, 0)))
    shrubagg <- resample(shrub01, trast, method ='average')
    writeRaster(shrubagg, file.path(where2save, paste0('prop_shrub_', yrs[[rr]], '.tif')))
    print("shrub")
    
    gc()
    wetland <- land == 80
    names(wetland) <- "wetland"
    wetland01 <- classify(wetland,cbind(from = c(TRUE, FALSE), to = c(1, 0)))
    wetlandagg <- resample(wetland01, trast, method ='average')
    writeRaster(wetlandagg, file.path(where2save, paste0('prop_wetland_', yrs[[rr]], '.tif')))
    print("wetland")
    
    gc()
    wet_treed <- land == 81
    names(wet_treed) <- "wet_treed"
    wet_treed01 <- classify(wet_treed,cbind(from = c(TRUE, FALSE), to = c(1, 0)))
    wet_treedagg <- resample(wet_treed01, trast, method ='average')
    writeRaster(wet_treedagg, file.path(where2save, paste0('prop_wet_treed_', yrs[[rr]], '.tif')))
    print("wet_treed")
    
    gc()
    herbs <- land == 100
    names(herbs) <- "herbs"
    herbs01 <- classify(herbs,cbind(from = c(TRUE, FALSE), to = c(1, 0)))
    herbsagg <- resample(herbs01, trast, method ='average')
    writeRaster(herbsagg, file.path(where2save, paste0('prop_herbs_', yrs[[rr]], '.tif')))
    print("herbs")
    
    gc()
    needleleaf <- land == 210 
    names(needleleaf) <- "needleleaf" # coniferous in CA_forest, but keeping for consistency with landsat
    needleleaf01 <- classify(needleleaf,cbind(from = c(TRUE, FALSE), to = c(1, 0)))
    needleleafagg <- resample(needleleaf01, trast, method ='average')
    writeRaster(needleleafagg, file.path(where2save, paste0('prop_needleleaf_', yrs[[rr]], '.tif')))
    print("needleleaf")
    
    gc()
    deciduous <- land == 220
    names(deciduous) <- "deciduous" # broadleaf in CA_forest, but keeping for consistency with landsat
    deciduous01 <- classify(deciduous,cbind(from = c(TRUE, FALSE), to = c(1, 0)))
    deciduousagg <- resample(deciduous01, trast, method ='average')
    writeRaster(deciduousagg, file.path(where2save, paste0('prop_deciduous_', yrs[[rr]], '.tif')))
    print("deciduous")
    
    gc()
    mixed <- land == 230
    names(mixed) <- "mixed"
    mixed01 <- classify(mixed,cbind(from = c(TRUE, FALSE), to = c(1, 0)))
    mixedagg <- resample(mixed01, trast, method ='average')
    writeRaster(mixedagg, file.path(where2save, paste0('prop_mixed_', yrs[[rr]], '.tif')))
    print("mixed")
    
    print(paste0('finished ', yrs[[rr]]))
  
  
  }
  }
