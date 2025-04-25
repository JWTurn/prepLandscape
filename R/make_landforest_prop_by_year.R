#' @title make moving window proportion layers from yearly CA_forest
#' @export
#' @author Julie W. Turner
#' 
make_landforest_prop_by_year <- function(landYearsStack, studyArea, crs, buff, startyr, endyr, where2save){
  if(is.null(startyr)&is.null(endyr)){
    yrs <- names(landYearsStack)
  } else {
    yrs <- startyr:endyr
  }
  
  # TODO this is inelegant -> fix
  # ls_loc <- c(file.path(canada, 'Landcover_1984-2022', paste0('CA_forest_VLCE2_', yrs), 
  #                    paste0('CA_forest_VLCE2_', yrs, '.tif')))
  # names(ls_loc) <- as.character(yrs)
  

 
   sArea <- vect(studyArea)
   # What to buffer for proportion of landclasses
   buff.diam <- buff  ## median step length rounded down to nearest 50
  
  foreach(rr = 1:length(yrs)) %do% {
    land.full <- landYearsStack[[rr]]
    if(st_crs(land.full) != st_crs(crs)){
      land.proj <- project(land.full, crs, method = 'near')
      land.full <- land.proj
    }
    
    land <- land.full#crop(land.full, sArea)

    ## This creates an object which can be used to make a layer of specified diameter
    # The d value is what determines the buffer size if you want to change it.
    ## If you're doing multiple landcover classes, you only need to run this line once, as long as each of the habitat variables has the same resolution
    Buff <- focalMat(land, d=buff.diam, type = 'circle')
    ## This generates a new raster where each cell corresponds to the mean wetland within the buffer.
    # Since it's all 1s and 0s, this is the same as the proportion of wetland surrounding the focal variable
    
    
    # pull just water and name it
    water <- land == 20
    names(water) <- "water"
    # proportion within buffer
    propwater <- focal(water, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
    # path to save file
    p.water <- file.path(where2save, buff,
                         paste0('prop_water_', yrs[[rr]], '.tif'))
    # save file
    writeRaster(propwater, p.water)
    print("water")
    
    gc()
    snow <- land == 31
    names(snow) <- "snow"
    propsnow <- focal(snow, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
    p.snow <- file.path(where2save, buff,
                        paste0('prop_snow_', yrs[[rr]], '.tif'))
    writeRaster(propsnow, p.snow)
    print("snow")
    
    gc()
    rock <- land == 32
    names(rock) <- "rock"
    proprock <- focal(rock, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
    p.rock <- file.path(where2save, buff,
                        paste0('prop_rock_', yrs[[rr]], '.tif'))
    writeRaster(proprock, p.rock)
    print('rock')
    
    gc()
    barrenland <- land == 33
    names(barrenland) <- "barrenland" # exposed_barren_land in CA_forest, but keeping for consistency with landsat
    propbarren <- focal(barrenland, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
    p.barren <- file.path(where2save, buff,
                          paste0('prop_barrenland_', yrs[[rr]], '.tif'))
    writeRaster(propbarren, p.barren)
    print('barren')
    
    gc()
    bryoids <- land == 40
    names(bryoids) <- "bryoids"
    propbryoids <- focal(bryoids, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
    p.bryoids <- file.path(where2save, buff,
                           paste0('prop_bryoids_', yrs[[rr]], '.tif'))
    writeRaster(propbryoids, p.bryoids)
    print('bryoids')
    
    gc()
    shrub <- land == 50
    names(shrub) <- "shrub"
    propshrub <- focal(shrub, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
    p.shrub <- file.path(where2save, buff,
                         paste0('prop_shrub_', yrs[[rr]], '.tif'))
    writeRaster(propshrub, p.shrub)
    print('shrub')
    
    gc()
    wet <- land == 80
    names(wet) <- "wetland"
    propwet <- focal(wet, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
    p.wetland <- file.path(where2save, buff,
                           paste0('prop_wetland_', yrs[[rr]], '.tif'))
    writeRaster(propwet, p.wetland)
    print('wetland')
    
    gc()
    wet_treed <- land == 81
    names(wet_treed) <- "wet_treed"
    propwettreed <- focal(wet_treed, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
    p.wettreed <- file.path(where2save, buff,
                            paste0('prop_wet_treed_', yrs[[rr]], '.tif'))
    writeRaster(propwettreed, p.wettreed)
    print('wet-treed')
    
    gc()
    herbs <- land == 100
    names(herbs) <- "herbs"
    propherbs <- focal(herbs, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
    p.herbs <- file.path(where2save, buff,
                         paste0('prop_herbs_', yrs[[rr]], '.tif'))
    writeRaster(propherbs, p.herbs)
    print('herbs')
    
    gc()
    needleleaf <- land == 210 
    names(needleleaf) <- "needleleaf" # coniferous in CA_forest, but keeping for consistency with landsat
    propneedle <- focal(needleleaf, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
    p.needleleaf <- file.path(where2save, buff,
                              paste0('prop_needleleaf_', yrs[[rr]], '.tif'))
    writeRaster(propneedle, p.needleleaf)
    print('needleleaf')
    
    gc()
    deciduous <- land == 220
    names(deciduous) <- "deciduous" # broadleaf in CA_forest, but keeping for consistency with landsat
    propdecid <- focal(deciduous, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
    p.deciduous <- file.path(where2save, buff,
                             paste0('prop_deciduous_', yrs[[rr]], '.tif'))
    writeRaster(propdecid, p.deciduous)
    print('deciduous')
    
    gc()
    mixed <- land == 230
    names(mixed) <- "mixed"
    propmixed <- focal(mixed, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
    p.mixed <- file.path(where2save, buff,
                         paste0('prop_mixed_', yrs[[rr]], '.tif'))
    writeRaster(propmixed, p.mixed)
    print('mixed')
    print(paste0('finished ', yrs[[rr]]))
    
  }
    

}