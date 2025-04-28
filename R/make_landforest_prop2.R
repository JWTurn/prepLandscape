#' @title make proportion layers from yearly CA_forest
#' @export
#' @author Julie W. Turner
#' 
make_landforest_prop2 <- function(studyArea, crs, buff, startyr, endyr){
  yrs <- startyr:endyr
  # TODO this is inelegant -> fix
  ls_loc <- c(file.path(canada, 'Landcover_1984-2019', paste0('CA_forest_VLCE2_', yrs), 
                     paste0('CA_forest_VLCE2_', yrs, '.tif')))
  names(ls_loc) <- as.character(yrs)

 
   sArea <- vect(studyArea)
   # What to buffer for proportion of landclasses
   buff.diam <- buff  ## median step length rounded down to nearest 50
  
  foreach(rr = 1:length(ls_loc)) %do% {
    land.full <- rast(ls_loc[[rr]])
    if(st_crs(land.full) != st_crs(crs)){
      land.proj <- project(land.full, crs, method = 'near')
      land.full <- land.proj
    }
    
    land <- crop(land.full, sArea)
    
    water <- land == 20
    names(water) <- "water"
    snow <- land == 31
    names(snow) <- "snow"
    rock <- land == 32
    names(rock) <- "rock"
    barrenland <- land == 33
    names(barrenland) <- "barrenland" # exposed_barren_land in CA_forest, but keeping for consistency with landsat
    bryoids <- land == 40
    names(bryoids) <- "bryoids"
    shrub <- land == 50
    names(shrub) <- "shrub"
    wet <- land == 80
    names(wet) <- "wetland"
    wet_treed <- land == 81
    names(wet_treed) <- "wet_treed"
    herbs <- land == 100
    names(herbs) <- "herbs"
    needleleaf <- land == 210 
    names(needleleaf) <- "needleleaf" # coniferous in CA_forest, but keeping for consistency with landsat
    deciduous <- land == 220
    names(deciduous) <- "deciduous" # broadleaf in CA_forest, but keeping for consistency with landsat
    mixed <- land == 230
    names(mixed) <- "mixed"
  
    ## This creates an object which can be used to make a layer of specified diameter
    # The d value is what determines the buffer size if you want to change it.
    ## If you're doing multiple landcover classes, you only need to run this line once, as long as each of the habitat variables has the same resolution
    Buff <- focalMat(land, d=buff.diam, type = 'circle')
    ## This generates a new raster where each cell corresponds to the mean wetland within the buffer.
    # Since it's all 1s and 0s, this is the same as the proportion of wetland surrounding the focal variable
    
    propwater <- focal(water, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
    
    propsnow <- focal(snow, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
    
    proprock <- focal(rock, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
    
    propbarren <- focal(barrenland, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
    
    propbryoids <- focal(bryoids, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
    
    propshrub <- focal(shrub, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
    
    propwet <- focal(wet, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
    
    propwettreed <- focal(wet_treed, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
    
    propherbs <- focal(herbs, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
    
    propneedle <- focal(needleleaf, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
    
    propdecid <- focal(deciduous, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
    
    propmixed <- focal(mixed, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
    
    
   
    
    # # paths
    # p.water <- file.path('data', 'raw-data', 'prop_land',
    #                    paste0('prop_water_', yrs[[rr]], '.tif'))
    # p.snow <- file.path('data', 'raw-data', 'prop_land',
    #                   paste0('prop_snow_', yrs[[rr]], '.tif'))
    # p.rock <- file.path('data', 'raw-data', 'prop_land',
    #                     paste0('prop_rock_', yrs[[rr]], '.tif'))
    # p.barren <- file.path('data', 'raw-data', 'prop_land',
    #                     paste0('prop_barrenland_', yrs[[rr]], '.tif'))
    # p.bryoids <- file.path('data', 'raw-data', 'prop_land', 
    #                      paste0('prop_bryoids_', yrs[[rr]], '.tif'))
    # p.shrub <- file.path('data', 'raw-data', 'prop_land',
    #                    paste0('prop_shrub_', yrs[[rr]], '.tif'))
    # p.wetland <- file.path('data', 'raw-data', 'prop_land',
    #                      paste0('prop_wetland_', yrs[[rr]], '.tif'))
    # p.wettreed <- file.path('data', 'raw-data', 'prop_land',
    #                         paste0('prop_wet_treed_', yrs[[rr]], '.tif'))
    # p.herbs <- file.path('data', 'raw-data', 'prop_land',
    #                    paste0('prop_herbs_', yrs[[rr]], '.tif'))
    # p.needleleaf <- file.path('data', 'raw-data','prop_land', 
    #                         paste0('prop_needleleaf_', yrs[[rr]], '.tif'))
    # p.deciduous <- file.path('data', 'raw-data', 'prop_land',
    #                        paste0('prop_deciduous_', yrs[[rr]], '.tif'))
    # p.mixed <- file.path('data', 'raw-data', 'prop_land',
    #                    paste0('prop_mixed_', yrs[[rr]], '.tif'))
    # 
    # 
    # # write raster
    # 
    # writeRaster(propwater, p.water)
    # writeRaster(propsnow, p.snow)
    # writeRaster(proprock, p.rock)
    # writeRaster(propbarren, p.barren)
    # writeRaster(propbryoids, p.bryoids)
    # writeRaster(propshrub, p.shrub)
    # writeRaster(propwet, p.wetland)
    # writeRaster(propwettreed, p.wettreed)
    # writeRaster(propherbs, p.herbs)
    # writeRaster(propneedle, p.needleleaf)
    # writeRaster(propdecid, p.deciduous)
    # writeRaster(propmixed, p.mixed)
  }
    

}