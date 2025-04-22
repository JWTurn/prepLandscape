#' @title make proportion layers from landsat
#' @export
#' @author Julie W. Turner
#' 
make_landsat_prop <- function(layer, studyArea, crs, buff, year){
  land.full <- rast(layer)
  if(st_crs(crs(land.full)) != st_crs(crs)){
    land.proj <- project(land.full, crs, method = 'near')
    land.full <- land.proj
    }
  sArea <- vect(studyArea)
  land <- crop(land.full, sArea)
  
  # What to buffer for proportion of landclasses
  buff.diam <- buff  ## median step length = 852, I chose something a bit less
  
  needleleaf <- land %in% c(1,2)
  names(needleleaf) <- "needleleaf"
  deciduous <- land == 5
  names(deciduous) <- "deciduous"
  mixed <- land == 6
  names(mixed) <- "mixed"
  shrub <- land == 8
  names(shrub) <- "shrub"
  grassland <- land == 10
  names(grassland) <- "grassland"
  lichenshrub <- land == 11
  names(lichenshrub) <- "lichenshrub"
  lichengrass <- land == 12
  names(lichengrass) <- "lichengrass"
  wet <- land == 14
  names(wet) <- "wetland"
  cropland <- land == 15
  names(cropland) <- "cropland"
  barrenland <- land == 16
  names(barrenland) <- "barrenland"
  urban <- land == 17
  names(urban) <- "urban"
  water <- land == 18
  names(water) <- "water"
  snow <- land == 19
  names(snow) <- "snow"
  
  ## This creates an object which can be used to make a layer of specified diameter
  # The d value is what determines the buffer size if you want to change it.
  ## If you're doing multiple landcover classes, you only need to run this line once, as long as each of the habitat variables has the same resolution
  Buff <- focalMat(land, d=buff.diam, type = 'circle')
  ## This generates a new raster where each cell corresponds to the mean wetland within the buffer.
  # Since it's all 1s and 0s, this is the same as the proportion of wetland surrounding the focal variable
  propneedle <- focal(needleleaf, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)

  propdecid <- focal(deciduous, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)

  propmixed <- focal(mixed, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)

  propshrub <- focal(shrub, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)

  propgrass <- focal(grassland, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
  
  proplichshrub <- focal(lichenshrub, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)

  proplichgrass <- focal(lichengrass, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)

  propwet <- focal(wet, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)

  propcrop <- focal(cropland, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)

  propbarren <- focal(barrenland, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)

  propurban <- focal(urban, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
  
  propwater <- focal(water, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
  
  propsnow <- focal(snow, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)

  # paths
  needleleaf <- file.path('data', 'raw-data','prop_land', year, 
                          paste0('prop_needleleaf', '.tif'))
  deciduous <- file.path('data', 'raw-data', 'prop_land', year,
                         paste0('prop_deciduous', '.tif'))
  mixed <- file.path('data', 'raw-data', 'prop_land', year,
                     paste0('prop_mixed', '.tif'))
  shrub <- file.path('data', 'raw-data', 'prop_land', year,
                     paste0('prop_shrub', '.tif'))
  grass <- file.path('data', 'raw-data', 'prop_land', year,
                     paste0('prop_grassland', '.tif'))
  lichshrub <- file.path('data', 'raw-data', 'prop_land', year,
                         paste0('prop_lichenshrub', '.tif'))
  lichgrass <- file.path('data', 'raw-data', 'prop_land', year,
                         paste0('prop_lichengrass', '.tif'))
  wetland <- file.path('data', 'raw-data', 'prop_land', year,
                       paste0('prop_wetland', '.tif'))
  crop <- file.path('data', 'raw-data', 'prop_land', year,
                    paste0('prop_cropland', '.tif'))
  barren <- file.path('data', 'raw-data', 'prop_land', year,
                      paste0('prop_barrenland', '.tif'))
  urban <- file.path('data', 'raw-data', 'prop_land', year,
                     paste0('prop_urban', '.tif'))
  water <- file.path('data', 'raw-data', 'prop_land', year,
                     paste0('prop_water', '.tif'))
  snow <- file.path('data', 'raw-data', 'prop_land', year,
                    paste0('prop_snow', '.tif'))
  
  # write raster
  writeRaster(propneedle, needleleaf)
  writeRaster(propdecid, deciduous)
  writeRaster(propmixed, mixed)
  writeRaster(propshrub, shrub)
  writeRaster(propgrass, grass)
  writeRaster(proplichshrub, lichshrub)
  writeRaster(proplichgrass, lichgrass)
  writeRaster(propwet, wetland)
  writeRaster(propcrop, crop)
  writeRaster(propbarren, barren)
  writeRaster(propurban, urban)
  writeRaster(propwater, water)
  writeRaster(propsnow, snow)
  
  
}


