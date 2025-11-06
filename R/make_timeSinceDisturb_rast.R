#' @title make rasters of most recent disturbance by year
#' @export
#' @author Julie W. Turner
#'
#'
make_timeSinceDisturb_rast <- function(layer, rast, disturbanceType, minyr = NULL, maxyr = NULL, backgrndYear = NULL, backgrndTS = NULL, where2save = NULL){

  lyr <- layer
  raster <- rast
  yearsWithData <- unique(lyr[["YEAR"]])$YEAR

  if(!is.null(minyr)){
    if(!is.null(maxyr)){
      yearsWithData <- minyr:maxyr
    }else{
      yearsWithData <- minyr:max(yearsWithData, na.rm = T)
    }

  }

  timeSinceYearly <- Map(yrName = paste0('year', yearsWithData),yr = yearsWithData, function(yrName, yr) {
    subst <- subset(lyr, lyr$YEAR<= yr)
    message(paste0('start ', yrName))
    #fires <-
    suppressWarnings({

      historicalRasters <- terra::rasterize(subst, raster, field="YEAR", fun=max, background = backgrndYear)
      if(is.null(backgrndTS)){
        timeSince <- yr - terra::mask(historicalRasters, raster)
      } else{
        timeSince1 <- yr - historicalRasters
        timeSince <- terra::mask(terra::ifel(is.na(timeSince1), backgrndTS, timeSince1), raster)
      }


      names(timeSince) <- paste0('timeSince', disturbanceType)
    })

    if(!is.null(where2save)){
      fnames <- paste0("ts_", disturbanceType, "_", yr, ".tif")
      terra::writeRaster(timeSince, file.path(where2save, fnames), overwrite = T)
      print(paste0('saved ', disturbanceType, ' ', yr))
    }

    return(timeSince)
  })
  message('finish yearly ', disturbanceType, ' rasterization')
  return(timeSinceYearly)
}
