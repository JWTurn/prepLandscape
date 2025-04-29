#' @title make grid proportion layers from yearly CA_forest
#' @export
#' @author Julie W. Turner


aggregate_landforest <- function(targetFile, trast, where2save = NULL){
  
  
  landRaw <- terra::rast(targetFile)
  landCrop <- postProcess(landRaw, cropTo=terra::buffer(trast, width = 10000)) #|>
  # Cache()
  
  # TODO make an argument/table so can use with diff datasets
  lccClasses <- c(20, 31, 32, 33, 40, 50, 80, 81, 100, 210, 220, 230)
  
  land.seg <- terra::segregate(landCrop, classes = lccClasses) #creates 1 file with diff layers, then focal on whole thing
  
  land.agg <- postProcess(land.seg, to = trast, method = 'average')# |>
  #Cache()
  
  
  names(land.agg) <- c('prop_water', 'prop_snow', 'prop_rock', 'prop_barrenland', 'prop_bryoids', 
                       'prop_shrub', 'prop_wetland', 'prop_wet_treed', 'prop_herbs', 
                       'prop_needleleaf', 'prop_deciduous', 'prop_mixed')
  if(!is.null(where2save)){
    writeRaster(land.agg, where2save)
  }
  return(land.agg)
  
  
}
