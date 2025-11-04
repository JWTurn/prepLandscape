#' @title make cumulative CanLaD disturbance per year per disturbance type
#' @export
#' @author Julie W. Turner
make_CanLad_cumulative <- function(yrs, disturbTypeCode, dPath, rtm){
  # make a list of the new harvests, make a spatRasterCollection, and mosaic
  mosaic(sprc(
    Map(yr = yrs, disturbType = disturbTypeCode, function(yr, disturbType){
      disturbYr <- reproducible::prepInputs(url = paste0('https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/canlad_including_insect_defoliation/v1/Disturbances_Time_Series/canlad_annual_', 
                                                         yr, '_v1.tif'),
                                            destinationPath = dPath,
                                            fun = 'terra::rast',
                                            to = rtm,
                                            method = 'near')
      indivDisturbYr <- (terra::match(disturbYr, disturbType))*yr
      names(indivDisturbYr) <- as.character(yr)
      return(indivDisturbYr)
      message(paste0('completed ', yr))
      
    })))
}
