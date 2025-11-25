#' @title combine NBAC and NFDB fire databases
#' @export
#' @author Julie W. Turner

combine_fire_DB <- function(nbacURL, nfdbURL, dPath, studyArea, studyAreaName = NULL, savePath = NULL){
  
  nbac <- reproducible::prepInputs(url = nbacURL,
                                   destinationPath = dPath,
                                   #targetFile = "NBAC_1972to2024_20250506.shp",  
                                   alsoExtract = "similar", fun = "terra::vect",
                                   to = studyArea) |>
    Cache()
  
  nfdb <- reproducible::prepInputs(url = nfdbURL,
                                   destinationPath = dPath,
                                   #targetFile = "NFDB_poly_20210707.shp",  
                                   alsoExtract = "similar", fun = "terra::vect",
                                   to = studyArea) |>
    Cache()
  
  # filter older to those prior to NBAC
  nfdb.pre <- subset(nfdb, nfdb$YEAR < min(nbac$YEAR, na.rm = T))
  
  #combine fires
  fires.all <- rbind(nfdb.pre, nbac)
  message('complete combine NBAC and NFDB')
  
  # harmonize years across datasets and fill best possible for missing years
  fires.all$year_burn <- fires.all$YEAR
  fires.all$REP_YEAR <- as.integer(gsub("/.*$", "", fires.all$REP_DATE))
  # default to year already in DF, but if that's missing, use year from report date
  fires.all$YEAR <- ifelse(!is.na(fires.all$year_burn), fires.all$year_burn,
                           fires.all$REP_YEAR)
  
  
  
  # Replace invalid year values (-9999 etc)
  fires.all$YEAR[fires.all$YEAR < 1800|is.na(fires.all$YEAR)] <- (as.integer(gsub("\\-.*", "", 
                                                                                  fires.all[fires.all$YEAR < 1800|is.na(fires.all$YEAR)]$DECADE))+5)
  
  message('complete harmonizing data')
  
  if (!is.null(savePath)){
    saveName <- file.path(savePath, paste0(studyAreaName, '_firesDB.shp'))
    terra::writeVector(fires.all, (saveName))
    message(paste0('saved processed fire to ', saveName))
  }
  
  return(fires.all)
  
}
