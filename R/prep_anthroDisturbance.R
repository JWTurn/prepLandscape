#' @title prepare anthropogenic disturbances not including harvest
#' @export
#' @author Julie W. Turner


prep_anthroDisturbance <- function(inputsPath, studyArea, dataPath, source = 'ECCC', studyAreaName = NULL) {
  ### ECCC disturbance ----
  if(source == 'ECCC'){
    eccc_lines_2010 <- reproducible::prepInputs(url = 'https://drive.google.com/file/d/1kgIUXuseEfiyv8tEWnAlWdRZkeIhQrwa/view?usp=share_link',
                                                destinationPath = inputsPath,
                                                targetFile = "EC_borealdisturbance_linear_2008_2010_FINAL_ALBERS.shp",  
                                                alsoExtract = "similar", fun = "terra::vect",
                                                to = studyArea
    )
    
    eccc_polys_2010 <- reproducible::prepInputs(url = 'https://drive.google.com/file/d/1kgIUXuseEfiyv8tEWnAlWdRZkeIhQrwa/view?usp=share_link',
                                                destinationPath = inputsPath,
                                                targetFile = "EC_borealdisturbance_polygonal_2008_2010_FINAL_ALBERS.shp",  
                                                alsoExtract = "similar", fun = "terra::vect",
                                                to = studyArea
    )
    
    eccc_lines_2015 <- reproducible::prepInputs(url = 'https://drive.google.com/file/d/1wFD7KWyd2GNYrcrkoh4xib5G5e35Odso/view?usp=share_link',
                                                destinationPath = inputsPath,
                                                targetFile = "eccc_disturb_lines_2015.shp",  
                                                alsoExtract = "similar", fun = "terra::vect",
                                                to = studyArea
    )
    
    eccc_polys_2015 <- reproducible::prepInputs(url = 'https://drive.google.com/file/d/1N-9H6RvFz94hfiMvE69qWHmube10MWmI/view?usp=share_link',
                                                destinationPath = inputsPath,
                                                targetFile = "eccc_disturb_polys_2015.shp",  
                                                alsoExtract = "similar", fun = "terra::vect",
                                                to = studyArea
    )
    
    eccc_lines_2020 <- reproducible::prepInputs(url = 'https://drive.google.com/file/d/1fUAbOPd4MWA5hlmiiJadUAVuCCkVwr8P/view?usp=share_link',
                                                destinationPath = inputsPath,
                                                targetFile = "eccc_disturb_lines_2020.shp",  
                                                alsoExtract = "similar", fun = "terra::vect",
                                                to = studyArea
    )
    
    eccc_polys_2020 <- reproducible::prepInputs(url = 'https://drive.google.com/file/d/1sICz5D6nvwyHCA-l95ZrmYjfCXxInPJz/view?usp=share_link',
                                                destinationPath = inputsPath,
                                                targetFile = "eccc_disturb_polys_2020.shp",  
                                                alsoExtract = "similar", fun = "terra::vect",
                                                to = studyArea
    )
    
    ## roads lfs
    roads_2010 <- subset(eccc_lines_2010, (eccc_lines_2010$Class %in% c('Road', 'Railway')))
    
    #summary(as.factor(eccc_lines_2015$Class))
    roads_2015 <- subset(eccc_lines_2015, (eccc_lines_2015$Class %in% c('Road', 'Railway')))
    
    #summary(as.factor(eccc_lines_2020$Class))
    roads_2020 <- subset(eccc_lines_2020, (eccc_lines_2020$Class %in% c('Road', 'Railway')))
    
    
    roads.mask.2015 <- terra::mask(roads_2010, roads_2015, inverse = T)
    roads_2015_merge <- rbind(roads_2015, roads.mask.2015)
    
    
    roads.mask.2020 <- terra::mask(roads_2010, roads_2020, inverse = T)
    roads_2020_merge <- rbind(roads_2020, roads.mask.2020)
    
    ## not roads lfs
    #summary(as.factor(eccc_lines_2010$Class))
    notroads_2010 <- subset(eccc_lines_2010, !(eccc_lines_2010$Class %in% c('Road', 'Railway')))
    
    #summary(as.factor(eccc_lines_2015$Class))
    notroads_2015 <- subset(eccc_lines_2015, !(eccc_lines_2015$Class %in% c('Road', 'Railway')))
    
    #summary(as.factor(eccc_lines_2020$Class))
    notroads_2020 <- subset(eccc_lines_2020, !(eccc_lines_2020$Class %in% c('Road', 'Railway')))
    
    
    notroads.mask.2015 <- terra::mask(notroads_2010, notroads_2015, inverse = T)
    notroads_2015_merge <- rbind(notroads_2015, notroads.mask.2015)
    
    
    notroads.mask.2020 <- terra::mask(notroads_2010, notroads_2020, inverse = T)
    notroads_2020_merge <- rbind(notroads_2020, notroads.mask.2020)
    
    # polygonal disturbance
    #summary(as.factor(eccc_polys_2010$Class))
    disturb_2010 <- subset(eccc_polys_2010, !(eccc_polys_2010$Class %in% c('Cutblock')))
    
    #summary(as.factor(eccc_polys_2015$Class))
    disturb_2015 <- subset(eccc_polys_2015, !(eccc_polys_2015$Class %in% c('Cutblock')))
    
    #summary(as.factor(eccc_polys_2020$Class))
    disturb_2020 <- subset(eccc_polys_2020, !(eccc_polys_2020$Class %in% c('Cutblock', 'Harvest')))
    
    
    
    dist.mask.2015 <- terra::mask(disturb_2010, disturb_2015, inverse = T)
    disturb_2015_merge <- terra::union(disturb_2015, dist.mask.2015)
    
    dist.mask.2020 <- terra::mask(disturb_2010, disturb_2020, inverse = T)
    disturb_2020_merge <- terra::union(disturb_2020, dist.mask.2020)
    
    # gather yearly disturbances
    disturbances2010 <- c(roads_2010, notroads_2010, disturb_2010)
    names(disturbances2010) <- c('paved', 'unpaved', 'polys')
    terra::writeVector(disturbances2010, file.path(dataPath, paste0(studyAreaName, '_2010.shp')))
    
    disturbances2015 <- c(roads_2015_merge, notroads_2015_merge, disturb_2015_merge)
    names(disturbances2015) <- c('paved', 'unpaved', 'polys')
    terra::writeVector(disturbances2015, file.path(dataPath, paste0(studyAreaName, '_2015.shp')))
    
    disturbances2020 <- c(roads_2020_merge, notroads_2020_merge, disturb_2020_merge)
    names(disturbances2020) <- c('paved', 'unpaved', 'polys')
    terra::writeVector(disturbances2020, file.path(dataPath, paste0(studyAreaName, '_2020.shp')))
    
    disturbances <- list(intYear2010 = disturbances2010, intYear2015 = disturbances2015, intYear2020 = disturbances2020)
    
    return(disturbances)
  }
  
}