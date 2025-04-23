#' @title make moving window proportion layers from yearly CA_forest
#' @export
#' @author Julie W. Turner
#' 
make_landforest_prop_spades <- function(targetFile, buff){
  land <- terra::rast(targetFile)
  
 
   # What to buffer for proportion of landclasses
  

    ## This creates an object which can be used to make a layer of specified diameter
    # The d value is what determines the buffer size if you want to change it.
    ## If you're doing multiple landcover classes, you only need to run this line once, as long as each of the habitat variables has the same resolution
    Buff <- focalMat(land, d=buff, type = 'circle')
    ## This generates a new raster where each cell corresponds to the mean wetland within the buffer.
    # Since it's all 1s and 0s, this is the same as the proportion of wetland surrounding the focal variable
    
    
    land.seg <- terra::segregate(land) #creates 1 file with diff layers, then focal on whole thing

    propLand <- focal(land.seg, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
    names(propLand) <- c(prop_water, prop_snow, prop_rock, prop_barrenland, prop_bryoids, 
                         prop_shrub, prop_wetland, prop_wet_treed, prop_herbs, 
                         prop_needleleaf, prop_deciduous, prop_mixed)
    
    return(propLand)
    

}