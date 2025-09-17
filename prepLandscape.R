## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "prepLandscape",
  description = "This module prepares landscape layers for habitat selection analyses",
  keywords = "",
  authors = structure(list(list(given = c("Julie", "W"), family = "Turner", role = c("aut", "cre"), email = "email@example.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(prepLandscape = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "prepLandscape.Rmd"),
  reqdPkgs = list("SpaDES.core (>= 2.1.5.9002)", "ggplot2", "terra", "sf", 'stringr'),
  parameters = bindrows(
    defineParameter("histLandYears", "integer", 2010:2023, NA, NA,
                    paste0("This is the year range we use past (not simulated) landscape layers.")),
    defineParameter("backgroundYr", "integer", 1900, NA, NA,
                    paste0("This is the year we fill fire and harvest rasters for time since calculations.")),
    
    
    
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used - e.g., a hash of the study",
                          "area obtained using `reproducible::studyAreaName()`"),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?")
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    #expectsInput(objectName = NA, objectClass = NA, desc = NA, sourceURL = NA)
    expectsInput(objectName = 'studyArea', objectClass = 'SpatVector', 
                 desc = 'Study area of telemetry data + herd areas', 
                 sourceURL = 'https://drive.google.com/file/d/1XduunieEoZLcNPQphGXnKG7Ql9MF1bme/view?usp=share_link'),
    
    expectsInput(objectName = 'studyArea_extendedLandscape', objectClass = 'SpatVector', 
                 desc = 'Extended study area for prepping landscape layers'),
    
    expectsInput(objectName = 'harvNTEMSurl', objectClass = 'SpatRaster', 
                 desc = 'harvest history', 
                 sourceURL = "https://opendata.nfis.org/downloads/forest_change/CA_Forest_Harvest_1985-2020.zip"),
    
    expectsInput(objectName = 'CanLadOldTypeURL', objectClass = 'SpatRaster', 
                 desc = 'CanLad disturbance type data before 1985', 
                 sourceURL = 'https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/canada_disturbances_1965to1984/v1/canlad_1965_1984_disturbanceType.tif'),
    
    expectsInput(objectName = 'CanLadOldYearURL', objectClass = 'SpatRaster', 
                 desc = 'CanLad disturbance year data before 1985', 
                 sourceURL = 'https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/canada_disturbances_1965to1984/v1/canlad_1965_1984_disturbanceYear.tif'),
    
    expectsInput(objectName = 'nfdbURL', objectClass = 'SpatVector', 
                 desc = 'National Fire Data Base (NFDB) for back filling NBAC', 
                 sourceURL = 'https://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_poly/current_version/NFDB_poly.zip'),
    
    expectsInput(objectName = 'nbacURL', objectClass = 'SpatVector', 
                 desc = 'National Burn Area Composite', 
                 sourceURL = 'https://cwfis.cfs.nrcan.gc.ca/downloads/nbac/NBAC_1972to2024_20250506_shp.zip'),
    
    expectsInput(objectName = 'anthroDisturbance', objectClass = 'SpatVector', 
                 desc = paste0('SpatVector collection of anthropogenic disturbances excluding harvest',
                               ' This includes paved/unpaved linear features and other polygonal disturbances.')),
    
    expectsInput("rasterToMatch_extendedLandscapeFine", "SpatRaster",
                 desc = paste("A raster to match of the study area plus larger buffer at finest resolution.")),
    expectsInput("rasterToMatch_extendedLandscape", "SpatRaster",
                 desc = paste("A raster to match of the study area plus larger buffer at target forecasting resolution.")),
    expectsInput("rasterToMatch_extendedLandscapeCoarse", "SpatRaster",
                 desc = paste("A coarser raster to match of the study area plus large bufferto caluculate proportions of landcover.")),
    expectsInput("rtmFuns", "list",
                 desc = paste0("List functions to apply to rasters for moving windows or not.",
                              " Currently only applies to landcover")),
    expectsInput("rtms", "list",
                 desc = paste0("List of template rasters. Only 1 if working at one resolution."))
    
  ),
  outputObjects = bindrows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    #createsOutput(objectName = NA, objectClass = NA, desc = NA)
    createsOutput(objectName = 'histHarv', objectClass = 'spatRaster', 
                  desc = 'spatRaster of historic harvest within extended study area')
  )
))

doEvent.prepLandscape = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)
      cacheTags <- c(currentModule(sim), "function:.inputObjects") 
      dPath <- asPath(getOption("reproducible.destinationPath", inputPath(sim)), 1)
      
      # load disturbances
      ##### FIRES ----
      mod$histFire <- make_timeSinceDisturb_rast(layer = sim$fires, rast = sim$rasterToMatch_extendedLandscapeFine, 
                                                 disturbanceType = 'Fire', minyr = min(P(sim)$histLandYears), 
                                                 backgrnd = backgroundYr, where2save = NULL)
      
      ##### HARVEST -----
      sim$harvNTEMS[sim$harvNTEMS==0]<-NA
      
      # get before 1985 harvest from CanLaD -- get year only for harv
      disturbCanLadOldHarvYear <- mask(sim$disturbCanLadOldYear, match(sim$disturbCanLadOldType, 3))
      
      # add recent harvest after NTEMS
      newYears <- (max(values(sim$harvNTEMs), na.rm = T)+1) : max(P(sim)$histLandYears)
      
      newHarvRast <- make_CanLad_cumulative(yrs = newYears, disturbTypeCode = 2, 
                                            dPath = dPath, rtm = sim$rasterToMatch_extendedLandscapeFine) |>
        Cache()
      
      # combine all types together
      sim$harv <- c(disturbCanLadOldHarvYear, sim$harvNTEMS, newHarvRast)
      names(sim$harv) <- c('CanLadOld', 'NTEMS', 'CanLadNew')
      
      timeSinceHarvest <- Map(nn = paste0('year', histLandYears), yr = histLandYears, 
                      rtm = rasterToMatch_extendedLandscapeFine, background = 100, 
                      function(nn, yr, rtm, background){
                        
                        
                        harvsYrPos <- clamp(harvs, upper = yr, value = F)
                        maxRast <- max(harvsYrPos, na.rm = T)
                        timeSince <- yr - maxRast
                        timeSinceFill <- mask(ifel(is.na(timeSince), background, timeSince), rtm)
                        names(timeSinceFill) <- 'timeSinceHarvest'
                        return(timeSinceFill)
                      })
      
      
      # TODO set these in setupProj
      
      
      #rtmsDigest <- .robustDigest(rtms)
     # names(P(sim)$historicLandYears) <- P(sim)$historicLandYears
      mod$histLand <- Map(rtmname = names(rtms), rtm = rtms, 
                              rtmDigest = rtmsDigest, rtmFun = rtmsFuns, function(rtm, rtmname, rtmFun) {
        
        Map(nn = paste0('year', P(sim)$histLandYears), ii=P(sim)$histLandYears, function(nn,ii){
          reproducible::prepInputs(
            url = paste0("https://opendata.nfis.org/downloads/forest_change/CA_forest_VLCE2_", ii, ".zip"),
            destinationPath = dPath, # end pre process
            fun = rtmFun, # end process
            rtm = rtm, 
            writeTo = file.path(dataPath(sim), paste0('propLand_', rtmname, '_', ii, '.tif'))) |> ## TODO set name
            Cache()
        })
        
      })|>
        Cache()
      
      
    
      # schedule future event(s)
      # sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "prepLandscape", "plot")
      # sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "prepLandscape", "save")
    },
    
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

### template initialization
Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}
### template for save events
Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
plotFun <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sampleData <- data.frame("TheSample" = sample(1:10, replace = TRUE))
  Plots(sampleData, fn = ggplotFn) # needs ggplot2

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}



.inputObjects <- function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create a named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can check if an object is 'suppliedElsewhere' to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call, or another module will supply or has supplied it. e.g.,
  # if (!suppliedElsewhere('defaultColor', sim)) {
  #   sim$map <- Cache(prepInputs, extractURL('map')) # download, extract, load file from url in sourceURL
  # }

  cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", inputPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #
  # TODO give a smaller area
  if (!suppliedElsewhere("studyArea", sim)){
    sim$studyArea <- prepInputs(url = extractURL("studyArea"),
                                destinationPath = dataPath(sim),
                                targetFile = "studyArea_bcnwt_4sims.shp",  
                                alsoExtract = "similar", fun = "terra::vect") |>
      Cache()
  }
  
  if (!suppliedElsewhere("studyArea_extendedLandscape", sim)){
    sim$studyArea_extendedLandscape <- terra::buffer(sim$studyArea, 100000)
  }
  
  if (!suppliedElsewhere("rasterToMatch_extendedLandscapeFine", sim)){
    sim$rasterToMatch_extendedLandscapeFine <- terra::rast(sim$studyArea_extendedLandscap, res = c(30, 30), vals = 1)
  }
  
  if (!suppliedElsewhere("rasterToMatch_extendedLandscape", sim)){
    sim$rasterToMatch_extendedLandscape <- terra::aggregate(sim$rasterToMatch, fact = 6)
  }
  
  if (!suppliedElsewhere("rasterToMatch_extendedLandscapeCoarse", sim)){
    sim$rasterToMatch_extendedLandscapeCoarse <- terra::aggregate(sim$rasterToMatch, fact = 16)
  }
  
  if (!suppliedElsewhere("rtms", sim)){
    sim$rtms <- list(sim$rasterToMatch_extendedLandscape)
    names(sim$rtms) <- c('window240')
  }
  
  if (!suppliedElsewhere("rtmFuns", sim)){
    sim$rtmFuns <- c(paste0('make_landforest_prop(targetFile = targetFile, trast = rtm, buff = ',
                             P(sim)$buffer,', where2save = dataPath(sim))'))
  }
  
  
  
    sim$harvNTEMS <- reproducible::prepInputs(url = extractURL("harvNTEMSurl"),
                                              destinationPath = dPath,
                                              to = sim$rasterToMatch_extendedLandscapeFine, 
                                              fun = 'terra::rast') |>
      Cache()
  
    sim$disturbCanLadOldType <- prepInputs(url = extractURL('CanLadOldTypeURL'),
                                       destinationPath = dPath,
                                       alsoExtract = "similar", fun = "terra::rast",
                                       to = sim$rasterToMatch_extendedLandscapeFine,
                                       method = 'near') |>
      Cache()
   
    sim$disturbCanLadOldYear <- prepInputs(url = extractURL('CanLadOldYearURL'), 
                                       destinationPath = dPath,
                                       alsoExtract = "similar", fun = "terra::rast",
                                       to = sim$rasterToMatch_extendedLandscapeFine,
                                       method = 'near') |>
      Cache()
    
    
    sim$fires <- combine_fire_DB(nbacURL = 'nbacURL', nfdbURL = 'nfdbURL', dPath, 
                                 sim$studyArea_extendedLandscape, 
                                 savePath = dataPath(sim)) |>
      Cache()
    
    sim$anthroDisturbance <- prep_anthroDisturbance(inputsPath = dPath, studyArea = sim$studyArea_extendedLandscape, 
                                                    dataPath = dataPath(sim), source = 'ECCC', 
                                                    studyAreaName = .studyAreaName) |>
      Cache()

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

ggplotFn <- function(data, ...) {
  ggplot2::ggplot(data, ggplot2::aes(TheSample)) +
    ggplot2::geom_histogram(...)
}

