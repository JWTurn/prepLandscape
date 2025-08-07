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
  reqdPkgs = list("SpaDES.core (>= 2.1.5.9002)", "ggplot2", "terra", "sf"),
  parameters = bindrows(
    defineParameter("historicLandYears", "integer", 2010:2023, NA, NA,
                    paste0("This is the year range we use historic (not simulated) landscape layers.")),
    defineParameter("harvNTEMS", "character", "https://opendata.nfis.org/downloads/forest_change/CA_Forest_Harvest_1985-2020.zip", NA, NA,
                    paste0("This is the year the initial disturbance layers are from", 
                           "and used for time since variables.",
                           "This parameter would need to be updated if use a different year of data.")),
    
    defineParameter("canLadDisturbYear", "character", "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/canada_disturbances_1965to1984/v1/canlad_1965_1984_disturbanceYear.tif", NA, NA,
                    paste0("CanLaD disturbance year layer", 
                           "and used for time since variables.")),
    defineParameter("canLadDisturbType", "character", "https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/canada_disturbances_1965to1984/v1/canlad_1965_1984_disturbanceType.tif", NA, NA,
                    paste0("CanLaD disturbance type layer", 
                           "and used for time since variables.")),
    
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
    
    expectsInput(objectName = 'harvNTEMS', objectClass = 'SpatRaster', 
                 desc = 'harvest history', 
                 sourceURL = "https://opendata.nfis.org/downloads/forest_change/CA_Forest_Harvest_1985-2020.zip"),
    
    expectsInput("rasterToMatch_extendedLandscapeFine", "SpatRaster",
                 desc = paste("A raster to match of the study area plus larger buffer.")),
    expectsInput("rasterToMatch_extendedLandscapeCoarse", "SpatRaster",
                 desc = paste("A coarser raster to match of the study area plus large bufferto caluculate proportions of landcover."))
    
  ),
  outputObjects = bindrows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    #createsOutput(objectName = NA, objectClass = NA, desc = NA)
    createsOutput(objectName = 'historicHarv', objectClass = 'spatRaster', 
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
      ##### HARVEST -----
      sim$harvNTEMS[sim$harvNTEMS==0]<-NA
      
      # get before 1985 harvest from CanLaD -- get year only for harv
      disturbCanLadOldHarvYear <- mask(sim$disturbCanLadOldYear, match(sim$disturbCanLadOldType, 3))
      
      # add recent harvest after NTEMS
      newYears <- (max(values(sim$harvNTEMs), na.rm = T)+1) : max(P(sim)$historicLandYears)
      
      newHarvRast <- make_CanLad_cumulative(yrs = newYears, disturbTypeCode = 2, 
                                            dPath = dPath, rtm = sim$rasterToMatch_extendedLandscapeFine) |>
        Cache()
      
      # combine all types together
      sim$harv <- c(disturbCanLadOldHarvYear, sim$harvNTEMS, newHarvRast)
      names(sim$harv) <- c('CanLadOld', 'NTEMS', 'CanLadNew')
      
      
      
      
      # TODO set these in setupProj
      rtms <- list(rasterToMatch_extendedLandscapeFine, rasterToMatch_extendedLandscapeCoarse)
      names(rtms)  <- c("window30", 'agg500') 
      rtmsFuns <- c(paste0('make_landforest_prop(targetFile = targetFile, trast = rtm, buff = ',
                          P(sim)$buffer,', where2save = dataPath(sim))'),
                   paste('aggregate_landforest(targetFile = targetFile, trast = rtm', 
                               ', where2save = dataPath(sim))'))
      #rtmsDigest <- .robustDigest(rtms)
     # names(P(sim)$historicLandYears) <- P(sim)$historicLandYears
      mod$historicLand <- Map(rtmname = names(rtms), rtm = rtms, 
                              rtmDigest = rtmsDigest, rtmFun = rtmsFuns, function(rtm, rtmname, rtmFun) {
        
        Map(nn = names(P(sim)$historicLandYears), ii=P(sim)$historicLandYears, function(nn,ii){
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
    plot = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      plotFun(sim) # example of a plotting function
      # schedule future event(s)

      # e.g.,
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "prepLandscape", "plot")

      # ! ----- STOP EDITING ----- ! #
    },
    save = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "prepLandscape", "save")

      # ! ----- STOP EDITING ----- ! #
    },
    event1 = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "prepLandscape", "templateEvent")

      # ! ----- STOP EDITING ----- ! #
    },
    event2 = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "prepLandscape", "templateEvent")

      # ! ----- STOP EDITING ----- ! #
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

### template for your event1
Event1 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event1Test1 <- " this is test for event 1. " # for dummy unit test
  # sim$event1Test2 <- 999 # for dummy unit test

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event2
Event2 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event2Test1 <- " this is test for event 2. " # for dummy unit test
  # sim$event2Test2 <- 777  # for dummy unit test

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
  if (!suppliedElsewhere("studyArea_4maps", sim)){
    sim$studyArea <- Cache(prepInputs,
                                 url = extractURL("studyArea"),
                                 destinationPath = dataPath(sim),
                                 targetFile = "studyArea_bcnwt_4sims.shp",  
                                 alsoExtract = "similar", fun = "terra::vect")
  }
  
  if (!suppliedElsewhere("rasterToMatch_extendedLandscapeFine", sim)){
    sim$rasterToMatch_extendedLandscapeFine <- terra::rast(studyArea, res = c(30, 30), vals = 1)
  }
  
  # TODO need to update this, it's not actually 500m
  if (!suppliedElsewhere("rasterToMatch_extendedLandscapeCoarse", sim)){
    sim$rasterToMatch_extendedLandscapeCoarse <- terra::aggregate(sim$rasterToMatch, fact = 16)
  }
  
  
    sim$harvNTEMS <- reproducible::prepInputs(url = extractURL("harvNTEMS"),
                                              destinationPath = dPath,
                                              to = rasterToMatch_extendedLandscapeFine, 
                                              fun = 'terra::rast') |>
      Cache()
  
    sim$disturbCanLadOldType <- prepInputs(url = 'https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/canada_disturbances_1965to1984/v1/canlad_1965_1984_disturbanceType.tif',
                                       destinationPath = dPath,
                                       alsoExtract = "similar", fun = "terra::rast",
                                       to = rasterToMatch_extendedLandscapeFine,
                                       method = 'near') |>
      Cache()
   
    sim$disturbCanLadOldYear <- prepInputs(url = 'https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/canada_disturbances_1965to1984/v1/canlad_1965_1984_disturbanceYear.tif',
                                       destinationPath = dPath,
                                       alsoExtract = "similar", fun = "terra::rast",
                                       to = rasterToMatch_extendedLandscapeFine,
                                       method = 'near') |>
      Cache()

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

ggplotFn <- function(data, ...) {
  ggplot2::ggplot(data, ggplot2::aes(TheSample)) +
    ggplot2::geom_histogram(...)
}

