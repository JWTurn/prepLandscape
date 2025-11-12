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
  reqdPkgs = list("SpaDES.core (>= 2.1.5.9002)", "ggplot2", "terra", "sf", 'stringr', 'data.table'),
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
                 sourceURL = 'https://drive.google.com/file/d/1iq1f53pAtZFIFoN1RFhlXEghSbvI3Dnf/view?usp=share_link'),

    expectsInput(objectName = 'studyArea_extendedLandscape', objectClass = 'SpatVector',
                 desc = 'Extended study area for prepping landscape layers'),

    expectsInput(objectName = 'buffer', objectClass = 'numeric',
                 desc = 'Buffer for moving windows'),

    expectsInput(objectName = 'harvNTEMS', objectClass = 'SpatRaster',
                 desc = 'harvest history',
                 sourceURL = "https://opendata.nfis.org/downloads/forest_change/CA_Forest_Harvest_1985-2020.zip"),

    expectsInput(objectName = 'disturbCanLadOldType', objectClass = 'SpatRaster',
                 desc = 'CanLad disturbance type data before 1985',
                 sourceURL = 'https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/canada_disturbances_1965to1984/v1/canlad_1965_1984_disturbanceType.tif'),

    expectsInput(objectName = 'disturbCanLadOldYear', objectClass = 'SpatRaster',
                 desc = 'CanLad disturbance year data before 1985',
                 sourceURL = 'https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/canada_disturbances_1965to1984/v1/canlad_1965_1984_disturbanceYear.tif'),

    expectsInput(objectName = 'nfdbURL', objectClass = 'SpatVector',
                 desc = 'National Fire Data Base (NFDB) for back filling NBAC',
                 sourceURL = 'https://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_poly/current_version/NFDB_poly.zip'),

    expectsInput(objectName = 'nbacURL', objectClass = 'SpatVector',
                 desc = 'National Burn Area Composite',
                 sourceURL = 'https://cwfis.cfs.nrcan.gc.ca/downloads/nbac/NBAC_1972to2024_20250506_shp.zip'),

    expectsInput(objectName = 'fires', objectClass = 'SpatVector',
                 desc = paste0('SpatVector of fire polygons (combined NBAC and NFDB).')),

    expectsInput(objectName = 'anthroDisturb', objectClass = 'SpatVector',
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
    createsOutput(objectName = 'landscapeYearly', objectClass = 'spatRaster',
                  desc = 'spatRaster stack of the yearly landscape layers'),
    createsOutput(objectName = 'landscape5Yearly', objectClass = 'spatRaster',
                  desc = 'spatRaster stack of the 5 yearly landscape layers')
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

      mod$histFire <- make_timeSinceDisturb_rast(layer = sim$fires, rast = sim$rasterToMatch_extendedLandscape,
                                                 disturbanceType = 'Fire',
                                                 minyr = min(P(sim)$histLandYears), #maxyr = max(P(sim)$histLandYears),
                                                 backgrndYear = P(sim)$backgroundYr, where2save = NULL) |>
        Cache(.cacheExtra = mod$dig, omitArgs = 'rast', .functionName = 'makeTimeSinceFire')

      ##### HARVEST -----
      sim$harvNTEMS[sim$harvNTEMS==0]<-NA

      # get before 1985 harvest from CanLaD -- get year only for harv
      disturbCanLadOldHarvYearFine <- terra::mask(sim$disturbCanLadOldYear, terra::match(sim$disturbCanLadOldType, 3)) |>
        Cache(.functionName = 'makeCanLadHarvOldFine')
      disturbCanLadOldHarvYear <- reproducible::postProcess(disturbCanLadOldHarvYearFine, to = sim$rasterToMatch_extendedLandscape)|>
        Cache(.functionName = 'makeCanLadHarvOld')
      message('gathered before 1985 harvest from CanLaD')

      # add recent harvest after NTEMS

      maxHarvNTEMS <- (max(terra::values(sim$harvNTEMS), na.rm = T)+1)
      newYears <-  seq(maxHarvNTEMS, max(P(sim)$histLandYears))

      newHarvRast <- make_CanLad_cumulative(yrs = newYears, disturbTypeCode = 2,
                                            dPath = dPath, rtm = sim$rasterToMatch_extendedLandscape) |>
        Cache(.cacheExtra = mod$dig, omitArgs = 'rtm', .functionName = 'makeNewHarvRast')
      message('add recent harvest after NTEMS')

      # combine all types together

      harvNTEMScoarse <- reproducible::postProcess(sim$harvNTEMS, to = sim$rasterToMatch_extendedLandscape)|>
        Cache(.functionName = 'makeHarvNTEMScoarse')

      harvs <- c(disturbCanLadOldHarvYear, harvNTEMScoarse, newHarvRast)
      names(harvs) <- c('CanLadOld', 'NTEMS', 'CanLadNew')

      timeSinceHarvest <- Map(nn = paste0('year', P(sim)$histLandYears), yr = P(sim)$histLandYears,
                              rtm = sim$rasterToMatch_extendedLandscape, background = P(sim)$backgroundYr,
                              function(nn, yr, rtm, background){


                                harvsYrPos <- clamp(harvs, upper = yr, value = F)
                                maxRast <- max(harvsYrPos, na.rm = T)
                                timeSince <- yr - maxRast
                                backgrnd <- yr - background
                                timeSinceFill <- mask(ifel(is.na(timeSince), backgrnd, timeSince), rtm)
                                names(timeSinceFill) <- 'timeSinceHarvest'
                                return(timeSinceFill)
                              })|>
        Cache(.cacheExtra = mod$dig, omitArgs = 'rtm', .functionName = 'prep_timeSinceHarv')

      message('combine all harvest data')

      #rtmsDigest <- .robustDigest(rtms)
      # names(P(sim)$historicLandYears) <- P(sim)$historicLandYears

      if (!is.null(sim$rtmFuns)){
        sim$rtmFuns <- c(paste0('make_landforest_prop(targetFile = targetFile, trast = rtm, buff = ',
                                sim$buffer,', where2save = NULL)'))
      }


      mod$histLand <- Map(rtmname = names(sim$rtms), rtm = sim$rtms,
                          #rtmDigest = rtmsDigest,
                          rtmFun = sim$rtmFuns, function(rtm, rtmname, rtmFun) {

                           Map(nn = paste0('year', P(sim)$histLandYears), ii=P(sim)$histLandYears, function(nn,ii){
                             propWindow <- reproducible::prepInputs(
                                url = paste0("https://opendata.nfis.org/downloads/forest_change/CA_forest_VLCE2_", ii, ".zip"),
                                destinationPath = dPath, # end pre process
                                fun = eval(parse(text = rtmFun)), # end process
                                rtm = rtm) |>
                               Cache(.functionName = paste0(rtmname, ii, '_propLand'))

                              return(propWindow)
                            })

                          })|>
        Cache(.functionName = 'make_histLand')

      # could also set a rule of check if file looking for in drive, download if is, or make here
      sim$landscapeYearly <- Map(nn = paste0('year', P(sim)$histLandYears), ii=P(sim)$histLandYears, function(nn,ii){
        yearly <- c(mod$histLand[[names(sim$rtms)[[1]]]][[nn]],
                    timeSinceHarvest[[nn]], mod$histFire[[nn]])
        #writeRaster(yearly, file.path(dataPath(sim), P(sim)$.studyAreaName, paste0('yearly_', names(sim$rtms)[[1]], '_', ii, '.tif')))
        return(yearly)
      }) |>
        Cache(.functionName = 'prep_landscapeYearly') # useCloud only works for exact studyArea

      sim$landscape5Yearly <- sim$anthroDisturb

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
    sim$studyArea <- reproducible::prepInputs(url = extractURL("studyArea"),
                                              destinationPath = dataPath(sim),
                                              targetFile = "studyArea_bcnwt_4sims.shp",
                                              alsoExtract = "similar", fun = "terra::vect") |>
      Cache()
  }

  if (!suppliedElsewhere("studyArea_extendedLandscape", sim)){
    sim$studyArea_extendedLandscape <- terra::buffer(sim$studyArea, 50000) |>
      Cache(.functionName = 'prep_studyArea_extendedLandscape')
  }

  sim$harvNTEMS <- reproducible::prepInputs(url = extractURL("harvNTEMS"),
                                            destinationPath = dPath,
                                            to = sim$studyArea_extendedLandscape,
                                            fun = 'terra::rast') |>
    Cache(.functionName = 'prepInputs_harvNTEMS')


  if (!suppliedElsewhere("rasterToMatch_extendedLandscapeFine", sim)){
    sim$rasterToMatch_extendedLandscapeFine <- terra::rasterize(sim$studyArea_extendedLandscape,
                                                           sim$harvNTEMS, vals = 1)
    sim$rasterToMatch_extendedLandscapeFine <- terra::mask(sim$rasterToMatch_extendedLandscapeFine,
                                                           sim$studyArea_extendedLandscape)|>
      Cache(.functionName = 'prep_rasterToMatch_extendedLandscapeFine')
  }
  mod$dig <- reproducible::CacheDigest(sim$rasterToMatch_extendedLandscapeFine)$outputHash

  if (!suppliedElsewhere("rasterToMatch_extendedLandscape", sim)){
    sim$rasterToMatch_extendedLandscape <- terra::aggregate(sim$rasterToMatch_extendedLandscapeFine,
                                                            fact = 8)|>
      Cache(.cacheExtra = mod$dig, omitArgs = 'x', .functionName = 'prep_rasterToMatch_extendedLandscape')
  }

  mod$dig <- c(mod$dig, reproducible::CacheDigest(sim$rasterToMatch_extendedLandscape)$outputHash)

  if (!suppliedElsewhere("rasterToMatch_extendedLandscapeCoarse", sim)){
    sim$rasterToMatch_extendedLandscapeCoarse <- terra::aggregate(sim$rasterToMatch_extendedLandscapeFine,
                                                                  fact = 16)|>
      Cache(.cacheExtra = mod$dig, omitArgs = 'x', .functionName = 'prep_rasterToMatch_extendedLandscapeCoarse')
  }

  if (!suppliedElsewhere("rtms", sim)){
    sim$rtms <- list(sim$rasterToMatch_extendedLandscape)
    names(sim$rtms) <- c('window240')
    mod$dig <- c(mod$dig, reproducible::CacheDigest(names(sim$rtms))$outputHash)

  }


  if (!suppliedElsewhere("buffer", sim)){
    sim$buffer <- 720
  }



  sim$disturbCanLadOldType <- reproducible::prepInputs(url = extractURL('disturbCanLadOldType'),
                                                       destinationPath = dPath,
                                                       alsoExtract = "similar", fun = "terra::rast",
                                                       to = sim$rasterToMatch_extendedLandscapeFine,
                                                       method = 'near') |>
    Cache(.cacheExtra = mod$dig, omitArgs = 'to', .functionName = 'load_disturbCanLadOldType')

  sim$disturbCanLadOldYear <- reproducible::prepInputs(url = extractURL('disturbCanLadOldYear'),
                                                       destinationPath = dPath,
                                                       alsoExtract = "similar", fun = "terra::rast",
                                                       to = sim$rasterToMatch_extendedLandscapeFine,
                                                       method = 'near') |>
    Cache(.cacheExtra = mod$dig, omitArgs = 'to', .functionName = 'load_disturbCanLadOldYear')


  sim$fires <- combine_fire_DB('nbacURL', 'nfdbURL', dPath,
                               sim$studyArea_extendedLandscape,
                               studyAreaName = Par$.studyAreaName,
                               savePath = NULL) |>
    Cache(.cacheExtra = mod$dig, omitArgs = 'studyArea', .functionName = 'combine_fire_DB')

  sim$anthroDisturb <- prep_anthroDisturbance(inputsPath = dPath, studyArea = sim$studyArea_extendedLandscape,
                                              dataPath = dataPath(sim), source = 'ECCC') |>
    Cache(.cacheExtra = mod$dig, omitArgs = 'studyArea', .functionName = 'prep_anthroDisturbance')

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}


