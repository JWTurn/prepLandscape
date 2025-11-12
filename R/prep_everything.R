
prep_everything <- function(histLandYears, fires, rasterToMatch, rtms, rtmFuns, backgroundYr,
                            harvNTEMS, disturbCanLadOldYear, disturbCanLadOldType, dig, dPath) {

  # load disturbances
  ##### FIRES ----
  histFire <- make_timeSinceDisturb_rast(layer = fires, rast = rasterToMatch,
                                         disturbanceType = 'Fire',
                                         minyr = min(histLandYears), #maxyr = max(histLandYears),
                                         backgrndYear = backgroundYr, where2save = NULL) |>
    Cache(.cacheExtra = dig, omitArgs = 'rast', .functionName = 'makeTimeSinceFire')

  ##### HARVEST -----
  harvNTEMS[harvNTEMS==0]<-NA

  # get before 1985 harvest from CanLaD -- get year only for harv
  disturbCanLadOldHarvYearFine <- terra::mask(disturbCanLadOldYear, terra::match(disturbCanLadOldType, 3)) |>
    Cache(.functionName = 'makeCanLadHarvOldFine')
  disturbCanLadOldHarvYear <- reproducible::postProcess(disturbCanLadOldHarvYearFine, to = rasterToMatch)|>
    Cache(.functionName = 'makeCanLadHarvOld', .cacheExtra = list(dig, attr(disturbCanLadOldHarvYearFine, 'tags')),
          omitArgs = c('to', 'x'))
  message('gathered before 1985 harvest from CanLaD')

  # add recent harvest after NTEMS

  maxHarvNTEMS <- (max(terra::values(harvNTEMS), na.rm = T)+1)
  newYears <-  seq(maxHarvNTEMS, max(histLandYears))

  newHarvRast <- make_CanLad_cumulative(yrs = newYears, disturbTypeCode = 2,
                                        dPath = dPath, rtm = rasterToMatch) |>
    Cache(.cacheExtra = dig, omitArgs = 'rtm', .functionName = 'makeNewHarvRast')
  message('add recent harvest after NTEMS')

  # combine all types together

  harvNTEMScoarse <- reproducible::postProcess(harvNTEMS, to = rasterToMatch)|>
    Cache(.functionName = 'makeHarvNTEMScoarse', .cacheExtra = dig, omitArgs = 'to')

  harvs <- c(disturbCanLadOldHarvYear, harvNTEMScoarse, newHarvRast)
  names(harvs) <- c('CanLadOld', 'NTEMS', 'CanLadNew')

  timeSinceHarvest <- Map(nn = paste0('year', histLandYears), yr = histLandYears,
                          rtm = rasterToMatch, background = backgroundYr,
                          function(nn, yr, rtm, background){


                            harvsYrPos <- clamp(harvs, upper = yr, value = F)
                            maxRast <- max(harvsYrPos, na.rm = T)
                            timeSince <- yr - maxRast
                            backgrnd <- yr - background
                            timeSinceFill <- mask(ifel(is.na(timeSince), backgrnd, timeSince), rtm)
                            names(timeSinceFill) <- 'timeSinceHarvest'
                            return(timeSinceFill)
                          })|>
    Cache(.cacheExtra = dig, omitArgs = 'rtm', .functionName = 'prep_timeSinceHarv')

  message('combine all harvest data')

  #rtmsDigest <- .robustDigest(rtms)
  # names(historicLandYears) <- historicLandYears



  histLand <- Map(rtmname = names(rtms), rtm = rtms,
                  #rtmDigest = rtmsDigest,
                  rtmFun = rtmFuns, function(rtm, rtmname, rtmFun) {

                    Map(nn = paste0('year', histLandYears), ii=histLandYears, function(nn,ii){
                      propWindow <- reproducible::prepInputs(
                        url = paste0("https://opendata.nfis.org/downloads/forest_change/CA_forest_VLCE2_", ii, ".zip"),
                        destinationPath = dPath, # end pre process
                        fun = eval(parse(text = rtmFun)), # end process
                        rtm = rtm) |>
                        Cache(.functionName = paste0(rtmname, ii, '_propLand'))

                      return(propWindow)
                    })

                  })|>
    Cache(.functionName = 'make_histLand', .cacheExtra = dig, omitArgs = 'rtm')

  # could also set a rule of check if file looking for in drive, download if is, or make here
  landscapeYearly <- Map(nn = paste0('year', histLandYears), ii=histLandYears, function(nn,ii){

     yearly <- c(histLand[[names(rtms)[[1]]]][[nn]],
                timeSinceHarvest[[nn]], histFire[[nn]])

    return(yearly)
  }) |>
    Cache(.functionName = 'prep_landscapeYearly')

  return(list(harvNTEMS = harvNTEMS, landscapeYearly = landscapeYearly))
}
