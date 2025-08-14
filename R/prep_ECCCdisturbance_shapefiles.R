# make all ECCC disturbance .gdbs into 1 shapefile for ease of use

year <- 2020

if(year == 2015){
  eccc2015URL <- 'https://data-donnees.az.ec.gc.ca/api/file?path=%2Fspecies%2Fdevelopplans%2Fanthropogenic-disturbance-footprint-within-boreal-caribou-ranges-across-canada-as-interpreted-from-2008-2010-Landsat-satellite-imagery-Updated-to-2012-range-boundaries)%2F2015-anthropogenic-disturbance-footprint-within-boreal-caribou-ranges-across-canada-as-interpreted-from-2015-landsat-satellite-imagery%2Fanthro-disturb-perturb-30m-2015.zip'
  download.file(url = eccc2015URL, file.path(canada, "ECCC-disturb-2015.zip"))
  unzip(file.path(canada, "ECCC-disturb-2015.zip"), list=TRUE)$Name
  lsFiles <- list.dirs(file.path(canada, 'Anthro_Disturb_Perturb_30m_2015'))
  
  lsFiles <- lsFiles[-c(1,2)]
} 
if(year ==2020){
  lsFiles <- list.dirs(file.path('G:', 'Shared drives', 'JWTscratch', 'ECCC-disturb-2020'))
  lsFiles <- lsFiles[-c(1)]
}

saHerds <- vect(file.path(canada, 'Anthro_Disturb_Perturb_30m_2015','_Caribou_51_Ranges_Aires_2012.gdb'),
                layer = 'Caribou_51_Ranges_Aires_2012')


# make a list
ls.herds <- sort(saHerds$Herd_OS)
# update misspelled and inconsistent names
ls.herds <- replace(ls.herds, ls.herds =='AtikakiBernes', 'AtikakiBerens')
# ls.layers <- replace(ls.herds, ls.herds== 'EastSideAthabascaRiver', 'ESAR')
# ls.layers <- replace(ls.layers, ls.layers== 'WestSideAthabascaRiver', 'WSAR')
# ls.layers <- replace(ls.layers, ls.layers== 'SaskatchewanBorealPlain', 'SaskBorealPlain')
# ls.layers <- replace(ls.layers, ls.layers== 'SaskatchewanBorealShield', 'SaskBorealShield')
# ls.layers <- replace(ls.layers, ls.layers== 'ManitobaSouthConservationUnit', 'ManitobaSouthCU')
# ls.layers <- replace(ls.layers, ls.layers== 'ManitobaNorthConservationUnit', 'ManitobaNorthCU')
# ls.layers <- replace(ls.layers, ls.layers== 'ManitobaEastConservationUnit', 'ManitobaEastCU')
# ls.layers <- replace(ls.layers, ls.layers== 'NorthwestTerritories', 'NWT')

# load the line disturbance layers for each herd in the list
lines <- list()
for (i in 1:length(lsFiles)) { # for each file in the list
  fileName <- lsFiles[[i]] # save filename of element i
  dataName <-  paste0(ls.herds[[i]],'_lines') # save data name of element i
  lsLyrs <- sf::st_layers(fileName)
  lyrName <- lsLyrs$name[grepl('Disturb_Perturb_Line', lsLyrs$name)] # save layer name of element i
  lines[[dataName]] <- vect(sf::st_read (fileName, layer = lyrName, type = 5)) # read file, force as line
  #assign (dataName, tempData, envir=.GlobalEnv)  # assign the results of file to the data named
  
}

# bind all lines together
allLines <- vect(lines)
writeVector(allLines, file.path(canada, 'ECCC_disturbance', paste0('eccc_disturb_lines_',year ,'.shp'))|> checkPath(create = T))


# load the polygonal disturbance layers for each herd in the list
polys <- list()
for (i in 1:length(lsFiles)) { # for each file in the list
  fileName <- lsFiles[[i]] # save filename of element i
  dataName <-  paste0(ls.herds[[i]],'_polys') # save data name of element i
  lsLyrs <- sf::st_layers(fileName)
  lyrName <- lsLyrs$name[grepl('Disturb_Perturb_Poly', lsLyrs$name)] # save layer name of element i
  polys[[dataName]] <- vect(sf::st_read (fileName, layer = lyrName, type = 6)) # read file, force as line
  #assign (dataName, tempData, envir=.GlobalEnv)  # assign the results of file to the data named
  
}

# bind all polys together
allPolys <- vect(polys)
plot(allPolys)
writeVector(allPolys, file.path(canada, 'ECCC_disturbance', paste0('eccc_disturb_polys_', year, '.shp'))|> checkPath(create = T))
