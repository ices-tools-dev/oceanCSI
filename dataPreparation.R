# ipak function ----------------------------------------------------------------
# install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.
ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("sf", "data.table", "dplyr")
ipak(packages)
memory.limit(size = 16000)

# Input files ------------------------------------------------------------------

coastlineFile <- "Input/Country_Europe_Extended.shp"
#coastlineFile <- "Input/EEA_Coastline_20170228.shp"
searegionFile <- "Input/EEA_SeaRegion_20180831.shp"
#searegionFile <- "Input/MSFD_Marine_Subregions_draft_EU_EEZ_20130614.shp"
stationFile <- "Input/OceanCSI_Station_20201002.txt"
sampleFile <- "Input/OceanCSI_Sample_20201002.txt"

# Coastline --------------------------------------------------------------------

# Read shapefile
coastline <- st_read(coastlineFile)

# Check if geometries is valid
#sf::st_is_valid(coastline)

# Make geometries valid by doing the buffer of nothing trick
#coastline <- sf::st_buffer(coastline, 0.0)

# Transform projection into UTM33N
coastline <- sf::st_transform(coastline, crs = 32633)

# Make 1km buffer
#coastline_Within1km <- sf::st_buffer(coastline, 1000)

# Make 20km buffer
coastline_Within20km <- sf::st_buffer(coastline, 20000)
#plot(coastline_Within20km)

# SeaRegions -------------------------------------------------------------------

# Read shapefile
searegions <- sf::st_read(searegionFile)

# Check if geometries is valid
#sf::st_is_valid(searegions)

# Make geometries valid by doing the buffer of nothing trick
#searegions <- sf::st_buffer(searegions, 0.0)

# Transform projection into UTM33N
searegions <- sf::st_transform(searegions, crs = 32633)

# Stations ---------------------------------------------------------------------

# Read stations
stations <- fread(input = stationFile, sep = "\t", na.strings = "NULL", stringsAsFactors = FALSE, header = TRUE)
# stations <- fread(input = stationFile, sep = "\t", nrows = 100000, na.strings = "NULL", stringsAsFactors = FALSE, header = TRUE)

# Make stations spatial keeping original latitude/longitude
stations <- sf::st_as_sf(stations, coords = c("Longitude", "Latitude"), remove = FALSE, crs = 4326)

# Project stations into UTM33N
stations <- sf::st_transform(stations, crs = 32633)
stations$UTM_E <- sf::st_coordinates(stations)[,1]
stations$UTM_N <- sf::st_coordinates(stations)[,2]
stations <- sf::st_transform(stations, crs = 4326)
stations$Lon <- sf::st_coordinates(stations)[,1]
stations$Lat <- sf::st_coordinates(stations)[,2]
stations <- sf::st_transform(stations, crs = 32633)

# Classify stations into sea regions - the R way. However the stations have allready been classified when extracting the data from the database
#stations$SeaRegionID_R <- sf::st_intersects(stations, searegions) %>% as.numeric()
#table(stations$SeaRegionID_R)
#table(stations$SeaRegionID)

# Classify stations into on land
#stations$OnLand <- apply(sf::st_intersects(stations, coastline, sparse = TRUE), 1, any)

# Classify stations into within 1km from land
#stations$Within1km <- apply(sf::st_intersects(stations, coastline_Within1km, sparse = TRUE), 1, any)

# Classify stations into within 20km from land
stations$Within20km <- apply(sf::st_intersects(stations, coastline_Within20km, sparse = TRUE), 1, any)

# Remove spatial column in order to merge station samples
stations <- sf::st_set_geometry(stations, NULL)

# Classify stations using square assignment
#
# Stations are defined geographical by position given as longitude and latitude in decimal degrees, but do not contain relaible
# and consistent station identification. The position of the same station migth vary slighly over time. In order to improve the
# aggregation into to time series, data are aggregated into squares with sides of 1.375 km for coastal stations within 20 km
# from the coastline (m = 80) and 5.5 km for open water station more than 20 km away from the coastline (m = 20).
# The procedure does not totally prevent errorneous aggregation of data belonging to stations close to each other or errorneous
# breakup of time series into fragments due to small shifts in position, but reduces the problem considerably.
#station$m <- 20
stations$m <- ifelse(stations$Within20km, 80, 20)
stations$iY <- round(stations$Latitude*stations$m)
stations$latitude_center <- stations$iY/stations$m
stations$rK <- stations$m/cos(stations$latitude_center*atan(1)/45)
stations$iX <- round(stations$Longitude*stations$rK)
stations$longitude_center <- stations$iX/stations$rK
comb <- with(stations, paste(iX, iY))
stations$ClusterID <- match(comb, unique(comb))

# Samples ----------------------------------------------------------------------

# Read samples
samples <- fread(sampleFile, sep = "\t", na.strings = "NULL", stringsAsFactors = FALSE, header = TRUE)

# Read average depths from EMODnet bathymetry produced with "getSoundingsFromEmodnet.R"
station_emodnetDepth <- fread("station_emodnetDepth.csv")[,list(StationID, avgDepth)]

# StationSamples ----------------------------------------------------------

setkey(station_emodnetDepth, StationID)
setkey(stations, StationID)
stations <- stations[station_emodnetDepth]

# merge stations and samples
setkey(stations, StationID)
setkey(samples, StationID, SampleID)
stationSamples <- stations[samples]

# free memory
rm(stations, samples, station_emodnetDepth)

# Extra cleaning for NA values 9999 and -999

nines <- stationSamples$HydrogenSulphide[which(grepl("999", as.character(stationSamples$HydrogenSulphide)))]
summary (nines)
nines <- stationSamples$Oxygen[which(grepl("999", as.character(stationSamples$Oxygen)))]
summary (nines)

hist(stationSamples$Oxygen)

# apparently, there are "-999 and "9999" values. replace with NA

hist(stationSamples$Oxygen)
stationSamples$Oxygen[stationSamples$Oxygen < -998 & !is.na(stationSamples$Oxygen)]

# or rather "-999.0001

for(j in seq_along(stationSamples)){
  set(stationSamples, i=which(stationSamples[[j]]==9999), j=j, value=NA)
}

for(j in seq_along(stationSamples)){
  set(stationSamples, i=which(stationSamples[[j]]==-999.0001), j=j, value=NA)
}

## In order to save the current state of data, run following code
save(stationSamples, file = "oceancsidata.RData")



