
rm(list = ls())

newData = FALSE
if(newData){source('~/OceanCSI/dataPreparation_v2.R')}


load("oceancsidata_nutrients.RData")

# Prepare for plotting
source("plotfunctions.r")
if(!dir.exists("output")){dir.create("output")} 


# Nitrate Nitrogen (Winter) -------------------------------------------------------------
#   Parameters: [NO3-N]
#   Depth: <= 10
#   Period: Winter
#     January - March for stations within Baltic Sea east of 15 E
#     January - February for other stations
#   Aggregation Method: Arithmetric mean of mean by station and cluster per year

# Filter stations rows and columns --> ClusterID, StationID, Year, Depth, Temperature, Salinity, Nitrate
wk <- stationSamples_nut[Depth <= 10 & ifelse(SeaRegionID == 1 & Longitude > 15, Month >= 1 & Month <= 3, Month >= 1 & Month <= 2) & !is.na(Nitrate) & (NitrateQ != 3 & NitrateQ != 4), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year, Depth, Temperature, Salinity, Nitrate)]

# Calculate station mean --> ClusterID, StationID, Year, MinDepth, MaxDepth, AvgTemperature, AvgSalinity, AvgNitrate, MinNitrate, MaxNitrate, CountSamples
wk1 <- wk[, list(MinDepth = min(Depth), MaxDepth = max(Depth), AvgTemperature = mean(Temperature), AvgSalinity = mean(Salinity), AvgNitrate = mean(Nitrate), MinNitrate = min(Nitrate), MaxNitrate = max(Nitrate), SampleCount = .N), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year)]

# Calculate cluster mean --> ClusterID, Year, MinMinDepth, MaxMaxDepth, AvgAvgAvgTemperature, AvgAvgSalinity, AvgAvgNitrate, MinMinNitrate, MaxMaxNitrate, SumCountSamples
wk2 <- wk1[, list(AvgLatitude = mean(Latitude), AvgLongitude = mean(Longitude), MinDepth = min(MinDepth), MaxDepth = max(MaxDepth), AvgTemperature = mean(AvgTemperature), AvgSalinity = mean(AvgSalinity), AvgNitrate = mean(AvgNitrate), MinNitrate = min(MinNitrate), MaxNitrate = max(MaxNitrate), SampleCount = sum(SampleCount)), list(SeaRegionID, ClusterID, Year)]

fwrite(wk2, "output/status_nitrate.csv")

# plot average status for last 5 years 
wk21 <- wk2[Year > 2012, list(Nitrate = mean(AvgNitrate), Longitude = mean(AvgLongitude), Latitude = mean(AvgLatitude)), list(ClusterID)]

plotStatusMaps(bboxEurope, data = wk21, xlong = "Longitude", ylat = "Latitude", 
               parameterValue = "Nitrate", 
               invJet = F, 
               limits = c(0,100))
saveEuropeStatusMap(parameter = "Nitrate")

# trend analysis using Kendall test

# ClusterIDs where one of the years is > 2006
yearcriteria <- wk2[Year>2006, unique(ClusterID)]
clusterSelection <- wk2[
  ClusterID %in% yearcriteria][
    , list(NrClustersPerYear = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), by = .(ClusterID, Year, SeaRegionID)][
      , .(NrYears = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), by = .(ClusterID, SeaRegionID)][
        NrYears >=5]
wk22 <- wk2[ClusterID %in% clusterSelection[[1]]]
l <- wk22 %>% as.data.frame() %>% split(.$ClusterID) 
timeserieslist <- lapply(
  l, function(x) xts::xts(x[,"AvgNitrate"], order.by = as.Date(as.character(x[,"Year"]),format = "%Y")))
KendallResult <- lapply(timeserieslist, function(x) MannKendall(x))
df.KendallResult <- as.data.frame((matrix(unlist(list.flatten(KendallResult)), ncol  = 5, byrow = T)))
names(df.KendallResult) <- c("tau", "sl", "S", "D", "varS")
df.KendallResult$ClusterID <- as.integer(names(KendallResult))
KendallResult.clustered <- df.KendallResult %>% 
  left_join(clusterSelection, by = c('ClusterID' = 'ClusterID')) %>%
  filter(!is.na(S)) %>%
  mutate(Trend = case_when(
    .$sl <= 0.05 & .$S < 0 ~ "Decreasing",
    .$sl <= 0.05 & .$S > 0 ~ "Increasing",
    .$sl > 0.05 ~ "No trend")
  ) %>%
  mutate(Trend = as.factor(Trend))
KendallResult.clustered$Trend <- factor(KendallResult.clustered$Trend, 
                                        levels =  c("No trend", "Decreasing", "Increasing"))

fwrite(KendallResult.clustered, "output/trend_nitrate.csv")

plotKendallClasses(plotdata = KendallResult.clustered, parameterValue = "Nitrate", year = "(2007-2017)")
saveEuropeTrendMap("Nitrate")


# Nitrite Nitrogen (Winter) -------------------------------------------------------------
#   Parameters: [NO2-N]
#   Depth: <= 10
#   Period: Winter
#     January - March for stations within Baltic Sea east of 15 E
#     January - February for other stations
#   Aggregation Method: Arithmetric mean of mean by station and cluster per year

# Filter stations rows and columns --> ClusterID, StationID, Year, Depth, Temperature, Salinity, Nitrite
wk <- stationSamples_nut[Depth <= 10 & ifelse(SeaRegionID == 1 & Longitude > 15, Month >= 1 & Month <= 3, Month >= 1 & Month <= 2) & !is.na(Nitrite) & (NitriteQ != 3 & NitriteQ != 4), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year, Depth, Temperature, Salinity, Nitrite)]

# Calculate station mean --> ClusterID, StationID, Year, MinDepth, MaxDepth, AvgTemperature, AvgSalinity, AvgNitrate, MinNitrite, MaxNitrite, CountSamples
wk1 <- wk[, list(MinDepth = min(Depth), MaxDepth = max(Depth), AvgTemperature = mean(Temperature), AvgSalinity = mean(Salinity), AvgNitrite = mean(Nitrite), MinNitrite = min(Nitrite), MaxNitrite = max(Nitrite), SampleCount = .N), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year)]

# Calculate cluster mean --> ClusterID, Year, MinMinDepth, MaxMaxDepth, AvgAvgAvgTemperature, AvgAvgSalinity, AvgAvgNitrite, MinMinNitrite, MaxMaxNitrite, SumCountSamples
wk2 <- wk1[, list(AvgLatitude = mean(Latitude), AvgLongitude = mean(Longitude), MinDepth = min(MinDepth), MaxDepth = max(MaxDepth), AvgTemperature = mean(AvgTemperature), AvgSalinity = mean(AvgSalinity), AvgNitrite = mean(AvgNitrite), MinNitrite = min(MinNitrite), MaxNitrite = max(MaxNitrite), SampleCount = sum(SampleCount)), list(SeaRegionID, ClusterID, Year)]

fwrite(wk2, "output/status_nitrite.csv")

# plot average status for last 5 years 
wk21 <- wk2[Year > 2012, list(Nitrite = mean(AvgNitrite)), list(ClusterID, AvgLongitude, AvgLatitude)]
plotStatusMaps(bboxEurope, data = wk21, xlong = "AvgLongitude", ylat = "AvgLatitude", 
               parameterValue = "Nitrite", 
               invJet = F, 
               limits = "auto"
)
saveEuropeStatusMap(parameter = "Nitrite")

# trend analysis using Kendall test
yearcriteria <- wk2[Year>2006, unique(ClusterID)]
clusterSelection <- wk2[
  ClusterID %in% yearcriteria][
    , list(NrClustersPerYear = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), by = .(ClusterID, Year, SeaRegionID)][
      , .(NrYears = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), by = .(ClusterID, SeaRegionID)][
        NrYears >=5]
wk22 <- wk2[ClusterID %in% clusterSelection[[1]]]
l <- wk22 %>% as.data.frame() %>% split(.$ClusterID) 
timeserieslist <- lapply(
  l, function(x) xts::xts(x[,"AvgNitrite"], order.by = as.Date(as.character(x[,"Year"]),format = "%Y")))
KendallResult <- lapply(timeserieslist, function(x) MannKendall(x))
df.KendallResult <- as.data.frame((matrix(unlist(list.flatten(KendallResult)), ncol  = 5, byrow = T)))
names(df.KendallResult) <- c("tau", "sl", "S", "D", "varS")
df.KendallResult$ClusterID <- as.integer(names(KendallResult))
KendallResult.clustered <- df.KendallResult %>% 
  left_join(clusterSelection, by = c('ClusterID' = 'ClusterID')) %>%
  filter(!is.na(S)) %>%
  mutate(Trend = case_when(
    .$sl <= 0.05 & .$S < 0 ~ "Decreasing",
    .$sl <= 0.05 & .$S > 0 ~ "Increasing",
    .$sl > 0.05 ~ "No trend")
  ) %>%
  mutate(Trend = as.factor(Trend))
KendallResult.clustered$Trend <- factor(KendallResult.clustered$Trend, levels =  c("No trend", "Decreasing", "Increasing"))

fwrite(KendallResult.clustered, "output/trend_nitrite.csv")

plotKendallClasses(plotdata = KendallResult.clustered, parameterValue = "Nitrite", year = "(2007-2017)")
saveEuropeTrendMap("Nitrite")

# Ammonium Nitrogen (Winter) ------------------------------------------------------------
#   Parameters: [NH4-N]
#   Depth: <= 10
#   Period: Winter
#     January - March for stations within Baltic Sea east of 15 E
#     January - February for other stations
#   Aggregation Method: Arithmetric mean of mean by station and cluster per year

# Filter stations rows and columns --> ClusterID, StationID, Year, Depth, Temperature, Salinity, Ammonium
wk <- stationSamples_nut[Depth <= 10 & ifelse(SeaRegionID == 1 & Longitude > 15, Month >= 1 & Month <= 3, Month >= 1 & Month <= 2) & !is.na(Ammonium) & (AmmoniumQ != 3 & AmmoniumQ != 4), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year, Depth, Temperature, Salinity, Ammonium)]

# Calculate station annual average --> ClusterID, StationID, Year, MinDepth, MaxDepth, AvgTemperature, AvgSalinity, AvgAmmonium, MinAmmonium, MaxAmmonium, CountSamples
wk1 <- wk[, list(MinDepth = min(Depth), MaxDepth = max(Depth), AvgTemperature = mean(Temperature), AvgSalinity = mean(Salinity), AvgAmmonium = mean(Ammonium), MinAmmonium = min(Ammonium), MaxAmmonium = max(Ammonium), SampleCount = .N), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year)]

# Calculate cluster annual average --> ClusterID, Year, MinMinDepth, MaxMaxDepth, AvgAvgAvgTemperature, AvgAvgSalinity, AvgAvgAmmonium, MinMinAmmonium, MaxMaxAmmonium, SumCountSamples
wk2 <- wk1[, list(AvgLatitude = mean(Latitude), AvgLongitude = mean(Longitude), MinDepth = min(MinDepth), MaxDepth = max(MaxDepth), AvgTemperature = mean(AvgTemperature), AvgSalinity = mean(AvgSalinity), AvgAmmonium = mean(AvgAmmonium), MinAmmonium = min(MinAmmonium), MaxAmmonium = max(MaxAmmonium), SampleCount = sum(SampleCount)), list(SeaRegionID, ClusterID, Year)]

fwrite(wk2, "output/status_ammonium.csv")

# plot average status for last 5 years 
wk21 <- wk2[Year > 2012, list(Ammonium = mean(AvgAmmonium)), list(ClusterID, AvgLongitude, AvgLatitude)]
plotStatusMaps(bboxEurope, data = wk21, xlong = "AvgLongitude", ylat = "AvgLatitude", 
               parameterValue = "Ammonium", 
               invJet = F, 
               limits = "auto")
saveEuropeStatusMap(parameter = "Ammonium")

# trend analysis using Kendall test
yearcriteria <- wk2[Year>2006, unique(ClusterID)]
clusterSelection <- wk2[
  ClusterID %in% yearcriteria][
    , list(NrClustersPerYear = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), by = .(ClusterID, Year, SeaRegionID)][
      , .(NrYears = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), by = .(ClusterID, SeaRegionID)][
        NrYears >=5]
wk22 <- wk2[ClusterID %in% clusterSelection[[1]]]
l <- wk22 %>% as.data.frame() %>% split(.$ClusterID) 
timeserieslist <- lapply(
  l, function(x) xts::xts(x[,"AvgAmmonium"], order.by = as.Date(as.character(x[,"Year"]),format = "%Y")))
KendallResult <- lapply(timeserieslist, function(x) MannKendall(x))
df.KendallResult <- as.data.frame((matrix(unlist(list.flatten(KendallResult)), ncol  = 5, byrow = T)))
names(df.KendallResult) <- c("tau", "sl", "S", "D", "varS")
df.KendallResult$ClusterID <- as.integer(names(KendallResult))
KendallResult.clustered <- df.KendallResult %>% 
  left_join(clusterSelection, by = c('ClusterID' = 'ClusterID')) %>%
  filter(!is.na(S)) %>%
  mutate(Trend = case_when(
    .$sl <= 0.05 & .$S < 0 ~ "Decreasing",
    .$sl <= 0.05 & .$S > 0 ~ "Increasing",
    .$sl > 0.05 ~ "No trend")
  ) %>%
  mutate(Trend = as.factor(Trend))
KendallResult.clustered$Trend <- factor(KendallResult.clustered$Trend, levels =  c("No trend", "Decreasing", "Increasing"))

fwrite(KendallResult.clustered, "output/trend_ammonium.csv")

plotKendallClasses(plotdata = KendallResult.clustered, parameterValue = "Ammonium", year = "(2007-2017)")
saveEuropeTrendMap("Ammonium")

# Dissolved Inorganic Nitrogen - DIN (Winter) ----------------------------------
#   Parameters: [NO3-N] + [NO2-N] + [NH4-N]
#   Depth: <= 10
#   Period: Winter
#     January - March for stations within Baltic Sea east of 15 E
#     January - February for other stations
#   Aggregation Method: Arithmetric mean of mean by station and cluster per year

# Filter stations rows and columns --> ClusterID, StationID, Latitude, Longitude, Year, Depth, Temperature, Salinity, Nitrate, Nitrite, Ammonium
wk <- stationSamples_nut[Depth <= 10 & ifelse(SeaRegionID == 1 & Longitude > 15, Month >= 1 & Month <= 3, Month >= 1 & Month <= 2) & !is.na(Nitrate|Nitrite|Ammonium) & (NitrateQ != 3 & NitrateQ != 4 & NitriteQ != 3 & NitriteQ != 4 & AmmoniumQ != 3 & AmmoniumQ != 4), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year, Depth, Temperature, Salinity, Nitrate, Nitrite, Ammonium)]
coalesce <- function(x) if (all(is.na(x))) NA else sum(x, na.rm = TRUE)
wk$DIN <- apply(wk[, c("Nitrate", "Nitrite", "Ammonium")], 1, coalesce)

# Calculate station mean --> ClusterID, StationID, Latitude, Longitude, Year, MinDepth, MaxDepth, AvgTemperature, AvgSalinity, AvgDIN, MinDIN, MaxDIN, CountSamples
wk1 <- wk[, list(MinDepth = min(Depth), MaxDepth = max(Depth), AvgTemperature = mean(Temperature), AvgSalinity = mean(Salinity), AvgDIN = mean(DIN), MinDIN = min(DIN), MaxDIN = max(DIN), SampleCount = .N), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year)]

# Calculate cluster mean --> SeaRegionID, ClusterID, AvgLatitude, AvgLongitude, Year, MinMinDepth, MaxMaxDepth, AvgAvgAvgTemperature, AvgAvgSalinity, AvgAvgDIN, MinMinDIN, MaxMaxDIN, SumCountSamples
wk2 <- wk1[, list(AvgLatitude = mean(Latitude), AvgLongitude = mean(Longitude), MinDepth = min(MinDepth), MaxDepth = max(MaxDepth), AvgTemperature = mean(AvgTemperature), AvgSalinity = mean(AvgSalinity), AvgDIN = mean(AvgDIN), MinDIN = min(MinDIN), MaxDIN = max(MaxDIN), SampleCount = sum(SampleCount)), list(SeaRegionID, ClusterID, Year)]

fwrite(wk2, "output/status_din.csv")

# plot average status for last 5 years 
wk21 <- wk2[Year > 2012, list(DIN = mean(AvgDIN)), list(ClusterID, AvgLongitude, AvgLatitude)]
plotStatusMaps(bboxEurope, data = wk21, xlong = "AvgLongitude", ylat = "AvgLatitude", 
               parameterValue = "DIN", 
               invJet = F, 
               limits = "auto")
saveEuropeStatusMap(parameter = "DIN")

# trend analysis using Kendall test
yearcriteria <- wk2[Year>2006, unique(ClusterID)]
clusterSelection <- wk2[
  ClusterID %in% yearcriteria][
    , list(NrClustersPerYear = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), by = .(ClusterID, Year, SeaRegionID)][
      , .(NrYears = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), by = .(ClusterID, SeaRegionID)][
        NrYears >=5]
wk22 <- wk2[ClusterID %in% clusterSelection[[1]]]
l <- wk22 %>% as.data.frame() %>% split(.$ClusterID) 
timeserieslist <- lapply(
  l, function(x) xts::xts(x[,"AvgDIN"], order.by = as.Date(as.character(x[,"Year"]),format = "%Y")))
KendallResult <- lapply(timeserieslist, function(x) MannKendall(x))
df.KendallResult <- as.data.frame((matrix(unlist(list.flatten(KendallResult)), ncol  = 5, byrow = T)))
names(df.KendallResult) <- c("tau", "sl", "S", "D", "varS")
df.KendallResult$ClusterID <- as.integer(names(KendallResult))
KendallResult.clustered <- df.KendallResult %>% 
  left_join(clusterSelection, by = c('ClusterID' = 'ClusterID')) %>%
  filter(!is.na(S)) %>%
  mutate(Trend = case_when(
    .$sl <= 0.05 & .$S < 0 ~ "Decreasing",
    .$sl <= 0.05 & .$S > 0 ~ "Increasing",
    .$sl > 0.05 ~ "No trend")
  ) %>%
  mutate(Trend = as.factor(Trend))
KendallResult.clustered$Trend <- factor(KendallResult.clustered$Trend, levels =  c("No trend", "Decreasing", "Increasing"))

fwrite(KendallResult.clustered, "output/trend_din.csv")

plotKendallClasses(plotdata = KendallResult.clustered, parameterValue = "DIN", year = "(2007-2017)")
saveEuropeTrendMap("DIN")

# Total Nitrogen (Annual) ------------------------------------------------------
#   Parameters: [N]
#   Depth: <= 10
#   Period: Annual
#   Aggregation Method: Arithmetric mean of mean by station and cluster per year

# Filter stations rows and columns --> ClusterID, StationID, Year, Depth, Temperature, Salinity, TotalNitrogen
wk <- stationSamples_nut[Depth <= 10 & !is.na(TotalNitrogen) & (TotalNitrogenQ != 3 & TotalNitrogenQ != 4), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year, Depth, Temperature, Salinity, TotalNitrogen)]

# Calculate station annual average --> ClusterID, StationID, Year, MinDepth, MaxDepth, AvgTemperature, AvgSalinity, AvgTotalNitrogen, MinTotalNitrogen, MaxTotalNitrogen, CountSamples
wk1 <- wk[, list(MinDepth = min(Depth), MaxDepth = max(Depth), AvgTemperature = mean(Temperature), AvgSalinity = mean(Salinity), AvgTotalNitrogen = mean(TotalNitrogen), MinTotalNitrogen = min(TotalNitrogen), MaxTotalNitrogen = max(TotalNitrogen), SampleCount = .N), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year)]

# Calculate cluster annual average --> ClusterID, Year, MinMinDepth, MaxMaxDepth, AvgAvgAvgTemperature, AvgAvgSalinity, AvgAvgTotalNitrogen, MinMinTotalNitrogen, MaxMaxTotalNitrogen, SumCountSamples
wk2 <- wk1[, list(AvgLatitude = mean(Latitude), AvgLongitude = mean(Longitude), MinDepth = min(MinDepth), MaxDepth = max(MaxDepth), AvgTemperature = mean(AvgTemperature), AvgSalinity = mean(AvgSalinity), AvgTotalNitrogen = mean(AvgTotalNitrogen), MinTotalNitrogen = min(MinTotalNitrogen), MaxTotalNitrogen = max(MaxTotalNitrogen), SampleCount = sum(SampleCount)), list(SeaRegionID, ClusterID, Year)]

fwrite(wk2, "output/status_totalnitrogen.csv")

# plot average status for last 5 years 
wk21 <- wk2[Year > 2012, list(TotalNitrogen = mean(AvgTotalNitrogen)), list(ClusterID, AvgLongitude, AvgLatitude)]
plotStatusMaps(bboxEurope, data = wk21, xlong = "AvgLongitude", ylat = "AvgLatitude", 
               parameterValue = "TotalNitrogen", 
               invJet = F, 
               limits = "auto")
saveEuropeStatusMap(parameter = "TotalNitrogen")

# trend analysis using Kendall test
yearcriteria <- wk2[Year>2006, unique(ClusterID)]
clusterSelection <- wk2[
  ClusterID %in% yearcriteria][
    , list(NrClustersPerYear = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), by = .(ClusterID, Year, SeaRegionID)][
      , .(NrYears = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), by = .(ClusterID, SeaRegionID)][
        NrYears >=5]
wk22 <- wk2[ClusterID %in% clusterSelection[[1]]]
l <- wk22 %>% as.data.frame() %>% split(.$ClusterID) 
timeserieslist <- lapply(
  l, function(x) xts::xts(x[,"AvgTotalNitrogen"], order.by = as.Date(as.character(x[,"Year"]),format = "%Y")))
KendallResult <- lapply(timeserieslist, function(x) MannKendall(x))
df.KendallResult <- as.data.frame((matrix(unlist(list.flatten(KendallResult)), ncol  = 5, byrow = T)))
names(df.KendallResult) <- c("tau", "sl", "S", "D", "varS")
df.KendallResult$ClusterID <- as.integer(names(KendallResult))
KendallResult.clustered <- df.KendallResult %>% 
  left_join(clusterSelection, by = c('ClusterID' = 'ClusterID')) %>%
  filter(!is.na(S)) %>%
  mutate(Trend = case_when(
    .$sl <= 0.05 & .$S < 0 ~ "Decreasing",
    .$sl <= 0.05 & .$S > 0 ~ "Increasing",
    .$sl > 0.05 ~ "No trend")
  ) %>%
  mutate(Trend = as.factor(Trend))
KendallResult.clustered$Trend <- factor(KendallResult.clustered$Trend, levels =  c("No trend", "Decreasing", "Increasing"))

fwrite(KendallResult.clustered, "output/trend_totalnitrogen.csv")

plotKendallClasses(plotdata = KendallResult.clustered, parameterValue = "TotalNitrogen", year = "(2007-2017)")
saveEuropeTrendMap("TotalNitrogen")

# Phosphate Phosphorus / Dissolved Inorganic Phophorus - DIP (Winter) ---------------------
#   Parameters: [PO4]
#   Depth: <= 10
#   Period: Winter
#     January - March for stations within Baltic Sea east of 15 E
#     January - February for other stations
#   Aggregation Method: Arithmetric mean of mean by station and cluster per year

# Filter stations rows and columns --> ClusterID, StationID, Year, Depth, Temperature, Salinity, Phosphate
wk <- stationSamples_nut[Depth <= 10 & ifelse(SeaRegionID == 1 & Longitude > 15, Month >= 1 & Month <= 3, Month >= 1 & Month <= 2) & !is.na(Phosphate) & (PhosphateQ != 3 & PhosphateQ != 4), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year, Depth, Temperature, Salinity, Phosphate)]

# Calculate station annual average --> ClusterID, StationID, Year, MinDepth, MaxDepth, AvgTemperature, AvgSalinity, AvgPhosphate, MinPhosphate, MaxPhosphate, CountSamples
wk1 <- wk[, list(MinDepth = min(Depth), MaxDepth = max(Depth), AvgTemperature = mean(Temperature), AvgSalinity = mean(Salinity), AvgPhosphate = mean(Phosphate), MinPhosphate = min(Phosphate), MaxPhosphate = max(Phosphate), SampleCount = .N), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year)]

# Calculate cluster annual average --> ClusterID, Year, MinMinDepth, MaxMaxDepth, AvgAvgAvgTemperature, AvgAvgSalinity, AvgAvgPhosphate, MinMinPhosphate, MaxMaxPhosphate, SumCountSamples
wk2 <- wk1[, list(AvgLatitude = mean(Latitude), AvgLongitude = mean(Longitude), MinDepth = min(MinDepth), MaxDepth = max(MaxDepth), AvgTemperature = mean(AvgTemperature), AvgSalinity = mean(AvgSalinity), AvgPhosphate = mean(AvgPhosphate), MinPhosphate = min(MinPhosphate), MaxPhosphate = max(MaxPhosphate), SampleCount = sum(SampleCount)), list(SeaRegionID, ClusterID, Year)]

fwrite(wk2, "output/status_phosphate.csv")

# plot average status for last 5 years 
wk21 <- wk2[Year > 2012, list(Phosphate = mean(AvgPhosphate)), list(ClusterID, AvgLongitude, AvgLatitude)]
plotStatusMaps(bboxEurope, data = wk21, xlong = "AvgLongitude", ylat = "AvgLatitude", 
               parameterValue = "Phosphate", 
               invJet = F, 
               limits = "auto")
saveEuropeStatusMap(parameter = "Phosphate")

# trend analysis using Kendall test
yearcriteria <- wk2[Year>2006, unique(ClusterID)]
clusterSelection <- wk2[
  ClusterID %in% yearcriteria][
    , list(NrClustersPerYear = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), by = .(ClusterID, Year, SeaRegionID)][
      , .(NrYears = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), by = .(ClusterID, SeaRegionID)][
        NrYears >=5]
wk22 <- wk2[ClusterID %in% clusterSelection[[1]]]
l <- wk22 %>% as.data.frame() %>% split(.$ClusterID) 
timeserieslist <- lapply(
  l, function(x) xts::xts(x[,"AvgPhosphate"], order.by = as.Date(as.character(x[,"Year"]),format = "%Y")))
KendallResult <- lapply(timeserieslist, function(x) MannKendall(x))
df.KendallResult <- as.data.frame((matrix(unlist(list.flatten(KendallResult)), ncol  = 5, byrow = T)))
names(df.KendallResult) <- c("tau", "sl", "S", "D", "varS")
df.KendallResult$ClusterID <- as.integer(names(KendallResult))
KendallResult.clustered <- df.KendallResult %>% 
  left_join(clusterSelection, by = c('ClusterID' = 'ClusterID')) %>%
  filter(!is.na(S)) %>%
  mutate(Trend = case_when(
    .$sl <= 0.05 & .$S < 0 ~ "Decreasing",
    .$sl <= 0.05 & .$S > 0 ~ "Increasing",
    .$sl > 0.05 ~ "No trend")
  ) %>%
  mutate(Trend = as.factor(Trend))
KendallResult.clustered$Trend <- factor(KendallResult.clustered$Trend, levels =  c("No trend", "Decreasing", "Increasing"))

fwrite(KendallResult.clustered, "output/trend_phosphate.csv")

plotKendallClasses(plotdata = KendallResult.clustered, parameterValue = "Phosphate", year = "(2007-2017)")
saveEuropeTrendMap("Phosphate")

# Total Phosphorus (Annual) ----------------------------------------------------
#   Parameters: [P]
#   Depth: <= 10
#   Period: Annual
#   Aggregation Method: Arithmetric mean of mean by station and cluster per year

# Filter stations rows and columns --> ClusterID, StationID, Year, Depth, Temperature, Salinity, TotalPhosphorus
wk <- stationSamples_nut[Depth <= 10 & !is.na(TotalPhosphorus) & (TotalPhosphorusQ != 3 & TotalPhosphorusQ != 4), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year, Depth, Temperature, Salinity, TotalPhosphorus)]

# Calculate station annual average --> ClusterID, StationID, Year, MinDepth, MaxDepth, AvgTemperature, AvgSalinity, AvgTotalPhosphorus, MinTotalPhosphorus, MaxTotalPhosphorus, CountSamples
wk1 <- wk[, list(MinDepth = min(Depth), MaxDepth = max(Depth), AvgTemperature = mean(Temperature), AvgSalinity = mean(Salinity), AvgTotalPhosphorus = mean(TotalPhosphorus), MinTotalPhosphorus = min(TotalPhosphorus), MaxTotalPhosphorus = max(TotalPhosphorus), SampleCount = .N), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year)]

# Calculate cluster annual average --> ClusterID, Year, MinMinDepth, MaxMaxDepth, AvgAvgAvgTemperature, AvgAvgSalinity, AvgAvgTotalPhosphorus, MinMinTotalPhosphorus, MaxMaxTotalPhosphorus, SumCountSamples
wk2 <- wk1[, list(AvgLatitude = mean(Latitude), AvgLongitude = mean(Longitude), MinDepth = min(MinDepth), MaxDepth = max(MaxDepth), AvgTemperature = mean(AvgTemperature), AvgSalinity = mean(AvgSalinity), AvgTotalPhosphorus = mean(AvgTotalPhosphorus), MinTotalPhosphorus = min(MinTotalPhosphorus), MaxTotalPhosphorus = max(MaxTotalPhosphorus), SampleCount = sum(SampleCount)), list(SeaRegionID, ClusterID, Year)]

fwrite(wk2, "output/status_totalphosphorus.csv")

# plot average status for last 5 years 
wk21 <- wk2[Year > 2012, list(TotalPhosphorus = mean(AvgTotalPhosphorus)), list(ClusterID, AvgLongitude, AvgLatitude)]
plotStatusMaps(bboxEurope, data = wk21, xlong = "AvgLongitude", ylat = "AvgLatitude", 
               parameterValue = "TotalPhosphorus", 
               invJet = F, 
               limits = "auto")
saveEuropeStatusMap(parameter = "TotalPhosphorus")

# trend analysis using Kendall test
yearcriteria <- wk2[Year>2006, unique(ClusterID)]
clusterSelection <- wk2[
  ClusterID %in% yearcriteria][
    , list(NrClustersPerYear = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), by = .(ClusterID, Year, SeaRegionID)][
      , .(NrYears = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), by = .(ClusterID, SeaRegionID)][
        NrYears >=5]
wk22 <- wk2[ClusterID %in% clusterSelection[[1]]]
l <- wk22 %>% as.data.frame() %>% split(.$ClusterID) 
timeserieslist <- lapply(
  l, function(x) xts::xts(x[,"AvgTotalPhosphorus"], order.by = as.Date(as.character(x[,"Year"]),format = "%Y")))
KendallResult <- lapply(timeserieslist, function(x) MannKendall(x))
df.KendallResult <- as.data.frame((matrix(unlist(list.flatten(KendallResult)), ncol  = 5, byrow = T)))
names(df.KendallResult) <- c("tau", "sl", "S", "D", "varS")
df.KendallResult$ClusterID <- as.integer(names(KendallResult))
KendallResult.clustered <- df.KendallResult %>% 
  left_join(clusterSelection, by = c('ClusterID' = 'ClusterID')) %>%
  filter(!is.na(S)) %>%
  mutate(Trend = case_when(
    .$sl <= 0.05 & .$S < 0 ~ "Decreasing",
    .$sl <= 0.05 & .$S > 0 ~ "Increasing",
    .$sl > 0.05 ~ "No trend")
  ) %>%
  mutate(Trend = as.factor(Trend))
KendallResult.clustered$Trend <- factor(KendallResult.clustered$Trend, levels =  c("No trend", "Decreasing", "Increasing"))

fwrite(KendallResult.clustered, "output/trend_totalphosphorus.csv")

plotKendallClasses(plotdata = KendallResult.clustered, parameterValue = "TotalPhosphorus", year = "(2007-2017)")
saveEuropeTrendMap("TotalPhosphorus")

# Chlorophyll a (Summer) -------------------------------------------------------
#   Parameters: Chlorophyll a
#   Depth: <= 10
#   Period: Summer
#     June - September for stations within Baltic Sea north of 59 N
#     May - September for all other stations
#   Aggregation Method: Arithmetric mean of mean by station and cluster per year

# Filter stations rows and columns --> ClusterID, StationID, Latitude, Longitude, Year, Depth, Temperature, Salinity, Chlorophyll
wk <- stationSamples_nut[Depth <= 10 & ifelse(SeaRegionID == 1 & Latitude > 59, Month >= 6 & Month <= 9, Month >= 5 & Month <= 9) & !is.na(Chlorophyll) & (ChlorophyllQ != 3 & ChlorophyllQ != 4), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year, Depth, Temperature, Salinity, Chlorophyll)]

# Calculate station mean --> ClusterID, StationID, Latitude, Longitude, Year, MinDepth, MaxDepth, AvgTemperature, AvgSalinity, AvgChlorophyll, MinChlorophyll, MaxChlorophyll, CountSamples
wk1 <- wk[, list(MinDepth = min(Depth), MaxDepth = max(Depth), AvgTemperature = mean(Temperature), AvgSalinity = mean(Salinity), AvgChlorophyll = mean(Chlorophyll), MinChlorophyll = min(Chlorophyll), MaxChlorophyll = max(Chlorophyll), SampleCount = .N), list(SeaRegionID, ClusterID, StationID, Latitude, Longitude, Year)]

# Calculate cluster mean --> SeaRegionID, ClusterID, AvgLatitude, AvgLongitude, Year, MinMinDepth, MaxMaxDepth, AvgAvgAvgTemperature, AvgAvgSalinity, AvgAvgChlorophyll, MinMinChlorophyll, MaxMaxChlorophyll, SumCountSamples
wk2 <- wk1[, list(AvgLatitude = mean(Latitude), AvgLongitude = mean(Longitude), MinDepth = min(MinDepth), MaxDepth = max(MaxDepth), AvgTemperature = mean(AvgTemperature), AvgSalinity = mean(AvgSalinity), AvgChlorophyll = mean(AvgChlorophyll), MinChlorophyll = min(MinChlorophyll), MaxChlorophyll = max(MaxChlorophyll), SampleCount = sum(SampleCount)), list(SeaRegionID, ClusterID, Year)]

fwrite(wk2, "output/status_chlorophyll.csv")

# plot average status for last 5 years 
wk21 <- wk2[Year > 2012, list(Chlorophyll = mean(AvgChlorophyll)), list(ClusterID, AvgLongitude, AvgLatitude, SeaRegionID)]
plotStatusMaps(bboxEurope, data = wk21, xlong = "AvgLongitude", ylat = "AvgLatitude", 
               parameterValue = "Chlorophyll", 
               invJet = F, 
               limits = "auto")
saveEuropeStatusMap(parameter = "Chlorophyll")

# Plot chlorophyll values for all regions separately
regionsToPlot <- unique(wk21$SeaRegionID)
for(ii in seq(1:length(regionsToPlot))){
  plotRegionStatusMaps(bboxEurope, data = wk21, xlong = "AvgLongitude", ylat = "AvgLatitude", 
                       parameterValue = "Chlorophyll", 
                       invJet = F, 
                       limits = "auto",
                       region = regionsToPlot[ii])
  saveEuropeStatusMap(parameter = paste0("Chlorophyll_", regionsToPlot[ii]))
}

# trend analysis using Kendall test
yearcriteria <- wk2[Year>2006, unique(ClusterID)]
clusterSelection <- wk2[
  ClusterID %in% yearcriteria][
    , list(NrClustersPerYear = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), by = .(ClusterID, Year, SeaRegionID)][
      , .(NrYears = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), by = .(ClusterID, SeaRegionID)][
        NrYears >=5]
wk22 <- wk2[ClusterID %in% clusterSelection[[1]]]
l <- wk22 %>% as.data.frame() %>% split(.$ClusterID) 
timeserieslist <- lapply(
  l, function(x) xts::xts(x[,"AvgChlorophyll"], order.by = as.Date(as.character(x[,"Year"]),format = "%Y")))
KendallResult <- lapply(timeserieslist, function(x) MannKendall(x))
df.KendallResult <- as.data.frame((matrix(unlist(list.flatten(KendallResult)), ncol  = 5, byrow = T)))
names(df.KendallResult) <- c("tau", "sl", "S", "D", "varS")
df.KendallResult$ClusterID <- as.integer(names(KendallResult))
KendallResult.clustered <- df.KendallResult %>% 
  left_join(clusterSelection, by = c('ClusterID' = 'ClusterID')) %>%
  filter(!is.na(S)) %>%
  mutate(Trend = case_when(
    .$sl <= 0.05 & .$S < 0 ~ "Decreasing",
    .$sl <= 0.05 & .$S > 0 ~ "Increasing",
    .$sl > 0.05 ~ "No trend")
  ) %>%
  mutate(Trend = as.factor(Trend))
KendallResult.clustered$Trend <- factor(KendallResult.clustered$Trend, levels =  c("No trend", "Decreasing", "Increasing"))

fwrite(KendallResult.clustered, "output/trend_chlorophyll.csv")

plotKendallClasses(plotdata = KendallResult.clustered, parameterValue = "Chlorophyll", year = "(2007-2017)")
saveEuropeTrendMap("Chlorophyll")