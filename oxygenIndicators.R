
rm(list = ls())

newData = FALSE
if(newData){source('~/OceanCSI/dataPreparation_v2.R')}


load("oceancsidata_oxygen.RData")
# Prepare for plotting
source("plotfunctions.r")
if(!dir.exists("output")){dir.create("output")} 

# reduce data quantity
DO_samples <- stationSamples_oxy[(!is.na(Oxygen) | ! is.na(HydrogenSulphide)) &
                               (OxygenQ != 3 & OxygenQ != 4 | HydrogenSulphideQ != 3 & HydrogenSulphideQ != 4),
                             list(SampleID, StationID, Year, Month, Day, Hour, Minute, Longitude, Latitude, longitude_center, 
                                  latitude_center, Sounding, SeaRegionID, ClusterID, DataSourceID, UTM_E, UTM_N, Depth, Temperature,
                                  Salinity, Oxygen, HydrogenSulphide, Sounding, avgDepth)]

# Records left: +/- 18.000.000
rm(stationSamples_oxy)

# ID	  Region	          SubRegion
# 1	    Baltic Sea	      Baltic Sea
# 2	    North-east        Atlantic Ocean	Greater North Sea	incl. the Kattegat and the English Channel
# 3	    North-east        Atlantic Ocean	Celtic Seas
# 4	    North-east        Atlantic Ocean	Bay of Biscay and the Iberian Coast
# 5	    North-east        Atlantic Ocean	Macaronesia
# 6	    Mediterranean Sea	Western Mediterranean Sea
# 7   	Mediterranean Sea	Adriatic Sea
# 8	    Mediterranean Sea	Ionian Sea and the Central Mediterranean Sea
# 9	    Mediterranean Sea	Aegean-Levantine Sea
# 10  	Black Sea	        Black Sea	
# 11  	Black Sea	        Sea of Marmara	
# 12	  Black Sea	        Sea of Azov

# DissolvedOxygen (Summer/Autumn) -----------------------------------------------------
#   Parameters: Dissolved Oxygen
#   Depth: <= 10 m above seafloor; Adapted to <= if(sounding < 100) 20 else 50
#   Period: July - October
#   Aggregation Method: mean of lower quartile by station and cluster per year
#   per class (<4, 4-6, >6) trend maps

# Raw dataset (no selections of depths)
DO_samples_summer_raw <- DO_samples[
  #SeaRegionID == seaRegionSelection &
  # (!is.na(Oxygen) | ! is.na(HydrogenSulphide)) &
  # (OxygenQ != 3 & OxygenQ != 4 | HydrogenSulphideQ != 3 & HydrogenSulphideQ != 4) &
  Depth <= Sounding &
    Year > 1989 &
    Month > 6 & Month < 11,
  list(SampleID, StationID, Year, Month, Day, Hour, Minute, Longitude, 
       Latitude, longitude_center, latitude_center, avgDepth, SeaRegionID, 
       ClusterID, DataSourceID, UTM_E, UTM_N, Depth, Temperature, Salinity, 
       Oxygen, HydrogenSulphide)]

DO_samples_summer_raw <- DO_samples_summer_raw %>%
  mutate(Oxygen = case_when(
    !is.na(Oxygen) ~ Oxygen/0.7,  # convert ml/l to mg/l  http://www.ices.dk/marine-data/tools/pages/unit-conversions.aspx <http://www.ices.dk/marine-data/tools/pages/unit-conversions.aspx> 
    is.na(Oxygen) & !is.na(HydrogenSulphide) ~ -HydrogenSulphide*0.022391/0.7 # convert umol/l via ml/l to mg/l
  )) %>%
  mutate(diff_depth = avgDepth - Depth) %>% 
  as.data.table()

# Leaves us with +/- 4.000.000 records

# Some old methods (using soundings)
#DO_samples_summer_old <- DO_samples[
#SeaRegionID == seaRegionSelection &
# (!is.na(Oxygen) | ! is.na(HydrogenSulphide)) &
# (OxygenQ != 3 & OxygenQ != 4 | HydrogenSulphideQ != 3 & HydrogenSulphideQ != 4) &
#  Depth <= Sounding &
#    case_when(
#      Sounding < 100 ~ Depth >= Sounding - 20,
#      Sounding >= 100 ~ Depth >= Sounding - 50) &
#    Year > 1989 &
#    Month > 6 & Month < 11,
#  list(SampleID, StationID, Year, Month, Day, Hour, Minute, Longitude, Latitude, longitude_center, 
#       latitude_center, Sounding, SeaRegionID, ClusterID, DataSourceID, UTM_E, UTM_N, Depth, 
#       Temperature, Salinity, Oxygen, HydrogenSulphide)]

# "old depth" (sounding) results in 840827 records

# Use new depth from EMODnet bathymetry
#DO_samples_summer_old2 <- DO_samples[
# (!is.na(Oxygen) | ! is.na(HydrogenSulphide)) &
#   (OxygenQ != 3 & OxygenQ != 4 | HydrogenSulphideQ != 3 & HydrogenSulphideQ != 4) &
#    Depth <= avgDepth &
#    case_when(
#      avgDepth < 100 ~ Depth >= avgDepth - 20,
#      avgDepth >= 100 ~ Depth >= avgDepth - 50) &
#    Year > 1989 &
#    Month > 6 & Month < 11,
#  list(SampleID, StationID, Year, Month, Day, Hour, Minute, Longitude, Latitude, longitude_center, 
#       latitude_center, avgDepth, SeaRegionID, ClusterID, DataSourceID, UTM_E, UTM_N, Depth, 
#       Temperature, Salinity, Oxygen, HydrogenSulphide)]

# "new depth" (avgDepth: from EMODnet bathmetry) results in 639326 records

DO_samples_summer <- DO_samples[
  Depth <= avgDepth &
    case_when(
      avgDepth < 100 ~ Depth >= avgDepth - 20,
      avgDepth >= 100 & avgDepth < 500 ~ Depth >= avgDepth - 50,
      avgDepth >= 500 ~ Depth >= avgDepth - 200) &
    Year > 1989 &
    Month > 6 & Month < 11,
  list(SampleID, StationID, Year, Month, Day, Hour, Minute, Longitude, 
       Latitude, longitude_center, latitude_center, avgDepth, SeaRegionID, 
       ClusterID, DataSourceID, UTM_E, UTM_N, Depth, Temperature, Salinity, 
       Oxygen, HydrogenSulphide)]

# modified new depth (higher tolerance when avgDepth between 100 and 500 or > 500 m): 687867 records

# Check number of samples per searegion
# DO_samples_summer %>% group_by(SeaRegionID) %>% summarize(timeRange = paste(range(Year)[1], "-", range(Year)[2]), nrOfSamples = n())

# Convert oxygen to mg/l and calculate difference between depth recorded and depth of side
DO_samples_summer <- DO_samples_summer %>%
  mutate(Oxygen = case_when(
    !is.na(Oxygen) ~ Oxygen/0.7,  # convert ml/l to mg/l  http://www.ices.dk/marine-data/tools/pages/unit-conversions.aspx <http://www.ices.dk/marine-data/tools/pages/unit-conversions.aspx> 
    is.na(Oxygen) & !is.na(HydrogenSulphide) ~ -HydrogenSulphide*0.022391/0.7 # convert umol/l via ml/l to mg/l
  )) %>%
  mutate(diff_depth = avgDepth - Depth) %>% 
  as.data.table()

hist(DO_samples_summer$diff_depth)


# Check raw data --------------------------------------------------------------------------

hist(DO_samples_summer_raw$diff_depth)

# Calculate 25 percentile per cluster and year
Q25all_raw <- DO_samples_summer_raw[, .(q25 = quantile(.SD, 0.25, na.rm = T)), by = c("Year", "ClusterID", "SeaRegionID")]

# Calculate mean of lower quartile 
mean25perc_raw <- DO_samples_summer_raw %>% 
  left_join(Q25all_raw) %>% 
  filter(Oxygen <= q25) %>%
  group_by(Year, ClusterID, SeaRegionID, UTM_E, UTM_N) %>%
  summarize(AvgOxygen = mean(Oxygen),
            AvgLatitude = mean(latitude_center),
            AvgLongitude = mean(longitude_center),
            AvgavgDepth = mean(avgDepth),
            AvgDepth = mean(Depth)) %>%
  as.data.table()

# plot average status for last 5 years 
wk21_raw_old <- mean25perc_raw[Year > 1989 | Year <= 2012, list(Oxygen = mean(AvgOxygen)), list(ClusterID, AvgLongitude, AvgLatitude)]
wk21_raw <- mean25perc_raw[Year > 2012, list(Oxygen = mean(AvgOxygen)), list(ClusterID, AvgLongitude, AvgLatitude)]

plotStatusMaps(bboxEurope, data = wk21_raw_old, xlong = "AvgLongitude", ylat = "AvgLatitude", 
               parameterValue = "Oxygen", invJet = T, limits = "auto")

plotStatusMaps(bboxEurope, data = wk21_raw, xlong = "AvgLongitude", ylat = "AvgLatitude", 
               parameterValue = "Oxygen", invJet = T, limits = "auto")

# Black Sea and Mediterranean Sea ----------------------------------------------------------

# Select these regions based on seaRegion number (see above)
seaRegionSelection1 <- c(10,11,12) # Black Sea
seaRegionSelection2 <- c(6,7,8,9) # Mediterranean Sea
seaRegionSelection3 <- c(6,7,8,9,10,11,12)

# First check the raw data for
# Black Sea 
DO_samples_BS <- DO_samples_summer_raw %>% 
  filter(SeaRegionID %in% c(seaRegionSelection1))

plotStatusMaps(bboxEurope, data = DO_samples_BS, xlong = "Longitude", ylat = "Latitude", 
               parameterValue = "Oxygen", 
               invJet = T, 
               limits = "auto")

hist(DO_samples_BS$Depth)
hist(DO_samples_BS$avgDepth)
hist(DO_samples_BS$diff_depth)
hist(DO_samples_BS$Year)

# Calculate 25 percentile per cluster and year
Q25all_BS <- DO_samples_BS[, .(q25 = quantile(.SD, 0.25, na.rm = T)), by = c("Year", "ClusterID", "SeaRegionID")]
Q25allBS <- DO_samples_BS[, .(q25 = quantile(.SD, 0.25, na.rm = T)), by = c("Year", "ClusterID", "SeaRegionID")]

# Calculate mean of lower quartile 
mean25perc_BS<- DO_samples_BS %>% 
  left_join(Q25all_BS) %>% 
  filter(Oxygen <= q25) %>%
  group_by(Year, ClusterID, SeaRegionID, UTM_E, UTM_N) %>%
  summarize(AvgOxygen = mean(Oxygen),
            AvgLatitude = mean(latitude_center),
            AvgLongitude = mean(longitude_center),
            AvgavgDepth = mean(avgDepth),
            AvgDepth = mean(Depth)) %>%
  as.data.table()

# plot average status for last 5 years 
wk21 <- mean25perc[Year > 2012, list(Oxygen = mean(AvgOxygen)), list(ClusterID, AvgLongitude, AvgLatitude)]

plotStatusMaps(bboxEurope, data = wk21, xlong = "AvgLongitude", ylat = "AvgLatitude", 
               parameterValue = "Oxygen", 
               invJet = T, 
               limits = "auto")

# Plot the frequence of differences
ggplot(DO_samples_BS,aes(x=Depth, y=avgDepth)) +
  ggtitle("Frequency of depth differences") +
  xlab("Sample depth (m)") +
  ylab("Depth at location (m)") +
  stat_bin_hex(colour="white", na.rm=TRUE) +
  scale_fill_gradientn(colours=c("purple","green"), 
                       trans = "log",
                       name = "Frequency",
                       #breaks = c(1,20,400,2000,10000,80000),
                       na.value=NA)

check_outliers1a <- DO_samples_BS %>%
  filter(diff_depth > -2000 | diff_depth < 2000 ) %>%
  filter(avgDepth < 100)

hist(check_outliers1a$avgDepth)

# Plot the frequence of differences
ggplot(check_outliers1a,aes(x=Depth, y=avgDepth)) +
  ggtitle("Frequency of depth differences") +
  xlab("Sample depth (m)") +
  ylab("Depth at location (m)") +
  stat_bin_hex(colour="white", na.rm=TRUE) +
  scale_fill_gradientn(colours=c("purple","green"), 
                       #trans = "log",
                       name = "Frequency",
                       #breaks = c(400,2000,10000),
                       na.value=NA)

check_outliers1b <- DO_samples_BS %>%
  filter(diff_depth > -2000 | diff_depth < 2000 )  

hist(check_outliers1b$avgDepth)

# Plot the frequence of differences
ggplot(check_outliers1b,aes(x=Depth, y=avgDepth)) +
  ggtitle("Frequency of depth differences") +
  xlab("Sample depth (m)") +
  ylab("Depth at location (m)") +
  stat_bin_hex(colour="white", na.rm=TRUE) +
  scale_fill_gradientn(colours=c("purple","green"), 
                       #trans = "log",
                       name = "Frequency",
                       #breaks = c(400,2000,10000),
                       na.value=NA)

check_outliers1c <- DO_samples_BS %>%
  filter(diff_depth > -2000 | diff_depth < 2000 )  

hist(check_outliers1c$avgDepth)

# Plot the frequence of differences
ggplot(check_outliers1c,aes(x=Depth, y=avgDepth)) +
  ggtitle("Frequency of depth differences") +
  xlab("Sample depth (m)") +
  ylab("Depth at location (m)") +
  stat_bin_hex(colour="white", na.rm=TRUE) +
  scale_fill_gradientn(colours=c("purple","green"), 
                       #trans = "log",
                       name = "Frequency",
                       #breaks = c(400,2000,10000),
                       na.value=NA)
check_outliers2 <- DO_samples_BS %>%
  filter(diff_depth < -2000 | diff_depth > 2000 )  

hist(check_outliers2$avgDepth)

# Plot the frequence of differences
ggplot(check_outliers2,aes(x=Depth, y=avgDepth)) +
  ggtitle("Frequency of depth differences") +
  xlab("Sample depth (m)") +
  ylab("Depth at location (m)") +
  stat_bin_hex(colour="white", na.rm=TRUE) +
  scale_fill_gradientn(colours=c("purple","green"), 
                       #trans = "log",
                       name = "Frequency",
                       #breaks = c(400,2000,10000),
                       na.value=NA)

# Preparation Trend analysis ----------------------------------------------------------

# Calculate 25 percentile per cluster and year
Q25all <- DO_samples_summer[, .(q25 = quantile(.SD, 0.25, na.rm = T)), by = c("Year", "ClusterID", "SeaRegionID")]

# Calculate mean of lower quartile 
mean25perc <- DO_samples_summer %>% 
  left_join(Q25all) %>% 
  filter(Oxygen <= q25) %>%
  group_by(Year, ClusterID, SeaRegionID, UTM_E, UTM_N) %>%
  summarize(AvgOxygen = mean(Oxygen),
            AvgLatitude = mean(latitude_center),
            AvgLongitude = mean(longitude_center),
            AvgavgDepth = mean(avgDepth),
            AvgDepth = mean(Depth)) %>%
  as.data.table()

# check value distribution
# hist(mean25perc[AvgOxygen < 0, list(AvgOxygen)]$AvgOxygen)

fwrite(mean25perc, "output/status_dissolvedoxygen.csv")

# Check number of clusters selected per searegion
# mean25perc %>% group_by(SeaRegionID) %>% summarize( timeRange = paste(range(Year)[1], "-", range(Year)[2]), nrOfClusters = n())

# plot average status for last 5 years 
wk21 <- mean25perc[Year > 2012, list(Oxygen = mean(AvgOxygen)), list(ClusterID, AvgLongitude, AvgLatitude)]

plotStatusMaps(bboxEurope, data = wk21, xlong = "AvgLongitude", ylat = "AvgLatitude", 
               parameterValue = "Oxygen", 
               invJet = T, 
               limits = "auto")
saveEuropeStatusMap(parameter = "Oxygen")


# trend analysis using Kendall test for each oxygen class
classes <- c("O2_4 mg_l", "4_O2_6 mg_l", "O2_6 mg_l")
prettyClassNames <- c("O2 < 4 mg/l", "4 < O2 < 6 mg/l", "O2 > 6 mg/l")

ID_class <- wk21 %>% mutate(
  class = case_when(
    Oxygen < 4 ~ 1,
    Oxygen >= 4 & Oxygen < 6 ~ 2,
    Oxygen >= 6 ~ 3
  )
) %>% select(ClusterID, class) %>% as.data.table()

# merge wk21 and class list
setkey(mean25perc, ClusterID)
setkey(ID_class, ClusterID)

mean25perc2 <- mean25perc[ID_class]

# Trend analysis I ----------------------------------------------------------

for(cc in seq(1:length(classes))){
  
  yearcriteria <- mean25perc2[Year>2006 & class == cc, unique(ClusterID)]
  
  clusterSelection <- mean25perc[ClusterID %in% yearcriteria][
    , list(NrClustersPerYear = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), by = .(ClusterID, Year, SeaRegionID)][
      , .(NrYears = .N), by = .(ClusterID, AvgLatitude, AvgLongitude, SeaRegionID)][NrYears >=5]
  hist(clusterSelection$NrYears)
  wk22 <- mean25perc[ClusterID %in% clusterSelection$ClusterID]
  l <- wk22 %>% as.data.frame() %>% split(.$ClusterID) 
  timeserieslist <- lapply(
    l, function(x) xts::xts(x[,"AvgOxygen"], order.by = as.Date(as.character(x[,"Year"]),format = "%Y")))
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
  
  
  fwrite(KendallResult.clustered, paste0("output/trend_dissolvedoxygen", classes[cc], ".csv"))
  
  #pretname <- prettyClassNames[cc]
  
  plotKendallClasses(plotdata = KendallResult.clustered,
                     parameterValue = "Oxygen", year ="(2006-2017)")
    saveEuropeTrendMap(paste("Oxygen", classes[cc]))
  
}

# Trend analysis II ----------------------------------------------------------
# BCPW: Hamed (2009) Bias Corrected Prewhitening (modifiedmk package)
#require(modifiedmk)

for(cc in seq(1:length(classes))){
  
  yearcriteria <- mean25perc2[Year>2006 & class == cc, unique(ClusterID)]
  
  clusterSelection <- mean25perc[ClusterID %in% yearcriteria][
    , list(NrClustersPerYear = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), by = .(ClusterID, Year, SeaRegionID)][
      , .(NrYears = .N), by = .(ClusterID, AvgLatitude, AvgLongitude, SeaRegionID)][NrYears >=5]
  hist(clusterSelection$NrYears)
  wk22 <- mean25perc[ClusterID %in% clusterSelection$ClusterID]
  l <- wk22 %>% as.data.frame() %>% split(.$ClusterID) 
  timeserieslist <- lapply(
    l, function(x) xts::xts(x[,"AvgOxygen"], order.by = as.Date(as.character(x[,"Year"]),format = "%Y")))
  
  df.timeserieslist <- as.data.frame((matrix(unlist(list.flatten(timeserieslist)), byrow = F)))
  
  # Apply BCPW
  BCPWResult <- lapply(timeserieslist, function(x) bcpw(as.vector(x)))
  df.BCPWResult <- as.data.frame((matrix(unlist(list.flatten(BCPWResult)), ncol  = 7, byrow = T)))
  names(df.BCPWResult) <- c("z-val", "pw_sl", "sl", "p-val", "S", "vars", "tau")
  df.BCPWResult$ClusterID <- as.integer(names(BCPWResult))
  BCPWResult.clustered <- df.BCPWResult %>% 
    left_join(clusterSelection, by = c('ClusterID' = 'ClusterID')) %>%
    filter(!is.na(S)) %>%
    mutate(Trend = case_when(
      .$`p-val` <= 0.05 & .$S < 0 ~ "Decreasing",
      .$`p-val` <= 0.05 & .$S > 0 ~ "Increasing",
      .$`p-val` <= 0.05 & .$S == 0 ~ "No trend",
      .$`p-val` > 0.05 ~ "No trend")
    ) %>%
    mutate(Trend = as.factor(Trend))
  BCPWResult.clustered$Trend <- factor(BCPWResult.clustered$Trend, 
                                       levels =  c("No trend", "Decreasing", "Increasing"))
  
  fwrite(KendallResult.clustered, paste0("output/trend_dissolvedoxygen_pw", classes[cc], ".csv"))
  
  plotKendallClasses(plotdata = BCPWResult.clustered, 
                     parameterValue = "Oxygen", year = "(2006-2017)")
  saveEuropeTrendMap(paste("PW_Oxygen", classes[cc]))
}
