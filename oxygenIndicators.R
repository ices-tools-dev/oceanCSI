
rm(list = ls())

newData = FALSE
if(newData){source('~/OceanCSI/dataPreparation.R')}


load("oceancsidata.RData")
# Prepare for plotting
source("plotfunctions.r")
if(!dir.exists("output")){dir.create("output")} 

# reduce data quantity
DO_samples <- stationSamples[(!is.na(Oxygen) | ! is.na(HydrogenSulphide)) &
                               (OxygenQ != 3 & OxygenQ != 4 | HydrogenSulphideQ != 3 & HydrogenSulphideQ != 4),
                             list(SampleID, StationID, Year, Month, Day, Hour, Minute, Longitude, Latitude, longitude_center, latitude_center, Sounding, SeaRegionID, ClusterID, DataSourceID, UTM_E, UTM_N, Depth, Temperature, Salinity, Oxygen, HydrogenSulphide, Sounding, avgDepth)]

rm(stationSamples)

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

seaRegionSelection <- c(10,11,12) # Black Sea

# DissolvedOxygen (Summer/Autumn) -----------------------------------------------------
#   Parameters: Dissolved Oxygen
#   Depth: <= 10 m above seafloor; Adapted to <= if(sounding < 100) 20 else 50
#   Period: July - October
#   Aggregation Method: mean of lower quartile by station and cluster per year
#   per class (<4, 4-6, >6) trend maps

DO_samples_summer_old <- DO_samples[
  SeaRegionID == seaRegionSelection &
  # (!is.na(Oxygen) | ! is.na(HydrogenSulphide)) &
  # (OxygenQ != 3 & OxygenQ != 4 | HydrogenSulphideQ != 3 & HydrogenSulphideQ != 4) &
  Depth <= Sounding &
    case_when(
      Sounding < 100 ~ Depth >= Sounding - 20,
      Sounding >= 100 ~ Depth >= Sounding - 50) &
    Year > 1989 &
    Month > 6 & Month < 11,
  list(SampleID, StationID, Year, Month, Day, Hour, Minute, Longitude, Latitude, longitude_center, latitude_center, Sounding, SeaRegionID, ClusterID, DataSourceID, UTM_E, UTM_N, Depth, Temperature, Salinity, Oxygen, HydrogenSulphide)]

# "old depth" (sounding) results in 887109 records

# Filter stations rows and columns
DO_samples_summer <- DO_samples[
  # (!is.na(Oxygen) | ! is.na(HydrogenSulphide)) &
  #   (OxygenQ != 3 & OxygenQ != 4 | HydrogenSulphideQ != 3 & HydrogenSulphideQ != 4) &
    # Depth <= avgDepth &
    case_when(
      avgDepth < 100 ~ Depth >= avgDepth - 20,
      avgDepth >= 100 ~ Depth >= avgDepth - 50) &
    Year > 1989 &
    Month > 6 & Month < 11,
  list(SampleID, StationID, Year, Month, Day, Hour, Minute, Longitude, Latitude, longitude_center, latitude_center, avgDepth, SeaRegionID, ClusterID, DataSourceID, UTM_E, UTM_N, Depth, Temperature, Salinity, Oxygen, HydrogenSulphide)]

# "new depth" (avgDepth: from EMODnet bathmetry) results in 980369 records


DO_samples_summer2 <- stationSamples[(!is.na(Oxygen) | ! is.na(HydrogenSulphide)) &
                                       (OxygenQ != 3 & OxygenQ != 4 | HydrogenSulphideQ != 3 & HydrogenSulphideQ != 4) &
                                       Depth <= avgDepth &
                                       case_when(
                                         avgDepth < 100 ~ Depth >= avgDepth - 20,
                                         avgDepth >= 100 & avgDepth < 500 ~ Depth >= avgDepth - 50,
                                         avgDepth >= 500 ~ Depth >= avgDepth - 200) &
                                       Year > 1989 &
                                       Month > 6 & Month < 11,
                                     list(SampleID, StationID, Year, Month, Day, Hour, Minute, Longitude, Latitude, longitude_center, latitude_center, avgDepth, SeaRegionID, ClusterID, DataSourceID, UTM_E, UTM_N, Depth, Temperature, Salinity, Oxygen, HydrogenSulphide)]

# modified new depth (higher tolerance when avgDepth > 500 m): 1206743 records



DO_samples_summer <- DO_samples_summer

# Check number of samples per searegion
# DO_samples_summer %>% group_by(SeaRegionID) %>% summarize(timeRange = paste(range(Year)[1], "-", range(Year)[2]), nrOfSamples = n())

DO_samples_summer <- DO_samples_summer %>%
  mutate(Oxygen = case_when(
    !is.na(Oxygen) ~ Oxygen/0.7,  # convert ml/l to mg/l  http://www.ices.dk/marine-data/tools/pages/unit-conversions.aspx <http://www.ices.dk/marine-data/tools/pages/unit-conversions.aspx> 
    is.na(Oxygen) & !is.na(HydrogenSulphide) ~ -HydrogenSulphide*0.022391/0.7 # convert umol/l via ml/l to mg/l
  )
  ) %>% as.data.table()

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
    mutate(trend = case_when(
      .$sl <= 0.05 & .$S < 0 ~ "decreasing",
      .$sl <= 0.05 & .$S > 0 ~ "increasing",
      .$sl > 0.05 ~ "no trend")
    ) %>%
    mutate(trend = as.factor(trend))
  KendallResult.clustered$trend <- factor(KendallResult.clustered$trend, levels =  c("no trend", "decreasing", "increasing"))
  
  
  fwrite(KendallResult.clustered, paste0("output/trend_dissolvedoxygen", classes[cc], ".csv"))
  
  plotKendallClasses(plotdata = KendallResult.clustered, parameterValue = "Oxygen")
  saveEuropeTrendMap(paste("Oxygen", classes[cc]))
  
}

