require(data.table)
require(tidyverse)

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

#ID	Code	Description						envDomain	zoneType	spZoneType	Shape_Length	Shape_Area
#1	BAL	Baltic							            water	marineRegion	MSFDregion		860.717834064261	61.1764774372317
#2	BLA	Black Sea - sea of Azov					water	marineRegion	MSFDregion_part		46.8036300233879	4.64210461222669
#3	BLM	Black Sea - sea of Marmara				water	marineRegion	MSFDregion_part		16.6122037987906	1.2499968958581
#4	AMA	Macaronesia						                                              water	marineRegion	MSFDsubregion		129.858146721819	403.800809039279
#5	MAD	Adriatic Sea						water	marineRegion	MSFDsubregion		100.416051967312	15.3706242561274
#6	MAL	Aegean-Levantine Sea					water	marineRegion	MSFDsubregion		222.518364756116	74.7396360823146
#7	ABI	Bay of Biscay and the Iberian Coast			water	marineRegion	MSFDsubregion		105.591710393974	88.0366900404256
#8	ANS	Greater North Sea, incl. Kattegat + English Channel	water	marineRegion	MSFDsubregion		464.95134457305		95.2014264637056
#9	MIC	Ionian Sea and the Central Mediterranean Sea		water	marineRegion	MSFDsubregion		98.7583461236747	76.5746705808825
#10	MWE	Western Mediterranean Sea				water	marineRegion	MSFDsubregion		147.767877527397	88.633689073479
#11	BAR	Barents Sea						water	marineRegion	nonMSFDsea		741.733713175722	656.236971936288
#12	ICE	Iceland Sea						water	marineRegion	nonMSFDsea		152.984613720431	142.344800339171
#13	NOR	Norwegian Sea						water	marineRegion	nonMSFDsea		577.843816458465	220.728020578548
#14	WHI	White Sea						water	marineRegion	nonMSFDsea		52.7603238847586	17.8195239452825
#15	ACS	Celtic Seas						water	marineRegion	MSFDsubregion		358.016500849447	136.003803526643
#16	ACSo	Celtic Seas - overlapping submissions to UNCLOS 	water	marineRegion	MSFDsubregion_part	26.2033727812915	22.9006291957611
#17	ATL	North East Atlantic Ocean				water	marineRegion	MSFDregion_part		1094.91157549377	1541.80056043198
#18	BLK	Black Sea						water	marineRegion	MSFDregion_part		77.0331675675044	46.8110900817822

# DissolvedOxygen (Summer/Autumn) -----------------------------------------------------
#   Parameters: Dissolved Oxygen
#   Depth: <= 10 m above seafloor; Adapted to <= if(sounding < 100) 20 else 50
#   Period: July - October
#   Aggregation Method: mean of lower quartile by station and cluster per year
#   per class (<4, 4-6, >6) trend maps

#=== preparation for map plotting ==================
searegionFile <- "Input/EEA_SeaRegion_20180831.shp"

# Read shapefile
searegions <- sf::st_read(searegionFile)
searegionBSMS <- searegions %>%
  filter(Region %in% c("Black Sea", "Mediterranean Sea"))

# create bounding box for plotting on European scale
bboxEurope <- st_bbox(searegions)
bboxBSMS <- st_bbox(searegionBSMS)
rm(searegions, searegionBSMS)
xxlim = c(bboxEurope[1], bboxEurope[3])
yylim = c(bboxEurope[2], bboxEurope[4])

require(rworldmap)
data("countriesLow")
world <- fortify(countriesLow) 
rm(countriesLow)
#==================================================

#== sample selection based on EMODnet depth (avgDepth) =============
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
       ClusterID, DataSourceID, UTM_E, UTM_N, Depth, Sounding, Temperature, Salinity, 
       Oxygen, HydrogenSulphide)]

#== sample selection based on Sounding ====================
DO_samples_summer_old <- DO_samples[
  Depth <= Sounding &
    case_when(
      Sounding < 100 ~ Depth >= Sounding - 20,
      Sounding >= 100 & Sounding < 500 ~ Depth >= Sounding - 50,
      Sounding >= 500 ~ Depth >= Sounding - 200) &
    Year > 1989 &
    Month > 6 & Month < 11,
  list(SampleID, StationID, Year, Month, Day, Hour, Minute, Longitude, 
       Latitude, longitude_center, latitude_center, Sounding, SeaRegionID, 
       ClusterID, DataSourceID, UTM_E, UTM_N, Depth, avgDepth, Temperature, Salinity, 
       Oxygen, HydrogenSulphide)]


# overlapping
DO_samples_summer %>% 
  inner_join(DO_samples_summer_old, by = c(SampleID = "SampleID")) %>% dim()
#non-overlapping in new dataset
# 901234
1088779 - 901234 # 187545


DO_samples_summer %>% 
  anti_join(DO_samples_summer_old, by = c(SampleID = "SampleID")) %>% 
  inner_join(DO_samples_summer, by = c(SampleID = "SampleID")) %>%
  dim()
# 340008 new datapoints added



#==== diffplot ========================
DO_samples_summer %>% anti_join(DO_samples_summer_old, by = c(SampleID = "SampleID")) %>% select(SampleID, Sounding, avgDepth, longitude_center, latitude_center) %>%
  mutate(reason = case_when(
    is.na(avgDepth) ~ "Emodnet depth missing",
    is.na(Sounding) ~ "sounding missing",
    !is.na(Sounding) & !is.na(avgDepth) ~ "different criteria"
  )) %>% 
  ggplot(aes(longitude_center, latitude_center)) + 
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "darkgrey", color = "black") +
  geom_point(aes(color = reason), size = 1) +
  coord_quickmap(xlim = xxlim, ylim = yylim) +
  ggtitle(paste("")) +
  # scale_fill_gradientn(colours  = colorscale(7), guide = "colourbar", limits = limits) +
  theme_bw() + 
  theme(
    text = element_text(size = 15),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "right",
    axis.line = element_blank(),
    axis.ticks = element_blank())

#==== diffplot ========================
DO_samples_summer %>% anti_join(DO_samples_summer_old, by = c(SampleID = "SampleID")) %>% 
  select(SampleID, Year, Sounding, avgDepth, longitude_center, latitude_center) %>%
  mutate(reason = case_when(
    is.na(avgDepth) ~ "Emodnet depth missing",
    is.na(Sounding) ~ "sounding missing",
    !is.na(Sounding) & !is.na(avgDepth) ~ "different criteria"
  )) %>% 
  ggplot(aes(Year)) + 
  geom_histogram(aes(fill = reason))



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


# Preparation Trend analysis ----------------------------------------------------------

# Calculate 25 percentile per cluster and year
Q25all <- DO_samples_summer[, .(q25 = quantile(.SD, 0.25, na.rm = T)), by = c("Year", "ClusterID", "SeaRegionID")]

# Calculate mean of lower quartile 
mean25perc <- DO_samples_summer %>% 
  left_join(Q25all) %>% 
  filter(Oxygen <= q25) %>%
  group_by(Year, ClusterID, SeaRegionID) %>%
  summarize(AvgOxygen = mean(Oxygen),
            AvgLatitude = mean(latitude_center),
            AvgLongitude = mean(longitude_center),
            AvgavgDepth = mean(avgDepth),
            AvgDepth = mean(Depth)) %>%
  #distinct(Year,ClusterID,SeaRegionID, .keep_all = TRUE) %>%
  as.data.table()

# check value distribution
# hist(mean25perc[AvgOxygen < 0, list(AvgOxygen)]$AvgOxygen)

fwrite(mean25perc, "output/status_dissolvedoxygen.csv")

# Check number of clusters selected per searegion
# mean25perc %>% group_by(SeaRegionID) %>% summarize( timeRange = paste(range(Year)[1], "-", range(Year)[2]), nrOfClusters = n())

# plot average status for last 5 years 
wk21 <- mean25perc[Year > 2012, list(Oxygen = mean(AvgOxygen)), list(ClusterID, AvgLongitude, AvgLatitude)]

plotStatusMaps(bboxEurope, data = wk21, xlong = "AvgLongitude", ylat = "AvgLatitude", 
               parameterValue = "Oxygen", unit = " (mg/L)", Year = "2013-2019",
               invJet = T, 
               limits = "auto")
saveEuropeStatusMap(parameter = "Oxygen", Year = "2013_2019", region = "")

wk21a <- mean25perc[Year > 1988, list(Oxygen = mean(AvgOxygen)), list(ClusterID, AvgLongitude, AvgLatitude)]

plotStatusMaps(bboxEurope, data = wk21a, xlong = "AvgLongitude", ylat = "AvgLatitude", 
               parameterValue = "Oxygen", unit = " (mg/L)", Year = "1989-2019",
               invJet = T, 
               limits = "auto")
saveEuropeStatusMap(parameter = "Oxygen", Year = "1989_2019", region = "")

# trend analysis using Kendall test for each oxygen class
classes <- c("O2_4 mg_l", "4_O2_6 mg_l", "O2_6 mg_l")
prettyClassNames <- c("O2 < 4 mg/l", "4 < O2 < 6 mg/l", "O2 > 6 mg/l")

ID_class <- wk21a %>% 
  mutate(
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
yr = 2006

for(cc in seq(1:length(classes))){
  
  yearcriteria <- mean25perc2[Year>yr & class == cc, unique(ClusterID)]
  
  clusterSelection <- mean25perc[ClusterID %in% yearcriteria][
    , list(NrClustersPerYear = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), by = .(ClusterID, Year, SeaRegionID)][
      , .(NrYears = .N), by = .(ClusterID, AvgLatitude, AvgLongitude, SeaRegionID)][NrYears >=5]
  hist(clusterSelection$NrYears)
  wk22 <- mean25perc[ClusterID %in% clusterSelection$ClusterID]
  countSR <- wk22 %>%
    distinct(ClusterID, .keep_all = TRUE) %>%
    group_by(SeaRegionID) %>%
    summarise(count=n())
  fwrite(countSR, paste0("output/Count_SR/count_SR", classes[cc], "_ends_after_", yr, ".csv"))
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
  
  
  fwrite(KendallResult.clustered, paste0("output/trend_dissolvedoxygen", classes[cc], "_end_after_", yr, ".csv"))
  
  #pretname <- prettyClassNames[cc]
  
  plotKendallClasses(plotdata = KendallResult.clustered,
                     parameterValue = "Oxygen", Year ="(1989-2019)", end_trend = yr)
    saveEuropeTrendMap(paste("Oxygen", classes[cc]), Year = "1989-2019", yr)

}




