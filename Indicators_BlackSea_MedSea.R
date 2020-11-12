rm(list = ls())

newData = FALSE
if(newData){source('~/OceanCSI/dataPreparation_v2.R')}


load("oceancsidata_sample_check.RData")
# Prepare for plotting
source("plotfunctions.r")
if(!dir.exists("output/BlackMedSea")){dir.create("output/BlackMedSea")} 

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

# Check with histogram what is measured most
sample_count <- data.frame(sumscol = colSums(!is.na(sampling_check[,10:20]))) 

sample_count[ "sort_sample" ] <- rownames(sample_count)

ggplot(sample_count, aes(x=sort_sample, y = sumscol)) +
  geom_bar(stat="identity") +
  labs(title = "Aantal monsters per parameter",
       subtitle = "Zwarte en Middellandse Zee",
       y = "Aantal monsters") +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x = element_blank())

ggsave(filename = "Aantal_monsters_per_para_ZW_MZ.png")

sampling_check3 <- sampling_check %>%
  mutate(`Measured parameter` = case_when(
    Oxygen > 0 | HydrogenSulphide > 0 ~ "Oxygen",
    Temperature > 0 | Salinity > 0 | Chlorophyll > 0 | TotalPhosphorus > 0 | Nitrate > 0 | 
      Nitrite > 0 | Ammonium > 0 | TotalPhosphorus > 0 | TotalNitrogen > 0 ~ "Remaining"
  )) %>%
  select(SampleID, Year, Latitude, Longitude, latitude_center, longitude_center, avgDepth, Depth, `Measured parameter`) %>%
  arrange(`Measured parameter`) %>%
  filter(!is.na(`Measured parameter`))

check2 <- sampling_check3 %>%
  group_by(`Measured parameter`) %>%
  summarise(count=n())

ggplot(check2, aes(x=`Measured parameter`, y = count)) +
  geom_bar(stat="identity") +
  labs(title = "Aantal monsters per parameter",
       subtitle = "Zwarte en Middellandse Zee",
       y = "Soort monsters") +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x = element_blank())

ggsave("Aantal_monster_per_soort.png")

#rm(sampling_check)

check3 <- sampling_check3 %>%
  filter(`Measured parameter` == "Oxygen") %>%
  group_by(Year) %>%
  summarise(count=n())

ggplot(check3, aes(x=Year, y = count)) +
  geom_bar(stat="identity") +
  labs(title = "Aantal zuurstofmonsters per jaar",
       subtitle = "Zwarte en Middellandse Zee",
       y = "Aantal monsters") +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x = element_blank())

ggsave("Aantal_zuurstofmonster_per_jaar.png")

sampling_check4 <- sampling_check3 %>%
  distinct(Year, latitude_center, longitude_center, `Measured parameter`, .keep_all = TRUE) %>%
  arrange(`Measured parameter`)

check4 <- sampling_check4 %>%
  group_by(`Measured parameter`) %>%
  summarise(count=n())

ggplot(check4, aes(x=`Measured parameter`, y = count)) +
  geom_bar(stat="identity") +
  labs(title = "Aantal monsters per parameter",
       subtitle = "Zwarte en Middellandse Zee",
       y = "Soort monsters") +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title.x = element_blank())

ggsave("Aantal_monster_per_soort_distinct_yr_lat_long.png")

# Plot maps
xxlim = c(bboxBSMS[1], bboxBSMS[3])
yylim = c(bboxBSMS[2], bboxBSMS[4])

ggplot(sampling_check4) + 
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "darkgrey", color = "black") +
  geom_point(data = subset(sampling_check4, `Measured parameter` == 'Remaining'), aes(longitude_center, latitude_center, color = `Measured parameter`, 
                             fill = `Measured parameter`), shape = 21, size = 0.75) +
  geom_point(data = subset(sampling_check4, `Measured parameter` == 'Oxygen'), aes(longitude_center, latitude_center, color = `Measured parameter`, 
                             fill = `Measured parameter`), shape = 21, size = 0.75) +
  coord_quickmap(xlim = xxlim, ylim = yylim) +
  labs(title = "Sampled parameters (1989-2017)",
       subtitle = "Black Sea and Mediterranean Sea") +
  guides(fill = guide_legend(override.aes = list(size=5))) +
  #scale_fill_gradientn(colours  = colorscale(7), guide = "colourbar", limits = limits) +
  theme_bw() + 
  theme(
    text = element_text(size = 15),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "right",
    axis.line = element_blank(),
    axis.ticks = element_blank())

ggsave(filename = "Aantal_monsters_per_para_ZW_MZ_map_distinct_yr_lat_long.png")

sampling_check5a <- sampling_check4 %>%
  filter(`Measured parameter` == "Oxygen")

ggplot(sampling_check5) + 
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "darkgrey", color = "black") +
  geom_point(aes(longitude_center, latitude_center, color = `Measured parameter`, 
                 fill = `Measured parameter`), shape = 21, size = 0.75) +
  coord_quickmap(xlim = xxlim, ylim = yylim) +
  labs(title = "Sampled parameters (1989-2017)",
       subtitle = "Black Sea and Mediterranean Sea") +
  guides(fill = guide_legend(override.aes = list(size=5))) +
  #scale_fill_gradientn(colours  = colorscale(7), guide = "colourbar", limits = limits) +
  theme_bw() + 
  theme(
    text = element_text(size = 15),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "right",
    axis.line = element_blank(),
    axis.ticks = element_blank())

ggsave(filename = "Monsters_zonder_zuurstof_ZW_MZ_map.png")

# Check Depth of parameters --------------------------------------------------------
rm(list = ls())

newData = FALSE
if(newData){source('~/OceanCSI/dataPreparation_v2.R')}


load("oceancsidata_oxygen.RData")
# Prepare for plotting
source("plotfunctions.r")
if(!dir.exists("output/BlackMedSea")){dir.create("output/BlackMedSea")} 

# reduce data quantity
DO_samples <- stationSamples_oxy[(!is.na(Oxygen) | ! is.na(HydrogenSulphide)) &
                                   (OxygenQ != 3 & OxygenQ != 4 | HydrogenSulphideQ != 3 & HydrogenSulphideQ != 4),
                                 list(SampleID, StationID, Year, Month, Day, Hour, Minute, Longitude, Latitude, longitude_center, 
                                      latitude_center, SeaRegionID, ClusterID, DataSourceID, UTM_E, UTM_N, Depth, Temperature,
                                      Salinity, Oxygen, HydrogenSulphide, avgDepth)]

# Records left: +/- 18.000.000
rm(stationSamples_oxy)

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

DO_samples_remain <- anti_join(DO_samples, DO_samples_summer)

DO_samples_summer <- DO_samples_summer %>%
  mutate(Oxygen = case_when(
    !is.na(Oxygen) ~ Oxygen/0.7,  # convert ml/l to mg/l  http://www.ices.dk/marine-data/tools/pages/unit-conversions.aspx <http://www.ices.dk/marine-data/tools/pages/unit-conversions.aspx> 
    is.na(Oxygen) & !is.na(HydrogenSulphide) ~ -HydrogenSulphide*0.022391/0.7 # convert umol/l via ml/l to mg/l
  )) %>%
  mutate(diff_depth = avgDepth - Depth) %>% 
  as.data.table()

DO_samples_remain <- DO_samples_remain %>%
  mutate(Oxygen = case_when(
    !is.na(Oxygen) ~ Oxygen/0.7,  # convert ml/l to mg/l  http://www.ices.dk/marine-data/tools/pages/unit-conversions.aspx <http://www.ices.dk/marine-data/tools/pages/unit-conversions.aspx> 
    is.na(Oxygen) & !is.na(HydrogenSulphide) ~ -HydrogenSulphide*0.022391/0.7 # convert umol/l via ml/l to mg/l
  )) %>%
  mutate(diff_depth = avgDepth - Depth) %>% 
  as.data.table()

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

# plot average status for last 5 years 
wk21 <- mean25perc[Year > 2012, list(Oxygen = mean(AvgOxygen)), list(ClusterID, AvgLongitude, AvgLatitude)]

plotStatusMaps(bboxEurope, data = wk21, xlong = "AvgLongitude", ylat = "AvgLatitude", 
               parameterValue = "Oxygen", unit = " (mg/L)", Year = "2013-2019",
               invJet = T, 
               limits = "auto")
#saveEuropeStatusMap(parameter = "Oxygen", Year = "2013_2017", region = "")

wk21a <- mean25perc[Year > 1988, list(Oxygen = mean(AvgOxygen)), list(ClusterID, AvgLongitude, AvgLatitude)]

plotStatusMaps(bboxEurope, data = wk21a, xlong = "AvgLongitude", ylat = "AvgLatitude", 
               parameterValue = "Oxygen", unit = " (mg/L)", Year = "1989-2019",
               invJet = T, 
               limits = "auto")
#saveEuropeStatusMap(parameter = "Oxygen", Year = "1989_2017", region = "")

Q25all_r <- DO_samples_remain[, .(q25 = quantile(.SD, 0.25, na.rm = T)), by = c("Year", "ClusterID", "SeaRegionID")]

# Calculate mean of lower quartile 
mean25perc_r <- DO_samples_remain %>% 
  left_join(Q25all_r) %>% 
  filter(Oxygen <= q25) %>%
  group_by(Year, ClusterID, SeaRegionID) %>%
  summarize(AvgOxygen = mean(Oxygen),
            AvgLatitude = mean(latitude_center),
            AvgLongitude = mean(longitude_center),
            AvgavgDepth = mean(avgDepth),
            AvgDepth = mean(Depth)) %>%
  #distinct(Year,ClusterID,SeaRegionID, .keep_all = TRUE) %>%
  as.data.table()

# plot average status for last 5 years 
wk21_r <- mean25perc_r[Year > 2012 & AvgOxygen > -10, list(Oxygen = mean(AvgOxygen)), list(ClusterID, AvgLongitude, AvgLatitude)]

plotStatusMaps(bboxEurope, data = wk21_r, xlong = "AvgLongitude", ylat = "AvgLatitude", 
               parameterValue = "Oxygen", unit = " (mg/L)", Year = "2013-2019",
               invJet = T, 
               limits = "auto")
saveEuropeStatusMap(parameter = "Oxygen", Year = "2013_2019", region = "")

wk21a_r <- mean25perc_r[Year > 1988 & AvgOxygen > -10, list(Oxygen = mean(AvgOxygen)), list(ClusterID, AvgLongitude, AvgLatitude)]

plotStatusMaps(bboxEurope, data = wk21a_r, xlong = "AvgLongitude", ylat = "AvgLatitude", 
               parameterValue = "Oxygen", unit = " (mg/L)", Year = "1989-2019",
               invJet = T, 
               limits = "auto")
saveEuropeStatusMap(parameter = "Oxygen", Year = "1989_2019", region = "")

# Check for consecutive years -------------------------------------------------------
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
yearcrit <- mean25perc[Year > 1989, unique(ClusterID)]
clusterSel <- mean25perc2[ClusterID %in% yearcrit][
  , list(NrClustersPerYear = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), 
  by = .(ClusterID, Year, SeaRegionID)][
    , .(NrYears = .N), by = .(ClusterID, AvgLatitude, AvgLongitude, SeaRegionID)]
wkt <- mean25perc[ClusterID %in% clusterSel$ClusterID]

countSR <- wkt %>%
  group_by(SeaRegionID) %>%
  summarise(count=n())

hist(clusterSel$NrYears)

setkey(clusterSel, ClusterID)
setkey(wkt, ClusterID)

mean25perc_sel2 <- wkt[clusterSel]
mean25perc_sel3 <- mean25perc_sel2 %>% distinct(ClusterID, .keep_all = TRUE) %>% as.data.frame()

hist(mean25perc_sel3$Year)

mean25perc_sel4 <- mean25perc_sel3 %>% mutate(`Number of years` = 
                                                case_when(NrYears < 5 ~ "< 5 years", NrYears >= 5 ~ "> 5 years")) %>%
  arrange(desc(`Number of years`)) %>%
  as.data.frame()
countSR_nry <- mean25perc_sel4 %>%
  group_by(SeaRegionID, `Number of years`) %>%
  summarise(count=n())
fwrite(countSR_nry, "output/countSR_nry_v1.csv")
xxlim = c(bboxEurope[1], bboxEurope[3])
yylim = c(bboxEurope[2], bboxEurope[4])

cons_5_min <- mean25perc_sel4 %>%
  filter(`Number of years` == "< 5 years") %>%
  as.data.frame()

cons_5_plus <- mean25perc_sel4 %>%
  filter(`Number of years` == "> 5 years") %>%
  as.data.frame()

ggplot() +
  geom_polygon(data = world, aes(long, lat, group = group), fill = "darkgrey", color = "black") +
  #geom_point(data = mean25perc_sel4, aes(i.AvgLongitude, i.AvgLatitude, 
  #                                       color = `Number of years`, group = ClusterID), size = 1) +
  geom_point(data = subset(mean25perc_sel4, `Number of years` == "< 5 years"),
             aes(i.AvgLongitude, i.AvgLatitude, color = `Number of years`, 
            fill = `Number of years`, group = ClusterID), shape = 21, size = 1) +
  geom_point(data = subset(mean25perc_sel4, `Number of years` == "> 5 years"),
             aes(i.AvgLongitude, i.AvgLatitude, color = `Number of years`, 
                 fill = `Number of years`, group = ClusterID), shape = 21, size = 1.25) +
    #scale_fill_gradient(low = "blue", high = "red") +
  coord_quickmap(xlim = xxlim, ylim = yylim) +
  ggtitle(paste("Number of years in which oxygen was measured (after 1989)")) +
  theme_bw() + 
  theme(
    text = element_text(size = 15),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "right",
    axis.line = element_blank(),
    axis.ticks = element_blank())

ggsave("Number of years after 1989.png")

# Calculated for larger clusters some parameters ----------------------------------
rm(list = ls())
load("oceancsidata_oxygen_v2.RData")
# Prepare for plotting
source("plotfunctions.r")
if(!dir.exists("output/BlackMedSea")){dir.create("output/BlackMedSea")} 
# reduce data quantity
DO_samples <- stationSamples_oxy[(!is.na(Oxygen) | ! is.na(HydrogenSulphide)) &
                                   (OxygenQ != 3 & OxygenQ != 4 | HydrogenSulphideQ != 3 & HydrogenSulphideQ != 4),
                                 list(SampleID, StationID, Year, Month, Day, Hour, Minute, Longitude, Latitude, longitude_center, 
                                      latitude_center, SeaRegionID, ClusterID, DataSourceID, UTM_E, UTM_N, Depth, Temperature,
                                      Salinity, Oxygen, HydrogenSulphide, avgDepth)]
# Records left: +/- 18.000.000
rm(stationSamples_oxy)
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
rm(DO_samples)
DO_samples_summer <- DO_samples_summer %>%
  mutate(Oxygen = case_when(
    !is.na(Oxygen) ~ Oxygen/0.7,  # convert ml/l to mg/l  http://www.ices.dk/marine-data/tools/pages/unit-conversions.aspx <http://www.ices.dk/marine-data/tools/pages/unit-conversions.aspx> 
    is.na(Oxygen) & !is.na(HydrogenSulphide) ~ -HydrogenSulphide*0.022391/0.7 # convert umol/l via ml/l to mg/l
  )) %>%
  mutate(diff_depth = avgDepth - Depth) %>% 
  as.data.table()
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
wk21a <- mean25perc[Year > 1988, list(Oxygen = mean(AvgOxygen)), list(ClusterID, AvgLongitude, AvgLatitude)]
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
yearcrit <- mean25perc[Year > 1989, unique(ClusterID)]
clusterSel <- mean25perc2[ClusterID %in% yearcrit][
  , list(NrClustersPerYear = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), 
  by = .(ClusterID, Year, SeaRegionID)][
    , .(NrYears = .N), by = .(ClusterID, AvgLatitude, AvgLongitude, SeaRegionID)]
wkt <- mean25perc[ClusterID %in% clusterSel$ClusterID]
setkey(clusterSel, ClusterID)
setkey(wkt, ClusterID)
mean25perc_sel2 <- wkt[clusterSel]
mean25perc_sel3 <- mean25perc_sel2 %>% distinct(ClusterID, .keep_all = TRUE) %>% as.data.frame()
mean25perc_sel4 <- mean25perc_sel3 %>% mutate(`Number of years` = 
                                                case_when(NrYears < 5 ~ "< 5 years", NrYears >= 5 ~ "> 5 years")) %>%
  arrange(desc(`Number of years`)) %>%
  as.data.frame()
countSR_nry <- mean25perc_sel4 %>%
  group_by(SeaRegionID, `Number of years`) %>%
  summarise(count=n())
fwrite(countSR_nry, "output/countSR_nry_v2.csv")
cons_5_min_v2 <- mean25perc_sel4 %>%
  filter(`Number of years` == "< 5 years") %>%
  as.data.frame()
cons_5_plus_v2 <- mean25perc_sel4 %>%
  filter(`Number of years` == "> 5 years") %>%
  as.data.frame()

# Cluster v3
rm(list = ls())
load("oceancsidata_oxygen_v3.RData")
# Prepare for plotting
source("plotfunctions.r")
if(!dir.exists("output/BlackMedSea")){dir.create("output/BlackMedSea")} 
# reduce data quantity
DO_samples <- stationSamples_oxy[(!is.na(Oxygen) | ! is.na(HydrogenSulphide)) &
                                   (OxygenQ != 3 & OxygenQ != 4 | HydrogenSulphideQ != 3 & HydrogenSulphideQ != 4),
                                 list(SampleID, StationID, Year, Month, Day, Hour, Minute, Longitude, Latitude, longitude_center, 
                                      latitude_center, SeaRegionID, ClusterID, DataSourceID, UTM_E, UTM_N, Depth, Temperature,
                                      Salinity, Oxygen, HydrogenSulphide, avgDepth)]
# Records left: +/- 18.000.000
rm(stationSamples_oxy)
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
rm(DO_samples)
DO_samples_summer <- DO_samples_summer %>%
  mutate(Oxygen = case_when(
    !is.na(Oxygen) ~ Oxygen/0.7,  # convert ml/l to mg/l  http://www.ices.dk/marine-data/tools/pages/unit-conversions.aspx <http://www.ices.dk/marine-data/tools/pages/unit-conversions.aspx> 
    is.na(Oxygen) & !is.na(HydrogenSulphide) ~ -HydrogenSulphide*0.022391/0.7 # convert umol/l via ml/l to mg/l
  )) %>%
  mutate(diff_depth = avgDepth - Depth) %>% 
  as.data.table()
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
wk21a <- mean25perc[Year > 1988, list(Oxygen = mean(AvgOxygen)), list(ClusterID, AvgLongitude, AvgLatitude)]
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
yearcrit <- mean25perc[Year > 1989, unique(ClusterID)]
clusterSel <- mean25perc2[ClusterID %in% yearcrit][
  , list(NrClustersPerYear = .N, AvgLatitude = mean(AvgLatitude), AvgLongitude = mean(AvgLongitude)), 
  by = .(ClusterID, Year, SeaRegionID)][
    , .(NrYears = .N), by = .(ClusterID, AvgLatitude, AvgLongitude, SeaRegionID)]
wkt <- mean25perc[ClusterID %in% clusterSel$ClusterID]
setkey(clusterSel, ClusterID)
setkey(wkt, ClusterID)
mean25perc_sel2 <- wkt[clusterSel]
mean25perc_sel3 <- mean25perc_sel2 %>% distinct(ClusterID, .keep_all = TRUE) %>% as.data.frame()
mean25perc_sel4 <- mean25perc_sel3 %>% mutate(`Number of years` = 
                                                case_when(NrYears < 5 ~ "< 5 years", NrYears >= 5 ~ "> 5 years")) %>%
  arrange(desc(`Number of years`)) %>%
  as.data.frame()
countSR_nry <- mean25perc_sel4 %>%
  group_by(SeaRegionID, `Number of years`) %>%
  summarise(count=n())
fwrite(countSR_nry, "output/countSR_nry_v3.csv")
cons_5_min_v3 <- mean25perc_sel4 %>%
  filter(`Number of years` == "< 5 years") %>%
  as.data.frame()
cons_5_plus_v3 <- mean25perc_sel4 %>%
  filter(`Number of years` == "> 5 years") %>%
  as.data.frame()