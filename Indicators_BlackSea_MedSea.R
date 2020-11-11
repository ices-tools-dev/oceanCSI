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
sample_count <- data.frame(sumscol = colSums(!is.na(sampling_check[,2:12]))) 

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

sampling_check2 <- sampling_check %>%
  mutate(Temp_let = case_when(
    Temperature > 0 ~ "T",
    TRUE ~ "NA"
  )) %>%
  mutate(Sal_let = case_when(
    Salinity > 0 ~ "S",
    TRUE ~ "NA"
  )) %>%
  mutate(Oxy_let = case_when(
    Oxygen > 0 ~ "O",
    TRUE ~ "NA"
  )) %>%
  mutate(Chl_let = case_when(
    Chlorophyll > 0 ~ "Chl",
    TRUE ~ "NA"
  )) %>%
  mutate(Nut_let = case_when(
    TotalPhosphorus > 0 | Nitrate > 0 | Nitrite > 0 | Ammonium > 0 | TotalPhosphorus > 0 | TotalNitrogen > 0 ~ "Nut",
    TRUE ~ "NA"
  )) %>%
  mutate(sample = paste(Temp_let, Sal_let, Oxy_let, Chl_let, Nut_let)) %>%
  select(SampleID, Year, Latitude, Longitude, avgDepth, Depth, sample)

sampling_check2$sample <- str_replace_all(sampling_check2$sample, "NA", "")

head(sampling_check2)

check <- sampling_check2 %>%
  group_by(sample) %>%
  summarise(count=n())

sampling_check3 <- sampling_check %>%
  mutate(`Measured parameter` = case_when(
    Oxygen > 0 | HydrogenSulphide > 0 ~ "Oxygen",
    Temperature > 0 | Salinity > 0 | Chlorophyll > 0 | TotalPhosphorus > 0 | Nitrate > 0 | 
      Nitrite > 0 | Ammonium > 0 | TotalPhosphorus > 0 | TotalNitrogen > 0 ~ "Others"
  )) %>%
  select(SampleID, Year, Latitude, Longitude, avgDepth, Depth, `Measured parameter`) %>%
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
  distinct(Latitude, Longitude, avgDepth, `Measured parameter`, .keep_all = TRUE) %>%
  arrange(desc(`Measured parameter`))

# Plot maps
xxlim = c(bboxEurope[1], bboxEurope[3])
yylim = c(bboxEurope[2], bboxEurope[4])

ggplot(sampling_check4) + 
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "darkgrey", color = "black") +
  geom_point(shape = 21, aes(Longitude, Latitude, fill = `Measured parameter`), color = "white", size = 1) +
  coord_quickmap(xlim = xxlim, ylim = yylim) +
  ggtitle("Sort of samples") +
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

ggsave(filename = "Aantal_monsters_per_para_ZW_MZ_map.png")
