
#call in the raw datasets:
rm(list=ls())

library(rgdal)
library(maptools)
library(raster)   ## To convert an "Extent" object to a "SpatialPolygons" object.
library(rgeos)
library(biomod2)
library(raster)
library(sp)
library(raster)
library(rgdal)
library(tidyverse)

#Call in maps as shape files
NAM <- readOGR("C:/Users/bar823/Documents/MGB_docs/Data_School/stats_proj/Stats_project/maps/NAM/NAM_adm1.shp", layer="NAM_adm1")
TZA <- readOGR("C:/Users/bar823/Documents/MGB_docs/Data_School/stats_proj/Stats_project/maps/TZA/TZA_adm2.shp", layer="TZA_adm2")
CAM <- readOGR("C:/Users/bar823/Documents/MGB_docs/Data_School/stats_proj/Stats_project/maps/CAM/CMR_adm2.shp", layer="CMR_adm2")
plot(NAM)
plot(TZA)
plot(CAM)
#names(NAM)
#print(NAM)
#TZA$NAME_2

#Call in the data files from FAO database
nam.df<-read_csv("C:/Users/bar823/Documents/MGB_docs/Data_School/stats_proj/Stats_project/Raw_Data/Namibia_Crops.csv")
tza.df<-read_csv("C:/Users/bar823/Documents/MGB_docs/Data_School/stats_proj/Stats_project/Raw_Data/Tanzania_Crops.csv")
cam.df<-read_csv("C:/Users/bar823/Documents/MGB_docs/Data_School/stats_proj/Stats_project/Raw_Data/Cameroon_Crops.csv")
nam.df
tza.df
cam.df
colnames(nam.df)

#Turn the differently formatted dates int eh different file sinto Julian day (day of year)
colnames(nam.df)  
colnames(nam.df)<-c("Country" , "AEZ" ,"Admin_areas","Ag_practices","Crop" , "Scientific_name" ,                
                "Botanical_family", "Other_names","Add_Info", "Planting_period_onset",         
                "Planting_period_end",    "Sowing_Planting_rate" ,"Sowing_Planting_rate_unit" ,  "Preferred_sowing_plantin_period",
                "Length_cropping_cycle" , "Harvesting_period_onset" ,"Harvesting_period_end", "Comments")
nam_juldays<-nam.df%>%
  mutate(Plant_onset_julday=as.POSIXlt(gsub('-', '',Planting_period_onset), format = "%d%b")$yday)%>%
  mutate(Plant_end_julday=as.POSIXlt(gsub('-', '',Planting_period_end), format = "%d%b")$yday)%>%
  mutate(Harvest_onset_julday=as.POSIXlt(gsub('-', '',Harvesting_period_onset), format = "%d%b")$yday)%>%
  mutate(Harvest_end_julday=as.POSIXlt(gsub('-', '',Harvesting_period_end), format = "%d%b")$yday)%>%
  select(-Planting_period_onset, -Planting_period_end, -Harvesting_period_onset, -Harvesting_period_end)

cam_tza_df<-rbind(tza.df,cam.df)
colnames(cam_tza_df)<-colnames(nam.df)
others_juldays<-cam_tza_df%>%
  mutate(Plant_onset_julday=as.POSIXlt(Planting_period_onset, format = "%d/%m")$yday)%>%
  mutate(Plant_end_julday=as.POSIXlt(Planting_period_end, format = "%d/%m")$yday)%>%
  mutate(Harvest_onset_julday=as.POSIXlt(Harvesting_period_onset, format = "%d/%m")$yday)%>%
  mutate(Harvest_end_julday=as.POSIXlt(Harvesting_period_end, format = "%d/%m")$yday)%>%
  select(-Planting_period_onset, -Planting_period_end, -Harvesting_period_onset, -Harvesting_period_end)


#Further clean some of the data, remove unwanted columns, and bind together as one
df<-rbind(nam_juldays,others_juldays)
df<-df%>%
  select(-Other_names, -Add_Info, -Preferred_sowing_plantin_period, -Comments)
  
###Question 0: Does one country have more zones than the others?
zone_num<-df%>%
  group_by(c(Country))%>%
  distinct(AEZ, .keep_all = TRUE)%>%
  summarise(Zone_number=n())
colnames(zone_num)<-c("Country", "Zone_number")
#ggplot at highest administration area - COUNTRY
ggplot(zone_num, aes(x=Country, y=Zone_number))+
  geom_bar(stat="identity", width = 0.4)+
  labs(y="Number of Agricultural Zones")

###QUESTION 1: Is there variation in the diversity of crops (total numbers) between the three countries
#Some of the regions have crops listed twice (for summer and winter seasons, so remove duplicates
tot_crop_num<-df%>%
  distinct(AEZ, Crop, .keep_all = TRUE)%>%
  group_by(c(Country))%>%
  summarise(Diversity=n())
colnames(tot_crop_num)<-c("Country", "C.Diversity")

#ggplot at highest administration area - COUNTRY
ggplot(tot_crop_num, aes(x=Country, y=C.Diversity))+
  geom_bar(stat="identity", width = 0.4)+
  labs(y="Crop Diversity")

#Cameroon has fewer crops followed by Namibia and Tanzania
#There is not much different between the diversity of crops grown in each county
regional_crop_num<-df%>%
  distinct(AEZ, Crop, .keep_all = TRUE)%>%
  group_by(c(AEZ))%>%
  summarise(Diversity=n())
colnames(regional_crop_num)<-c("AEZ", "R.Diversity")
reg_crop_num<-left_join(regional_crop_num, df, by="AEZ")%>%
  group_by(AEZ)
  

#ggplot diversity within each region in the country
ggplot(reg_crop_num, aes(x=Country, y=R.Diversity))+
  geom_boxplot() +
  labs(y="Diversity within Agricultural Zones")

##ggplot for the proportion of the total countries crops that are grown in each region
reg_crop_prop<-left_join(reg_crop_num, tot_crop_num, by="Country")%>%
  group_by(AEZ)
#ggplot diversity within each region in the country
ggplot(reg_crop_prop, aes(x=Country, y=R.Diversity))+
  geom_boxplot() +
  labs(y="Proportion of Total Crops grown in Agricultural Zones")


library(lmerTest)
library(emmeans)

m1<-glm(R.Diversity~Country, data=df3, family=poisson)
anova(m1)
summary(m1)
emmeans(m1, pairwise~Country)
plot(m1)#these look ok, except for the outlier in Tanzania
#regional diversity of the countries are all different from each other
#Cameroon>Namibia>Tanzania
df3$propotion.diversity<-(df3$R.Diversity/df3$C.Diversity)*100

ggplot(df3, aes(x=Country, y=propotion.diversity))+
  geom_boxplot() +
  labs(y="Proportion of Country's crops grown in region")

library(lmerTest)
library(emmeans)
str(df2)
m1<-glm(propotion.diversity~Country, data=df3, family=poisson)
anova(m1)
summary(m1)
emmeans(m1, pairwise~Country)
plot(m1)#these look ok, except for the outlier in Tanzania
#
#
#While cameroon has fewer zones (5 vs 9), and grows a smaller number of different crops;
#the crops are grown more evenly across the zones in comparison to Namibia and Tanz, 
#who's zones have relatively less diversity

#Agricultural practises: 
ggplot(df, aes(x=Country, y=Zone_number))+
  geom_boxplot()

###Question 2: Given the differences in regional diversity, what is driving these patterns?
#A) Climate driven - Climate 
#B) Socially driven - Cameroon farmers employ a greater range of methods than other two coutries
#C) Economonically driven - Is there a link between subsistence or commercial farming with crop diversity?
#D) Comination of two - Cameroon capitalises on longer/multiple growing seasons - if you count multi9ple season crops - higher diversity in Cameroon than the other two

#A-C required georeferencing information to extract climate/socieconomic data, so start with #D



#Climate and crop diversity - no georeferencing in the datasets, so need to link the regions(zones) to 
#pre-existing shape or raster files
#Namibia and the zones - already defined (I think)
#call in zones.csv and rasterize
library(raster)
zones<-read.csv("C:/Users/bar823/Documents/MGB_docs/Data_School/stats_proj/Stats_project/Raw_Data/AEZ16_CLAS--SSA.csv")
head(zones)
#check that these can be plotted:

#select out only Namibia from this file (other two don't have zones matched)
zone_NAM<-subset(zones, ISO3=="NAM")
head(zone_NAM); nrow(zone_NAM)
#turn the "AEZCLAS into a number (for the zone) - allocate a number to each zone (don't worry if it's correlct at the moment)
zns<-levels(zone_NAM$AEZ16_CLAS)
cods<-seq(1,length(zns),1)
zone_dict<-as.data.frame(cbind(zns,cods))

#generate another column in the namibia zone file to include the numeric zone code for mapping
for (i in (1:nrow(zone_NAM))){
  zone_NAM$zone_code[i]<-zone_dict$cods[zone_dict$zns==zone_NAM$AEZ16_CLAS[i]]
}
             
#plot the raster of the AEZs for Namibia
str(RDF)
library(raster)
z<-zone_NAM$zone_code
RR<-as.matrix(cbind(zone_NAM$Y, zone_NAM$X, z), header=TRUE)
colnames(RR)<-c("y","x","z")


# set up an 'empty' raster, here via an extent object derived from your data
e <- extent(RR[,1:2])
num.lats<-length(levels(as.factor(RR[,2])))
num.lons<-length(levels(as.factor(RR[,1])))
r <- raster(e, ncol=num.lons-13, nrow=num.lats-10)
# you need to provide a function 'fun' for when there are multiple points per cell
x <- rasterize(RR[, 1:2], r, RR[,3], fun=mean)
plot(x)
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
proj4string(x) <- crs.geo

#call in the namibia crop file and allocate diversity to the regions:
reg_crops_NAM<-subset(reg_crop_num, Country=="Namibia")
reg_crops_NAM<-distinct(reg_crops_NAM, AEZ, R.Diversity)

potential_NAM_zones<-data.frame(t("Zone 01", "Zone 02", "Zone 03", "Zone 04", "Zone 05",
                       "Zone 06", "Zone 07", "Zone 08", "Zone 09", "Zone 10",
                       "Zone 11", "Zone 12", "Zone 13", "Zone 14", "Zone 15",
                       "Zone 16"))
potential_NAM_zones<-as.tibble(t(potential_NAM_zones))
colnames(potential_NAM_zones)<-"AEZ"

###the zones in teh namibia file don't correspond to the zones in the global file, 
#so allocate manually based on administrative areas description in the file
""
list<-("Zone 01"=6, "Zone 02"=2, "Zone 03", "Zone 04"=9, "Zone 05",
"Zone 06", "Zone 07"=0, "Zone 08", "Zone 09", "Zone 10"=0,
"Zone 11"=, "Zone 12"=0, "Zone 13"=0, "Zone 14"=0, "Zone 15"=0,
"Zone 16"=0)
dff<-left_join(potential_NAM_zones,reg_crops_NAM, by="AEZ", .keep_all=TRUE)%>%
  mutate(Diversity=ifelse(is.na(R.Diversity)==TRUE, 0 ,R.Diversity))%>%
  select(-R.Diversity)

colnames(zone_NAM)[9]<-"AEZ1"
dff$AEZ1<-as.integer(seq(1,nrow(dff),1))
zone_NAM<-as.tibble(zone_NAM)
zone_NAM_diversity<-left_join(zone_NAM,dff, by="AEZ1", .keep_all=TRUE)

#plot the diversity and the zones to make a brick:

# set up an 'empty' raster, here via an extent object derived from your data
z<-zone_NAM_diversity$Diversity
RR<-as.matrix(cbind(zone_NAM_diversity$X, zone_NAM_diversity$Y, z), header=TRUE)
colnames(RR)<-c("x","y","z")

e <- extent(RR[,1:2])
num.lats<-length(levels(as.factor(RR[,2])))
num.lons<-length(levels(as.factor(RR[,1])))
r <- raster(e, ncol=num.lons-13, nrow=num.lats-10)
# need to provide a function 'fun' for when there are multiple points per cell
x <- rasterize(RR[, 1:2], r, RR[,3], fun=mean)
plot(x)
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
proj4string(x) <- crs.geo
plot(NAM, add=TRUE)


###Question 3: With climate change, will Cameroon be more robust than other two???


gaez <- readOGR("C:/Users/bar823/Documents/MGB_docs/Data_School/stats_proj/Stats_project/GAEZ_shapes/gaez18.shp", layer="gaez18")
plot(gaez)
