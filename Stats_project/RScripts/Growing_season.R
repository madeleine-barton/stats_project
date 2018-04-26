
##### Explore timing of growing season, with duplicate crops in there...
library(tidyverse)
rm(list=ls())
df<-read.csv("C:/Users/bar823/Documents/MGB_docs/Data_School/stats_proj/Stats_project/Cleaned_Data/Cleaned_data_all.csv")
colnames(df)
df1<- df%>%
  separate(Length_cropping_cycle, into=c("days", "text"), sep = " ")%>%
  unite(Combo, Country,AEZ, sep = "_" , remove=FALSE)%>%
  separate(days, into=c("min_days", "max_days"), sep="-", convert = TRUE, fill="left")
df1$max_days<-as.numeric(df1$max_days)
df2<-df1%>%
  mutate(min_days=ifelse(is.na(min_days)==TRUE,0, min_days))%>%
  mutate(max_days=ifelse(is.na(max_days)==TRUE,0, max_days))%>%
  mutate(mean_Length_cropping_cycle = (min_days+max_days)/2)%>%
  mutate(range_Length_cropping_cycle =ifelse((min_days==0),1, (max_days-min_days)))%>%
  select(-text, -Comments, -Preferred_sowing_plantin_period)

colnames(df2)
ggplot(df2, aes(x=Crop, y=mean_Length_cropping_cycle, colour=Country)) +
  geom_point()


colnames(df2)
ggplot(df2, aes(x=Country, y=range_Length_cropping_cycle)) +
  geom_boxplot()+
  labs(y="Range Length of the Cropping Cycle")
library(lmerTest)
library(emmeans)
library(pbkrtest)
mod1<-lm(range_Length_cropping_cycle~Country, data=df2)
anova(mod1)
summary(mod1)
emmeans(mod1, pairwise~Country)
#Cameroon has a shorter cropping season than Namibia and Tanzania

ggplot(df2, aes(x=AEZ, y=range_Length_cropping_cycle, fill=Country)) +
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(y="Average Length of the Cropping Cycle")


#not sure how to do this - want to compare AEZs within each 
#country - so group by country and then run stats on separate datasets?
mod2<-lmer(mean_Length_cropping_cycle~AEZ + (1|Country:AEZ), data=df2)
anova(mod2)
summary(mod2)
emmeans(mod2, pairwise~AEZ)

gplot(df2, aes(x=Crop, y=mean_Length_cropping_cycle, colour=Country)) +
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(y="Average Length of the Cropping Cycle")



#Determine the numeber of seasons of each crop in Country and AEZ:
#select only duplicate crops in each AEZ
df3<-df2%>%
  unite(Combo_Crop, Country,AEZ,Crop, sep = "_" , remove=FALSE)%>%
  group_by(Combo_Crop)%>%
  summarize(Num_seasons=n())

df4<-df2%>%
  unite(Combo_Crop, Country,AEZ,Crop, sep = "_" , remove=FALSE)
df5<-left_join(df4, df3, by="Combo_Crop")

ggplot(df5, aes(x=Country, y=Num_seasons, colour=Country)) +
  geom_boxplot()+
  geom_point(position=position_dodge(width = .75))+
  labs(y="Average Number of seasons")

#Cameroon has more rounds of planting than Namibia or Tanzania
ggplot(data=df5, aes(as.numeric(Num_seasons), fill=Country)) +
  geom_histogram(alpha=1, bins=4, colour="black")+
  facet_wrap(~Country)+
  labs(x="Number of Seasons")+
  theme(legend.position = "none")

ggplot(data=df5, aes(as.numeric(Num_seasons), fill=Combo), colour="black") +
  geom_histogram(alpha=0.7, binwidth = 1, colour="black")+
  facet_wrap(~Country)+
  theme(legend.position="none")+
  labs(x="Number of Seasons")


ggplot(df5, aes(x=AEZ, y=Num_seasons, fill=Country)) +
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(y="Average Length of the Cropping Cycle")

ggplot(df5, aes(x=Crop, y=Num_seasons, colour=Country)) +
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(y="Average Length of the Cropping Cycle")


#throughout the year, how many crops are growing at the same time (within each Combo)
df6<-df5%>%
  select(Combo_Crop, Combo, Country, AEZ, Crop, Plant_onset_julday, Plant_end_julday)

df6<-subset(df6,Plant_onset_julday!=0)
df6<-subset(df6,Plant_end_julday!=0)

Calendar<-matrix(nrow=365, ncol=nrow(df6), data=NA)

i<-1
j<-1
i<-280
rm(i, j)
for (j in (1:nrow(df6))){
  for (i in (1:365)){
    if((df6$Plant_end_julday[j]-df6$Plant_onset_julday[j])<0){ #if the growing season spans over the change of year
      Calendar[i,j]<-ifelse(((i<df6$Plant_end_julday[j])|(i>df6$Plant_onset_julday[j])), as.numeric(1) ,as.numeric(0))
    }else{
      Calendar[i,j]<-ifelse(((i>df6$Plant_onset_julday[j])&(i<df6$Plant_end_julday[j])), as.numeric(1) ,as.numeric(0)) 
    }
  }}

colnames(df6)
df7<-cbind(df6[,2],as.data.frame(t(Calendar)))
df7[1,]
df6[1,]
colnames(df7)[1]<-"Combo"
colnames(df7)

year_planting<-df7%>%
    group_by(Combo)%>%
    summarise_all(funs(sum))%>%
    separate(Combo, into=c("Country", "AEZ"), sep="_", remove=FALSE)%>%
    gather(key=Day_of_year, value=Num_plants_growing, starts_with("V"))%>%
    separate(Day_of_year, into=c("XX", "Julian_Day"), sep="V", remove=TRUE)%>%
    select(-XX)%>%
    arrange(Country,AEZ)

year_planting$Julian_Day<-as.numeric(year_planting$Julian_Day)

ggplot(data=year_planting, aes(x=Julian_Day, y=Num_plants_growing, colour=AEZ))+
  geom_point() + geom_line() +
  #geom_smooth(method ="oess", aes(colour=AEZ)) +
  facet_grid(Country~.)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="none")

plot(seq(1,365,1), unlist(year_planting[1,]))




colnames(df6)
df7<-cbind(df6[,3],as.data.frame(t(Calendar)))
df7[1,]
df6[1,]
colnames(df7)[1]<-"Country"
colnames(df7)

year_planting<-df7%>%
  group_by(Country)%>%
  summarise_all(funs(sum))%>%
  #separate(Combo, into=c("Country", "AEZ"), sep="_", remove=FALSE)%>%
  gather(key=Day_of_year, value=Num_plants_growing, starts_with("V"))%>%
  separate(Day_of_year, into=c("XX", "Julian_Day"), sep="V", remove=TRUE)%>%
  select(-XX)#%>%
  #arrange(Country,AEZ)

year_planting$Julian_Day<-as.numeric(year_planting$Julian_Day)

ggplot(data=year_planting, aes(x=Julian_Day, y=Num_plants_growing, colour=Country))+
  geom_point() + geom_line() +
  #geom_smooth(method ="oess", aes(colour=AEZ)) +
  facet_grid(Country~.)+
  labs(x="Day of Year", y="Number of crops being planted")
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position="bottom")


