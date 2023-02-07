#Load these packages into your working environment:
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require("readxl")) install.packages("readxl"); library("readxl")
if (!require("vegan")) install.packages("vegan"); library("vegan")

library("readxl")
library("tidyverse")
library("vegan")
library(lme4)
library(lmerTest)


#SNOW DATA ==========
setwd("C:/Users/poles/Documents/00Deakin_Docs/R/SusannaVenn/SEM/Shrub-snowdrift/Structural Equation Model/Emily/shrubs snowdrift/Snowdrift")
SnowData <- read.csv("alpine_data_updated.csv", header = TRUE)# Type of updates on original alpine.data:
#1 Column U: 0 was replaced with real values based on raw data in "shrub plus snow data PW.xlvs" - negative values were removed as the 
#SV: "These couple of negative snow densities occur because there isn’t enough snow to make accurate measurements.
#When the standard depth of snow is around 20cm or less this happens. So these negative values can’t be used, and we simply don’t have measurements of snow density at those shrubs.."
#2 late and early snow depth and snow density were merged into one column - hence ~6 times longer dataset.
#3 ratio_check and other ratios were deleted
#4 32 new snow records (processed by Pawel in May 2022), was added from: "ToMerge_NewSnowDays_DoneManuallyInExcel_ByPawel"

View(SnowData)
dim(SnowData)#3040   20
names(SnowData) 
unique(SnowData$site_site_no) #These are Sites
unique(SnowData$year)#2015 2016 2017 2018 - for years of snow measurements and veg survey was done in 2019

#Remove heath and grass  (no veg surveys for Closed.heath, no aspects in both) :
SnowData2 <- SnowData %>%
  filter(! shrub =="Open.grassy") %>%  #Exclude grassy areas as these had no aspect accounted for
  filter(! shrub =="Closed.heath")      #Exclude heathy areas as these had no aspect accounted for


#DATA_Plot snow days by year:========
ggplot(SnowData, aes(x = height_cm, y = snow_days_1_aug, color=aspect)) +
  geom_point()+
  scale_y_continuous(limits = c(0,100))+
  facet_grid(.~year) +
  ggtitle("SE (leeward) and NW (windward) aspect")+
  theme(axis.text.y=element_text(size=14 , color = "black"),
        axis.text.x = element_text(size = 12, angle=90, color = "black"),
        axis.title.y=element_text(size=14),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(color = "black", size = 18, face = "bold"),
        plot.subtitle = element_text(color = "red"),
        strip.text=element_text(size=20))

setwd("C:/Users/poles/Documents/00Deakin_Docs/R/SusannaVenn/SEM/Shrub-snowdrift/Structural Equation Model/Emily/shrubs snowdrift/Snowdrift/Figures")
ggsave("DATA_FIG_snow_days_1_aug.jpg", height = 6.3, width = 8)
setwd("C:/Users/poles/Documents/00Deakin_Docs/R/SusannaVenn/SEM/Shrub-snowdrift/Structural Equation Model/Emily/shrubs snowdrift/Snowdrift")


#DATA_Plot snow days by snow_days_1_aug:========
ggplot(SnowData, aes(x = snow_days, y = snow_days_1_aug, color=aspect)) +
  geom_point()+
  scale_y_continuous(limits = c(0,100))+
  facet_grid(.~year) +
  ggtitle("SE (leeward) and NW (windward) aspect")+
  theme(axis.text.y=element_text(size=14 , color = "black"),
        axis.text.x = element_text(size = 12, angle=90, color = "black"),
        axis.title.y=element_text(size=14),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(color = "black", size = 18, face = "bold"),
        plot.subtitle = element_text(color = "red"),
        strip.text=element_text(size=20))

setwd("C:/Users/poles/Documents/00Deakin_Docs/R/SusannaVenn/SEM/Shrub-snowdrift/Structural Equation Model/Emily/shrubs snowdrift/Snowdrift/Figures")
ggsave("DATA_FIG_snow_days_only_by_year.jpg", height = 6.3, width = 8)
setwd("C:/Users/poles/Documents/00Deakin_Docs/R/SusannaVenn/SEM/Shrub-snowdrift/Structural Equation Model/Emily/shrubs snowdrift/Snowdrift")


#DATA-plot Shrub versus height_cm============
ggplot(SnowData2, aes(x = as.factor(year), y = height_cm, shape= Region,color=aspect)) +
  geom_point(aes(size = snow_depth_cm),alpha=0.6)+
  facet_grid(Region~shrub)+
  ggtitle("Shrub versus height_cm (Both SE & NW aspects)")+
  theme_bw()+
  theme(axis.text.y=element_text(size=14 , color = "black"),
        axis.text.x = element_text(size = 12, angle=90, color = "black"),
        axis.title.y=element_text(size=14),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(color = "black", size = 18, face = "bold"),
        plot.subtitle = element_text(color = "red"),
        legend.position = "bottom",
        strip.text=element_text(size=12, face="italic"))

setwd("C:/Users/poles/Documents/00Deakin_Docs/R/SusannaVenn/SEM/Shrub-snowdrift/Structural Equation Model/Emily/shrubs snowdrift/Snowdrift/Figures")
ggsave("DATA_FIG_height_cm.jpg", height = 6.3, width = 8)
setwd("C:/Users/poles/Documents/00Deakin_Docs/R/SusannaVenn/SEM/Shrub-snowdrift/Structural Equation Model/Emily/shrubs snowdrift/Snowdrift")


#ADD-in VEG DATA========
#The Veg Data was cleaned and duplicates removed as per Duplicates.R file :
#Load DAta from Master file (Previously called: "Alpine Shrub Floristics combined_MASTER.csv")
setwd("C:/Users/poles/Documents/00Deakin_Docs/R/SusannaVenn/SEM/Shrub-snowdrift/Structural Equation Model/Emily/shrubs snowdrift/Snowdrift/FloristicsRawCleaned")
files <- list.files()
files

#The Veg Data was cleaned and duplicats removes as per Duplicates.R file :
veg1 <- read.csv("1BHPshrubdata_CLEAN_TRIMMED.csv")
names(veg1)
veg2 <- read.csv('1Bundarashrubdata_CLEAN.csv') %>% select(-Species, -species, -Genus)
names(veg2)
veg3 <- read.csv("1JimAngeShrubData_CLEAN.csv" )%>% select(-Species, -species, -Genus)
names(veg3)
veg4 <- read.csv("1KnollAngeShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
names(veg4)
veg5 <- read.csv("1MarumAngeShrubData_CLEAN.csv" )%>% select(-Species, -species, -Genus)
names(veg5)
veg6 <- read.csv("1MarumShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
names(veg6)
veg7 <- read.csv("1Rolling1PhebShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
names(veg7)
veg8 <- read.csv("1Rolling1ShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
names(veg8)
veg9 <- read.csv("1RuinedAngeShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
names(veg9)
veg10 <- read.csv("1SaddlePShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
names(veg10)
veg11 <- read.csv("1SS21ShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
names(veg11)
veg12 <- read.csv("1SS2ShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
names(veg12)
veg13 <- read.csv("1SS3ShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
names(veg13)
veg14 <- read.csv("1ThredboRolling1ShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
veg15 <- read.csv("1ThredboShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
names(veg15)
veg16 <- read.csv("1Watchbed1ShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
names(veg16)
veg17 <- read.csv("1Watchbed2ShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
names(veg17)
veg18 <- read.csv("BHP_OpenGrassyData2022_CLEAN.csv")%>% select(-Species, -species, -Genus) #Our OG
# OG plots were recorded once and subsequently duplicated to match NW-SE design from all plots before.
names(veg18) #25

#Check if any no sites are missing from the trimmed file of original veg1 (many duplicated records were there before):
veg1_orig <- read.csv("1BHPshrubdata_CLEAN.csv") %>% select(-Species, -species, -Genus)
colnames(veg1_orig) #94
colnames(veg17) #236 SampleID +SpeciesID
#Check if none left behind:
veg1_check <- veg1_orig[ , which ( colnames(veg1_orig) %in% colnames(veg17))]
colnames(veg1_check)#NULL has been left behind = GOOD NEWS!

#Create MASTER SPECIES LIST:
sp1 <- select (veg1, SpecID)
sp2 <- select (veg2, SpecID)
sp3 <- select (veg3, SpecID)
sp4 <- select (veg4, SpecID)
sp5 <- select (veg5, SpecID)
sp6 <- select (veg6, SpecID)
sp7 <- select (veg7, SpecID)
sp8 <- select (veg8, SpecID)
sp9 <- select (veg9, SpecID)
sp10 <- select (veg10, SpecID)
sp11 <- select (veg11, SpecID)
sp12 <- select (veg12, SpecID)
sp13 <- select (veg13, SpecID)
sp14 <- select (veg14, SpecID)
sp15 <- select (veg15, SpecID)
sp16 <- select (veg16, SpecID)
sp17 <- select (veg17, SpecID)
sp18 <- select (veg18, SpecID)

#Create Master Species list: First bind all data:
sp <- as.data.frame(rbind(sp1,sp2,sp3,sp4,sp5,sp6,sp7,sp8,sp9,sp10,sp11,sp12,sp13,sp14,sp15,sp16,sp17,sp18))

#Then remove duplicates:
SpeciesList <- gather(sp, variable, SpecID) %>% #convert to long format
  group_by(SpecID) %>%
  summarise(nDuplicated = n())
  
SpeciesList[duplicated(SpeciesList$SpecID),]
unique(SpeciesList$SpecID)#116 plant species
#write.csv(SpeciesList, file = "SpeciesList_FULL.csv", row.names = F)

#JOIN VEG sets together=========
#Join all veg datasets to include SpeciesList and create a Master Veg file:
#Use SpecID to bind data by species!!!
v1<- left_join(SpeciesList, veg1, by = "SpecID" ) 
v2<- left_join(v1, veg2, by = "SpecID" )
v3<- left_join(v2, veg3, by = "SpecID", suffix = c("", ".y") ) 
v4<- left_join(v3, veg4, by = "SpecID", suffix = c("", ".y") )
v5<- left_join(v4, veg5, by = "SpecID" , suffix = c("", ".y")) 
v6<- left_join(v5, veg6, by = "SpecID" , suffix = c("", ".y")) 
v7<- left_join(v6, veg7, by = "SpecID", suffix = c("", ".y") ) 
v8<- left_join(v7, veg8, by = "SpecID" , suffix = c("", ".y")) 
v9<- left_join(v8, veg9, by = "SpecID" , suffix = c("", ".y")) 
v10<- left_join(v9, veg10, by = "SpecID" , suffix = c("", ".y")) 
v11<- left_join(v10, veg11, by = "SpecID", suffix = c("", ".y") ) 
v12<- left_join(v11, veg12, by = "SpecID" , suffix = c("", ".y")) 
v13<- left_join(v12, veg13, by = "SpecID" , suffix = c("", ".y")) 
v14<- left_join(v13, veg14, by = "SpecID" , suffix = c("", ".y")) 
v15<- left_join(v14, veg15, by = "SpecID" , suffix = c("", ".y")) 
v16<- left_join(v15, veg16, by = "SpecID" , suffix = c("", ".y")) 
v17<- left_join(v16, veg17, by = "SpecID" , suffix = c("", ".y")) 
v18<- left_join(v17, veg18, by = "SpecID" , suffix = c("", ".y")) 

names(v18) #260 Columns. Many columns were duplicated and x/y suffixes were added. Remove .y columns 
#Remove duplicated columns if any: https://stackoverflow.com/questions/24142942/how-to-remove-duplicated-column-names-in-r
#Check which ones are duplicated:
dupli <- v18 %>%  select_at(vars(ends_with(".y")))  
dupli #NONE NOW. YAY! I cleaned all the raw files off duplicates and triplicates

v18[is.na(v18)] <-0 #Replace NA with 0:


VegMaster_knp_bhp <- v18 %>%  
  select_at(vars(-ends_with(".y"))) %>% #Remove duplicated columns
  select(-nDuplicated) #Remove nDuplicated column

dim(v18)# 119 260
dim(VegMaster_knp_bhp)#259 observational units + SpecID column - select(-nDuplicated)
names(VegMaster_knp_bhp)#259 observational units + SpecID column

unique(VegMaster_knp_bhp $ SpecID) # 116 Species including litt, rock and bare!
VegMaster_knp_bhp <- as.data.frame(VegMaster_knp_bhp)# reformat from tibble to data.frame
#View(VegMaster_knp_bhp)


#Turn Sites into rows and species into columns to compute plant cover:
VegMaster_knp_bhp_long <- gather(VegMaster_knp_bhp, SampleID, Cover, -SpecID)%>%
  unite("ID", SpecID:SampleID, sep = "_", remove = F) %>% #Creating new ID to pull a mean on duplicated records
  group_by(ID, SpecID, SampleID)%>% #Way to remove duplicated entries
  summarise(GoodCover = mean(Cover, na.rm=T)) #pulling a mean cover aka GoodCover

duplicateID <- VegMaster_knp_bhp_long[duplicated(VegMaster_knp_bhp_long$ID),] #checking if any columns are duplicated
duplicateID #No duplicates. if they keep on popping up. Let's remove them with group by "ID" as per above

#View(VegMaster_knp_bhp_long)
unique(VegMaster_knp_bhp_long$SampleID) #258 observational units in veg data.
dim(VegMaster_knp_bhp_long)#29928     4
VegMaster_knp_bhp_long <- as.data.frame(VegMaster_knp_bhp_long) #Change from tibble (problematic for spread and gather function) to data.frame


#Join LIFE Forms & Compute % Contributions ======= 
#form data and compute % contributions of each groth form:
setwd("C:/Users/poles/Documents/00Deakin_Docs/R/SusannaVenn/SEM/Shrub-snowdrift/Structural Equation Model/Emily/shrubs snowdrift/Snowdrift")
LifeFormData <- read.csv("SpecID_LifeForms.csv")
names(LifeFormData)#Columns: SpecID","Species",

VegMaster_knp_bhp_long_forms <- left_join(VegMaster_knp_bhp_long, LifeFormData , by = "SpecID" )
#View(VegMaster_knp_bhp_long_forms)

#Compute % Contribution for each SampleID:
VegMaster_LifeForm <- VegMaster_knp_bhp_long_forms %>%
                         group_by(SampleID, LifeForm) %>%
                        summarise(LifeFormSum = sum(GoodCover))  %>% na.omit()

View(VegMaster_LifeForm)

VegMaster_LifeForm_wide <-  spread(VegMaster_LifeForm, LifeForm, LifeFormSum)
names(VegMaster_LifeForm_wide)
View(VegMaster_LifeForm_wide)


VegMaster_LifeForm_wide_perc <- VegMaster_LifeForm_wide %>%
  mutate( TotalCover = Bare+Grass+Herb+ Litter+ Moss+Rock+Shrub) %>% #Get total Cover 
  mutate( Bare_perc = round ( Bare/TotalCover*100,1)) %>%
  mutate( Grass_perc = round ( Grass/TotalCover*100,1)) %>%
  mutate( Herb_perc = round ( Herb/TotalCover*100,1)) %>%
  mutate( Litter_perc = round ( Litter/TotalCover*100,1)) %>%
  mutate( Moss_perc = round ( Moss/TotalCover*100,1))%>%
  mutate( Rock_perc = round ( Rock/TotalCover*100,1))%>%
  mutate( Shrub_perc = round ( Shrub/TotalCover*100,1))

#View( VegMaster_LifeForm_wide_perc )



#DATA-Plot of % Life Form vs aspect==========

#Wrangle for ggploting:
VegMaster_LifeForm_wide_perc2 <- VegMaster_LifeForm_wide_perc  %>%
  select(SampleID, Bare_perc,Grass_perc,Herb_perc, Litter_perc,  Moss_perc,   Rock_perc , Shrub_perc ) %>%
  gather(key = "Form", value =  "Percent", -SampleID) %>%
  separate(SampleID, into = c("Region", "site", "shrub_code", "Aspect"), remove = F) %>%
  filter(Aspect == "NW" | Aspect =="SE") %>%
  filter(Form != "Moss_perc") %>%
  mutate (OpenGrass =ifelse(grepl('OG',shrub_code), 'Yes', 'No')) %>%
  filter(OpenGrass == "No")

head(VegMaster_LifeForm_wide_perc2)
unique(VegMaster_LifeForm_wide_perc2$Form)# "Bare_perc"   "Grass_perc"  "Herb_perc"   "Litter_perc" "Rock_perc"   "Shrub_perc" 
VegMaster_LifeForm_wide_perc2$Form <- factor(VegMaster_LifeForm_wide_perc2$Form, levels = c("Shrub_perc","Herb_perc","Grass_perc","Bare_perc" ,  "Rock_perc" ,"Litter_perc" ))



ggplot(data = VegMaster_LifeForm_wide_perc2 , aes(x= Aspect, y=Percent))+
  geom_boxplot(outlier.shape = NA) +geom_jitter(aes(color = Aspect)) +
  facet_wrap(.~Form)+
  #ggtitle("Richness is significantly higher at high shrubs (P = 0.0118)",
  # subtitle = "open_grass <0,1cm>, low_shrubs <1,50cm>, high_shrubs <50,110cm>")+
  labs(x = "Aspect", y = "Percent Contribution (%)", color = "Height (cm)")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12, color= "black"),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "none",               #c(0.2, 0.5)
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 17, face = "bold", vjust = 0.5))

setwd("C:/Users/poles/Documents/00Deakin_Docs/R/SusannaVenn/SEM/Shrub-snowdrift/Structural Equation Model/Emily/shrubs snowdrift/Snowdrift/Figures")
#ggsave("DATA_FIG_LifeForm_Percentages.jpg", height = 6.3, width = 8)
setwd("C:/Users/poles/Documents/00Deakin_Docs/R/SusannaVenn/SEM/Shrub-snowdrift/Structural Equation Model/Emily/shrubs snowdrift/Snowdrift")


#VEG INDICES:========
#Create veg matrix and compute plant species indices:
VegMaster_knp_bhp_long_NoID <- select (VegMaster_knp_bhp_long, -ID) #Remove ID which we used to remove duplicates

veg_matrix <- spread(VegMaster_knp_bhp_long_NoID,  SpecID, value = GoodCover, fill=0)
dim(veg_matrix)#258 117 - each sampling unit has its own row now.

#Separate soil data from veg data:
soil <- veg_matrix %>%
  select(SampleID, rock, litt, bare) #keep rock, litter and bare ground as these are none-plant variables

vege <- veg_matrix %>%
    select(-rock, -litt, -bare) #remove rock, litter and bare_ground as these are none-plant variables


#Plot to see raw data:
names(vege)#114
dim(vege)# 234 114
vege$Richness  <- specnumber(vege[,2:114])#No soilm no rock, no litter here
vege$Cover   <- rowSums(vege[,2:114])
vege$Diversity <- diversity(vege[,2:114])
vege$SampleID_FromVegData <- vege$SampleID

par(mfrow=c(1,1))
plot(vege$Richness, main = "Alpine Plant Richness")
plot(vege$Cover, main = "Alpine Plant Cover")
plot(vege$Diversity) 

#Join vege data with percent data:
vege2 <- left_join(vege,VegMaster_LifeForm_wide_perc, by = "SampleID" )
names(vege2)



#WIND DATA=========
wind <- read_excel("WindData.xlsx", sheet = "WindData")
str(wind)# tibble [84 x 11]

#Join SNOW DATA with vege DATA and WIND DATA (alpine.data.csv)======
dim(SnowData)#2336   20
unique(vege2$SampleID) #258
unique(SnowData$SampleID) #567 - these data have more sampling units


#Combine Snow, Wind and Veg data===========
VegSnowData <- left_join(SnowData, vege2, by = "SampleID")
VegSnowWindData <- left_join(VegSnowData, wind, by = "SampleID")
names(VegSnowWindData)
dim(VegSnowWindData)#3040  162
#View(VegSnowWindData)


#MASTER Snowdrift data file======
write.csv(VegSnowWindData , file = "VegSnowWindData_MasterFile.csv", row.names = F) #THIS IS OUR MASTER DATA FILE!


#Run Regressions========
dat <- read.csv("VegSnowSoilWindData_SEM7.csv")
names(dat)
summary(lm(Wind_Ave ~ aspect, data = dat)) #YES = significant, P =1.71e-11 ***
summary(lm(litt ~ aspect, data = dat)) #NO
summary(lm(bare ~ aspect, data = dat)) #No
summary(lm(rock ~ aspect, data = dat)) #No
summary(lm(Richness ~ aspect, data = dat)) #YES! P = 0.036 *

summary(lm(snow_days ~ aspect, data = dat)) #No 
summary(lm(snow_days_1_aug ~ aspect, data = dat)) #No 
summary(lm(late_density_gcm3 ~ aspect, data = dat)) #No 
summary(lm(early_density_gcm3 ~ aspect, data = dat)) #YES, P=3.22e-08 ***
qplot(dat$aspect,dat$early_density_gcm3, geom = "boxplot")

summary(lm(early_depth_cm ~ aspect, data = dat)) #YES, P= 3.01e-05 ***
qplot(dat$aspect,dat$early_depth_cm, geom = "boxplot")
summary(lm(late_depth_cm ~ aspect, data = dat)) #No

#LMER runs==========

#Shrub contribution higher in NW
summary (lmer(Percent ~ Aspect + (1|Region)+(1|shrub_code),
              data = VegMaster_LifeForm_wide_perc2[VegMaster_LifeForm_wide_perc2$Form=="Shrub_perc",]))
#Estimate Std. Error      df t value Pr(>|t|)  
#(Intercept)   16.478      7.888   1.056   2.089   0.2737  
#AspectSE      -5.109      2.295 205.254  -2.226   0.0271 *


#No effect of aspect on Herb%:
summary (lmer(Percent ~ Aspect + (1|Region)+(1|shrub_code),
              data = VegMaster_LifeForm_wide_perc2[VegMaster_LifeForm_wide_perc2$Form=="Herb_perc",]))

#No effect of aspect on Grass %
summary (lmer(Percent ~ Aspect + (1|Region)+(1|shrub_code),
              data = VegMaster_LifeForm_wide_perc2[VegMaster_LifeForm_wide_perc2$Form=="Grass_perc",]))


summary (lmer(Percent ~ Aspect + (1|Region)+(1|shrub_code),
              data = VegMaster_LifeForm_wide_perc2[VegMaster_LifeForm_wide_perc2$Form=="Bare_perc",]))

summary (lmer(Percent ~ Aspect + (1|Region)+(1|shrub_code),
              data = VegMaster_LifeForm_wide_perc2[VegMaster_LifeForm_wide_perc2$Form=="Rock_perc",]))

summary (lmer(Percent ~ Aspect + (1|Region)+(1|shrub_code),
              data = VegMaster_LifeForm_wide_perc2[VegMaster_LifeForm_wide_perc2$Form=="Litter_perc",]))






