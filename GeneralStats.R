#GENERAL STATS FOR STARTING RESULTS/ABSTRACT sections IN THE PAPER:
library(tidyverse)
library(vegan)

#Load snow prepared and cured in CombineData.R file:
snow <- read.csv("VegSnowWindData_MasterFile.csv") #Master Data joined in CombineData.R file - early and late snow was combined in the excel as per email/cat (email title: "Original snow depth and density calculations sheet" )

pre_veg_matrix <- read.csv("VegSnowWindData_MasterFile.csv") #As produced in CombineData.R file
names(pre_veg_matrix) #Veg matrix is occupying columns between Acae_nova and Xero_subu


#Species number and names=======
#Filter veg species columns only:
veg_matrix <- pre_veg_matrix %>% 
  unite("SampleID2",c(SampleID,  shrub), sep = "_", remove = F) %>%  #Keeping full shrub name as O may mean Orites or Ozothamnus
  select(SampleID2, Acae_nova:Xero_subu ) %>%
  select(-moss) %>%
  mutate(SpecRowSums = specnumber(pre_veg_matrix[,(which(names(pre_veg_matrix)=='Acae_nova'):which(names(pre_veg_matrix)=='Xero_subu'))])) %>% #Summing rows of species covers
  filter(SpecRowSums > 0) %>%  #removing 0-rows of species
  select( Acae_nova:Xero_subu ) #Select Species only

length(names(veg_matrix)) #Total species number = 109
View(veg_matrix)

#Trait Table of 6 focus plant (Height, LAI, Area_cm3)======
#Exclude non-shrubs
snow <- read.csv("VegSnowWindData_MasterFile.csv") #Master Data joined in CombineData.R file - early and late snow was combined in the excel as per email/cat (email title: "Original snow depth and density calculations sheet" )

snow2 <- snow %>% 
  filter(! shrub =="Open.grassy") %>%  #Exclude grassy areas as these had no aspect accounted for
  filter(! shrub =="Closed.heath")      #Exclude "Closed.heath" areas as these had no aspect accounted for nor veg survey undertaken.

unique(snow2$shrub) #6 focus plants were: "Grevillea.australis",  "Hovea.montana" ,"Orites.lanceolata" , "Epacris.petrophylla","Ozothamnus.alpina",  "Nematolepis.ovatifolia"
dim(snow2)#2334rows of data

summary(snow2$height_cm)#Summary of focus shrub plant height_cm:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 15.00   30.00   45.00   46.57   60.00  110.00       4 
sd(snow2$height_cm, na.rm = TRUE)#19.23388

#HEIGHT by FOCUS SPECIES:
Six_height_cm <- snow2 %>%
  group_by(shrub, genus_species, shrub_genus, shrub_species) %>%
  summarize( mean_height_cm = round (mean (height_cm, na.rm=T),1),
             n_height_cm = length(height_cm),
             sd_height_cm = round (sd(height_cm, na.rm=T),1),
             se_height_cm = round ( sd_height_cm/sqrt(n_height_cm),1),
             
             mean_LAI = round (mean (leaf.area.index, na.rm=T),1),
             n_LAI = length(leaf.area.index),
             sd_LAI = round (sd(leaf.area.index, na.rm=T),1),
             se_LAI = round ( sd_LAI/sqrt(n_LAI),1),
             
             
             mean_area_cm3 = round (mean (area_cm3, na.rm=T),1),
             n_area_cm3 = length(area_cm3),
             sd_area_cm3 = round (sd(area_cm3, na.rm=T),1),
             se_area_cm3 = round ( sd_area_cm3/sqrt(n_area_cm3),1))
  

             

Six_height_cm
#write.table(Six_height_cm, file = "TRAITsumstats.txt", sep = ",", quote = FALSE, row.names = F)  #Save as table.txt and then copy into Word, 
#in Word to get a Word-Table, select it all, go to Table → Convert → Convert Text to Tabl3

#Turn Table 1 (Six_height_cm) into Plot===========
names(Six_height_cm)

#Panel Plot of Table 1:
s1 <- ggplot(Six_height_cm, aes( x =  "", y=mean_height_cm)) +
  geom_point(aes(size = as.numeric(mean_area_cm3), fill=mean_LAI),shape=21, colour = "black")+
  scale_size(range = c(2,20))+
  scale_y_continuous(limits = c(0, 80))+
  scale_fill_gradient(low = "grey", high = "black")+
  facet_grid(.~shrub_genus + shrub_species) +
  
  
  theme_classic()+
  labs( x = "Target shrub", y = "Average target shrub height (cm)",
        size = "Area:",  fill = "LAI:")+
  theme(axis.text.x = element_blank(),   #text(size=16,hjust=.5,angle=45,vjust=1,face="italic", color="black"),
        axis.text.y = element_text(size=16, hjust=.5,vjust=0,face="plain", color="black"),  
        axis.title.x = element_blank(),    #text(size=22,hjust=0.5,vjust=9,face="plain"),
        axis.title.y = element_text(size=22),
        axis.ticks.x.bottom = element_blank(),
        plot.title = element_text(size=20, lineheight=1.8, face="bold", hjust = 0.5),
        legend.title = element_text(size=16, face="bold"),
        legend.text = element_text(size = 8),
        legend.key = element_rect( fill = "white", color = "black"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        legend.direction="horizontal",
        legend.box = "horizontal",
        strip.text=element_text(size=14, face = "italic"),
        strip.background = element_rect(colour="white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = c(0.5 , 0.15)) 

s1
#ggsave(s1, dpi=300, width = 2401, height = 2037, units = "px", filename = "Table1_PlotNew.jpg")




#Point Plot (optional):
s2 <-ggplot(Six_height_cm, aes( x =  genus_species, y=mean_height_cm)) +
  geom_point(aes(size = as.numeric(mean_area_cm3), fill=mean_LAI),shape=21, colour = "black")+
  scale_size(range = c(2,20))+
  scale_y_continuous(limits = c(20, 80))+
  scale_fill_gradient(low = "grey", high = "black")+
  
  
  
  theme_classic()+
  labs( x = "", y = "Average target shrub height (cm)",
        fill = "LAI:", size = "Area:")+
  theme(axis.text.x = element_text(size=14,angle= 30, face="italic", color="black"),
        axis.text.y = element_text(size=14, hjust=.5,vjust=0,face="plain", color="black"),  
        #axis.title.x = element_text(size=22,hjust=0.5,vjust=9,face="plain"),
        axis.title.y = element_text(size=22),
        plot.title = element_text(size=20, lineheight=1.8, face="bold", hjust = 0.5),
        legend.title = element_text(size=8,, face="bold"),
        legend.text = element_text(size = 6),
        legend.key = element_rect( fill = "white", color = "black"),
        legend.box.background = element_rect(),
        #legend.box.margin = margin(6, 6, 6, 6),
        legend.position = c(0.1,0.65)) 

#ggsave(s2, dpi=300, width = 2401, height = 2037, units = "px", filename = "Table1_Plot.jpg")






#Richness and rare species names:=======
round (mean(snow$Richness,na.rm = T) , 1) # 10.8
round (sd (snow$Richness,na.rm = T), 1) #3.7

#Total Species Table (Full name, frequencies, life forms)========
#Remove Singletons columns:
occur.cols <- apply(veg_matrix,2,sum)#sum species occurrences in each column
zero_species <- as.data.frame(occur.cols) %>% filter(occur.cols == 0) #Check for zero species
zero_species # Cras_grac= 0 
  
#Turn fraction into integers to continue  ordination in vegan:
veg.matrix2 <- veg_matrix %>% mutate_if (is.numeric, round, digits = 0)

#Remove zero species/ singletons:
good.matrix <- veg.matrix2 [ , ! occur.cols <= 0  ] #removing all 0-sum columns
dim(good.matrix) #1493  112


#Species Frequencies:
veg_matrix3 <- veg_matrix
veg_matrix3 [veg_matrix3 > 0] <- 1 #Convert all values greater than 0 to 1
veg_matrix3

occur.cols3 <- apply(veg_matrix3, 2, sum)#sum species occurrences in each column
occur.cols3
freq <- as.data.frame(occur.cols3)
new_order <- arrange(freq, occur.cols3)
row.names(new_order)
new_order$SpecID <- row.names(new_order)

#Compute Species occurance frequencies:
total_species_number <- as.numeric(length(names(veg_matrix))) 
total_species_number #Total species number = 109
new_order$Freq_percent <- round(new_order$occur.cols3/ total_species_number  * 100, 1) #

#Join column on Life Forms
LifeFormData <- read.csv("SpecID_LifeForms.csv")
names(LifeFormData)#Columns: SpecID","LifeForm"  

new_order_LifeForms <- left_join(new_order, LifeFormData , by = "SpecID" )
View(new_order_LifeForms)

SpeciesList <- read.csv("SpecID_FullSpeciesName_List.csv")
Match_Sp <- left_join(new_order_LifeForms, SpeciesList, by = "SpecID") #full species names by matching new_order with Species List:
View(Match_Sp)
#write.table(Match_Sp, file = "SpeciesTable.txt", sep = ",", quote = FALSE, row.names = F)  #Save as table.txt and then copy into Word, 
#Copy to Word -> Select it all -> Table → Convert → Convert Text to Table…:




#Rarefaction:=======
names(good.matrix)

#Richness:
S <- specnumber(good.matrix)
S

# Number of INDIVIDULS per site (?)
raremax <- min(rowSums(good.matrix)) # 0
raremax

# rarefy, w/ raremax as input (?)
Srare <- rarefy(good.matrix, raremax)
Srare

#Slope of curve:
round (mean(rareslope(good.matrix, 48)),3) # 0.027
sd(rareslope(good.matrix, 48))#0.02909054

#Slope at 48 sample (our sample size)
rarecurve(good.matrix, step = 20, 
          sample = 48, 
          col = "blue", 
          cex = 0.1,
          xlab = "Sample Size",
          ylab = "Plant species number",
          main = "Rarefaction curves")
abline(v= 48, col = "red", lty=1, lwd=3)



#SNOW summaries=====
names(snow)

#early_depth_cm
round( summary(snow$early_depth_cm),1)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 10.0    34.0    43.0    43.4    51.4    90.0      12 
round(sd(snow$early_depth_cm, na.rm = TRUE),1)# 12.9


#By AreaType:
General_Snow <- snow2 %>%
  group_by(AreaType) %>%
  summarize(AV_early_depth_cm = mean(early_depth_cm, na.rm=T),
            N_early_depth_cm = n(),
            SD_early_depth_cm = sd(early_depth_cm, na.rm=T),
            SE_early_depth_cm = SD_early_depth_cm / sqrt(N_early_depth_cm))

General_Snow
#AreaType AV_early_depth_cm   N_early_depth_cm SD_early_depth_cm SE_early_depth_cm
#1 grass                 40.2               46              10.5             1.54 
#2 shrub                 43.3              284              12.9             0.763


#late_depth_cm
round( summary(snow$late_depth_cm),1)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.0    37.2    52.0    51.7    65.8   150.0      62 
round(sd(snow$late_depth_cm, na.rm = TRUE),1)# 27.1

#early_density_gcm3
round( summary(snow$early_density_gcm3),1)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.0     0.2     0.3     0.3     0.4     0.7      53 
round(sd(snow$early_density_gcm3, na.rm = TRUE),1)# 0.1

#late_density_gcm3
round( summary(snow$late_density_gcm3),1)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#-0.1     0.4     0.4     0.4     0.5     0.7      90 
round(sd(snow$late_density_gcm3, na.rm = TRUE),1)# 0.1

#snow_days_average
round( summary(snow$snow_days_average),1)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 42.0   115.9   125.0   123.3   133.0   163.0      63 
round(sd(snow$snow_days_average, na.rm = TRUE),1)# 14.7

#snow_days_1_aug_average
round( summary(snow$snow_days_1_aug_average),1)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#29.0    58.0    63.5    64.2    71.2   102.5      63 
round(sd(snow$snow_days_1_aug_average, na.rm = TRUE),1)# 10.6

#Wind_Max
round( summary(snow$Wind_Max),1)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  1.3     4.1     5.7     6.6     9.5    17.4     301 
round( sd(snow$Wind_Max, na.rm=T),1) # 3.4

#LAI from raw alpine.data.csv:==========
raw_data <- read.csv("alpine_data_updated.csv")
names(raw_data)
round( summary(raw_data$leaf.area.index),1)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.0     3.4     4.5     4.3     5.8     8.7       5 

ggplot(raw_data, aes(y = leaf.area.index, x = shrub))+geom_boxplot()

summary (lm(leaf.area.index ~ shrub, data = raw_data))

#Effect of Traits (LAI,AREA,Height) on snow========
