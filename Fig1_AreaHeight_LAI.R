#Trait Table of 6 focus plant (Height, LAI, Area_cm3)======
#Load LIBRARIES:
library(tidyverse)
library(vegan)

#LOAD DATA:
snow <- read.csv("VegSnowWindData_MasterFile.csv") #Master Data joined in CombineData.R file - early and late snow was combined in the excel as per email/cat (email title: "Original snow depth and density calculations sheet" )

snow2 <- snow %>%  #Exclude non-shrubs
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

#Turning Table 1 (Six_height_cm) into Plot===========
names(Six_height_cm)

#Panel Plot of Table 1:
s1 <- ggplot(Six_height_cm, aes( x =  "", y=mean_height_cm)) +
  geom_point(aes(size = as.numeric(mean_area_cm3), fill=mean_LAI),shape=21, colour = "black")+
  geom_errorbar( aes(ymin= mean_height_cm+se_height_cm,
                     ymax = mean_height_cm-se_height_cm), width=.2, colour = "white")+
  scale_size(range = c(10,20))+
  scale_y_continuous(limits = c(1, 80),expand = c(0, 0))+
  scale_fill_gradient(low = "grey", high = "black")+
  facet_grid(.~shrub_genus + shrub_species) +
  
  
  theme_classic()+
  labs( x = "Target shrub", y = "Mean target shrub height (cm)",
        size = "Area:",  fill = "LAI:")+
  theme(axis.text.x = element_blank(),   #text(size=16,hjust=.5,angle=45,vjust=1,face="italic", color="black"),
        axis.text.y = element_text(size=16, hjust=.5,vjust=0,face="plain", color="black"),  
        axis.title.x = element_blank(),    #text(size=22,hjust=0.5,vjust=9,face="plain"),
        axis.title.y = element_text(size=22),
        axis.ticks.x.bottom = element_blank(),
        plot.title = element_text(size=20, lineheight=1.8, face="bold", hjust = 0.5),
        legend.title = element_text(size=16, face="bold"),
        legend.text = element_text(size = 8),
        #legend.key = element_rect( fill = "white", color = "black"),
        legend.key=element_blank(), #Removes the frames around the points in legends
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        legend.direction="vertical",
        
        strip.text=element_text(size=14, face = "italic"),
        strip.background = element_rect(colour="white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        
        axis.line.x.bottom=element_blank(),
        
        legend.box = "horizontal",
        legend.position = c(0.8 , 0.25)) 

s1
#ggsave(s1, dpi=300, width = 2401, height = 2037, units = "px", filename = "Table1_PlotNewUpdated1.jpg")

