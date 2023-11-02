#Load DATA + LIBRARIES:=======
if (!require(vegan)) install.packages('vegan')
if (!require(tidyverse)) install.packages('tidyverse')

library(tidyverse)
library(vegan)

#LOAD DATA:
snow <- read.csv("VegSnowWindData_MasterFile.csv") #Master Data joined in CombineData.R file - early and late snow was combined in the excel as per email/cat (email title: "Original snow depth and density calculations sheet" )

snow2 <- snow %>%  #Exclude non-shrubs
  filter(! shrub =="Open.grassy") %>%  #Exclude grassy areas as these had no aspect accounted for
  filter(! shrub =="Closed.heath")      #Exclude "Closed.heath" areas as these had no aspect accounted for nor veg survey undertaken.

unique(snow2$shrub) #6 focus plants were: "Grevillea.australis",  "Hovea.montana" ,"Orites.lanceolata" , "Epacris.petrophylla","Ozothamnus.alpina",  "Nematolepis.ovatifolia"
dim(snow2)#2334 rows of data with 167 columns


#Trait Table (S8) ======
#Table of target shrub traits: Height, LAI, Area_cm3. 

#AREA/LAI/HEIGHT by FOCUS SPECIES:
Six_height_cm <- snow2 %>%
  group_by(Region, shrub, genus_species, shrub_genus, shrub_species) %>%
  summarize( mean_height_cm = round (mean (height_cm, na.rm=T),1),
             n_height_cm = length(height_cm),
             sd_height_cm = round (sd(height_cm, na.rm=T),1),
             se_height_cm = round ( sd_height_cm/sqrt(n_height_cm),1),
             
             mean_LAI = round (mean (leaf.area.index, na.rm=T),1),
             n_LAI = length(leaf.area.index),
             sd_LAI = round (sd(leaf.area.index, na.rm=T),1),
             se_LAI = round ( sd_LAI/sqrt(n_LAI),1),
             
             #ellipse area 9is 0.79 of square area (area_cm3 was square)
             
             mean_area_cm3 = round (mean (area_cm3*0.79/10000, na.rm=T),1),
             n_area_cm3 = length(area_cm3),
             sd_area_cm3 = round (sd(area_cm3*0.79/10000, na.rm=T),1),
             se_area_cm3 = round ( sd_area_cm3/sqrt(n_area_cm3),5))



View(Six_height_cm)# 6 17

TableS1 <- Six_height_cm[-7,c(1,2,6,9,10,13,14,17)]
TableS1
#write.table(TableS1, file = "6TargetSgrubsTraits_TableS1.txt", sep = ",", quote = FALSE, row.names = F)  #Save as table.txt and then copy into Word, 
#in Word to get a Word-Table, select it all, go to Table → Convert → Convert Text to Table

#PLOT Trait Table===========
names(Six_height_cm)

#Panel Plot of Table 1:
Fig02 <- ggplot(Six_height_cm, aes( x =  "", y=mean_height_cm)) +
  geom_point(aes(size = as.numeric(mean_area_cm3)/10000, color=mean_LAI))+  #,shape=21, colour = "black"
  scale_shape_manual(values=c(21,22))+
  
  #geom_errorbar( aes(ymin= mean_height_cm+se_height_cm, ymax = mean_height_cm-se_height_cm), width=.2, colour = "white")+ #Error bars look like little H letter. We removed them and described them in captions instead:
  
  scale_size_area(max_size =20,breaks=c(1,2,3))+
  scale_y_continuous(limits = c(1, 80),expand = c(0, 0))+
  scale_color_gradient(low = "#CCCCCC", high =  "black")+  #Keep Colour pattern consistent with Fig4 and Fig 5. Converting to grey scale.

  facet_grid(.~Region + shrub_genus + shrub_species) +
  
  
  theme_classic()+
  labs( x = "", y = "Mean target shrub height (cm)",
        size =  expression("Canopy area"~(~m^2)),  #size =  expression( "Area(~m^2)")
        color = "LAI")+   
  
  theme(axis.text.x = element_blank(),   #text(size=16,hjust=.5,angle=45,vjust=1,face="italic", color="black"),
        axis.text.y = element_text(size=16, hjust=.5,vjust=0,face="plain", color="black"),  
        axis.title.x = element_blank(),    #text(size=22,hjust=0.5,vjust=9,face="plain"),
        axis.title.y = element_text(size=22),
        axis.ticks.x.bottom = element_blank(),
        plot.title = element_text(size=20, lineheight=1.8, face="bold", hjust = 0.5),
    
        legend.title = element_text(size=16,face="bold"),  # face="bold"
        legend.text = element_text(size = 12),
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
        legend.position = c(0.8 , 0.18)) 

Fig02

ggsave(Fig02, dpi=300, width = 2491, height = 2037, units = "px", filename = "Fig02_Height_LAI_GREY.jpg")
