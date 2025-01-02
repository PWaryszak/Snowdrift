#LOAD snow DATA and LIBRARIES:=========
if (!require(broom)) install.packages('broom')
if (!require(ggpmisc)) install.packages('ggpmisc')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(ggpubr)) install.packages('ggpubr')

library(tidyverse)
library(broom)
library(ggpmisc)
library(ggpubr)

#Load snow prepared and cured in CombineData_updated.R file:
snow <- read.csv(file = "VegSnowWindData_MasterFile.csv")

#Create and Clean AreaType:
snow2 <- snow %>% 
  mutate( AreaType = ifelse(height_cm < 1, "grass", "shrub")) %>% #Only OG (Open Grass) plots have height below 1 cm.
  
  #Identify and remove Closed Heath (CH) No data there:  
  filter(shrub_genus !=  "Closed" ) %>% #Remove  "Closed" heath (CH) No data there. 
  
  filter( ! is.na(AreaType) ) %>%    #4 NA-s got filtered out 
  
  mutate( AreaType = ifelse(shrub_genus == "Open", "grass", "shrub"))  %>%  #All OG (Open Grass) plots have height below 1 cm.

  mutate(aspect2 = ifelse(aspect == "NW", "Windward", "Leeward")) %>%  #Renaming. Moving away from always describing SE and NW: so the reader / reviewer doesn’t have to remember which way the wind is coming from
  #NW in the heading to Windward and SE to Leeward

  mutate(Aspect_OG = ifelse(AreaType == "grass", "Grass", aspect2)) #OG has no aspects. Shrubs have NW and SE aspects
  

#After merging early and late snow and density into just snow depth and density (for alpine_data_updated.csv)
#SampleID plots were triplicated after merging columns together hence average the terms before stats:
snow_stats_OG <- snow2 %>%
  filter(shrub_genus !=  "Closed" ) %>% #Remove  "Closed" heath (CH) No data there. 

  mutate(SampleID = as.factor(SampleID)) %>%
  
  group_by(SampleID, Aspect_OG, aspect2,Region) %>%
  
  summarise(snow_depth_cm =  mean(snow_depth_cm, na.rm=T),
            height_cm = mean (height_cm, na.rm=T),
            snow_depth_cm =  mean(snow_depth_cm, na.rm=T),
            height_cm = mean (height_cm, na.rm=T),
            snow_days = mean(snow_days, na.rm=T),
            snow_days_1_aug =  mean(snow_days_1_aug, na.rm=T),
            snow_density_gcm3 = mean(snow_density_gcm3,na.rm=T),
            Richness = mean(Richness,na.rm=T),
            N=n())

snow_stats_OG

snow_stats_OG2 <- na.omit(snow_stats_OG) #Remove NaNs as these give us grey points on the plot we do not want to see
names(snow_stats_OG2)

#PLOT Snow Depth and Shrub Height:
Fig03 <- ggplot(data = snow_stats_OG2, aes(x= snow_depth_cm, y=height_cm))+ #, color= AreaType, data=[snow2$AreaType=="shrub",]
         geom_point(aes(color=Richness), size=4,  alpha=0.8) +   #pch=21, shape=Region, was alpha = 0.9 but makes it too blurry

  stat_smooth(method = "lm", col = "black")+
  
  scale_color_gradient(low = "#CCCCCC", high =  "black")+  #Keep Colour pattern consistent with Fig4 and Fig 5. Converting to grey scale.

  facet_grid(.~Aspect_OG)+
  scale_y_continuous(limits = c(0,100))+
  scale_x_continuous(limits = c(0,130))+

  labs(x = "Snow depth (cm)",y = "Target shrub height (cm)", col = "Richness: ") + #, color = "Dominant Life Form: "
  theme_bw()+
  
  theme(axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size=24, hjust=.5,vjust=0,face="plain", color="black"),
        axis.title.y = element_text(size = 24),
        axis.title.x = element_text(size = 24),
        
        legend.position = c(.16,.8),              #c(0.2, 0.5)
        legend.title = element_text( size=16, face = "bold"),
        legend.text = element_text(size = 12),
        #legend.key = element_rect( fill = "white", color = "black", linetype=2),
        legend.key=element_blank(), #Removes the frames around the points in legends
        legend.box.background = element_rect(size=2, colour = "white"),
        legend.box.margin = margin(6, 6, 6, 6),
        
        strip.text=element_text(size=16, face = "bold"),
        strip.background = element_rect("white"),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(1, "lines"),
        
        panel.grid.major = element_blank())


Fig03
#ggsave(Fig03, dpi=300, width = 2400, height = 2037,  units = "px", filename = "Fig03_Height_SnowDepth_GREY_New2.jpg")



#Flipped Fig03=========
Fig03_flipped <- ggplot(data = snow_stats_OG2, aes(y= snow_depth_cm, x=height_cm))+ #, color= AreaType, data=[snow2$AreaType=="shrub",]
         geom_point(aes(color=Richness), size=4,  alpha=0.8) +   #pch=21, shape=Region, was alpha = 0.9 but makes it too blurry

  stat_smooth(method = "lm", col = "black")+
  
  scale_color_gradient(low = "#CCCCCC", high =  "black")+  #Keep Colour pattern consistent with Fig4 and Fig 5. Converting to grey scale.

  facet_grid(.~Aspect_OG)+
  scale_y_continuous(limits = c(0,130))+
  scale_x_continuous(limits = c(0,100))+

  labs(y = "Snow depth (cm)",x = "Target shrub height (cm)", col = "Richness: ") + #, color = "Dominant Life Form: "
  theme_bw()+
  
  theme(axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = element_text(size=24, hjust=.5,vjust=0,face="plain", color="black"),
        axis.title.y = element_text(size = 24),
        axis.title.x = element_text(size = 24),
        
        legend.position = c(.16,.8),              #c(0.2, 0.5)
        legend.title = element_text( size=16, face = "bold"),
        legend.text = element_text(size = 12),
        #legend.key = element_rect( fill = "white", color = "black", linetype=2),
        legend.key=element_blank(), #Removes the frames around the points in legends
        legend.box.background = element_rect(size=2, colour = "white"),
        legend.box.margin = margin(6, 6, 6, 6),
        
        strip.text=element_text(size=16, face = "bold"),
        strip.background = element_rect("white"),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(1, "lines"),
        
        panel.grid.major = element_blank())


Fig03_flipped
ggsave(Fig03_flipped, dpi=300, width = 2400, height = 2037,  units = "px", filename = "Fig03_Height_SnowDepth_GREY_New3.jpg")#axes flipped 
 

#Allocation of open grassy (OG) plots across regions:=============
names(snow_stats_OG2)

grass <- snow_stats_OG2 %>%
        filter(Aspect_OG == "Grass") %>%
   group_by( Aspect_OG, SampleID) %>%
   summarise(N = n())

grass

grass_snow <- snow %>%
        filter(shrub_code == "OG" | shrub_code == "OG1" |shrub_code == "OG2") %>%
   group_by(Region,shrub_code, Richness) %>%
   summarise(N = n())

grass_snow
#write.csv(grass_snow, file = "GrassPresence.csv")#Shows what OG plots where surveyed. NA were not surveyed
