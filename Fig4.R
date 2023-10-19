#LOAD snow DATA and LIBRARIES:=========
if (!require(ggpubr)) install.packages('ggpubr')
if (!require(ggpmisc)) install.packages('ggpmisc')
if (!require(tidyverse)) install.packages('tidyverse')

library(tidyverse)
library(ggpmisc)
library (ggpubr)

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
  
  group_by(SampleID,Aspect_OG, aspect2,Region) %>%
  
  summarise(snow_depth_cm =  mean(snow_depth_cm, na.rm=T),
            height_cm = mean (height_cm, na.rm=T),
            snow_depth_cm =  mean(snow_depth_cm, na.rm=T),
            height_cm = mean (height_cm, na.rm=T),
            snow_days = mean(snow_days, na.rm=T),
            snow_days_1_aug =  mean(snow_days_1_aug, na.rm=T),
            snow_density_gcm3 = mean(snow_density_gcm3,na.rm=T),
            Richness = mean(Richness,na.rm=T),
            Cover = mean(Cover,na.rm=T),
            Diversity = mean(Diversity,na.rm=T),

            N=n())

snow_stats_OG


#Cover PLOT=====
names(snow_stats_OG)
unique(snow_stats_OG$Aspect_OG)

i1 <- ggplot(data = snow_stats_OG, aes(x= Aspect_OG, y=Cover))+
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = Aspect_OG, size =  snow_days), alpha=0.5)+
  labs(x = "", y = "Cover abundance (%)",  color = "Plot Type:", size = "Snow (days): ")+
  scale_color_manual(values =  c("darkgreen", "blue", "firebrick1"))+
  guides(fill = "none", color = "none")+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"),
        legend.position = "right",              # c(0.12, 0.8)
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 17, face = "bold", vjust = 0.5))


i1

#Diversity:
i2 <- ggplot(data = snow_stats_OG, aes(x= Aspect_OG, y=Diversity))+
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = Aspect_OG, size = snow_density_gcm3), alpha=0.5) +
  labs(x = "",  color = "", size = "Snow (g/cm3):")+
  scale_color_manual(values =  c("darkgreen", "blue", "firebrick1"))+
  guides(fill = FALSE, color = FALSE)+
  theme_classic()+
  theme(axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"),
        legend.position = "right",               # c(0.12, 0.8)
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 17, face = "bold", vjust = 0.5))

i2


#Richness:
i3 <- ggplot(data = snow_stats_OG, aes(x= Aspect_OG, y=Richness))+
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = Aspect_OG, size =  snow_depth_cm), alpha=0.5)+
  
  labs(x = "", y = "Species richness",  color = "Plot Type:", size = "Snow (cm): ")+
  scale_color_manual(values =  c("darkgreen", "blue", "firebrick1"))+
  
  guides(fill = FALSE, color = FALSE)+
  theme_classic()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"),
        axis.title.x = element_text(size = 18, color = "black"),
        legend.position = "right",              # c(0.12, 0.8)
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 17, face = "bold", vjust = 0.5))


i3


p_plots_horizon  <- ggarrange(i1,i3, ncol=1, 
                              labels = c("a)", "b)"),
                              label.x = 0,
                              label.y = 1)
p_plots_horizon

ggsave(p_plots_horizon, filename = "FIG4_CoverRichnessBoxplots.jpg", width = 18, height = 23, units = "cm", dpi = 600)

