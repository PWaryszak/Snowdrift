#LOAD snow DATA and LIBRARIES:=========
library(tidyverse)
library(lme4)
library(lmerTest)
library(broom)
library(sjPlot)
library(sjstats)
library(sjmisc)
library(ggpmisc)

#Load snow prepared and cured in CombineData_updated.R file:
setwd("C:/Users/poles/Documents/00Deakin_Docs/R/SusannaVenn/SEM/Shrub-snowdrift/Structural Equation Model/Emily/shrubs snowdrift/Snowdrift")
getwd()
snow <- read.csv(file = "VegSnowWindData_MasterFile.csv")

#Create and Clean AreaType:
snow2 <- snow %>% 
  mutate( AreaType = ifelse(height_cm < 1, "grass", "shrub")) %>% #Only OG (Open Grass) plots have height below 1 cm.
  
  #Identify and remove Closed Heath (CH) No data there:  
  filter(shrub_genus !=  "Closed" ) %>% #Remove  "Closed" heath (CH) No data there. 
  
  mutate( AreaType = ifelse(shrub_genus == "Open", "grass", "shrub"))  %>% #All OG (Open Grass) plots have height below 1 cm.
  
  filter( ! is.na(AreaType) ) %>%    #4 NA-s got filtered out 
  mutate(Aspect_OG = ifelse(AreaType == "grass", "Grass", aspect)) #(OG has no aspects. Shrubs have NW and SE aspects


#After merging early and late snow and density into just snow depth and density (for alpine_data_updated.csv)
#lots of SampleID plots are triplicated hence average the terms before stats:
snow_stats_OG <- snow2 %>%
  filter(shrub_genus !=  "Closed" ) %>% #Remove  "Closed" heath (CH) No data there. 

  mutate(SampleID = as.factor(SampleID)) %>%
  
  group_by(SampleID,Aspect_OG,aspect,Region) %>%
  
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


#PLOT:
s2 <- ggplot(data = snow_stats_OG, aes(x= snow_depth_cm, y=height_cm, color=Region))+ #, color= AreaType, data=[snow2$AreaType=="shrub",]
  geom_point(aes(size=Richness),alpha=0.5)+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  
  stat_smooth(method = "lm", col = "firebrick1")+
  scale_color_manual(values =  c("darkgreen", "blue"))+
  
  facet_grid(.~Aspect_OG)+
  scale_y_continuous(limits = c(0,100))+
  
 labs(x = "Snow depth (cm)",y = "Plant height (cm)", color= "Region: ", size = "Richness: ") + #, color = "Dominant Life Form: "
  theme_bw()+
  
  theme(axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = c(.08,.8),              #c(0.2, 0.5)
        legend.text = element_text(size = 10),
        legend.title = element_text(face = "bold", size=12),
        legend.key = element_rect( fill = "white", color = "black", linetype=2),
        legend.box.background = element_rect(size=2, colour = "#FFCC66"),
        legend.box.margin = margin(6, 6, 6, 6),
        strip.text=element_text(size=16, face = "bold"),
        strip.background = element_rect("white"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_text(size = 17, face = "bold", vjust = 0.5))


s2

#ggsave(s2, width = 10, height = 8, filename = "FIG2_SmallLegend.jpg")

#ggsave(s2, dpi=300, width = 1961, height = 1307, units = "px", filename = "FIG2_SmallLegend.jpg")
#ggsave(s2, dpi=300, width = 1961, height = 1307, units = "px", device = "pdf", filename = "FIG2.pdf")

#Descriptive STATS:
summary(lm(snow_days ~ Aspect_OG, data = snow_stats_OG))
summary(lm(Richness ~ Aspect_OG, data = snow_stats_OG))


#OIKOS Guidelines:
#WEB: https://www.oikosjournal.org/authors/author-guidelines
#The preferred file formats are vector-images, EPS, TIFF or PDF. Rasterised (pixelated) files are ok as long as the specifications below are followed.
#Width: 945 (single column), 1476 (1.5 column) or 1961 (double column) pixels (at 300 dpi). Resolution: 300-600 dpi. Size: <50MB
#For fonts in the figures use only common sans-serif fonts, such  as Geneva, Helvetica, or Arial. Letters, numbers and symbols must appear clearly but not oversized.

