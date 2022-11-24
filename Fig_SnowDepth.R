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
setwd("C:/Users/BlueCarbon/Documents/00DeakinUni/R/SusannaVenn/SEM/Shrub-snowdrift/Structural Equation Model/Emily/shrubs snowdrift/Snowdrift_SEM")
snow <- read.csv(file = "VegSnowWindData_MasterFile.csv")

#Create and Clean AreaType:
snow2 <- snow %>% 
  mutate( AreaType = ifelse(height_cm < 1, "grass", "shrub")) %>% #Only OG (Open Grass) plots have height below 1 cm.
  
  #Identify and remove Closed Heath (CH) No data there:  
  filter(shrub_genus !=  "Closed" ) %>% #Remove  "Closed" heath (CH) No data there. 
  
  mutate( AreaType = ifelse(shrub_genus == "Open", "grass", "shrub"))  %>% #All OG (Open Grass) plots have height below 1 cm.
  
  filter( ! is.na(AreaType) ) %>%    #4 NA-s got filtered out 
  mutate(Aspect_OG = ifelse(AreaType == "grass", "Grass", aspect)) #(OG height is always =1):


#After merging early and late snow and density into just snow depth and density (for alpine_data_updated.csv)
#lots of SampleID plots are triplicated hence average the terms before stats:
snow_stats <- snow2 %>%
  filter(shrub_genus !=  "Closed" ) %>% #Remove  "Closed" heath (CH) No data there. 
  filter(AreaType =="shrub") %>%
  mutate(SampleID = as.factor(SampleID)) %>%
  group_by(SampleID,AreaType,aspect,region) %>%
  summarise(snow_depth_cm =  mean(snow_depth_cm, na.rm=T),
            height_cm = mean (height_cm, na.rm=T),
            snow_days = mean(snow_days, na.rm=T),
            N=n())

snow_stats


#PLOT:
ggplot(data = snow_stats, aes(x= snow_depth_cm, y=height_cm, color=region))+ #, color= AreaType, data=[snow2$AreaType=="shrub",]
  geom_point(aes(size=snow_days),alpha=0.5)+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  
  stat_smooth(method = "lm", col = "firebrick1")+
  scale_color_manual(values =  c("darkgreen", "blue"))+
  
  facet_grid(.~aspect)+
  scale_y_continuous(limits = c(0,100))+
  
  labs(x = "Snow depth (cm)",y = "Shrub height (cm)", color= "Region: ", size = "Snow days: ") + #, color = "Dominant Life Form: "
  theme_bw()+
  
  theme(axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "right",               #c(0.2, 0.5)
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 17, face = "bold", vjust = 0.5))
 
# + optional to show stats on the figure (messy!):
stat_fit_glance(method = "lm",
                # label.x =  c(0.5,0),
                method.args = list(formula = y ~ x),
                mapping = aes(label = sprintf('R^2~"="~%.3f~~italic(P)~"="~%.2g',
                                              round(stat(r.squared),2), stat(p.value))),
                parse = TRUE)
