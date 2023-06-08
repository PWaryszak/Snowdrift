#LOAD snow DATA and LIBRARIES:=========
library(tidyverse)
library(lme4)
library(lmerTest)
library(broom)
library(sjPlot)
library(sjstats)
library(sjmisc)
library(ggpmisc)

#Load snow prepared and cured in CombineData.R file:
setwd("C:/Users/poles/Documents/00Deakin_Docs/R/SusannaVenn/SEM/Shrub-snowdrift/Structural Equation Model/Emily/shrubs snowdrift/Snowdrift")
snow <- read.csv(file = "VegSnowWindData_MasterFile.csv") #Master Data joined in CombineData.R file - early and late snow was combined in the excel as per email/cat (email title: "Original snow depth and density calculations sheet" )
#As a result the dta grew in leght
#Explore/Wrangle Data:
dim(snow)#3040  162
names(snow)
unique(snow$shrub_code) #See names

#NA-s and Height_cm:
summary(snow$height_cm)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.00   25.00   40.00   41.59   60.00  110.00      68 

#Clean the snow data:
snow2 <- snow %>% 
  mutate( AreaType = ifelse(height_cm < 1, "grass", "shrub")) %>% #Only OG (Open Grass) plots have height below 1 cm.
  filter(shrub_genus !=  "Closed" ) %>% #Remove  "Closed" heath (CH) No data there. 
  
  #Fill NA-rows in AreaType:  
  mutate( AreaType = ifelse(shrub_genus == "Open", "grass", "shrub"))  %>% #All OG (Open Grass) plots have height below 1 cm.
  filter( ! is.na(AreaType) )    #4 NA-s got filtered out 


#First STEP, lm for data exploration:=========
#Check if CH and OG are different in snow/wind variables:
summary(lm(snow_depth_cm ~ aspect+AreaType, data = snow2)) #Yes aspect only
summary(lm(snow_depth_cm ~ AreaType, data = snow2)) #No
summary(lm(snow_density_gcm3 ~ AreaType, data = snow2)) #No
summary(lm(snow_density_gcm3 ~ AreaType, data = snow2)) #No
summary(lm(snow_days_1_aug ~ AreaType, data = snow2)) #No
summary(lm(snow_days ~ AreaType, data = snow2)) #Yes
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    112.176      2.788  40.242  < 2e-16 ***
#AreaTypeshrub   12.780      2.836   4.507 7.34e-06 ***

#After merging early and late snow and density (we updated alpine_data to alpine_data_updated.csv)
#lots of SampleID plots are triplicated hence average the terms before stats:

snow_stats <- snow2 %>%
  filter(shrub_genus !=  "Closed" ) %>% #Remove  "Closed" heath (CH) No data there. 
  mutate(SampleID = as.factor(SampleID)) %>%
  group_by(SampleID,AreaType,aspect,Region) %>%
  summarise(snow_depth_cm =  mean(snow_depth_cm, na.rm=T),
            height_cm = mean (height_cm, na.rm=T),
            area_cm3 = mean(area_cm3, na.rm=T),
            snow_days = mean(snow_days, na.rm=T),
            snow_days_1_aug =  mean(snow_days_1_aug, na.rm=T),
            snow_density_gcm3 = mean(snow_density_gcm3,na.rm=T),
            Richness = mean(Richness,na.rm=T),
            Cover = mean(Cover,na.rm=T),
            Diversity = mean(Diversity,na.rm=T),
            Litter  = mean(Litter, na.rm=T),
            Bare = mean (Bare, na.rm=T),
            Rock = mean(Rock, na.rm=T),
            LAI = mean(leaf.area.index, na.rm=T),
            area_cm3 = mean(area_cm3, na.rm=T),
            N=n())
snow_stats
unique(snow_stats$AreaType)# "shrub" "grass"


#Wind model:=========
#After merging early and late snow depth & snow density records were triplicated. Run AV first:
snow3 <- snow2 %>%
  group_by(SampleID,aspect) %>%
  summarise(Wind_Ave_ms = mean(Wind_Ave, na.rm=T),
            Wind_Max_ms = mean(Wind_Max, na.rm=T))

Wind_model<-lm(Wind_Ave_ms ~  aspect  , data=snow3)
tab_model(Wind_model)

Wind_max_model<-lm(Wind_Max_ms ~  aspect, data=snow3)
tab_model(Wind_model, Wind_max_model, show.stat=T, show.se=T, show.ci = F) #Table S2 n the Manuscript, output of snow3 = output of snow2 YAY!

#AREA_cm3 =======
#ellipse is 0.79 of square surface hence = area_cm3*0.79 (it is area_cm2 in real terms)
#area_cm3 was not measured for grasses!aArea_cm3 measured entire target shrub (no split between aspects!)

summary(lm(area_cm3*0.79 ~ height_cm, data = snow_stats[snow_stats$AreaType=="shrub",])) #YES but there are reports on faulty area data
#                  Estimate Std. Error t value Pr(>|t|)    
#Intercept)   -5.127e-11  5.029e+03    0.00        1    
#AreaTypeshrub  3.806e+04  5.421e+03    7.02 1.28e-11 ***

summary(lmer(area_cm3*0.79 ~ aspect* height_cm + (1|Region) , data = snow_stats)) #YES but there are reports on faulty area data


#LAI (Ignore, set to 0 for grasses)=======
summary(lmer(LAI ~ AreaType + aspect* height_cm + (1|Region), data = snow_stats)) #YES but there are reports on faulty area data
#                     Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        -4.002e-14  2.464e-01   0.000  1.00000    
#AreaTypeshrub       5.668e+00  2.902e-01  19.535  < 2e-16 ***
#aspectSE            1.327e-15  2.897e-01   0.000  1.00000    
#height_cm          -1.640e-02  5.203e-03  -3.152  0.00177 ** 
#aspectSE:height_cm  5.290e-17  6.194e-03   0.000  1.00000    

#LAI ~HEIGHT=======
summary( lm(LAI ~ height_cm, data=snow_stats)) #All when Grass set to 0
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 2.675289   0.212957  12.563  < 2e-16 ***
#height_cm   0.038441   0.004553   8.442 1.04e-15 ***

summary( lm(LAI ~ height_cm, data=snow_stats[snow_stats$AreaType =="shrub",])) #Shrubs Only
#(Intercept)  5.66826    0.22646  25.030  < 2e-16 ***
# height_cm   -0.01640    0.00449  -3.653 0.000309 ***

------------------------------------------------------------------------------



#LMER 4 RESULTS (snow_depth Table S3)========
summary( lmer(snow_depth_cm ~ AreaType + aspect * height_cm + (1|Region), data=snow_stats))
#                  Estimate    Std. Error  df     t value  Pr(>|t|)    
#(Intercept)         46.07079    8.01706   1.28963   5.747  0.07122 . 
#AreaTypeshrub       -5.37752    3.43863 315.00003  -1.564  0.11886   
#aspectSE            -2.24916    3.42986 315.00001  -0.656  0.51246   
#height_cm            0.06440    0.06347 315.00209   1.015  0.31101   
#aspectSE:height_cm   0.23658    0.07498 315.00007   3.155  0.00176 **
  
model_snow_depth_cm <- lmer(snow_depth_cm ~ AreaType + aspect * height_cm + (1|Region), data=snow_stats)
tab_model(model_snow_depth_cm, show.stat = T, digits=1, show.se = T )



#extract slopes:
coef(summary(model_snow_depth_cm))
ranef(model_snow_depth_cm)
fixef(model_snow_depth_cm)

#Get slope for height in SE vs NW:
snowSE <- snow2 %>% filter( aspect == "SE")
summary(lm(snowSE$snow_depth_cm ~ snowSE$height_cm,data=snowSE))

snowNW <- snow2 %>% filter( aspect == "NW")
summary(lm(snowNW$snow_depth_cm ~ snowNW$height_cm,data=snowNW))


round(fixef( lmer(snow_depth_cm ~ AreaType + height_cm + (1|Region), data=snowSE)),2)
#(Intercept) AreaTypeshrub     height_cm 
#40.2234848    -6.6800969     0.2947837 
round(fixef( lmer(snow_depth_cm ~ AreaType + height_cm + (1|Region), data=snowNW)),2)
#(Intercept) AreaTypeshrub     height_cm 
#40.22348485   -4.45124901    0.08956889 




#Snow density_gcm3 model ============
density_gcm3_model <- lmer(snow_density_gcm3 ~ AreaType + aspect * height_cm + (1|Region), data=snow_stats)
summary(density_gcm3_model)#No effect
tab_model(density_gcm3_model, show.stat=T, show.se=T, show.ci = F )


#snow_days_1_aug_model============
snow_days_1_aug_model <- lmer(snow_days_1_aug ~ AreaType + aspect* height_cm   + (1|Region), data=snow_stats)
summary(snow_days_1_aug_model)#No effect of aspect
#             Estimate Std. Error       df  t value Pr(>|t|)    
#(Intercept)          59.52290    2.73185    3.00504  21.789 0.000209 ***
#AreaTypeshrub        -1.16808    2.17363 1004.03974  -0.537 0.591120    
#aspectSE              0.55131    1.78402 1004.00037   0.309 0.757366    
#height_cm             0.11410    0.02558 1004.06804   4.461 9.08e-06 ***
#aspectSE:height_cm    0.01068    0.03559 1004.00081   0.300 0.764054   



#snow_days_model============
snow_days_model <- lmer(snow_days ~ AreaType + aspect * height_cm + (1|Region), data=snow_stats) #same results for shrubs only when [snow_stats$AreaType =="shrub",]
summary(snow_days_model)#No effect of aspect
#(Intercept)   113.01490    5.36001   2.57146  21.085 0.000597 ***
#AreaTypeshrub   2.73964    4.04284 286.03100   0.678 0.498540    
#aspectSE        1.91146    1.57394 286.00385   1.214 0.225579    
#height_cm       0.15167    0.04098 286.00404   3.701 0.000257 ***

#Table S5 in the Manuscript:
tab_model(snow_days_model, snow_days_1_aug_model, show.stat=T, show.se=T, show.ci = F )

#Plot snow_days:
ggplot(data = snow2, aes(x= snow_days, y=height_cm, fill = aspect ))+
  geom_boxplot() +
  geom_jitter(aes(color = aspect)) +
  facet_wrap(.~AreaType)+
  #ggtitle("Richness is significantly higher at high shrubs (P = 0.0118)",
  # subtitle = "open_grass <0,1cm>, low_shrubs <1,50cm>, high_shrubs <50,110cm>")+
  #labs(x = "Aspect", fill = "Height Rank: ", color = "Height (cm)")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "bottom",               #c(0.2, 0.5)
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 17, face = "bold", vjust = 0.5))


#Richness_model============
Richness_model <- lmer(Richness ~ AreaType + aspect * height_cm + (1|Region), data = snow_stats)
Richness_model <- lmer(Richness ~ AreaType + aspect * height_cm + (1|Region), data = snow2)

tab_model(Richness_model) # YES EFFECT!
#            Estimate Std. Error  df t value Pr(>|t|)    
#(Intercept)     9.03990    2.10270   1.31611   4.299 0.099156 .  
#AreaTypeshrub  -1.56778    0.97230  31.89429  -1.612 0.116719    
#aspectSE        0.97479    0.40605 215.31042   2.401 0.017217 *  
#height_cm       0.04782    0.01228  33.84179   3.894 0.000441 ***

#Diagnostics:
qqnorm(resid(Richness_model)) #GOOD
qqline(resid(Richness_model))

#Key findings:
summary(lm(Richness ~ AreaType, data = snow_stats))
summary(lm(Richness ~ height_cm, data = snow_stats))


#Plot significant effects (Richness):
ggplot(data = snow2, aes(x= aspect, y=Richness, fill = height_cm))+
  geom_boxplot() +geom_jitter(aes(color = height_cm)) +
  facet_wrap(.~AreaType)+
  #ggtitle("Richness is significantly higher at high shrubs (P = 0.0118)",
  # subtitle = "open_grass <0,1cm>, low_shrubs <1,50cm>, high_shrubs <50,110cm>")+
  #labs(x = "Aspect", fill = "Height Rank: ", color = "Height (cm)")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "bottom",               #c(0.2, 0.5)
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 17, face = "bold", vjust = 0.5))

#Cover_model============
#Simplified lmer 4 Key Findings
#open grassy datasets has no aspect nor height data in them:
Cover_model_simple <- lmer(Cover ~ AreaType +(1|Region), data = snow_stats)
Cover_model_simple <- lmer(Cover ~ AreaType +(1|Region), data = snow2) 

summary(Cover_model_simple)#YES effect
#Estimate Std. Error      df t value Pr(>|t|)    
#(Intercept)    103.006      5.257 240.000  19.595  < 2e-16 ***
#AreaTypeshrub   14.677      5.513 240.000   2.662  0.00829 ** 

Cover_model<- lmer(Cover ~ AreaType+ aspect * height_cm +(1|Region), data = snow_stats)

#un-averaged data (snow2) shows clearer relationships between thee variables. Not sure why?:
Cover_model<- lmer(Cover ~ AreaType+ aspect * height_cm +(1|Region), data = snow2)

#Table S5 in Manuscript:
tab_model(Richness_model, Cover_model, show.stat=T, show.se=T, show.ci = F )



#Diversity_model============
Diversity_model <- lmer(Diversity ~ AreaType + aspect * height_cm + (1|Region), data = snow_stats)
tab_model(Diversity_model) # Height EFFECT ONLY!

#Check if Richness correlates with Diversity
summary(lm(Richness~Diversity, data = snow2))
cor.test(snow2$Richness, snow2$Diversity,method = "pearson") #STRONG CORRELATION OF 0.645383 
#0.6355397 , t = 31.785, df = 1491, p-value < 2.2e-16


x#bare_model============
bare_model <- lmer(Bare ~ AreaType  + aspect * height_cm + (1|Region), data = snow_stats)
summary(bare_model)#No effect


#Litter_model============
litt_model <- lmer(Litter ~ AreaType + aspect * height_cm + (1|Region), data = snow_stats)
summary(litt_model)
#                 Estimate Std. Error       df t value Pr(>|t|)    
#(Intercept)           7.10960    2.25526   22.34717   3.152  0.00456 ** 
#AreaTypeshrub         9.27243    2.56844   26.96883   3.610  0.00123 ** 
#  aspectSE            0.38970    0.96174 1424.11254   0.405  0.68539    
#height_cm            -0.11783    0.02282  628.83861  -5.163 3.26e-07 ***
# aspectSE:height_cm   0.01466    0.02029 1424.04740   0.723  0.46997  
tab_model(litt_model)

#For Key findings:
summary(lm(Litter ~ AreaType, data = snow_stats))
7.545 + 3.479   # ] 11.024% for shrubs

ggplot(data = snow2, aes(x= aspect, y=Litter))+
  geom_boxplot() + #geom_jitter(aes(color = aspect)) +
  facet_wrap(.~AreaType)
  
#Having grasses set to zero height_cm affects the effect size wbove. Reove Grasses before lm:
summary(lm(Litter ~ height_cm, data = snow_stats[snow_stats$AreaType == "shrub",]))
##Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 15.98946    1.72023   9.295  < 2e-16 ***
#height_cm   -0.10652    0.03412  -3.122  0.00205 ** 


#rock_model============
rock_model <- lmer(Rock ~ AreaType + aspect * height_cm + (1|shrub_code), data = snow2)
summary(rock_model)#No effect

# plot fixed effects:
plot_model(rock_model, type = "pred", terms = c("aspect", "HeightRank_")) +
  labs(x="Aspect",y="rock", title = "Predicted effect")


#LIFE FORM MODEL:=======
#snow_depth_cm_model============
summary( lmer(snow_depth_cm ~ aspect * height_cm  +(1|Region), data=snow_stats))
#Estimate Std. Error        df t value Pr(>|t|)    
#(Intercept)  35.21301    1.88090  56.91796  18.721  < 2e-16 ***
#  aspectSE      5.42486    1.21549 324.82225   4.463 1.12e-05 ***
#  height_cm     0.12690    0.03711  59.88693   3.420  0.00113 ** 
#plot snow_depth_cm:
ggplot(data = snow2, aes(x= snow_depth_cm, y=height_cm))+ #, color= AreaType
  geom_point(size=2)+
  stat_smooth(method = "lm", col = "black")+
  scale_color_manual(values =  c("blue", "red"))+
  facet_wrap(.~aspect)+
  scale_y_continuous(limits = c(0,120))+
  labs(x = "Early snow snow_depth (cm)",y = "Shrub Height (cm)") + #, color = "Dominant Life Form: "
  theme_bw()+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "bottom",               #c(0.2, 0.5)
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 17, face = "bold", vjust = 0.5))+
  
  stat_fit_glance(method = "lm",
                  label.x =  c(0.5,0),
                  method.args = list(formula = y ~ x),
                  mapping = aes(label = sprintf('R^2~"="~%.3f~~italic(P)~"="~%.2g',
                                                stat(r.squared), stat(p.value))),parse = TRUE)



