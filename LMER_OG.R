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
setwd("C:/Users/BlueCarbon/Documents/00DeakinUni/R/SusannaVenn/SEM/Shrub-snowdrift/Structural Equation Model/Emily/shrubs snowdrift/Snowdrift_SEM")
snow <- read.csv("VegSnowSoilWindData_SEM7.csv") #Master Data joined in CombineData.R file

#Explore/Wrangle Data:
dim(snow)#374 157
names(snow)
unique(snow$shrub_code) #See names

#NA-s and Height_cm:
summary(snow$height_cm)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.00   25.00   40.00   41.76   60.00  110.00       4 

snow2 <- snow %>% mutate( AreaType = ifelse(height_cm < 1, "grass", "shrub")) %>% #Only OG (Open Grass) plots have height below 1 cm.
  mutate(AreaType = ifelse(grepl("CH", shrub_code), "heath", AreaType)) %>%  #Identify Closed Heath (CH) No data there.  
 filter(AreaType != "heath")%>% #Remove heath (CH) No data there. 
 filter(!is.na(AreaType))    #4 NA-s got filtered out 


#First STEP, lm for data exploration:=========
#Check if CH and OG are different in snow/wind variables:
summary(lm(early_depth_cm ~ aspect+AreaType, data = snow2)) #Yes aspect only
summary(lm(late_depth_cm ~ AreaType, data = snow2)) #No
summary(lm(early_density_gcm3 ~ AreaType, data = snow2)) #No
summary(lm(late_density_gcm3 ~ AreaType, data = snow2)) #No
summary(lm(snow_days_1_aug_average ~ AreaType, data = snow2)) #No

summary(lm(snow_days_average ~ AreaType, data = snow2)) #Yes
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    113.833      3.618  31.464   <2e-16 ***
# AreaTypeshrub   10.038      3.715   2.702   0.0073 ** 


#WIND:=========
round(summary(snow2$Wind_Ave),1)
###Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.5     1.1     1.7     2.5     4.1     5.9     257 

summary(lm(Wind_Ave ~ aspect, data = snow2)) #No
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   3.5389     0.1893  18.697  < 2e-16 ***
# aspectSE     -2.1254    0.2659  -7.994 1.71e-11 ***

summary( lmer(Wind_Ave ~  aspect  + (1|region), data=snow2))
#Estimate Std. Error      df t value Pr(>|t|)    
#(Intercept)   3.4987     0.2292  1.4540  15.262    0.014 *  
# aspectSE     -2.1223     0.2648 70.0541  -8.015 1.71e-11 ***


Wind_model<-lm(Wind_Ave ~  aspect  , data=snow2)
tab_model(Wind_model)

Wind_max_model<-lm(Wind_Max ~  aspect , data=snow2)
tab_model(Wind_model, Wind_max_model, show.stat=T, show.se=T, show.ci = F) #Table S2 n the Manuscript

#AREA_cm3=======
summary(lm(area_cm3 ~ AreaType, data = snow2)) #YES but there are reports on faulty area data
#                  Estimate Std. Error t value Pr(>|t|)    
#Intercept)   -5.127e-11  5.029e+03    0.00        1    
#AreaTypeshrub  3.806e+04  5.421e+03    7.02 1.28e-11 ***


#LAI=======
summary(lm(LAI ~ AreaType, data = snow2)) #YES but there are reports on faulty area data
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   -3.139e-14  2.031e-01    0.00        1    
#AreaTypeshrub  4.904e+00  2.190e-01   22.39   <2e-16 ***

summary(lm(height_cm ~ AreaType, data = snow2)) #YES but there are reports on faulty area data
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   -1.252e-13  2.634e+00     0.0        1    
#AreaTypeshrub  4.657e+01  2.840e+00    16.4   <2e-16 ***




------------------------------------------------------------------------------



#The results from LMERs that incorporate AreaType
#LMER 4 RESULTS========
### Snow~Veg+aspect models tested below: ##
#early_depth_cm_model (Table S3 in Manuscript):============
summary( lmer(early_depth_cm ~ AreaType + aspect + height_cm + (1|region), data=snow2))
#               Estimate    Std. Error  df     t value  Pr(>|t|)    
#(Intercept)    37.16987    2.57058   3.50215  14.460 0.000301 ***
# AreaTypeshrub  -5.56970    2.57754 313.00165  -2.161 0.031465 *  
# aspectSE        6.10723    1.30615 313.00000   4.676 4.36e-06 ***
# height_cm       0.19251    0.03844 313.05044   5.008 9.19e-07 ***

  
model_early_depth_cm <- lmer(early_depth_cm ~ AreaType+ aspect + height_cm + (1|region), data=snow2)
tab_model(model_early_depth_cm )

#extract slopes:
coef(summary(model_early_depth_cm))
ranef(model_early_depth_cm)
fixef(model_early_depth_cm)

#Trying to get slope for height in SE vs NW:
#GEt slope from plot:
snowSE <- snow2 %>% filter( aspect == "SE")
summary(lm(snowSE$early_depth_cm ~ snowSE$height_cm,data=snowSE))

snowNW <- snow2 %>% filter( aspect == "NW")
summary(lm(snowNW$early_depth_cm ~ snowNW$height_cm,data=snowNW))


round(fixef( lmer(early_depth_cm ~ AreaType + height_cm + (1|region), data=snowSE)),2)
#(Intercept) AreaTypeshrub     height_cm 
#40.2234848    -6.6800969     0.2947837 
round(fixef( lmer(early_depth_cm ~ AreaType + height_cm + (1|region), data=snowNW)),2)
#(Intercept) AreaTypeshrub     height_cm 
#40.22348485   -4.45124901    0.08956889 


#late_depth_cm_model============
late_depth_cm_model <- lmer(late_depth_cm ~ AreaType+ aspect + height_cm + (1|region), data=snow2)
summary(late_depth_cm_model)#Height  effect only
tab_model(late_depth_cm_model)

#Table S3 in Manuscript:
tab_model(model_early_depth_cm,late_depth_cm_model,show.stat=T, show.se=T, show.ci = F )



#early_density_gcm3_model============
early_density_gcm3_model <- lmer(early_density_gcm3 ~ AreaType + aspect + height_cm + (1|region), data=snow2)
summary(early_density_gcm3_model)#Yes effect of Aspect
#(Intercept)    2.947e-01  4.549e-02  1.192e+00   6.478   0.0712 .  
#AreaTypeshrub -3.732e-02  1.925e-02  2.760e+02  -1.939   0.0535 .  
#aspectSE       5.445e-02  9.716e-03  2.760e+02   5.604 5.06e-08 ***
#height_cm      4.116e-04  2.808e-04  2.760e+02   1.466   0.1438    

model_early_density_gcm3 <- lmer(early_depth_cm ~ AreaType + aspect + height_cm + (1|region), data=snow2)


#late_density_gcm3_model============
late_density_gcm3_model <- lmer(late_density_gcm3 ~  AreaType+ aspect + height_cm + (1|region), data=snow2)
summary(late_density_gcm3_model)#No effect

tab_model(model_early_density_gcm3,late_density_gcm3_model, show.stat=T, show.se=T, show.ci = F )


#snow_days_1_aug_model============
snow_days_1_aug_model <- lmer(snow_days_1_aug_average ~ AreaType + aspect + height_cm  + (1|region), data=snow2)
summary(snow_days_1_aug_model)#No effect of aspect
#             Estimate Std. Error       df  t value Pr(>|t|)    
#(Intercept)  59.31708    2.70210   3.13333  21.952 0.000155 ***
#  aspectSE   0.39718    1.06890 267.82204   0.372  0.710498    
#height_cm    0.10016    0.03553  91.45865   2.819  0.005901 **



#snow_days_model============
snow_days_model <- lmer(snow_days_average ~ AreaType+ aspect + height_cm + (1|region), data=snow2)
summary(snow_days_model)#No effect of aspect
##(Intercept)   113.01490    5.36001   2.57146  21.085 0.000597 ***
#AreaTypeshrub   2.73964    4.04284 286.03100   0.678 0.498540    
#aspectSE        1.91146    1.57394 286.00385   1.214 0.225579    
#height_cm       0.15167    0.04098 286.00404   3.701 0.000257 ***

#Table S5 in the Manuscript:
tab_model(snow_days_model, snow_days_1_aug_model, show.stat=T, show.se=T, show.ci = F )


#late_snow_depth_ratio model=======
snow3<- snow2 %>%
  filter(late_snow_depth_ratio > 0)

late_snow_depth_ratio_model <- lmer(late_snow_depth_ratio ~  height_cm + AreaType+ LAI +area_cm3 + (1|region), data=snow3)
summary(late_snow_depth_ratio_model)#No effect


#LAI + area_cm3 model=======
late_snow_density_ratio_model <- lmer(late_snow_density_ratio ~  AreaType+ height_cm+ LAI+area_cm3  + (1|region), data=snow3)
summary(late_snow_density_ratio_model)#No effect of aspect
#Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)    1.007e+00  7.912e-02  1.967e+01  12.731 5.95e-11 ***
#AreaTypeshrub  1.052e-01  1.608e-01  2.428e+02   0.654  0.51376    
#height_cm     -2.969e-03  2.164e-03  2.424e+02  -1.372  0.17148    
#LAI           -2.324e-02  1.914e-02  2.430e+02  -1.214  0.22576    
#area_cm3       3.219e-06  1.093e-06  2.401e+02   2.947  0.00353 ** 


#Richness_model============
Richness_model <- lmer(Richness ~ AreaType + aspect + height_cm + (1|region), data = snow2)
summary(Richness_model) # YES EFFECT!
#            Estimate Std. Error  df t value Pr(>|t|)    
#(Intercept)     9.03990    2.10270   1.31611   4.299 0.099156 .  
#AreaTypeshrub  -1.56778    0.97230  31.89429  -1.612 0.116719    
#aspectSE        0.97479    0.40605 215.31042   2.401 0.017217 *  
#height_cm       0.04782    0.01228  33.84179   3.894 0.000441 ***


#Diagnostics:
qqnorm(resid(Richness_model))
qqline(resid(Richness_model))

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


#Diversity_model============
Diversity_model <- lmer(Diversity ~ AreaType + aspect + height_cm + (1|region), data = snow2)
summary(Diversity_model) # Height EFFECT ONLY!
#Estimate Std. Error        df t value Pr(>|t|)    
#(Intercept)   8.533e-01  1.903e-01 1.580e+00   4.484   0.0702 .  
#AreaTypeshrub 6.726e-02  1.094e-01 2.333e+02   0.615   0.5393    
#aspectSE      1.657e-02  5.020e-02 2.330e+02   0.330   0.7416    
#height_cm     6.321e-03  1.378e-03 2.330e+02   4.587 7.36e-06 ***

#Check if Richness correlates with Diversity
summary(lm(Richness~Diversity, data = snow2))
cor.test(snow2$Richness, snow2$Diversity,method = "pearson") #STRONG CORRELATION OF 0.645383 

#Cover_model============
Cover_model <- lmer(Cover ~ AreaType + aspect  + height_cm +(1|region), data = snow2)
summary(Cover_model)#No effect

#Table S5 in Manuscri[t:

tab_model(Richness_model, Cover_model, show.stat=T, show.se=T, show.ci = F )


#bare_model============
bare_model <- lmer(bare ~ AreaType+aspect + height_cm + (1|shrub_code), data = snow2)
summary(bare_model)#No effect

# plot fixed effects:
plot_model(bare_model, type = "pred", terms = c("aspect", "height_cm")) +
  labs(x="Aspect",y="bare", title = "Predicted effect")


#litt_model============
litt_model <- lmer(litt ~ aspect + height_cm + (1|shrub_code), data = snow2)
summary(litt_model)
#                 Estimate Std. Error       df t value Pr(>|t|)    
#(Intercept)             9.9212     1.6888   8.5731   5.875 0.000285 ***
#  aspectSE              0.3796     1.2924 209.3833   0.294 0.769253    
#HeightRank_high_shrub  -3.2361     1.6342  90.0740  -1.980 0.050722 .  


#rock_model============
rock_model <- lmer(rock ~ aspect + height_cm + (1|shrub_code), data = snow2)
summary(rock_model)#No effect

# plot fixed effects:
plot_model(rock_model, type = "pred", terms = c("aspect", "HeightRank_")) +
  labs(x="Aspect",y="rock", title = "Predicted effect")


#LIFE FROM MODEL:=======
#early_depth_cm_model============
summary( lmer(early_depth_cm ~ aspect + height_cm +(1|region)+(1|shrub_code), data=snow2))
#Estimate Std. Error        df t value Pr(>|t|)    
#(Intercept)  35.21301    1.88090  56.91796  18.721  < 2e-16 ***
#  aspectSE      5.42486    1.21549 324.82225   4.463 1.12e-05 ***
#  height_cm     0.12690    0.03711  59.88693   3.420  0.00113 ** 
#plot early_depth_cm:
ggplot(data = snow2, aes(x= early_depth_cm, y=height_cm))+ #, color= AreaType
  geom_point(size=2)+
  stat_smooth(method = "lm", col = "black")+
  scale_color_manual(values =  c("blue", "red"))+
  facet_wrap(.~aspect)+
  scale_y_continuous(limits = c(0,120))+
  labs(x = "Early snow depth (cm)",y = "Shrub Height (cm)") + #, color = "Dominant Life Form: "
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



