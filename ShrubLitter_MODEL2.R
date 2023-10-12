#LOAD DATA and PACKAGES===============
library(readxl)#install.packages(readxl) if library throws an error.
library(tidyverse)
library(lme4)
library(lmerTest)
library(sjPlot)
library(sjstats)
library(sjmisc)
library(ggpmisc)

ShrubLitter_DATA <- read_xlsx("ShrubLitter_DATA.xlsx", sheet = "ShrubLitter_DATA")
head(ShrubLitter_DATA); summary(ShrubLitter_DATA); names(ShrubLitter_DATA);table(ShrubLitter_DATA$site)

ShrubLitter_DATA $ months <- as.factor(ShrubLitter_DATA $ Months_Of_Deployment) #turn numbers into factor for plotting
unique(ShrubLitter_DATA$months)

names(ShrubLitter_DATA)
ShrubLitter_DATA$Burial_cm <-  as.factor(ShrubLitter_DATA$burialdepth_cm)


# massloss_mgday MODEL==============
#RESCALED MODEL (Better Fit)
#Rescale the data to increase Interpretability of model
#When variables have different scales,#it can be challenging to interpret the coefficients or feature importances in your model
#because the effect of each variable is not on the same scale.

#scale-function = Standardization (Z-score scaling): Subtract the mean of each variable from its values and divide #by the standard deviation. 
#This scales the variables to have a mean of 0 and a standard deviation of 1.
ShrubLitter_DATA$SLA_mm2mg_scaled <- scale(ShrubLitter_DATA$SLA_mm2mg)
ShrubLitter_DATA$Elevation<- scale(ShrubLitter_DATA$Site_m)
ShrubLitter_DATA$Elevation_scaled<- scale(ShrubLitter_DATA$Site_m)
ShrubLitter_DATA$burialdepth_cm_scaled<- scale(ShrubLitter_DATA$burialdepth_cm)
ShrubLitter_DATA$timeburied_days_scaled <- scale(ShrubLitter_DATA$timeburied_days)

litter_model_scaled <- lmer(massloss_mgday ~  Elevation_scaled +burialdepth_cm_scaled + 
    timeburied_days_scaled + SLA_mm2mg_scaled  +
    Elevation_scaled          * timeburied_days_scaled +
    Elevation_scaled         * SLA_mm2mg_scaled +
    timeburied_days_scaled * SLA_mm2mg_scaled +  (1|site), data = ShrubLitter_DATA)

tab_model(litter_model_scaled , show.stat=T, show.se=T, show.ci = F , digits = 1,show.re.var=F, show.icc=F)
plot_model(litter_model_scaled)

#ORIGINAL MODEL (Originally Different scales):
litter_model_orig <- lmer(massloss_mgday ~  Elevation +burialdepth_cm + 
    timeburied_days + SLA_mm2mg  +
    Elevation         * timeburied_days +
    Elevation         * SLA_mm2mg +
    timeburied_days * SLA_mm2mg +  (1|site), data = ShrubLitter_DATA)

tab_model(litter_model_orig , show.stat=T, show.se=T, show.ci = F , digits = 1, 
         show.icc=FALSE, show.dev = FALSE, show.re.var=F)


#AIC=========
AIC(litter_model_scaled,litter_model_orig)
#                    df      AIC
#litter_model_scaled 10 3979.035
#litter_model_orig   10 4012.931
