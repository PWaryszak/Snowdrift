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
snow <- read.csv(file = "VegSnowWindData_MasterFile.csv") #Master Data joined in CombineData.R file - early and late snow was combined in the excel as per email/cat (email title: "Original snow depth and density calculations sheet" )
dim(snow)#3040  167
names(snow)

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



names(snow2)


#Wind model:=========
#After merging early and late snow depth & snow density records were triplicated. Run AV first:
snow3 <- snow2 %>%
  group_by(SampleID,aspect) %>%
  summarise(Wind_Ave_ms = mean(Wind_Ave, na.rm=T),
            Wind_Max_ms = mean(Wind_Max, na.rm=T))

Wind_model<-lm(Wind_Ave_ms ~  aspect  , data=snow3)
tab_model(Wind_model)
plot_model(Wind_model,  type = "pred",dot.size = 5,line.size=2,
  title = "Wind Speed (m/s)", axis.title =  "")

Wind_max_model<-lm(Wind_Max_ms ~  aspect, data=snow3)
tab_model(Wind_model, Wind_max_model, show.stat=T, show.se=T, show.ci = F) #Table S2 n the Manuscript, output of snow3 = output of snow2 YAY!

#Wind Rose Plot==========
#install.packages("clifro")
library("clifro")
# Create some dummy wind data with predominant south to westerly winds, and
# occasional yet higher wind speeds from the NE (not too dissimilar to
# Auckland).

wind_df = data.frame(wind_speeds = c(rweibull(80, 2, 4), rweibull(20, 3, 9)),
                     wind_dirs = c(rnorm(80, 135, 55), rnorm(20, 315, 35)) %% 360,
                     station = rep(rep(c("Station A", "Station B"), 2),
                                   rep(c(40, 10), each = 2)))

# Plot a simple windrose using all the defaults, ignoring any facet variable
with(wind_df, windrose(wind_speeds, wind_dirs))

# Create custom speed bins, add a legend title, and change to a B&W theme
with(wind_df, windrose(wind_speeds, wind_dirs,
                       speed_cuts = c(3, 6, 9, 12),
                       legend_title = "Wind Speed\n(m/s)",
                       legend.title.align = .5,
                       ggtheme = "bw",
                       col_pal = "Greys"))

# Note that underscore-separated arguments come from the windrose method, and
# period-separated arguments come from ggplot2::theme().

# Include a facet variable with one level
with(wind_df, windrose(wind_speeds, wind_dirs, "Artificial Auckland Wind"))

# Plot a windrose for each level of the facet variable (each station)
with(wind_df, windrose(wind_speeds, wind_dirs, station, n_col = 2))

## Not run: 
# Save the plot as a png to the current working directory
library(ggplot2)
ggsave("my_windrose.png")



snow4 <- snow2 %>%
  group_by(SampleID,aspect,Region) %>%
  summarise(Wind_Ave_ms = mean(Wind_Ave, na.rm=T),
            Wind_Max_ms = mean(Wind_Max, na.rm=T)) %>%
  mutate(wind_speeds = Wind_Ave_ms) %>%
  mutate(wind_dirs = ifelse(aspect == "NW", 315, 135))


max(snow4$wind_speeds)

with(snow4, windrose(wind_speeds, wind_dirs, Region, n_col = 2))

with(snow4, windrose(wind_speeds, wind_dirs, Region, n_col = 2,
                       speed_cuts = c(2, 4, 6),
                       legend_title = "Wind Speed\n(m/s)",
  legend.position = "top",
                       legend.title.align = .5,
                       ggtheme = "bw"))


