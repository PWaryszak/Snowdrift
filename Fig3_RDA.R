#Load VEG DATA and these packages=======
library("readxl")
library("tidyverse")
library("vegan")#WEB on NMDS: https://jonlefcheck.net/2012/10/24/nmds-tutorial-in-r/
library("plotly")#WEB: https://towardsdatascience.com/make-beautiful-3d-plots-in-r-an-enhancement-on-the-story-telling-613ddd11e98
library(ggfortify)

setwd("C:/Users/poles/Documents/00Deakin_Docs/R/SusannaVenn/SEM/Shrub-snowdrift/Structural Equation Model/Emily/shrubs snowdrift/Snowdrift_SEM")
pre_veg_matrix <- read.csv("VegSnowSoilWindData_SEM7.csv") #As produced in CombineData.R file - That's our MASTER FILE!
names(pre_veg_matrix) #Veg matrix is occupying columns between Acae_nova and Xero_subu

#Filter-out veg species columns only:
veg_matrix <- pre_veg_matrix %>% 
  unite("SampleID2",c(SampleID,  shrub), sep = "_", remove = F) %>%  #Keeping full shrub name in SampleID as letter O may mean Orites or Ozothamnus
  
  select(SampleID2, Acae_nova:Xero_subu ) %>%
  select(-moss) %>%
  
  mutate(SpecRowSums = specnumber(pre_veg_matrix[,(which(names(pre_veg_matrix)=='Acae_nova'):which(names(pre_veg_matrix)=='Xero_subu'))])) %>% #Summing rows of species covers
  filter(SpecRowSums > 0) %>%  #removing 0-rows of species (singletons)
  select(SampleID2, Acae_nova:Xero_subu )

names(veg_matrix) #Check names 109 species
dim(veg_matrix)#242 110 = 242 plots with total of 109 species columns + SampleID2 columns


#CLEAN VEG data========
community_matrix <- veg_matrix  %>%  select(-SampleID2) #Remove non-plants
community_matrix[is.na(community_matrix)] <- 0 #replaces NA=s with zero
dim(community_matrix) #242 109

#check if  Singleton columns were all removed:
occur.cols <- apply(community_matrix,2,sum)#sum species occurrences in each column
table(occur.cols) #no zero abundance

#remove 0-sum  columns if any:
good.matrix <- community_matrix [ , ! occur.cols <= 0  ] #removing all 0-sum  columns
dim(good.matrix) #242 109
RowSum<-rowSums(good.matrix)
range(RowSum)#24.5 191.5 = ALL GOOD!
names(good.matrix)

#Triple Check:
zero_species <- as.data.frame(occur.cols) %>% filter(occur.cols == 0) #Double checking on 0-sum columns
zero_species #NONE


#ENV DATA ========
#Convert SampleID2 into environmental explanatory variables for plotting:
old_env <- as.data.frame(veg_matrix %>% 
                           select(SampleID2) %>%
                           separate(SampleID2, c("region", "site", "shrub_code", "aspect","shrub"), sep="_", remove = F)) %>%
  filter(aspect =="SE" | aspect == "NW") %>%
  mutate(shrub2 = str_replace(shrub, "\\.", " ")) %>%     #shrub2 is for plotting. Removing connecting "." with "space" for beautiful plots
  unite("SampleID", c("region", "site", "shrub_code", "aspect"), sep="_", remove = F) #unite again to match with plant height data from snow_stats (LMER_OG)




#TRAIT DATA:=====
#MErge height variables to old_env to run RDA on it (Find snow_stats in LMER_OG Rfile):
#After merging early and late snow and density (we updated alpine_data to alpine_data_updated.csv)
#lots of SampleID plots are triplicated hence average the terms before stats:
#Load snow prepared and cured in CombineData.R file:
setwd("C:/Users/poles/Documents/00Deakin_Docs/R/SusannaVenn/SEM/Shrub-snowdrift/Structural Equation Model/Emily/shrubs snowdrift/Snowdrift")
snow <- read.csv(file = "VegSnowWindData_MasterFile.csv") #Master Data joined in CombineData.R file - early and late snow was combined in the excel as per email/cat (email title: "Original snow depth and density calculations sheet" )
dim(snow)#3040  162

#Clean the snow data:
snow2 <- snow %>% 
  mutate( AreaType = ifelse(height_cm < 1, "grass", "shrub")) %>% #Only OG (Open Grass) plots have height below 1 cm.
  filter(shrub_genus !=  "Closed" ) %>% #Remove  "Closed" heath (CH) No data there. 
  
  #Fill NA-rows in AreaType:  
  mutate( AreaType = ifelse(shrub_genus == "Open", "grass", "shrub"))  %>% #All OG (Open Grass) plots have height below 1 cm.
  filter( ! is.na(AreaType) )    #4 NA-s got filtered out 

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

snow_stats <- as.data.frame(snow_stats) #snow_stats hold extra tibble-style grouping variables info. This line removes it.
plant_traits <- select(snow_stats , SampleID, height_cm, LAI, area_cm3)
env <-left_join(old_env, plant_traits, by = "SampleID")

env$AreaType <- ifelse(grepl('OG', env$shrub_code), 'Grass', 'Shrub') #Rename OG to Grass (OG = Open Grassy)
veg.env<-cbind(good.matrix,env)
names(veg.env)
unique(env$shrub2)#"Nematolepis ovatifolia", "Orites lanceolata","Epacris petrophylla" ,"Grevillea australis","Hovea montana" "Open grassy"   ,  "Ozothamnus alpina"   
names(env)#"region", "site","shrub_code" "aspect", "shrub",  "shrub2" ,"AreaType"  


#RDA on region=======
BRAYalpine <- vegdist(good.matrix, distance = "bray")#compute dissimilarity indices between comm-s
df.response <- decostand( BRAYalpine, method = 'hellinger' )#standardization method  for community data frame (df)

#Run RDA model:
region_rda_model<- rda(df.response ~ region, env, tidy =T)
#Plot simple:
plot(region_rda_model, choices=c(1,2), display= c('bp','sites'), scaling=1) #c('bp','sites')
plot(region_rda_model)
coef(region_rda_model)#regionKNP -0.1366486

summary(region_rda_model)
anova(region_rda_model,by = "margin", first = TRUE)

R2 <- RsquareAdj(region_rda_model)$adj.r.squared
round (R2,2) # 0.23

#RDA on aspect=======
BRAYalpine <- vegdist(good.matrix, distance = "bray")#compute dissimilarity indices between comm-s
df.response <- decostand( BRAYalpine, method = 'hellinger' )#standardization method  for community data

aspect_rda_model<- rda(df.response ~ aspect, env, tidy =T)

plot(aspect_rda_model, choices=c(1,2), display= c('bp','sites'), scaling=1) #c('bp','sites')
anova(aspect_rda_model,by = "margin")
##Model: rda(formula = df.response ~ aspect, data = env, tidy = T)
#Df  Variance      F             Pr(>F)
#aspect     1 0.0000133 0.1333  0.999

R2 <- RsquareAdj(aspect_rda_model)$adj.r.squared
round (R2,2) # 0! No Effect!


#RDA on AreaType=======
BRAYalpine <- vegdist(good.matrix, distance = "bray")#compute dissimilarity indices between comm-s
df.response <- decostand( BRAYalpine, method = 'hellinger' )#standardization method  for community data

AreaType_rda_model<- rda(df.response ~ AreaType, env, tidy =T)

plot(AreaType_rda_model, choices=c(1,2), display= c('bp','sites'), scaling=1) #c('bp','sites')
summary(AreaType_rda_model)

anova(AreaType_rda_model,by = "margin")
##Model: rda(formula = df.response ~ AreaType, data = env, tidy = T)
#Df  Variance      F             Pr(>F)
#AreaType   1 0.002075 22.678  0.001 ***
#Residual 240 0.021960            

R2 <- RsquareAdj(AreaType_rda_model)$adj.r.squared
round (R2,2) # 0.08

RDA1_varex<-round(summary(AreaType_rda_model)$cont$importance[2,1]*100,1) #Get percentage of variance explained by first axis
RDA1_varex #8.6

#RDA on plant height_cm=======
BRAYalpine <- vegdist(good.matrix, distance = "bray")#compute dissimilarity indices between comm-s
df.response <- decostand( BRAYalpine, method = 'hellinger' )#standardization method  for community data frame (df)


#Run RDA model:
height_cm_rda_model<- rda(df.response ~ height_cm, env, tidy =T,  na.action = na.omit) #na.action = na.omit handles zeros that rda does not accept
#Plot simple:
plot(height_cm_rda_model, choices=c(1,2), display= c('bp','sites'), scaling=1) #c('bp','sites')
plot(height_cm_rda_model)
coef(height_cm_rda_model)#height_cmKNP -0.1366486

summary(height_cm_rda_model)
anova(height_cm_rda_model,by = "margin", first = TRUE)

R2 <- RsquareAdj(height_cm_rda_model)$adj.r.squared
round (R2,2) # 0.03

RDA1_varex<-round(summary(height_cm_rda_model)$cont$importance[2,1]*100,1) #Get percentage of variance explained by first axis
RDA1_varex #3.9

#Multi-Model RDA (Shrub, aspect, region) ==========

#Same as above but only one shrub column (Same results of anova)
#Vegetation distance matrix construction [Cover Values]:
BRAYalpine <- vegdist(good.matrix, distance = "bray")#compute dissimilarity indices between comm-s
df.response <- decostand( BRAYalpine, method = 'hellinger' )#standardization method  for community data
multi_alpine_rda <- rda(df.response ~ shrub+region, env)
summary(multi_alpine_rda)

#PLOT multi_alpine_rda
plot(multi_alpine_rda, display = c("sites", "bp"), scaling=2)
screeplot(multi_alpine_rda)

ggord(multi_alpine_rda ,env$shrub) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



anova.cca(multi_alpine_rda, by = "margin", first =FALSE)
anova.cca(multi_alpine_rda, by = "axis")
#Model: rda(formula = df.response ~ shrub + region, data = env)
#Df  Variance       F Pr(>F)    
#RDA1       1 0.0060090 86.4963  0.001 ***
#RDA2       1 0.0014252 20.5145  0.001 ***

R2 <- RsquareAdj(multi_alpine_rda)$adj.r.squared
round (R2,2) # 0.3

#RDA on 6 target shrubs============
env2 <- env %>%          #Throw shrub levels into columns to fit rda data format where 1=present, 0=absent
  select(shrub) %>%
  mutate(Grevillea.australis =  ifelse(grepl('Grevillea.australis', env$shrub), 1, 0)) %>%
  mutate(Hovea.montana =  ifelse(grepl('Hovea.montana', env$shrub), 1,0)) %>%
  mutate(Orites.lanceolata =  ifelse(grepl('Orites.lanceolata', env$shrub), 1,0)) %>%
  mutate(Ozothamnus.alpina =  ifelse(grepl('Ozothamnus.alpina', env$shrub), 1,0)) %>%
  mutate(Nematolepis.ovatifolia =  ifelse(grepl('Nematolepis.ovatifolia', env$shrub), 1,0)) %>%
  mutate(Epacris.petrophylla =  ifelse(grepl('Epacris.petrophylla', env$shrub), 1,0)) %>%
  mutate(Open.grassy =  ifelse(grepl('Open.grassy', env$shrub), 1,0)) %>%
  select(-shrub)


#RDA:
BRAYalpine <- vegdist(good.matrix, distance = "bray")#compute dissimilarity indices between comm-s
df.response <- decostand( BRAYalpine, method = 'hellinger' )#standardization method  for community data

rda_model<- rda(df.response ~ ., env2, tidy =T)

plot(rda_model, choices=c(1,2), display= c('bp','sites'), scaling=1) #c('bp','sites')

ggord(rda_model, env2$shrub) + 
  scale_y_continuous(limits = c(-1, 1))+
  scale_x_continuous(limits = c(-1,1))+
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

summary(rda_model)
anova(rda_model,by = "margin")
#Number of permutations: 999
#Model: rda(formula = df.response ~ Grevillea.australis + Hovea.montana + Orites.lanceolata + Ozothamnus.alpina + Nematolepis.ovatifolia + Open.grassy, data = env2)
#           Df  Variance      F Pr(>F)    
#Model      6 0.0077091 18.495  0.001 ***
# Residual 235 0.0163256   

R2 <- RsquareAdj(rda_model)$adj.r.squared
round (R2,2) # 0.3


#PLOT RDA on 6 target shrubs + open grassy============
#Good Code: https://programmer.ink/think/r-redundancy-analysis-rda-ggplot2.html
# Enable the r-universe repository. RUN ONCE:
options(repos = c(  fawda123 = 'https://fawda123.r-universe.dev',  CRAN = 'https://cloud.r-project.org'))
# Install ggord
install.packages('ggord')
library(ggord) #READ THIS on RDA: https://fukamilab.github.io/BIO202/06-B-constrained-ordination.html

#Same as above but only one shrub column (Same results of anova)
#Vegetation distance matrix construction [Cover Values]:
BRAYalpine <- vegdist(good.matrix, distance = "bray")#compute dissimilarity indices between comm-s
df.response <- decostand( BRAYalpine, method = 'hellinger' )#standardization method  for community data

alpine_rda <- rda(df.response ~ shrub, env)
alpine_rda
summary(alpine_rda)

plot(alpine_rda, display = c("sites", "bp"), scaling=2)
screeplot(alpine_rda)

ggord(alpine_rda ,env$shrub) + 
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



anova.cca(alpine_rda, by = "margin")
#Model: rda(formula = df.response ~ shrub, data = env)
#          Df  Variance      F Pr(>F)    
#shrub      6 0.0077091 18.495  0.001 ***
#Residual 235 0.0163256                  

#R2  of shrub effect:
R2 <- RsquareAdj(alpine_rda)$adj.r.squared
round (R2,2) # 0.3

RDA1_alpine <- round(100 * RsquareAdj(alpine_rda)$adj.r.squared * summary(alpine_rda)$concont$importance[2,1], digits = 1)
RDA1_alpine  #23.6
RDA2_alpine <- round(100 * RsquareAdj(alpine_rda)$adj.r.squared * summary(alpine_rda)$concont$importance[3,2], digits = 1)
RDA2_alpine  #29.3

#Variance % explained:
RDA1_varex<-round(summary(alpine_rda)$cont$importance[2,1]*100,1) #Get percentage of variance explained by first axis
RDA1_varex #25
RDA2_varex<-round(summary(alpine_rda)$cont$importance[2,2]*100,1) #Get percentage of variance explained by second axis
RDA2_varex #5.93


#Plot alpine RDA with ggplot
# Use the "scores" function, for casting the scores to data frames:
df.sites <- as.data.frame( scores(alpine_rda)$site)
df.sites $ Shrub <- env$shrub
df.sites $ ID <- env$SampleID
df.sites $ Region <- env$region


# The centroids 4 environment variables 
df.env <- as.data.frame( alpine_rda$CCA$centroids[, 1:2] )
df.env$var <- rownames(df.env)
df.env$xOrg <- 0 #for plotting arrows start point
df.env$yOrg <- 0 #for plotting arrows start point

#PLOT:
df.env$var <-as.factor(as.character(df.env$var))
df.env$var <- factor(df.env$var,levels=c("shrubEpacris.petrophylla","shrubNematolepis.ovatifolia","shrubOzothamnus.alpina" ,
                                         "shrubHovea.montana","shrubGrevillea.australis","shrubOrites.lanceolata",
                                         "shrubOpen.grassy" ))
unique(df.env$var )

alpinePlot <- ggplot(data=df.sites, aes(x=RDA1, y=RDA2 )) +  # , color= Region
  geom_point(size=4,aes(shape=Region),alpha=0.4) +
  scale_shape_manual(values=c(22,24))+
  
  #scale_color_manual(values =  c("red","green", "blue"))+
  #scale_x_continuous(limits = c(-0.3,0.6))+
  
  xlab(paste0(as.character(RDA1_varex) ,' % of variation (RDA1)')) + 
  ylab(paste0(as.character(RDA2_varex) ,' % of variation (RDA2)')) +
  
  geom_hline(yintercept=0, colour="black", linetype="dotted" ) +
  geom_vline(xintercept=0, colour="black", linetype="dotted" ) +
  
  geom_segment(data=df.env, aes(x=xOrg, y=yOrg, xend=RDA1, yend=RDA2), size=3,
               colour= c("deeppink", "royalblue", "royalblue","deeppink","darkolivegreen", "royalblue","deeppink" ), arrow=arrow(length=unit(7,"point"),type = "open") )+
  
  annotate("text", x = -0.1, y = 0.2, label = c("Epacris petrophylla"), size=5, color="deeppink",fontface="italic")+
  annotate("text", x = -0.1, y = 0.15, label = c("Nematolepis ovatifolia"), size=5, color="deeppink",fontface="italic") + 
  annotate("text", x = -0.1, y = 0.1, label = c("Ozothamnus alpina"), size=5, color="deeppink",fontface="italic") + 
  annotate("text", x = 0.15, y = -0.2, label = c("Orites lancifolius"), size=5, color="royalblue",fontface="italic") + 
  annotate("text", x = 0.15, y = -0.15, label = c("Grevillea australis"), size=5, color="royalblue",fontface="italic") + 
  annotate("text", x = 0.15, y = -0.1, label = c("Hovea montana"), size=5, color="royalblue",fontface="italic") +
  annotate("text", x = 0.08, y = 0.2, label = c("Open grassy"), size=5, color="darkolivegreen",fontface="bold")+ 
  
  
  theme_classic()+
  theme(axis.text.x = element_text(size=20,hjust=.5,vjust=.5,face="plain", color="black"),
        axis.text.y = element_text(size=20,hjust=1,vjust=0,face="plain", color="black"),  
        axis.title.x = element_text(size=22,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(size=22),
        plot.title = element_text(size=20, lineheight=1.8, face="bold", hjust = 0.5),
        legend.title = element_text(size=22),
        legend.text = element_text(size = 12),
        legend.key = element_rect( fill = "white", color = "black"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        strip.text=element_text(size=14),
        legend.position = c(0.1,0.85)) 

#ggtitle("Alpine Plant Communities (Australia)")

alpinePlot     

#ggsave(alpinePlot, width = 12, height = 7, file = "AlpineRDA_GOOD03.png")
