#Load VEG DATA and these packages=======
library("readxl")
library("tidyverse")
library("vegan")#WEB on NMDS: https://jonlefcheck.net/2012/10/24/nmds-tutorial-in-r/
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

dim(veg_matrix)#242 110 = 242 plots with total of 110 species columns + SampleID2 columns


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
range(RowSum)#24.5 191.5 = ALL GOOD! No Zeros!
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
#lots of SampleID plots are triplicated hence average the terms before stats:
#Load snow prepared and cured in CombineData.R file:
setwd("C:/Users/poles/Documents/00Deakin_Docs/R/SusannaVenn/SEM/Shrub-snowdrift/Structural Equation Model/Emily/shrubs snowdrift/Snowdrift")
snow <- read.csv(file = "VegSnowWindData_MasterFile.csv") #Master Data joined in CombineData.R file - early and late snow was combined in the excel as per email/cat (email title: "Original snow depth and density calculations sheet" )
dim(snow)#3040  166

#Clean the snow data:
snow2 <- snow %>% 
  mutate( AreaType = ifelse(height_cm < 1, "grass", "shrub")) %>% #Only OG (Open Grass) plots have height below 1 cm.
  filter(shrub_genus !=  "Closed" ) %>% #Remove  "Closed" heath (CH) No data there. 
  
  #Fill NA-rows in AreaType:  
  mutate( AreaType = ifelse(shrub_genus == "Open", "grass", "shrub"))  %>% #All OG (Open Grass) plots have height below 1 cm.
  filter( ! is.na(AreaType) )    #4 NA-s got filtered out 


#Removing duplicate records by using mean function
##After merging early and late snow and density (we updated alpine_data to alpine_data_updated.csv)
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


#Plot RDA on 6 target shrubs + open grassy============
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
  #ggtitle("Alpine Plant Communities (Australia)")
  
  
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


alpinePlot     
#ggsave(alpinePlot, width = 12, height = 7, file = "AlpineRDA_GOOD03.png")
