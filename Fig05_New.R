#Load VEG DATA and these packages=======
if (!require(vegan)) install.packages('vegan')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(readxl)) install.packages('readxl')
if (!require(ggfortify)) install.packages('ggfortify')

library("readxl")
library("tidyverse")
library("vegan")#WEB on NMDS: https://jonlefcheck.net/2012/10/24/nmds-tutorial-in-r/
library("ggfortify")

pre_veg_matrix <- read.csv("VegSnowWindData_MasterFile.csv") #Master Data joined in CombineData.R file - early and late snow was combined in the excel as per email/cat (email title: "Original snow depth and density calculations sheet" )
names(pre_veg_matrix) #Veg matrix is occupying columns between Acae_nova and Xero_subu

#Filter-out veg species columns only:
veg_matrix <- pre_veg_matrix %>% 
  unite("SampleID2",c(SampleID,  shrub), sep = "_", remove = F) %>%  #Keeping full shrub name in SampleID as letter O may mean Orites or Ozothamnus
  
  select(SampleID2, Acae_nova:Xero_subu ) %>%
  select(-moss) %>%
  
  mutate(SpecRowSums = specnumber(pre_veg_matrix[,(which(names(pre_veg_matrix)=='Acae_nova'):which(names(pre_veg_matrix)=='Xero_subu'))])) %>% #Summing rows of species covers
  filter(SpecRowSums > 0) %>%  #removing 0-rows of species (singletons)
  select(SampleID2, Acae_nova:Xero_subu ) %>%
  
#After merging early and late snow and density into just snow depth and density (for alpine_data_updated.csv)
#SampleID plots were triplicated after merging columns together hence average the terms before stats:
   
  group_by(SampleID2)%>%  
  summarise_at(vars(Acae_nova:Xero_subu),mean,na.rm=T) #Reversing the triplicates

dim(veg_matrix)#242 114 = 242 plots with total of 113 species columns + SampleID2 columns
names(veg_matrix)






#CLEAN VEG data========
community_matrix <- veg_matrix  %>%  select(-SampleID2) #Remove non-plants
community_matrix[is.na(community_matrix)] <- 0 #replaces NA=s with zero
dim(community_matrix) #242 113

#check if  Singleton columns were all removed:
occur.cols <- apply(community_matrix,2,sum)#sum species occurrences in each column
table(occur.cols) #no zero abundance

#remove 0-sum  columns if any:
good.matrix <- community_matrix [ , ! occur.cols <= 0  ] #removing all 0-sum  columns
dim(good.matrix) #242 112
names(good.matrix)

RowSum<-rowSums(good.matrix)
range(RowSum)#24.5 191.5 = ALL GOOD! No Zeros!

#Triple Check 4 veg data quality:
zero_species <- as.data.frame(occur.cols) %>% filter(occur.cols == 0) #Double checking on 0-sum columns, remove them if any from good.matrix
zero_species #Cras_grac          0 but it seems absent already from good.matrix
#good.matrix <- good.matrix %>% select ( - Cras_grac)

#ENV DATA ========
#Convert SampleID2 into environmental explanatory variables for plotting:
env <- as.data.frame(veg_matrix %>% 
                           select(SampleID2) %>%
                           separate(SampleID2, c("region", "site", "shrub_code", "aspect","shrub"), sep="_", remove = F)) %>%
  filter(aspect =="SE" | aspect == "NW") %>%
  mutate(shrub2 = str_replace(shrub, "\\.", " ")) %>%     #shrub2 is for plotting. Removing connecting "." with "space" for beautiful plots
  unite("SampleID", c("region", "site", "shrub_code", "aspect"), sep="_", remove = F) #unite again to match with plant height data from snow_stats (LMER_OG)


#Plot RDA on 6 target shrubs + open grassy============
#Vegetation distance matrix construction [Cover Values]:
BRAYalpine <- vegdist(good.matrix, distance = "bray")#compute dissimilarity indices between comm-s
df.response <- decostand( BRAYalpine, method = 'hellinger' )#standardization method  for community data

alpine_rda <- rda(df.response ~ shrub, env)
alpine_rda
summary(alpine_rda)

plot(alpine_rda, display = c("sites", "bp"), scaling=2)
screeplot(alpine_rda)

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
alpinePlot <- ggplot(data=df.sites, aes(x=RDA1, y=RDA2 )) +  # , color= Region
  geom_point(size=4,aes(shape=Region)) +   #blur edges with alpha=0.4
  scale_shape_manual(values=c(22,24))+
  
  #scale_color_manual(values =  c("red","green", "#666666"))+
  #scale_x_continuous(limits = c(-0.3,0.6))+
  #ggtitle("Alpine Plant Communities (Australia)")
  
  
  xlab(paste0(as.character(RDA1_varex) ,' % of variation (RDA1)')) + 
  ylab(paste0(as.character(RDA2_varex) ,' % of variation (RDA2)')) +
  
  geom_hline(yintercept=0, colour="black", linetype="dotted" ) +
  geom_vline(xintercept=0, colour="black", linetype="dotted" ) +
  
  geom_segment(data=df.env, aes(x=xOrg, y=yOrg, xend=RDA1, yend=RDA2), size=3,
               #Change colours from  to: "#999999", "#666666",  scale_color_gradient(low = "#666666", high =  "#999999")+  #Colour pattern consistent with Fig3 and Fig2
               colour= c("#999999", "#666666", "#666666","#999999","#CCCCCC", "#666666","#999999" ), arrow=arrow(length=unit(7,"point"),type = "open") )+
  
  annotate("text", x = -0.1, y = 0.2, label = c("Epacris petrophylla"), size=5, color="#999999",fontface="italic")+
  annotate("text", x = -0.1, y = 0.15, label = c("Nematolepis ovatifolia"), size=5, color="#999999",fontface="italic") + 
  annotate("text", x = -0.1, y = 0.1, label = c("Ozothamnus alpina"), size=5, color="#999999",fontface="italic") + 
  annotate("text", x = 0.15, y = -0.2, label = c("Orites lancifolius"), size=5, color="#666666",fontface="italic") + 
  annotate("text", x = 0.15, y = -0.15, label = c("Grevillea australis"), size=5, color="#666666",fontface="italic") + 
  annotate("text", x = 0.15, y = -0.1, label = c("Hovea montana"), size=5, color="#666666",fontface="italic") +
  annotate("text", x = 0.08, y = 0.2, label = c("Open grassy"), size=5, color="#CCCCCC",fontface="bold")+ 
  
  
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
ggsave(alpinePlot, width = 12, height = 7, file = "Fig05_AlpinePlantComposition_RDA_GREY.jpg")



#ANOVA RDA:  assessing the significance of constraint of aspect:=========
alpine_rda2 <- rda(df.response ~ aspect, env)
alpine_rda2
summary(alpine_rda2)

plot(alpine_rda2, display = c("sites", "bp"), scaling=2)
screeplot(alpine_rda2)

anova.cca(alpine_rda2, by = "margin")

#Model: rda(formula = df.response ~ aspect, data = env)
#          Df  Variance      F Pr(>F)
#aspect     1 0.0000133 0.1334      1
3Residual 240 0.0240213                     

#ANOVA RDA:  assessing the significance of constraint of region:=========
alpine_rda3 <- rda(df.response ~ region, env)
alpine_rda3
summary(alpine_rda3)

plot(alpine_rda3, display = c("sites", "bp"), scaling=2)
anova.cca(alpine_rda3, by = "margin")

#Model: rda(formula = df.response ~ aspect, data = env)
#          Df  Variance      F Pr(>F)
#aspect     1 0.0000133 0.1334      1
#Residual 240 0.0240213                     


