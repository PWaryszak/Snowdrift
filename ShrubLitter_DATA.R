#DATA===============
library("readxl")
library(tidyverse)
library(lme4)
library(lmerTest)
library("vegan")#WEB on NMDS: https://jonlefcheck.net/2012/10/24/nmds-tutorial-in-r/
library(sjPlot)
library(sjstats)
library(sjmisc)
library(ggpmisc)


ShrubLitter_DATA <- read_xlsx("ShrubLitter_DATA.xlsx", sheet = "ShrubLitter_DATA")
head(ShrubLitter_DATA); summary(ShrubLitter_DATA); names(ShrubLitter_DATA);table(ShrubLitter_DATA$site)

ShrubLitter_DATA $ months <- as.factor(ShrubLitter_DATA $ Months_Of_Deployment) #turn numbers into factor for plotting
unique(ShrubLitter_DATA$months)


#Table:==========
#run Lmer:
soilgdds_model <- lmer(massloss_gday ~ surfaceGDDs + (1|site), data = ShrubLitter_DATA) 
tab_model(soilgdds_model, show.stat=T, show.se=T, show.ci = F , digits = 7)


#Run lm surfaceGDDs:
lm_gdd <- lm(massloss_mgday ~ surfaceGDDs, data = ShrubLitter_DATA)
plot(lm_gdd) 
summary(lm_gdd)
tab_model(lm_gdd, show.stat=T, show.se=T, show.ci = F , digits = 3)

#PLOT:==========
names(ShrubLitter_DATA)
ShrubLitter_DATA$B <-  as.factor(ShrubLitter_DATA$burialdepth_cm)

, shape = B

ggplot(aes(y = massloss_mgday, x = surfaceGDDs, shape= B), data = ShrubLitter_DATA) +
  geom_point(size = 4,aes(fill=months), stroke = 0.5) +
  labs( title = "",
    y = "Mass loss (mg/day)",
    x = "Growing degree days",
    shape = "Deployment depth: ",
    color = "Months of deployment:") +       #,  shape = "Site" or burialdepth_cm
  
  scale_shape_manual(values = c(19,15))+
  scale_color_manual(values = c("white", "grey70", "#666666")) +
  #scale_fill_manual(values = c("white", "grey80", "#666666")) +
  facet_grid(. ~ species) +
  theme_bw() +
    guides(colour = guide_legend(show = FALSE)) +

    geom_smooth( method = "lm", se = TRUE, size = 1, color = "black") +
  theme(legend.position = "top",
    legend.key.size = unit(1.5, "lines"),
    legend.background = element_rect(fill = "white", color = "black"),
    axis.title = element_text(size = 18),
    panel.grid = element_blank(),
    strip.text = element_text(face = "italic", size = 14),
    axis.text = element_text(size = 12, color = "black"))


#ggsave(width = 12, height = 7, file = "MassLoss_Species_Lm.png")


#No split by Burial Depth Plot:
ggplot(aes(y = massloss_mgday, x = surfaceGDDs, color = months), data = ShrubLitter_DATA) +
  geom_point(size = 4, aes(fill = months), shape = 21, stroke = 0.5, color = "black") +
  labs( title = "",
    y = "Mass Loss (mg/day)",
    x = "Growing Degree Days",
    fill = "Months of Deployment:") +       #,  shape = "Site" or burialdepth_cm
  
  scale_color_manual(values = c("white", "grey80", "#666666")) +
  scale_fill_manual(values = c("white", "grey80", "#666666")) +
  facet_grid(. ~ species) +
  theme_bw() +
    guides(colour = guide_legend(show = FALSE)) +

    geom_smooth( method = "lm", se = TRUE, size = 1, color = "red") +
  theme(legend.position = "top",
    legend.key.size = unit(1.5, "lines"),
    legend.background = element_rect(fill = "white", color = "black"),
    axis.title = element_text(size = 18),
    panel.grid = element_blank(),
    strip.text = element_text(face = "italic", size = 14),
    axis.text = element_text(size = 12, color = "black"))

#ggsave(width = 12, height = 7, file = "MassLoss_Species_Lm.png")
