#ShrubLitter_DATA
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
head(ShrubLitter_DATA); summary(ShrubLitter_DATA); names(ShrubLitter_DATA)
table(ShrubLitter_DATA$site)
#  B   H  KB   S 
#174 176 173 110 

#Run Regression on surfaceGDDs:
soilgdds_model <- lmer(massloss_gday ~ surfaceGDDs + (1|site), data = ShrubLitter_DATA) 
summary(lm(massloss_gday ~ surfaceGDDs, data = ShrubLitter_DATA)) 
plot(lm(massloss_gday ~ surfaceGDDs, data = ShrubLitter_DATA)) 

#Table:
tab_model(soilgdds_model, show.stat=T, show.se=T, show.ci = F , digits = 7)

ShrubLitter_DATA $ months <- as.factor(ShrubLitter_DATA $ Months_Of_Deployment)
unique(ShrubLitter_DATA$months)

#PLOT:
names(ShrubLitter_DATA)


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

ggsave(width = 12, height = 7, file = "MassLoss_Species_Lm.png")

