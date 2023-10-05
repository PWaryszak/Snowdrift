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

names(ShrubLitter_DATA)
ShrubLitter_DATA$Burial_cm <-  as.factor(ShrubLitter_DATA$burialdepth_cm)

#Table Combined:==========
#run Lmer:
soilgdds_model <- lmer(massloss_gday ~ surfaceGDDs + (1|site), data = ShrubLitter_DATA) 
tab_model(soilgdds_model, show.stat=T, show.se=T, show.ci = F , digits = 7)


#Run lm surfaceGDDs:
#LM:
lm_gdd <- lm(massloss_mgday ~ surfaceGDDs, data = ShrubLitter_DATA)
plot(lm_gdd) 
summary(lm_gdd)
tab_model(lm_gdd, show.stat=T, show.se=T, show.ci = F , digits = 3)

#LMER:
names(ShrubLitter_DATA)
soilgdds_model2 <- lmer(massloss_gday ~ surfaceGDDs + (1|site), data = ShrubLitter_DATA) 
tab_model(soilgdds_model2, show.stat=T, show.se=T, show.ci = F , digits = 7)


#STATS TABLE:=====
library(officer)
library(flextable)

# Perform linear regression for each group (factor B) and calculate R-squared and p-value
regression_results <- ShrubLitter_DATA %>%
  group_by(Burial_cm,species) %>%
  summarise(
    R2 =round(  summary(lm(massloss_mgday ~ surfaceGDDs))$r.squared,2),
    p_value = round( coef(summary(lm(massloss_mgday ~ surfaceGDDs)))["surfaceGDDs", "Pr(>|t|)"],5))

doc <- read_docx() # Create a new Word document
ft <- flextable(regression_results)# Convert regression_results to a flextable
doc <- body_add_flextable(doc, value = ft) # Insert the flextable into the Word document
print(doc, target = "ShrubLitter_regression_results.docx")# Save the Word document:


#THE PLOT:==========
#Different lines in lm:
ggplot(aes(y = massloss_mgday, x = surfaceGDDs, shape = Burial_cm, fill = months), data = ShrubLitter_DATA) +
  geom_point(size = 4, aes(fill = factor(months)), stroke = 0.5) +
  labs(
    title = "",
    y = "Mass loss (mg/day)",
    x = "Growing degree days",
    shape = "Deployment depth (cm): ",
    fill = "Months of deployment:"
  ) +
  scale_shape_manual(values = c(21, 22)) +
  scale_fill_manual(values = c("white", "grey80", "#666666")) +
  facet_grid(. ~ species) +
  theme_bw() +
  guides(colour = guide_legend(show = FALSE)) +

  geom_smooth(method = "lm",
    se = TRUE,
    size = 1,
    color = "black",  # Set the color to black
    aes(fill = NA, linetype = Burial_cm)) +  # Use factor(Burial_cm) to separate lines by Burial_cm-variable
    
  scale_linetype_manual(values = c("solid", "dashed")) +  # Set linetype for B levels
  
    theme(legend.position = "top",
    legend.key.size = unit(1.5, "lines"),
    legend.background = element_rect(fill = "white", color = "black"),
    axis.title = element_text(size = 18),
    panel.grid = element_blank(),
    strip.text = element_text(face = "italic", size = 14),
    axis.text = element_text(size = 12, color = "black"))+
  
    guides(linetype = guide_legend(show = FALSE)) 

#ggsave(width = 12, height = 7, file = "GDD_MassLoss_BySpecies_ByBurialDepths_2LineTypes.png")