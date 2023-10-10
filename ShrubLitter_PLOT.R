#LOAD DATA:==========
#Install Packages:
install.packages("readxl")
install.packages("tidyverse")
install.packages("ggpmisc")

#Load Packages:
library("readxl")
library("tidyverse")
library("ggpmisc")

#Shrub Litter DATA:
ShrubLitter_DATA <- read_xlsx("ShrubLitter_DATA.xlsx", sheet = "ShrubLitter_DATA")
head(ShrubLitter_DATA); summary(ShrubLitter_DATA); names(ShrubLitter_DATA);table(ShrubLitter_DATA$site) #Quick Glimpse at data
names(ShrubLitter_DATA)

ShrubLitter_DATA $ months <- as.factor(ShrubLitter_DATA $ Months_Of_Deployment) #turn numbers into factor for plotting
unique(ShrubLitter_DATA$months)#Levels: 7 9 12

ShrubLitter_DATA$Burial_cm <-  as.factor(ShrubLitter_DATA$burialdepth_cm) #turn numbers into factor for plotting
unique(ShrubLitter_DATA$Burial_cm)# Levels: 0 15 (cm)

#THE PLOT:==========
#Different lines in lm:
ShrubLitter_DATA$months3 <- ifelse(ShrubLitter_DATA$months == 7, "07", ifelse(ShrubLitter_DATA$months==9, "09","12"))

ggplot(aes(y = massloss_mgday, x = surfaceGDDs, shape = Burial_cm, fill = months3), data = ShrubLitter_DATA) +
  geom_point(size = 4, aes(fill = months3), stroke = 0.5) +
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
    scale_linetype_manual(values = c("solid", "dashed"), guide = "none") +  # Set linetype for B levels
  
    theme(legend.position = "top",
    legend.key.size = unit(1.5, "lines"),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.key = element_rect(color = "black", fill = "white", size = 0.1),
    axis.title = element_text(size = 18),
    panel.grid = element_blank(),
    strip.text = element_text(face = "italic", size = 14),
    axis.text = element_text(size = 12, color = "black")) 
  
    

#ggsave(width = 12, height = 7, file = "GDD_MassLoss_BySpecies_ByBurialDepths_2LineTypes_UPDATED2.png")
