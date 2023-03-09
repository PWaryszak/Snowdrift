#GENERAL STATS FOR STARTING RESULTS/ABSTRACT sections IN THE PAPER:
library(tidyverse)
library(vegan)

#Load snow prepared and cured in CombineData.R file:
snow <- read.csv("VegSnowWindData_MasterFile.csv") #Master Data joined in CombineData.R file - early and late snow was combined in the excel as per email/cat (email title: "Original snow depth and density calculations sheet" )

pre_veg_matrix <- read.csv("VegSnowWindData_MasterFile.csv") #As produced in CombineData.R file
names(pre_veg_matrix) #Veg matrix is occupying columns between Acae_nova and Xero_subu


#Species number and names=======
#Filter veg species columns only:
veg_matrix <- pre_veg_matrix %>% 
  
  unite("SampleID2",c(SampleID,  shrub), sep = "_", remove = F) %>%  #Keeping full shrub name as O may mean Orites or Ozothamnus
  select(SampleID2, Acae_nova:Xero_subu ) %>%
  select(-moss) %>%
  
  group_by(SampleID2) %>%  ##After merging early and late snow depth & snow density records often duplicated. Run AV first:
  summarise_at(vars(Acae_nova:Xero_subu) , mean, na.rm = TRUE) %>%
  
  select( Acae_nova:Xero_subu ) #Select Species only

length(names(veg_matrix)) #Total species number = 113
dim(veg_matrix) #383 113

#Species List Table S (Full name, frequencies, life forms)========
#Remove Singletons columns:
occur.cols <- apply(veg_matrix,2,sum)#sum species occurrences in each column
zero_species <- as.data.frame(occur.cols) %>% filter(occur.cols == 0) #Check for zero species
zero_species # Cras_grac= 0 

#Species Frequencies:
veg_matrix3 <- veg_matrix
veg_matrix3 [veg_matrix3 > 0] <- 1 #Convert all values greater than 0 to 1
veg_matrix3

occur.cols3 <- apply(veg_matrix3, 2, sum)#sum species occurrences in each column
occur.cols3
freq <- as.data.frame(occur.cols3)
new_order <- arrange(freq, occur.cols3)
row.names(new_order)
new_order$SpecID <- row.names(new_order)

#Compute Species occurance frequencies:
total_species_number <- as.numeric(length(names(veg_matrix))) 
total_species_number #Total species number = 113
new_order$Freq_percent <- round(new_order$occur.cols3/ total_species_number  * 100, 1) #

#Join column on Life Forms
LifeFormData <- read.csv("SpecID_LifeForms.csv") #contains  SpecID","LifeForm" info
names(LifeFormData)#Columns: SpecID","LifeForm"  

new_order_LifeForms <- left_join(new_order, LifeFormData , by = "SpecID" ) %>% na.omit()

SpeciesList <- read.csv("SpecID_FullSpeciesName_List.csv")
Match_Sp <- left_join(new_order_LifeForms, SpeciesList, by = "SpecID") #full species names by matching new_order with Species List:
Match_Sp

#SAVE into the Table S
Match_Sp_Table <- Match_Sp %>% select(Plant.species.name, Freq_percent, LifeForm)

write.table(Match_Sp_Table, file = "SpeciesTableUpdated.txt", sep = ",", quote = FALSE, row.names = F)  #Save as table.txt and then copy into Word, 
#Copy to Word -> Select it all -> Table → Convert → Convert Text to Table…:
