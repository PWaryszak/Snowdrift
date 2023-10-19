#LOAD LIBRARIES FIRST:
#Script to process raw iButton temperature data to get number of snow days (days under snow per year):
#library(filesstrings) # useful package to change file names: https://cran.r-project.org/web/packages/filesstrings/vignettes/files.html

if (!require(data.table)) install.packages('data.table')
if (!require(lubridate)) install.packages('lubridate')
if (!require(tidyverse)) install.packages('tidyverse')

library(data.table)
library(tidyverse)
library(lubridate)

#IMPORT iButton DATA ======
setwd("C:/Users/poles/Documents/00Deakin_Docs/R/SusannaVenn/SEM/Shrub-snowdrift/raw data - yet to be processed/soil temperature ibuttons/iButtonsMerged") #SET Working Directory on your computer
files <- list.files() #These files were cleaned before in iButton.R to remove spaces and special characters from file names.
files                 #see the listed file names. I used csv file names to create SampleID (refers to sampling unit in Master File)

#Create a function to read all iButton csv files:
Read_iButton <- function(x) read.csv (file = x, skip = 19) #Adjust to suit your file format. Skip  19 rows full of iButton "junk" in raw iButton csv file.

#Use Read_iButton function to read-in all csv files into one dataset:
iButtonData <- sapply(files, Read_iButton, simplify=FALSE) %>%  #Combines all files into one dataset using function 
  bind_rows(.id = "id1") #column name where csv file name goes

str(iButtonData)# Check the new combined data structure 
names(iButtonData)#Mine looked like this: "id1"       "Date.Time" "Unit"      "Value"    

#COMPUTE SNOW DAYS===========

iButtonData_processed <- iButtonData %>%
  separate( Date.Time,  c("date", "time", "AM_PM"), sep = " ", remove = F) %>% #Separates date from time
  
  mutate(SampleID = str_sub (id1, 1,-5)) %>%    #Cut end characters out off "id" column = ".csv"
  
  mutate(YearOffFile = as.numeric(as.character(str_sub (SampleID, -2)))) %>% #Get Year off File to compare to Year of Logged Date 
  mutate(YearOffDate = as.numeric(as.character(str_sub (date, -2)))) %>%
  
  #Loggers collected in 2020 recorded soil temp in 2019 so if 2020-2019=1 keep, else chuck:
  mutate(FullYear = YearOffDate) %>%
  mutate(ToKeep = ifelse(YearOffFile - YearOffDate == 1, "keep","chuck" )) %>% #Chuck away files from different year as these were incomplete (lab cabinet air T record)
  filter(ToKeep == "keep") %>%
  
  group_by(date, SampleID, FullYear) %>% #aggregate by date, year and plot (Our SampleID with 4 measurements a day))
  summarise(AV_diurnal = mean(Value))%>%  #compute mean temp per day (4 measurements, same as rolling mean)
  
  mutate( SnowPresence = ifelse(AV_diurnal < 1.6 & AV_diurnal> -1, 1,0 )) %>%  # SnowPresence 1=OneDay,
   
  #Temperature close to 0 C and diurnal oscillation in the temp. trace by < 1C generally meant that the soil was snow covered;
  group_by(SampleID,FullYear) %>% #aggregate by plot (SampleID)
  summarise(SnowDayNumber = sum(SnowPresence)) %>% #compute SnowDay number in 2016. it includes zeros as well
  
  filter(SnowDayNumber>1)#SnowDayNumber Records of 0 or 1 snowdays are faulty. Filter them out and keep values grater than 1

head(iButtonData_processed,n=3)
#SampleID                  FullYear SnowDayNumber
#1 BHP_Knoll_G1_NW_2020       19           113
#2 BHP_Knoll_G1_SE_2020       19           130
#3 BHP_Knoll_G2_NW_2020       19           124
