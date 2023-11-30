library(tidyverse)
library(reshape2)

library(plotly)
library(lubridate)
library(anytime)
library(scales)
library(gridExtra)
library(viridis)
library(zoo)
library(readxl)
library(gridExtra)
library(RColorBrewer)
library(dplyr)


################################################################################
#POA FOS Protein restained

pPOA <- read_csv("./2POA_Cell_Pos_detect_cFOS_Hibexp1_20230811.csv")



#check structure
str(pPOA)

#check Names, Image and spellings are correct and fix eventual misspellings etc
unique(pPOA$Name)
#pPOA$Name <- sub("c", "", pPOA$Name) #Including caudal AVPE regions

unique(pPOA$Image)

#remove redundant stings
pPOA$Image <- sub(" - 40x_\\d{1,2} #\\d{1,2}", "", pPOA$Image)

# Now printing unique values
unique(pPOA$Image)

table(sort(pPOA$Image, decreasing = TRUE))



#Give Sample group ID
pPOA <- pPOA %>%
  mutate(group = case_when(
    Image == "Image_01.vsi" ~ 'T40',
    Image == "Image_02.vsi" ~ 'IBE1',
    Image == "Image_03.vsi" ~ 'st40',
    Image == "Image_04.vsi" ~ 'T40',
    Image == "Image_05.vsi" ~ 'IBE2',
    Image == "Image_06.vsi" ~ 'ENT',
    Image == "Image_07.vsi" ~ 'A2', #wrong annot! manual check! H236_6 -> resolved
    Image == "Image_08.vsi" ~ 'T40', #Wrong annot. manual check! H028_4 ->resolved
    Image == "Image_09_01.vsi" ~ 'IBE2', 
    Image == "Image_12.vsi" ~ 'A2',
    Image == "Image_13.vsi" ~ 'ENT',
    Image == "Image_14.vsi" ~ 'IBE1',
    Image == "Image_16.vsi" ~ 'IBE2',
    Image == "Image_17.vsi" ~ 'IBE1',
    Image == "Image_18.vsi" ~ 'A1',
    Image == "Image_19.vsi" ~ 'A1',
    Image == "Image_20.vsi" ~ 'A1',
    Image == "Image_21.vsi" ~ 'IBE1', #non usable material (too caudal)
    Image == "Image_22_01.vsi" ~ 'IBE1',
    Image == "Image_23.vsi" ~ 'A2',
    Image == "Image_24.vsi" ~ 'A1', #check ID -> duplicate removed below
    Image == "Image_25_01.vsi" ~ 'A2',
    Image == "Image_26.vsi" ~ 'st64',
    Image == "Image_27.vsi" ~ 'T40',
    Image == "Image_29.vsi" ~ 'A1', #check ID
    Image == "Image_31.vsi" ~ 'A1',
    Image == "Image_32.vsi" ~ 'ENT',
    Image == "Image_34.vsi" ~ 'ENT',
    Image == "Image_35.vsi" ~ 'st40',
    Image == "Image_36.vsi" ~ 'ENT',
    Image == "Image_37.vsi" ~ 'IBE2',
    TRUE ~ 'Unknown'
  ))


sum(is.na(pPOA$group))
unique(pPOA$Image)
summary(pPOA)

pPOA <- pPOA %>% filter(Image != "Image_24.vsi")%>%
  filter(group != "st40")

head(pPOA)
# Selecting columns and renaming them
pPOA <- pPOA %>%
  select(Image, Name, `Num Detections`, `Num 1+`, `Num 2+`, `Num 3+`, `Num Negative`, `Positive %`, `Area µm^2`, group) %>%
  rename(
    num_detections = `Num Detections`,
    num1 = `Num 1+`,
    num2 = `Num 2+`,
    num3 = `Num 3+`,
    num_neg = `Num Negative`,
    fraq_pos = `Positive %`,
    area = `Area µm^2`)%>%
  mutate(tot_num_pos = (num1 + num2 + num3))%>%
  mutate(num23 = (num2 + num3))

summary(pPOA)

#Calculates individual mean FOS expression for each Animal and adds it to df. There are minimum 2 annotations of each area pr animal. 
# Columns to calculate mean
columns_to_mean <-c("num_detections", "num1", "num2","num3", 'num_neg',"tot_num_pos","num23", "fraq_pos","fraq_pos", "area")

#Multiplies rows in column Name containing string x05 with 2 and then deletes that string in the Name
pPOA <- pPOA %>%
  mutate(across(all_of(columns_to_mean), ~ifelse(grepl("_x05", Name), . * 2, .)))%>%
  mutate(Name = gsub("_x05", "", Name))

#Calculating individual mean values.
sum.pPOA <- pPOA %>%
  group_by(Name, Image, group) %>%
  summarise(
    across(
      all_of(columns_to_mean), 
      list(mean = mean), 
      na.rm = TRUE
    ), 
    .groups = "drop")

summary(sum.pPOA)

IHC_poa <- sum.pPOA %>%
  filter(Name %in% c('AVPE', 'MnPO')) %>%  # Keep only AVPE and MnPO
  group_by(group, Image) %>%  # Group by ID
  summarise(across(where(is.numeric), sum, na.rm = TRUE))%>%  # Sum numeric columns, removing NAs
  mutate(Name = 'POA')

sum.pPOA<- full_join(sum.pPOA, IHC_poa)


#check structure and summary data:
str(sum.pPOA)
summary(sum.pPOA)



######################################################################################################


