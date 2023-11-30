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


#initial data wrageling
################################################################################
#MBH FOS Protein restained

pMBH <- read_csv("./Histo/MBH_Cell_Pos_detect_cFOS_Hibexp1_20230816.csv")


#check structure
str(pMBH)

#check Names, Image and spellings are correct and fix eventual misspellings etc
unique(pMBH$Name)


unique(pMBH$Image)

#remove redundant stings
pMBH$Image <- sub(" - 40x_\\d{1,2} #\\d{1,2}", "", pMBH$Image)

# Now printing unique values
unique(pMBH$Image)

table(sort(pMBH$Image, decreasing = TRUE))



#Give Sample group ID
pMBH <- pMBH %>%
  mutate(group = case_when(
    Image == "Image_01.vsi" ~ 'T40', #H028 
    Image == "Image_02.vsi" ~ 'IBE1',
    Image == "Image_03.vsi" ~ 'st40',
    Image == "Image_04.vsi" ~ 'ENT',
    Image == "Image_05.vsi" ~ 'T40', #H028 filter away -> duplicate img 01 more optimal position
    Image == "Image_06.vsi" ~ 'IBE2',
    Image == "Image_07.vsi" ~ 'IBE2', 
    Image == "Image_08.vsi" ~ 'T40',  #H074 filter away -> duplicate img 24 more optimal position
    Image == "Image_09.vsi" ~ 'IBE2', 
    Image == "Image_10.vsi" ~ 'T40', #H069 filter away -> duplicate img 28 more optimal position
    Image == "Image_11.vsi" ~ 'ENT',
    Image == "Image_12.vsi" ~ 'T40', #H038
    Image == "Image_13.vsi" ~ 'A1',
    Image == "Image_14.vsi" ~ 'IBE2',
    Image == "Image_15.vsi" ~ 'IBE1',
    Image == "Image_16.vsi" ~ 'T40', #H038 filter away -> duplicate img 12 more optimal position
    Image == "Image_17.vsi" ~ 'ENT',
    Image == "Image_18.vsi" ~ 'A2',
    Image == "Image_19.vsi" ~ 'A1',
    Image == "Image_20.vsi" ~ 'A1',
    Image == "Image_21.vsi" ~ 'ENT', 
    Image == "Image_22.vsi" ~ 'IBE1',
    Image == "Image_23.vsi" ~ 'IBE1',
    Image == "Image_24.vsi" ~ 'T40', #H074
    Image == "Image_25.vsi" ~ 'A2',
    Image == "Image_26.vsi" ~ 'A1',
    Image == "Image_27.vsi" ~ 'A2',
    Image == "Image_28.vsi" ~ 'T40', #H069
    Image == "Image_29.vsi" ~ 'A1',
    Image == "Image_30.vsi" ~ 'A1',
    Image == "Image_31.vsi" ~ 'IBE1',
    Image == "Image_32.vsi" ~ 'ENT',
    Image == "Image_33.vsi" ~ 'IBE1',
    Image == "Image_34.vsi" ~ 'ENT',
    TRUE ~ 'Unknown'
  ))




sum(is.na(pMBH$group))
unique(pMBH$group)


#Some filtering to exclude data that are not relevant. 
pMBH <- pMBH%>%
  filter(group != "st40")%>%
  filter(Image !=  "Image_09.vsi")%>% #Animal removed because thermoregulatory action discovered at time of sampling. See animal H274 in Anipill_plotall.R
  filter(!(Image %in% c("Image_08.vsi", "Image_10.vsi", "Image_16.vsi", "Image_05.vsi"))) #Duplicates filtered away

head(pMBH)


#Calculates individual mean FOS expression for each Animal and adds it to df. There are minimum 2 annotations of each area pr animal. 
# Columns to calculate mean
columns_to_mean <-c("num_detections", "num1", "num2","num3", 'num_neg',"tot_num_pos","num23", "fraq_pos","fraq_pos", "area")

#Missing values are 0 (missing from data generation in Qupath)
pMBH <- pMBH %>%
  mutate_if(is.numeric, ~ifelse(is.na(.), 0, .))

head(pMBH)

# Selecting columns and renaming them
pMBH <- pMBH %>%
  select(Image, Name, `Num Detections`, `Num 1+`, `Num 2+`, `Num 3+`, `Num Negative`, `Positive %`, `Area µm^2`, group) %>%
  rename(
    num_detections = `Num Detections`,
    num1 = `Num 1+`,
    num2 = `Num 2+`,
    num3 = `Num 3+`,
    num_neg = `Num Negative`,
    fraq_pos = `Positive %`,
    area = `Area µm^2`)%>%
  replace_na(list(num3 = 0))%>%
  mutate(tot_num_pos = (num1 + num2 + num3))%>%
  mutate(num23 = (num2 + num3))


###############################################################################


str(pMBH)

columns_to_mean <-c("num_detections", "num1", "num2","num3", 'num_neg',"tot_num_pos","num23", "fraq_pos","fraq_pos", "area")

#Calculating individual mean values.
sum.pMBH <- pMBH %>%
  group_by(Name, Image, group) %>%
  summarise(
    across(
      all_of(columns_to_mean), 
      list(mean = mean), 
      na.rm = TRUE
    ), 
    .groups = "drop")
str(sum.pMBH)

#Before transform (DMH)
pretrans_data <- sum.pMBH %>%
  filter(Name == "DMH")



ggplot(pretrans_data, aes(x = num_detections_mean)) +
 geom_histogram(binwidth = 4, fill = "blue", color = "black", alpha = 0.7) +
 labs(title = "Histogram of tot_num_pos_mean", x = "tot_num_pos_mean", y = "Frequency") +
  theme_minimal()

#Check the spread if cells values are evenly spread. 
#ggplotly(pMBH %>% 
  #         filter(Name=="DMH")%>%
 #          group_by(group) %>%
 #          ggplot(aes(num_detections,num23, color = group)) +
 #          geom_point(position = position_dodge(width = 0.5)))

#applying sqrt transform (count data containing 0)
columns_to_trans <-c("num_detections_mean","tot_num_pos_mean","num23_mean", "area_mean")

sum.pMBH <- sum.pMBH %>%
  mutate(across(all_of(columns_to_trans), ~sqrt(.), .names = "trans_{.col}"))

#trans_data <- sum.pMBH %>%
#  filter(Name == "DMH")


#qqnorm(trans_data$tot_num_pos)
#qqline(trans_data$tot_num_pos, col = "blue")

#ggplot(trans_data, aes(x = trans_num_detections_mean)) +
#  geom_histogram(binwidth = 0.5, fill = "blue", color = "black", alpha = 0.7) +
 # labs(title = "Histogram of tot_num_pos_mean", x = "tot_num_pos", y = "Frequency") +
#  theme_minimal()


#check structure and summary data:
str(sum.pMBH)
summary(sum.pMBH)

###################################################################################################
# Compute mean IBE2 group values for each 'Name' (ROIs):
IBE_K_values <-sum.pMBH %>%
  filter(group == "IBE2") %>%
  group_by(Name) %>%
  summarise(IBE_K = mean((trans_num23_mean)/(trans_area_mean*trans_num_detections_mean)), .groups = "drop")

# Add the column 'IBE_K' to the main df
sum.pMBH<- left_join(sum.pMBH, IBE_K_values, by = "Name")

# Rows with NA in ibe_k cannot be used
sum.pMBH <- sum.pMBH[complete.cases(sum.pMBH$IBE_K), ]

# Now calculate 'FOS_to_IBE_k_individual' for all rows. Normalization of expression relative to mean value of IBE2 group and total number of detection in the ROI.
sum.pMBH <- sum.pMBH%>%
  mutate(FOS_to_IBE_k_individual = (trans_num23_mean /(trans_area_mean*trans_num_detections_mean))/ (IBE_K))



summary(sum.pMBH)


