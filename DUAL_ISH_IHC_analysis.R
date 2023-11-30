library(readr)
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
library(tidyr)
library(stringr)
library(ggplot2)
library(dplyr)




#Script for importing and groups assignments: 
################################################################################
#Dual IHC - ISH MBH, POA, CP
rna <- read_csv("./Histo/1-ISH-IHC-dual-CFOS-20230801.csv")

#check structure
str(rna)

#check Names, Image and spellings are correct and fix eventual misspellings etc
unique(rna$Name)


unique(rna$Image)
rna$Image <- as.character(rna$Image)

#remove redundant stings
rna$Image <- sub(" COMBINED","", rna$Image)
rna$Image <- sub("_Image01.vsi - EFI 40x_\\d{1,2} #\\d{1,2}","", rna$Image)
rna$Image <- sub(" - 40x_\\d{1,2} #\\d{1,2}","", rna$Image)
rna$Image <- sub(" - 40x #\\d{1,2}","", rna$Image)
rna$Image <- sub("_01.vsi - EFI 40x_\\d{1,2} #\\d{1,2}","", rna$Image)
rna$Image <- sub(" - 40x_\\d{1,2} #\\d{1,2}","", rna$Image)


# Now printing unique values
uniqueID <- rna %>%
  mutate(SlideNumber = as.integer(str_extract(Image, "\\d+"))) %>%
  arrange(SlideNumber) %>%
  select(-SlideNumber)%>%
  distinct(Image)

print(uniqueID, n=100)

#Assining sample ID
rna <- rna %>%
  mutate(ID = case_when(
    Image == "Slide 1" ~ 'H298',# 298
    Image == "Slide 2" ~ 'SH6', #SH6
    Image == "Slide 3" ~ 'H273',  #273
    Image == "Slide 4" ~ 'H301',# 301_31 Duplicate
    Image == "Slide 5" ~ 'H224',#224
    Image == "Slide 6" ~ 'H274',#274 
    Image == "Slide 7" ~ 'H263',#H263_39 
    Image == "Slide 8" ~ 'H300',#300
    Image == "Slide 9" ~ 'H236',#236
    Image == "Slide 10" ~ 'H276',#276
    Image == "Slide 11" ~ 'H275',#275
    Image == "Slide 12" ~ 'H243',#243
    Image == "Slide 13" ~ 'SH19',#SH19
    Image == "Slide 14" ~ 'H245',#245
    Image == "Slide 15" ~ 'H327', #327 DUPLICATE
    Image == "Slide 16" ~ 'H301', #301_23 Duplicate
    Image == "Slide 17" ~ 'SH26', #SH26
    Image == "Slide 18" ~ 'SH14', #SH14
    Image == "Slide 19" ~ 'H327', #327 DUPLICATE
    Image == "Slide 20" ~ 'H211', #211
    Image == "Slide 21" ~ 'H254', #254
    Image == "Slide 22" ~ 'H278', #H278_37
    Image == "Slide 23" ~ 'SH21', #SH21_12
    Image == "Slide 24" ~ 'H028', #H028_22
    Image == "Slide 25" ~ 'H301', #H301_43
    Image == "Slide 26" ~ 'H264', #H264_37
    Image == "Slide 27" ~ 'H069', #H069_29 DUPLICATE
    Image == "Slide 28" ~ 'H069', #H069_18 DUPLICATE
    Image == "Slide 29" ~ 'H038', #H038_16
    Image == "Slide 30" ~ 'H301', #H301_35 DUPLICATE
    Image == "Slide 31" ~ 'H074', #H074_17 DUPLICATE
    Image == "Slide 32" ~ 'H074', #H074_25 DUPLICATE
    Image == "Slide 33" ~ 'H268', #H268_41
    Image == "Slide 34" ~ 'H222', #H222_21
    Image == "Slide 35" ~ 'H248', #H248_15 DUPLICATE
    Image == "Slide 36" ~ 'H248', #H248_13 DUPLICATE
    Image == "Slide 37" ~ 'H028', #H028_17
    Image == "Slide 38" ~ 'SH4', #SH4_36 
    Image == "Slide 39" ~ 'H241', #H241_39
    Image == "Slide 40" ~ 'H301', #H301_7
    Image == "Slide 41" ~ 'H300', #H300_1
    Image == "Slide 42" ~ 'H069', #H069_7
    Image == "Slide 43" ~ 'H038', #H038_7
    Image == "Slide 44" ~ 'H264', #H264_1 (NB! wrong id in .vsi metadata)
    Image == "Slide 45" ~ 'H245', #H245_1
    Image == "Slide 46" ~ 'H241', #H241_1
    Image == "Slide 47" ~ 'H216', #H216_1
    Image == "Slide 48" ~ 'H278', #H278_3
    Image == "Slide 49" ~ 'SH4', #SH4_1   
    Image == "Slide 50" ~ 'H319', #"H319_3"
    Image == "Slide 51" ~ 'H298', #H298_7
    Image == "Slide 52" ~ 'H268', #H268_1
    Image == "Slide 53" ~ 'H028', #H028_5
    Image == "Slide 54" ~ 'H074', #H074_5
    Image == "Slide 55" ~ 'H250', #H250_1
    Image == "Slide 56" ~ 'H263', #H263_1
    Image == "Slide 57" ~ 'H301', #H301_33
    Image == "Slide 58" ~ 'H230', #H230_1
    Image == "Slide 59" ~ 'SH26', #SH26_2
    Image == "Slide 60" ~ 'H276', #H276_3
    Image == "Slide 61" ~ 'H254', #H254_1
    Image == "Slide 62" ~ 'H224', #H224_1
    Image == "Slide 63" ~ 'H243', #H243_3
    Image == "Slide 64" ~ 'H211', #H211_1
    Image == "Slide 65" ~ 'SH21', #SH21_41
    Image == "Slide 67" ~ 'H236', #H236_1
    Image == "Slide 68" ~ 'H222', #H222_3
    Image == "Slide 69" ~ 'SH6', #"SH6_2"
    Image == "Slide 70" ~ 'SH19', #SH19_61
    Image == "Slide 71" ~ 'H243', #H243_1
    Image == "Slide 72" ~ 'H236', #H236_3
    Image == "Slide_73" ~ 'BS1', #brainstem
    Image == "Slide 74" ~ 'H273', #H273_3
    Image == "Slide 75" ~ 'H307', #H307_1
    Image == "Slide 76" ~ 'BS2', #brainstem
    Image == "Slide 77" ~ 'H319', #H319_37
    Image == "Slide 78" ~ 'H275', #H275_1
    Image == "Slide 79" ~ 'SH14', #SH14_1
    Image == "Image_10.vsi" ~ 'H216', #H216
    TRUE ~ 'Unknown'
  ))

table(unique(rna$ID))

#Give Sample group ID
rna <- rna %>%
  mutate(group = case_when(
    Image == "Slide 1" ~ 'A1',# 298
    Image == "Slide 2" ~ 'SHT64', #SH6
    Image == "Slide 3" ~ 'A2',  #273
    Image == "Slide 4" ~ 'IBE2',# 301
    Image == "Slide 5" ~ 'IBE1',#224
    Image == "Slide 6" ~ 'IBE2',#274
    Image == "Slide 7" ~ 'A1',#263
    Image == "Slide 8" ~ 'A2',#300
    Image == "Slide 9" ~ 'A2',#236
    Image == "Slide 10" ~ 'A1',#276
    Image == "Slide 11" ~ 'IBE2',#275
    Image == "Slide 12" ~ 'ENT',#243
    Image == "Slide 13" ~ 'SHT40',#SH19
    Image == "Slide 14" ~ 'IBE1',#245
    Image == "Slide 15" ~ 'IBE2', #327 DUPLICATE
    Image == "Slide 16" ~ 'IBE2', #301
    Image == "Slide 17" ~ 'SHT40', #SH26
    Image == "Slide 18" ~ 'SHT26', #SH14
    Image == "Slide 19" ~ 'IBE2', #327 DUPLICATE
    Image == "Slide 20" ~ 'IBE2', #211
    Image == "Slide 21" ~ 'ENT', #254                       Pretty
    Image == "Slide 22" ~ 'IBE1', #H278_37
    Image == "Slide 23" ~ 'SHT64', #SH21_12
    Image == "Slide 24" ~ 'T40', #H028_22                   For figure
    Image == "Slide 25" ~ 'IBE2', #H301_43
    Image == "Slide 26" ~ 'IBE1', #H264_37
    Image == "Slide 27" ~ 'T40', #H069_29 DUPLICATE
    Image == "Slide 28" ~ 'T40', #H069_18 DUPLICATE
    Image == "Slide 29" ~ 'T40', #H038_16
    Image == "Slide 30" ~ 'IBE2', #H301_35 
    Image == "Slide 31" ~ 'T40', #H074_17  DUPLICATE
    Image == "Slide 32" ~ 'T40', #H074_25 DUPLICATE
    Image == "Slide 33" ~ 'IBE1', #H268_41
    Image == "Slide 34" ~ 'A1', #H222_21
    Image == "Slide 35" ~ 'A2', #H248_15 DUPLICATE
    Image == "Slide 36" ~ 'A2', #H248_13 DUPLICATE
    Image == "Slide 37" ~ 'T40', #H028_17
    Image == "Slide 38" ~ 'SHT32', #SH4_36
    Image == "Slide 39" ~ 'ENT', #H241_39                           Pretty
    Image == "Slide 40" ~ 'IBE2', #H301_7
    Image == "Slide 41" ~ 'A2', #H300_1
    Image == "Slide 42" ~ 'T40', #H069_7
    Image == "Slide 43" ~ 'T40', #H038_7
    Image == "Slide 44" ~ 'IBE1', #H264_1 (NB! wrong id in .vsi metadata)
    Image == "Slide 45" ~ 'IBE1', #H245_1
    Image == "Slide 46" ~ 'ENT', #H241_1
    Image == "Slide 47" ~ 'A1', #H216_1
    Image == "Slide 48" ~ 'IBE1', #H278_3
    Image == "Slide 49" ~ 'SHT32', #SH4_1
    Image == "Slide 50" ~ 'ENT', #"H319_3"
    Image == "Slide 51" ~ 'A1', #H298_7
    Image == "Slide 52" ~ 'IBE1', #H268_1
    Image == "Slide 53" ~ 'T40', #H028_5
    Image == "Slide 54" ~ 'T40', #H074_5
    Image == "Slide 55" ~ 'A2', #H250_1
    Image == "Slide 56" ~ 'A1', #H263_1
    Image == "Slide 57" ~ 'IBE2', #H301_33
    Image == "Slide 58" ~ 'ENT', #H230_1
    Image == "Slide 59" ~ 'SHT40', #SH26_2
    Image == "Slide 60" ~ 'A1', #H276_3
    Image == "Slide 61" ~ 'ENT', #H254_1
    Image == "Slide 62" ~ 'IBE1', #H224_1
    Image == "Slide 63" ~ 'ENT', #H243_3 duplicate
    Image == "Slide 64" ~ 'IBE2', #H211_1
    Image == "Slide 65" ~ 'SHT64', #SH21_41
    Image == "Slide 66" ~ 'IBE2', #H274_13
    Image == "Slide 67" ~ 'A2', #H236_1
    Image == "Slide 68" ~ 'A1', #H222_3
    Image == "Slide 69" ~ 'SHT64', #"SH6_2"
    Image == "Slide 70" ~ 'SHT40', #SH19_61
    Image == "Slide 71" ~ 'ENT', #H243_1
    Image == "Slide 72" ~ 'A2', #H236_3
    Image == "Slide_73" ~ 'SHT32', #SH14_1
    Image == "Slide 74" ~ 'A2', #H273_3
    Image == "Slide 75" ~ 'ENT', #H307_1
    Image == "Slide 76" ~ 'BS', #brainstem
    Image == "Slide 77" ~ 'ENT', #H319_37
    Image == "Slide 78" ~ 'IBE2', #H275_1
    Image == "Slide 79" ~ 'BS', #brainstem
    Image == "Image_10.vsi" ~ 'A1', #H216
    TRUE ~ 'Unknown'
  ))


unique(rna$group)


#Some filtering to exclude data that are not relevant. 
rna <- rna %>%
  filter(!group %in% c("SHT40", "SHT26", "SHT64", "SHT32")) #%>%
 

str(rna)

# Selecting columns and renaming them
rna <- rna%>%
  select(Image,ID, group, Name, `Area µm^2`, `Num Detections`, `Num Negative`, `Num Positive`, `Positive %`, `FOS-Thresh-1: Positive area µm^2`, `FOS-Thresh-1: Negative area µm^2`) %>%
  rename(
    num_det = `Num Detections`,
    num_neg = `Num Negative`,
    num_pos = `Num Positive`,
    fraq_pos = `Positive %`,
    area = `Area µm^2`,
    pos_th = `FOS-Thresh-1: Positive area µm^2`,
    neg_th = `FOS-Thresh-1: Negative area µm^2`)

head(rna)
summary(rna)


columns_to_mean <-c("num_det", "num_neg", "num_pos", "fraq_pos", "area", "pos_th", "neg_th")


#Multiplies rows in column Name containing string x05 with 2 and then deletes that string in the Name
rna <- rna %>%
  mutate(across(all_of(columns_to_mean), ~ifelse(grepl("_x05", Name), . * 2, .)))%>%
  mutate(Name = gsub("_x05", "", Name))


#Calculates individual mean FOS expression for each Animal and adds it to df. There are minimum 2 annotations of each area pr animal. 
#Columns to calculate mean
#Calculating individual mean values.
sum.rna <- rna %>%
  group_by(Name, ID, group) %>%
  summarise(
    across(
      all_of(columns_to_mean), 
      list(mean = mean), 
      na.rm = TRUE
    ), 
    .groups = "drop")


rna_poa <- sum.rna %>%
  filter(Name %in% c('AVPE', 'MnPO')) %>%  # Keep only AVPE and MnPO
  group_by(group,ID) %>%  # Group by ID
  summarise(across(where(is.numeric), sum, na.rm = TRUE))%>%  # Sum numeric columns, removing NAs
  mutate(Name = 'POA')

sum.rna<- full_join(sum.rna, rna_poa)


table_inspect<- sum.rna %>%
  filter(group == "ENT" & Name == "MnPO")

sum.ISH$num_pos_mean <- replace_na(data = sum.ISH$num_pos_mean, replace = 0)

write.csv(sum.rna, "./Histo/ISH-SUMMARY-ALL.csv")

#################################################################################
################################################################################
head(sum.rna)

#Name: Regions (ROI)
#group = groups in the experiment
#ID = individual ID
#Num_det_mean: Mean number of cells within ROI
#pos_th_mean: Positive detection model that gave the best result. 
#This is the AREA of Positive cFOS RNA. Area is used because several cells may share
#the same area, thus computer counts detections as one but in reality there are 2 or more
#thus area better reflects the amount of RNA present in the section. 

#area_mean: The mean area of the ROI


# Calculation of 'AreaFOSmm2' for all rows: individal mean expression nr of cFOS per mm2
sum.rna <- sum.rna%>%
  mutate(AreaFOSmm2 = (pos_th_mean /(area_mean/1000000))) #divide area by 1000000 to convert to mm2

summary(sum.rna)


SE<- function(x) sd(x)/sqrt(length(x)) #Function for creating SEM


#Then create tibble containing group-wise mean value normalized to area ROI.
gr.sum.rna <- sum.rna %>%
  group_by(Name, group) %>%
  summarise(grFOSmm2=mean(AreaFOSmm2), 
            se=SE(AreaFOSmm2))

#sorting data:
table(sort(gr.sum.rna$group, decreasing = TRUE))

#check structure and summary data:
str(gr.sum.rna )
summary(gr.sum.rna)


#generate color palette:
my_palette <- brewer.pal(8, "Pastel1")

#To see what Regions are available for plotting: 
unique(sum.rna$Name)

#Enter regoin of interest
ROI <- c('POA') 
  
p1_rnMBH <- gr.sum.rna %>%
    filter(Name == ROI) %>%
    group_by(group) %>%
    ggplot(aes(group,  grFOSmm2 , fill = group)) +
    geom_col(colour = "black", width = 0.7, linewidth = 1.1) +
    geom_errorbar(
      aes(ymin = grFOSmm2 - se, ymax = grFOSmm2 + se),
      width = 0.2,
      colour = "black"
    ) +
    geom_point(
      data = subset(sum.rna, Name %in% c(ROI)),
      aes(y = AreaFOSmm2),
      color = 'black',
      size = 2,
      position = position_dodge2(width = 0.5)
    ) +
    scale_x_discrete(
      limits = c("IBE2", "ENT", "T40", "A1", "A2", "IBE1"),
      name = ""
    ) +
    ylab(expression(  frac("Area cFOS"^"+", mm^2)))+
    scale_fill_manual(values = my_palette, guide = guide_legend(override.aes = list(colour = NULL))) +
    labs(
      title = paste("ISH -", ROI)
    ) +
    ylim(0,20000)+
    theme_classic() +
    theme(
      text = element_text(family = "Arial", size = 12),
      legend.position = "none",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.margin = margin(1, 1, 1, 1, "cm"),
      plot.background = element_rect(fill = "white", colour = "black", linewidth = 1),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)
    )
  
p1_rnMBH





