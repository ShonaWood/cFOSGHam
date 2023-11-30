
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
library(ggplot2)
library(dplyr)

#POA IHC sections
pcp.1 <- read.csv('./Histo/1-IHC-CP-Hibexp1.csv', sep = ',') 
unique(pcp.1$Image)

#remove redundant stings
pcp.1$Image <- sub(".vsi - 40x_\\d{1,2} #\\d{1,2}","", pcp.1$Image)

pcp.1<- pcp.1%>%
  mutate(ID = case_when(
    Image == "Image_05" ~ 'H211',
    Image == "Image_09" ~ 'H275',
    Image == "Image_11" ~ 'H069',
    Image == "Image_12" ~ 'H243',
    Image == "Image_13" ~ 'H038',
    Image == "Image_09_01" ~ 'H275',
    Image == "Image_09_02" ~ 'H275',
    Image == "Image_15" ~ 'SH19',
    Image == "Image_16" ~ 'H301',
    Image == "Image_19" ~ 'H263',
    Image == "Image_20" ~ 'H298',
    Image == "Image_25" ~ 'H250',
    Image == "Image_25_01" ~ 'H250',
    Image == "Image_28" ~ 'SH6',
    Image == "Image_31" ~ 'H276',
    Image == "Image_37" ~ 'H274',
))

pcp.1<- pcp.1%>%
  mutate(group = case_when(
    Image == "Image_05" ~ 'IBE2',
    Image == "Image_09" ~ 'IBE2',
    Image == "Image_11" ~ 'T40',
    Image == "Image_12" ~ 'ENT',
    Image == "Image_13" ~ 'T40',
    Image == "Image_09_01" ~ 'IBE2',
    Image == "Image_09_02" ~ 'IBE2',
    Image == "Image_15" ~ 'SHT40',
    Image == "Image_16" ~ 'IBE2',
    Image == "Image_19" ~ 'A1',
    Image == "Image_20" ~ 'A1',
    Image == "Image_25" ~ 'A2',
    Image == "Image_25_01" ~ 'A2',
    Image == "Image_28" ~ 'SHT40',
    Image == "Image_31" ~ 'A1',
    Image == "Image_37" ~ 'IBE2',
  ))


head(pcp.1)
summary(pcp.1)
unique(pcp.1$group)
unique(pcp.1$ID)


#Calculates individual mean FOS expression for each Animal and adds it to df. There are minimum 2 annotations of each area pr animal. 
# Columns to calculate mean
columns_to_mean <-c("num_detections", "num1", "num2","num3", 'num_neg',"tot_num_pos","num23", "fraq_pos","fraq_pos", "area")

#Missing values are 0 (missing from data generation in Qupath)
pcp.1 <- pcp.1 %>%
  mutate_if(is.numeric, ~ifelse(is.na(.), 0, .))

str(pcp.1)

# Selecting columns and renaming them
pcp.1 <- pcp.1 %>%
  select(Name, `Num.Detections`, `Num.1.`, `Num.2.`, `Num.3.`, `Num.Negative`, `Positive..`, `Area.µm.2`, group, ID) %>%
  rename(
    num_detections = `Num.Detections`,
    num1 =`Num.1.`,
    num2 = `Num.2.`,
    num3 = `Num.3.`,
    num_neg = `Num.Negative`,
    fraq_pos = `Positive..`,
    area = `Area.µm.2`)%>%
  replace_na(list(num3 = 0))%>%
  mutate(tot_num_pos = (num1 + num2 + num3))%>%
  mutate(num23 = (num2 + num3))


################################################################################
#MBH manual scan sections
pcp.2 <- read.csv('./Histo/2-IHC-CP-Hibexp1.csv', sep = ',')


#remove redundant stings
pcp.2$Image <- s <- sub(".vsi - 40x_\\d{1,2} #\\d{1,2}","", pcp.2$Image)

unique(pcp.2$Image)



pcp.2<- pcp.2%>%
  mutate(group = case_when(
    Image == "Image_01" ~ 'T40', #H028 
    Image == "Image_02" ~ 'IBE1',
    Image == "Image_03" ~ 'st40',
    Image == "Image_04" ~ 'ENT',
    Image == "Image_05" ~ 'T40', #H028 filter away -> duplicate img 01 more optimal position
    Image == "Image_06" ~ 'IBE2',
    Image == "Image_07" ~ 'IBE2', 
    Image == "Image_08" ~ 'T40', #H074 filter away -> duplicate img 24 more optimal position
    Image == "Image_09" ~ 'IBE2', 
    Image == "Image_10" ~ 'T40', #H069 filter away -> duplicate img 28 more optimal position
    Image == "Image_11" ~ 'ENT',
    Image == "Image_12" ~ 'T40', #H038
    Image == "Image_13" ~ 'A1',
    Image == "Image_14" ~ 'IBE2',
    Image == "Image_15" ~ 'IBE1',
    Image == "Image_16" ~ 'T40', #H038 filter away -> duplicate img 12 more optimal position
    Image == "Image_17" ~ 'ENT',
    Image == "Image_18" ~ 'A2',
    Image == "Image_19" ~ 'A1',
    Image == "Image_20" ~ 'A1',
    Image == "Image_21" ~ 'ENT', 
    Image == "Image_22" ~ 'IBE1',
    Image == "Image_23" ~ 'IBE1',
    Image == "Image_24" ~ 'T40', #H074
    Image == "Image_25" ~ 'A2',
    Image == "Image_26" ~ 'A1',
    Image == "Image_27" ~ 'A2',
    Image == "Image_28" ~ 'T40', #H069
    Image == "Image_29" ~ 'A1',
    Image == "Image_30" ~ 'A1',
    Image == "Image_31" ~ 'IBE1',
    Image == "Image_32" ~ 'ENT',
    Image == "Image_33" ~ 'IBE1',
    Image == "Image_34" ~ 'ENT',
    TRUE ~ 'Unknown'
  ))

pcp.2<- pcp.2%>%
  mutate(ID = case_when(
    Image == "Image_01" ~ 'H028', #H028 
    Image == "Image_02" ~ 'H268',
    Image == "Image_03" ~ 'SH',
    Image == "Image_04" ~ 'H319',
    Image == "Image_05" ~ 'H028', 
    Image == "Image_06" ~ 'H301',
    Image == "Image_07" ~ 'H327', 
    Image == "Image_08" ~ 'H074', 
    Image == "Image_09" ~ 'H274', 
    Image == "Image_10" ~ 'H069', 
    Image == "Image_11" ~ 'H243',
    Image == "Image_12" ~ 'H038', 
    Image == "Image_13" ~ 'H298',
    Image == "Image_14" ~ 'H211',
    Image == "Image_15" ~ 'H245',
    Image == "Image_16" ~ 'H038',
    Image == "Image_17" ~ 'H307',
    Image == "Image_18" ~ 'H300',
    Image == "Image_19" ~ 'H216',
    Image == "Image_20" ~ 'H263',
    Image == "Image_21" ~ 'H254', 
    Image == "Image_22" ~ 'H264',
    Image == "Image_23" ~ 'H245',
    Image == "Image_24" ~ 'H074', 
    Image == "Image_25" ~ 'H250',
    Image == "Image_26" ~ 'H276',
    Image == "Image_27" ~ 'H236',
    Image == "Image_28" ~ 'H069', 
    Image == "Image_29" ~ 'H222',
    Image == "Image_30" ~ 'H273',
    Image == "Image_31" ~ 'H224',
    Image == "Image_32" ~ 'H241',
    Image == "Image_33" ~ 'H278',
    Image == "Image_34" ~ 'H230',
    TRUE ~ 'Unknown'
  ))


head(pcp.2)
summary(pcp.2)
unique(pcp.2$group)
unique(pcp.2$ID)




#Calculates individual mean FOS expression for each Animal and adds it to df. There are minimum 2 annotations of each area pr animal. 
# Columns to calculate mean
columns_to_mean <-c("num_detections", "num1", "num2","num3", 'num_neg',"tot_num_pos","num23", "fraq_pos","fraq_pos", "area")

#Missing values are 0 (missing from data generation in Qupath)
pcp.2 <- pcp.2 %>%
  mutate_if(is.numeric, ~ifelse(is.na(.), 0, .))

str(pcp.2)

# Selecting columns and renaming them
pcp.2 <- pcp.2 %>%
  select(Name, `Num.Detections`, `Num.1.`, `Num.2.`, `Num.3.`, `Num.Negative`, `Positive..`, `Area.µm.2`, group, ID) %>%
  rename(
    num_detections = `Num.Detections`,
    num1 =`Num.1.`,
    num2 = `Num.2.`,
    num3 = `Num.3.`,
    num_neg = `Num.Negative`,
    fraq_pos = `Positive..`,
    area = `Area.µm.2`)%>%
  replace_na(list(num3 = 0))%>%
  mutate(tot_num_pos = (num1 + num2 + num3))%>%
  mutate(num23 = (num2 + num3))


################################################################################
pcp <- full_join(pcp.1, pcp.2)



columns_to_mean <-c("num_detections", "num1", "num2","num3", 'num_neg',"tot_num_pos","num23", "fraq_pos","fraq_pos", "area")

#Calculating individual mean values.
sum.pcp <- pcp %>%
  group_by(Name, ID, group) %>%
  summarise(
    across(
      all_of(columns_to_mean), 
      list(mean = mean), 
      na.rm = TRUE
    ), 
    .groups = "drop")

sum.pcp$Name<- sub("PathAnnotationObject", "CP", sum.pcp$Name)

head(sum.pcp)
