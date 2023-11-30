library(tidyverse)
library(reshape2)
library(plyr)
library(plotly)
library(lubridate)
library(anytime)
library(scales)
library(gridExtra)
library(viridis)
library(zoo)
library(readxl)
library(gridExtra)

#AREA of dertections instead of number of detections
#############################################################################

###############################################################################
#low sens ISH 
##############################################################################
#ISH MBH

#import data
low_ish_mbh <- read_csv("./4_low_sens_ISH-MBH-HibExp1.csv")

#check structure
str(low_ish_mbh)

#check Names, Image and spellings are correct and fix eventual misspellings etc
unique(low_ish_mbh$Name)

low_ish_mbh$Name<- str_replace_all(low_ish_mbh$Name, c("AVPV"="AVPE", "BSNT"="BNST", "MnPo"="MnPO", "vMPO"="MPA", "dMPO"="MPA" ))


unique(low_ish_mbh$Image)
low_ish_mbh$Image<- str_replace_all(low_ish_mbh$Image, c(" - 20xPOA"="" ," - 20x"="", "_01POA"="", "_02POA"="", "_03POA"="","_"="" ))



#Give Sample group ID
low_ish_mbh<- low_ish_mbh%>%
  mutate(group = case_when(
    Image == "H254-39.vsi"  ~ 'ENT',
    Image == "H211-45.vsi" ~ 'IBE2',
    Image == "H216-39.vsi"  ~'A1',
    Image == "H224-35.vsi"  ~'IBE1',
    Image == "H236-39.vsi"  ~'A2',
    Image == "h241-39.vsi"  ~'ENT',
    Image == "h243-39.vsi"  ~'ENT',
    Image == "H245-37.vsi"  ~ 'IBE1',
    Image == "H263-39.vsi"  ~ 'A1',  
    Image == "H264-37.vsi"  ~ 'IBE1',
    Image == "H273-43.vsi"  ~'A2',
    Image == "H274-35.vsi"  ~'IBE2', #thermoregulating? YEs
    Image == "H275-41.vsi"  ~'IBE2',
    Image == "H276-33.vsi"  ~'A1', 
    Image == "H278-37.vsi"  ~'IBE1',
    Image == "H300-19.vsi"  ~'A2',
    Image == "H298-41.vsi"  ~'A1',
    Image == "H301-23.vsi"  ~'IBE2',
    Image == "H319 - MBH.vsi"  ~'ENT',
    Image == "H327-37.vsi"  ~'IBE2',
    Image == "SH4-36.vsi"  ~'T1',
    Image == "SH6-30.vsi"  ~'T3',
    Image == "SH14-34.vsi"  ~'T1',
    Image == "SH19-11.vsi"  ~'T2',
    Image == "SH21-12.vsi"  ~'T3',
    Image == "SH26-25.vsi"  ~'T2',
    Image == "H2541.vsi"  ~ 'ENT',
    Image == "H211-1.vsi" ~ 'IBE2',
    Image == "H216-1.vsi"  ~'A1',
    Image == "H222-3.vsi"  ~'A1',
    Image == "H224-1.vsi"  ~'IBE1',
    Image == "H230-1.vsi"  ~'ENT',
    Image == "H236-3.vsi"  ~'A2',
    Image == "H241-1.vsi"  ~'ENT',
    Image == "H243-1.vsi"  ~'ENT',
    Image == "H245-1.vsi"  ~ 'IBE1',
    Image == "H250-1.vsi"  ~ 'A2',
    Image == "H263-1.vsi"  ~ 'A1',  
    Image == "H268-1.vsi"  ~ 'IBE1',
    Image == "H273-3.vsi"  ~'A2',
    Image == "H274-13.vsi"  ~'IBE2', #thermoregulating? YEs
    Image == "H275-1.vsi"  ~'IBE2',
    Image == "H278-303.vsi"  ~'IBE1',
    Image == "H300-1.vsi"  ~'A2',
    Image == "H298-7.vsi"  ~'A1',
    Image == "H301-7.vsi"  ~'IBE2',
    Image == "H307-101.vsi"  ~'ENT',
    Image == "H319-3.vsi"  ~'ENT',
    Image == "SH4-1.vsi"  ~'T1',
    Image == "SH6-2.vsi"  ~'T3',
    Image == "SH14-1.vsi"  ~'T1',
    Image == "SH19-61.vsi"  ~'T2',
    Image == "SH21-41.vsi"  ~'T3',
    Image == "SH26-2.vsi"  ~'T2',
  ))

sum(is.na(low_ish_mbh$group))

str(low_ish_mbh)
summary(low_ish_mbh)

#Some areas need to be multiplied by 2 to compensate for *0.5 area size
# Columns to multiply by 2
columns_to_modify <- c("Num Detections", "Num FOS", 'Area µm^2', "MIDLOW-SENS-FOS-thersholder: FOS area µm^2", 
                       'DAB_FOS_LOWSENS: FOS area µm^2', 'DAB_Thresh_Very LOW SENS: FOS area µm^2', 
                       'DAB-Thresholder_1: FOS area µm^2' )

#Multiplies rows in column Name containing string x05 with 2 and then deletes that string in the Name
low_ish_mbh1 <- low_ish_mbh %>%
  mutate(across(all_of(columns_to_modify), ~ifelse(grepl("x05", Name), . * 2, .)))%>%
  mutate(Name = gsub("x05", "", Name))


#Calculates individual mean FOS expression for each Animal and adds it to df. There are minimum 3 annotations of each area pr animal. 
# Columns to calculate mean
columns_to_mean <- c("Num Detections", "Num FOS", "MIDLOW-SENS-FOS-thersholder: FOS area µm^2", 'Area µm^2', 
                     'DAB_FOS_LOWSENS: FOS area µm^2', 'DAB_Thresh_Very LOW SENS: FOS area µm^2',
                     'DAB-Thresholder_1: FOS area µm^2')

#Calculating and adding the columns.
low_ish_mbh2 <- low_ish_mbh1 %>%
  group_by(Name, Image, group) %>%
  summarise(
    across(
      all_of(columns_to_mean), 
      list(mean = mean, standard_error = ~sd(.)/sqrt(length(.))), 
      na.rm = TRUE
    ), 
    .groups = "drop"
  )

str(low_ish_mbh2)


# Compute mean IBE2 group values for each 'Name' (ROIs):
IBE_K_values <- low_ish_mbh2 %>%
  filter(group == "IBE1") %>%
  group_by(Name) %>%
  summarise(IBE_K = mean(`DAB-Thresholder_1: FOS area µm^2_mean`/`Area µm^2_mean`), .groups = "drop")

# Add the column 'IBE_K' to the main df
low_ish_mbh2 <- left_join(low_ish_mbh2, IBE_K_values, by = "Name")

# Rows with NA in ibe_k cannot be used
low_ish_mbh2 <- low_ish_mbh2[complete.cases(low_ish_mbh2$IBE_K), ]

# Now calculate 'FOS_to_IBE_k_individual' for all rows
low_ish_mbh2 <- low_ish_mbh2 %>%
  mutate(FOS_to_IBE_k_individual = (`DAB-Thresholder_1: FOS area µm^2_mean`/`Area µm^2_mean`) / (IBE_K))

#Then create tibble containing group-wise mean value that's compared to IBE2
group_MBH_to_IBE2 <- low_ish_mbh2 %>%
  group_by(Name, group) %>%
  summarise(FOS_to_IBE_k=mean(FOS_to_IBE_k_individual), 
            sd=sd(FOS_to_IBE_k_individual))


#check structure and summary data:
str(group_MBH_to_IBE2)
summary(group_MBH_to_IBE2)


#Check the spread if total area values are evenly spread. 
ggplotly(low_ish_mbh2 %>% 
           group_by(group) %>%
           ggplot(aes(Name,`Area µm^2_mean`, color = group)) +
           geom_point(position = position_dodge(width = 0.5)))



#first plot to check calculations work and look good. 
#here we select the column of interest, then we filter by Name (ROI), then we calculate the group mean (not individual mean),
#and we adjust the area from µm^2 to something that is more intuitive. Then we normalize all values to the IBE2 group with their corresponding value,#
#and plot it out with mean value in column, the calculated SE and add the points for clarity.
ROI <- c('ATAN')

AREA_low_DMH <- group_MBH_to_IBE2%>%
  filter(Name == ROI) %>%
  group_by(group)%>%
  ggplot(aes(group, FOS_to_IBE_k, fill=group)) +
  geom_col(colour = "black", width = 0.7) +
  geom_errorbar(aes(ymin=FOS_to_IBE_k-sd, ymax=FOS_to_IBE_k+sd), width=.2,
                position=position_dodge(.9))  +
  geom_point(data = low_ish_mbh2 %>% filter(Name == ROI), 
             aes(y=FOS_to_IBE_k_individual), 
             color = "black")+
  geom_text(aes(label=round(FOS_to_IBE_k, 2)), vjust=-0.3, size=3.5) +
  scale_x_discrete(limits=c("IBE2","ENT","T1","T2","T3", "A1","A2","IBE1"), name="Group") +
  scale_y_continuous(name = ("(NUM FOS+ IBE2*Area-1)/(NUM FOS+ ROI*area-1)")) +
  scale_fill_viridis_d(option = "H", alpha=0.75, guide = guide_legend(override.aes = list(colour = NULL))) +
  labs(title = paste("DAB ISH", ROI), 
       subtitle = "AREA FOS+/Area Compared to IBE1") +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.margin = margin(1,1,1,1, "cm"),
        plot.background = element_rect(fill = "white", colour = "black", linewidth = 1),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        axis.line = element_line(linewidth = 0.5, linetype = 'solid', colour = "black"))

(AREA_low_DMH)


#check the impact of area on number of detection
create_plots <- function(name) {
  df <- group_MBH_to_IBE2%>%
    filter(Name == name) %>%
    group_by(group)%>%
    ggplot(aes(group, FOS_to_IBE_k, fill=group)) +
    geom_col(colour = "black", width = 0.7) +
    geom_errorbar(aes(ymin=FOS_to_IBE_k-sd, ymax=FOS_to_IBE_k+sd), width=.2,
                  position=position_dodge(.9))  +
    geom_point(data = low_ish_mbh2 %>% filter(Name == name), 
               aes(y=FOS_to_IBE_k_individual), 
               color = "black")+
    geom_text(aes(label=round(FOS_to_IBE_k, 2)), vjust=-0.3, size=3.5) +
    scale_x_discrete(limits=c("IBE2","ENT","T1","T2","T3", "A1","A2","IBE1"), name="Group") +
    scale_y_continuous(name = ("(NUM FOS+ IBE2*Area-1)/(NUM FOS+ ROI*area-1)")) +
    scale_fill_viridis_d(option = "H", alpha=0.75, guide = guide_legend(override.aes = list(colour = NULL))) +
    labs(title = paste("DAB ISH-",name), 
         subtitle = "AREA FOS+/Area Compared to IBE2") +
    theme_classic() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.margin = margin(1,1,1,1, "cm"),
          plot.background = element_rect(fill = "white", colour = "black", linewidth = 1),
          panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
          axis.line = element_line(linewidth = 0.5, linetype = 'solid', colour = "black"))
  return(df)
}

#MBH ROIs into a list
MBH_region_names <- c("VMH", "ARC", "PH", "ATAN","BTAN", "ME","PT", "DMH")

#use lapply to make all the plots
MBH_plots <- lapply(MBH_region_names, create_plots)

#display all plots 
grid.arrange(grobs = MBH_plots, ncol = 4)

#MBH ROIs into a list
POA_region_names <- c("AVPE", "MnPO", "MPA", "cp", "cg", "BNST", "LSV", "OVLT")

#use lapply to make all the plots
POA_plots <- lapply(POA_region_names, create_plots)
  
grid.arrange(grobs =POA_plots, ncol = 4)





# Generate 17 colors using the hcl.colors function
my_palette  <- hcl.colors(n = 17, palette = "Dark3", alpha = 1)



#Function to split the plots by Name (ROIs)
create_plots <- function(name) {
  df <- group_MBH_to_IBE2 %>%
    filter(Name == name) %>%
    group_by(Name, group) %>%
    ggplot(aes(group, FOS_to_IBE_k, fill=group)) +
    geom_col(colour = "black", width = 0.7) +
    geom_errorbar(aes(ymin=FOS_to_IBE_k-sd_FOS_to_IBE_k, ymax=FOS_to_IBE_k+sd_FOS_to_IBE_k), 
                  width=.2, colour = "black",
                  position=position_dodge(.9)) +
    geom_point(data = subset(low_ish_mbh2, Name %in% c(name)), 
               aes(x=group, y=FOS_to_IBE_k_individual), 
               size=2, alpha=0.5, colour = "black", position=position_jitterdodge(.1)) +
    geom_text(aes(label=round(FOS_to_IBE_k, 2)), vjust=-0.3, size=3.5)+
    scale_x_discrete(limits=c("IBE2","ENT","T1","T2","T3", "A1","A2","IBE1"), name="Group") +
    scale_y_continuous(name = "AREA IBE2/(AREA FOS+/Area)") + #, limits = c(0,150)
    scale_fill_manual(values=my_palette) +
    labs(title = paste("AREA LOW SENS ISH-", name), 
         subtitle = "AREA FOS+/Area Compared to IBE2") +
    theme_classic() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.margin = margin(1,1,1,1, "cm"),
          plot.background = element_rect(fill = "white", colour = "black", linewidth = 1),
          panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
          axis.line = element_line(linewidth = 0.5, linetype = 'solid', colour = "black"))
  return(df)
}


#Enter ROI Name and create plot
ROI <- c('OVLT')
lapply(ROI, create_plots)


#MBH ROIs into a list
MBH_region_names <- c("VMH", "ARC", "PH", "ATAN","BTAN", "ME","PT", "DMH")

#use lapply to make all the plots
MBH_plots <- lapply(MBH_region_names, create_plots)

#display all plots 
grid.arrange(grobs = MBH_plots, ncol = 4)

#MBH ROIs into a list
POA_region_names <- c("AVPE", "MnPO", "dMPA", "cp", "cg", "BNST", "LSV", "OVLT")

#use lapply to make all the plots
POA_plots <- lapply(POA_region_names, create_plots)

#display all plots 
grid.arrange(grobs = POA_plots, ncol = 4)


#intensity measures by area
str(low_ish_mbh1)

ggplot(low_ish_mbh1, aes(group, `ROI: 1.00 µm per pixel: OD Sum: Max`, color=Image ))+
  geom_point()+
  scale_x_discrete(limits=c("IBE2","ENT","T1","T2","T3", "A1","A2","IBE1"), name="Group")+
  facet_wrap(vars(c(Name)))


