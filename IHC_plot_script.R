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
library(RColorBrewer)

#Need to run separate processing scripts to run. 
IHC <- full_join(sum.pPOA, sum.pMBH)
IHC <- full_join(IHC,sum.pcp)

##
IHC <-IHC%>%
  mutate(numFOSmm2 = (num23_mean /(area_mean/1000000)))

#all IHC script needs to run before this:
write.csv(IHC, file = "./Histo/IHC_SUM_ALL.csv")


IHC <- read.csv("./Histo/IHC_SUM_ALL.csv", sep = ",")

head(IHC)

#Name: Regions (ROI)
#group = groups in the experiment
#ID = individual ID
#Num_detections_mean: Mean number of cells within ROI
#num23_mean: Positive detection model that gave the best result. This is the number of Positive Protein CFOS cells.
#area_mean: The mean area of the ROI


# Calculation of 'FOSmm2' for all rows: individal mean expression nr of cFOS per mm2
sum.IHC <-IHC%>%
  mutate(FOSmm2 = (num23_mean /(area_mean/1000000))) #divide area by 1000000 to convert to mm2

summary(sum.IHC)


SE<- function(x) sd(x)/sqrt(length(x)) #Function for creating SEM


#Then create tibble containing group-wise mean value normalized to area ROI.
gr.sum.IHC <-sum.IHC %>%
  group_by(Name, group) %>%
  summarise(grFOSmm2=mean(FOSmm2), 
            se=SE(FOSmm2))


#sorting data:
table(sort(gr.sum.IHC$group, decreasing = TRUE))

#check structure and summary data:
str(gr.sum.IHC )
summary(gr.sum.IHC)


#To see what Regions are available for plotting: 
unique(sum.IHC$Name)

#Enter regoin of interest
ROI <- c('PT')

create_plot <- function(ROI) {
  p1_MBH <- gr.sum.IHC %>%
    filter(Name == ROI) %>% 
    group_by(group) %>% 
    ggplot(aes(group, grFOSmm2, fill = group)) + 
    ggchicklet::geom_chicklet(colour = "black", width = .75, radius = grid::unit(2, "mm"))+
  geom_errorbar(aes(ymin = grFOSmm2 - se, ymax = grFOSmm2 + se), width = 0.2, colour = "black") + 
  geom_point(
    data = subset(sum.IHC, Name %in% c(ROI)),  # Corrected the dataset name
    aes(y = FOSmm2),  # Adjusted the y aes to grFOSmm2
    color = 'black',
    size = 2,
    position = position_dodge2(width = 0.15)
  ) +
  scale_x_discrete(
    limits = c("IBE2", "ENT", "T40", "A1", "A2", "IBE1"),
    name = ""
  ) +
  ylab(expression(paste("# cFOS"^"+", " / mm"^2))) +
  scale_fill_manual(
      name = NULL,
      values = c(
        "IBE2" = "#cc7c3a",
        "IBE1" = "#ae6544",
        "ENT" = "#a4ad6f",
        "A2" = "#ae4544",
        "T40" = "#436f82",
        "A1" = "#7c5981",
        "Other" = "#cccccc"
      ))+
  labs(title = paste("IHC -", ROI)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial", face = "bold", size = 16),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(1, 1, 1, 1, "cm"),
    panel.grid.major.x = element_blank(),  # Remove x gridlines
    panel.grid.major.y = element_blank(),  # Remove y gridlines
    panel.grid.minor.x = element_blank(),  # Remove x gridlines
    panel.grid.minor.y = element_blank(),  # Remove y gridlines
    axis.line = element_line(color = "black")  # Add axis lines
  )
  return(p1_MBH)
}

# List of ROIs
ROIs <- c('DMH', 'VMH', 'ARC', 'AVPE', 'MnPO', 'LPO', 'ATAN', 'CP', 'PT')

# Iterate through each ROI and save the plot
for (ROI in ROIs) {
  p1_MBH <- create_plot(ROI)
  file_name <- paste0("./graphix/IHC-", ROI, ".svg")
  svglite::svglite(file_name, width = 7, height = 6, bg = "transparent")
  print(p1_MBH)
  dev.off()
}
