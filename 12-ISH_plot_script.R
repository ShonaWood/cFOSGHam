library(readr)
library(reshape2)
library(plotly)
library(lubridate)
library(anytime)
library(scales)
library(tibble)
library(gridExtra)
library(viridis)
library(zoo)
library(readxl)
library(gridExtra)
library(ggpubr)
library(RColorBrewer)
library(tidyr)
library(stringr)
library(ggplot2)
library(dplyr)
library(ggpubr)


ISH <- read.csv("./Histo/ISH-SUMMARY-ALL.csv", sep = ",")

head(ISH)

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
sum.ISH <-ISH%>%
  mutate(AreaFOSmm2 = (pos_th_mean /(area_mean/1000000))) #divide area by 1000000 to convert to mm2

summary(sum.ISH)


SE<- function(x) sd(x)/sqrt(length(x)) #Function for creating SEM


#Then create tibble containing group-wise mean value normalized to area ROI.
gr.sum.ISH <-sum.ISH %>%
  group_by(Name, group) %>%
  summarise(grFOSmm2=mean(AreaFOSmm2), 
            se=SE(AreaFOSmm2))


#sorting data:
table(sort(gr.sum.ISH$group, decreasing = TRUE))

#check structure and summary data:
str(gr.sum.ISH )
summary(gr.sum.ISH)



#################################################################################
#Dotplots and dunnets tests, sign stars are taken from dunnets test and add manually in inkscape (using graphpad signif levels added manually)
library(DescTools)
head(sum.ISH)
#the area of investingation:

ROI <- c('PT')

anov.res <- aov(((num_pos_mean/num_det_mean)*100) ~ group, data = sum.ISH %>% filter(Name == ROI))
summary(anov.res)

dun.res <- DunnettTest(((num_pos_mean/num_det_mean)*100) ~ group, data = sum.ISH %>% filter(Name == ROI), control ="IBE2")
dun.res

# add signif level to match graphpad signif levels:
dun.res_df <- as.data.frame(dun.res$IBE2)
dun.res_df <- rownames_to_column(dun.res_df, var = "gr.comp")
p.val <- dun.res_df$pval
gr.comp <- dun.res_df$gr.comp
#loop to create custon signif leves
cust.sign <- function(p.val ) {
  if (p.val  < 0.0001) {
    return('****')  
  } else if (p.val  < 0.001) {
    return('***') 
  } else if (p.val  < 0.01) {
    return('**')    
  } else if (p.val  < 0.05) {
    return('*')    
  } else if (p.val < 0.1) {
    return('.')   
  } else {
    return('ns')   
  }
}

stars <- sapply(p.val, cust.sign)
sign.result <- data.frame(
  gr.comp = gr.comp,
  pval = p.val,
  stars = stars
)


sign.result #stars used to manually annotate in Inkscape 


ROI <- c('PT')
#script to make the different flot are adjusted manually per ROI and saved :)
p1_ISH<- sum.ISH %>%
  filter(Name == ROI) %>%
  group_by(group) %>%
  ggplot( aes(group, (num_pos_mean/num_det_mean)*100)) + 
  geom_dotplot(method='histodot', binaxis = "y", stackdir = "center", dotsize=0.75)+
  stat_summary(fun = mean, na.rm = TRUE, 
               geom = "crossbar",
               width= .65, color = "black", 
  )+
  stat_summary(fun.data = mean_se, na.rm = TRUE, 
               geom = "errorbar",
               size = .5, color = "black", 
               width=0.35
  )+
  scale_y_continuous(limits = c(0, 40)) +
  scale_x_discrete(
    limits = c("IBE2", "ENT", "T40", "A1", "A2", "IBE1"),
    name = ""
  )+
  ylab(expression(paste("% Positive"))) +
  theme_classic() +
  theme(text = element_text(family = "Arial",  size = 25),
    legend.position = "none",
    plot.margin = margin(1, 1, 1, 1, "cm"),
    axis.line = element_line(linewidth = 1.025)
  ) +
  stat_compare_means(method = "t.test",
                     comparisons = list(c("IBE2", "A1") ),
                     label = "p.signif",
                     size = 9,
                     hide.ns = TRUE,
                     vjust= 0.5,
                    label.y= 35,
                    tip.length= c(0.09, 0.81),
                    bracket.size=0.75
                    )+
  stat_compare_means(method = "t.test",
                     comparisons = list(c("IBE2", "A2") ),
                     label = "p.signif",
                     size = 9,
                     hide.ns = TRUE,
                     vjust= 0.5,
                     label.y= 37,
                     tip.length= c(0.2, 0.01),
                     bracket.size=0.75
  )

p1_ISH


#save the plots, build easch individually
#file_name <- paste0("./graphix/ISH-", ROI, ".svg")
#ggsave(file_name, plot = p1_ISH, width = 7, height = 7, device = "svg")

