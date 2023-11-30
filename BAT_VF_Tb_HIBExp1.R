library(reshape2)
library(dplyr)
library(plotly)
library(lubridate)
library(anytime)
library(scales)
library(gridExtra)
library(viridis)
library(zoo)
library(readxl)
library(RColorBrewer)
library(tidyverse)



#import all core body temperatures
allcoreT <- read.csv("./anipill_tablets/allcoreT.csv")
allcoreT$datetime <- as.POSIXct(allcoreT$datetime, format = "%Y-%m-%d %H:%M:%S", tz="UTC")

#Import ambient temperature and formatting data
amb <- read_csv("./env/Tempisolat.csv", na = "empty")
amb$ID <- paste("amb")
names(amb)[1:3] <-c("datetime", "amb", "ID")
amb$datetime <- as.POSIXct(amb$datetime, format = "%Y/%m/%d %H:%M:%S", tz="GMT")
amb$datetime <- amb$datetime - dhours(0)
amb$unix <- as.numeric(amb$datetime)
amb$amb <- amb$amb + 0.88 #cold calibrated value 
amb$datetime <- round_date(amb$datetime, unit = "minute") #rounding obsevations to nearest minute
amb <- amb%>%distinct(datetime, .keep_all = TRUE)#Removing excess observations


#################################
#H278 July 24 Spontaneous arousal
#################################

#import data and genaral formatting
H278_jl24 <- read.csv("./BAT/20210724_H278.csv") 

names(H278_jl24)[1:2] <-c("datetime", "IPTT")
H278_jl24$ID <- paste("H278")
H278_jl24$ar_type <- paste("spontaneous")
H278_jl24$ar_group <- paste("full")
H278_jl24$datetime <- as.POSIXct(H278_jl24$datetime, format = "%Y-%m-%d %H:%M:%S", tz="UTC")
H278_jl24$unix <- as.numeric(H278_jl24$datetime)

#Adjust IPTT acording to post hoc calibration curves
H278_jl24$IPTT_adj <- H278_jl24$IPTT -1.8

#filter noise
H278_jl24<- filter(H278_jl24, IPTT_adj <39 &  IPTT_adj >8.6) 

#remove errors
H278_jl24 <- H278_jl24[-c(4524,10126,10193,10155),] #errors in dataframe seen manually in plot

#rollmean smoothing for iBAT temperature (~60 sec/12 obs )
H278_jl24$IPTT_adj<- rollmean(H278_jl24$IPTT_adj, k=12, fill = NA)

ggplot(H278_jl24, aes(datetime, IPTT_adj))+geom_point()


#get tb from  R environment (dets. anipill_import_all.R)
apH278 <- filter(allcoreT, ID == "H278" ) 
apH278$datetime <- as.POSIXct(apH278$datetime, format = "%Y-%m-%d %H:%M:%S", tz="UTC")
apH278<- filter(apH278, ap_t <39 &  ap_t>8.1)



#import ventilation rate H278
VH278 <- read_excel("./ventilation/Ventilation H278 20210724.xlsx", 
                    col_types = c("date", "numeric", "numeric", 
                                  "text"))

#Genaral formatting of VF data
VH278$ID <- paste("H278")
VH278$datetime <- as.POSIXct(VH278$datetime, format = "%Y-%m-%d %H:%M:%S", tz="UTC")
VH278$unix <- as.numeric(VH278$datetime)

#Convert Ventilation/second to ventilation/minute
VH278$min <- (VH278$vento)*60
#calculate 20 sec rollmean smoothing
VH278$sma <- rollmean(VH278$min, k= 20, fill = NA)

(ggplot(VH278, aes(datetime, sma))+
  geom_line())

str(VH278)
str(apH278)


#Join data
H278_jl24 <- H278_jl24 %>% 
  full_join( apH278)%>%
  full_join(VH278)%>%
  full_join(amb)


#make sure data is ordered according to datetime
H278_jl24 <- H278_jl24[order(H278_jl24$unix),]

#fill missing sma ventilation rate values introduced by merge:
H278_jl24<- H278_jl24 %>% 
  fill(sma, .direction = "down")

#filter to arousal time span for cleaner data
H278_jl24 <- H278_jl24%>%
  filter(.,unix>as.numeric(as.POSIXct("2021/07/24 12:00")) & unix< as.numeric(as.POSIXct("2021/07/25 20:00")))



#clean environment
remove(apH278, VH278)
gc()

#fill in missing observations for ambient temperatures based on nearest observervation
H278_jl24 <- H278_jl24 %>% 
  fill(amb, .direction = "updown")

#calculating mean torpid thermoregulatory distance from ambient temperature for present arousal (TRD)
TRD <-H278_jl24 %>% 
  select(IPTT_adj, amb)%>%
  filter( IPTT_adj <8.9 & IPTT_adj> 7.1 & IPTT_adj <= mean(H278_jl24$IPTT_adj, na.rm = T) )



TRD <- mean(TRD$IPTT_adj - TRD$amb)


#Calculating IPTT and Core body temperature distance from ambient temperature (TRD compensated)
H278_jl24$IPTT_diff <- H278_jl24$IPTT_adj - (mean(H278_jl24$amb) + TRD)
H278_jl24$ap_diff <- H278_jl24$ap_t - (mean(H278_jl24$amb) + TRD)

#zeroing unix time at 0.5C distance and converting to hours
#select closest time point where IPTT_diff == 0.5
p278 <- H278_jl24 %>%
  filter(IPTT_diff > 0.47 & IPTT_diff < 0.52) %>%
  select(unix) %>%
  summarise(mean = mean(unix))

# Convert the 'mean' column of p278 to numeric
p278 <- as.numeric(p278$mean)


H278_jl24$unix_0 <- (H278_jl24$unix - p278)/3600  



#Clean dataframe
H278_jl24 <- H278_jl24 %>% select("datetime","ID", "unix", "unix_0", "IPTT_adj","IPTT_diff",
                              "ap_diff", "ap_t","sma","amb", "ar_type", "ar_group")


#Inspect data
summary(H278_jl24)
head(H278_jl24)
str(H278_jl24)


p_H278 <- ggplot(H278_jl24, aes(unix_0), na.rm = T)+
  geom_point( aes(y=IPTT_diff, color=sma), na.rm = T, size=2) +
  geom_line(data=H278_jl24[!is.na(H278_jl24$ap_diff),], aes(y=ap_diff), colour="steelblue", na.rm = T) +
  geom_line(data = H278_jl24[!is.na(H278_jl24$amb),], aes(y=amb-8.25), size=0.5,  color="black", alpha= 0.5)+
  scale_y_continuous(limits = c(-2,28), n.breaks = 20, sec.axis =  sec_axis( trans=~.+8.25, name="Ambient C", breaks = c(6:12)) )+
  theme_bw()+xlim(-3,5)+
  scale_color_distiller(palette = "Paired", limits= c(0,190))

library(scales)
custom_color <- function(x) {
  breaks <- c(2, 10, 15, 20, 30, 40, 50, 70, 90, 100)
  labels <- c("<= 2", "2-10", "10-15", "15-20", "20-30", "30-40", "40-50", "50-70", "70-90", "90-100", "> 100")
  return(cut(x, breaks = c(-Inf, breaks, Inf), labels = labels, right = TRUE, include.lowest = TRUE))
}

# Now, add a new column for the labels
H278_jl24$color_labels <- custom_color(H278_jl24$sma)

spectral_colors <- colorRampPalette(brewer.pal(11, "Spectral"))(10)

color_mapping <- c("<= 2" = "gray85", 
                   "2-10" = spectral_colors[1], 
                   "10-15" = spectral_colors[2], 
                   "15-20" = spectral_colors[3], 
                   "20-30" = spectral_colors[4], 
                   "30-40" = spectral_colors[5], 
                   "40-50" = spectral_colors[6], 
                   "50-70" = spectral_colors[7], 
                   "70-90" = spectral_colors[8], 
                   "90-100" = spectral_colors[9], 
                   "> 100" = "deeppink3")

reversed_spectral_colors <- rev(spectral_colors)

color_mapping <- c("<= 5" = "gray85", 
                   "5-10" = reversed_spectral_colors[1], 
                   "10-15" = reversed_spectral_colors[2], 
                   "15-20" = reversed_spectral_colors[3], 
                   "20-30" = reversed_spectral_colors[4], 
                   "30-40" = reversed_spectral_colors[5], 
                   "40-50" = reversed_spectral_colors[6], 
                   "50-70" = reversed_spectral_colors[7], 
                   "70-90" = reversed_spectral_colors[8], 
                   "90-100" = reversed_spectral_colors[9], 
                   "> 100" = "deeppink3")

p_H278 <- ggplot(H278_jl24, aes(x = unix_0), na.rm = TRUE) +
  geom_point(aes(y = IPTT_diff, color = color_labels), size = 2, alpha = 0.95) +
  geom_smooth(data = subset(H278_jl24, !is.na(ap_diff)), aes(y = ap_diff), colour = "steelblue", size = 0.9, method = "loess", span = 0.2, alpha=0.5, se=F) + # Added smoothing
  geom_line(data = subset(H278_jl24, !is.na(amb)), aes(y = amb - 8.25), size = 0.5, color = "grey50", alpha = 0.6) +
  scale_color_manual(values = color_mapping)+
  scale_y_continuous(limits = c(-2,28), n.breaks = 20,
                     sec.axis =  sec_axis( trans=~.+8.18, name="IPTT(\u00B0C)", breaks = c(6:37)) )+
  theme_bw() +
  theme(
    text = element_text(family = "Arial", size = 13),
    axis.text = element_text(color = "black"), 
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank(), 
    legend.position = c(1.12, 1), # Move legend to top right corner
    legend.justification = c(1, 1), # Justify legend to the top right corner
    axis.title.y.right = element_text(hjust = 0.5),
    legend.key.size = unit(0.8, "lines"), # Adjust key size in the legend
    legend.text = element_text(size = 5),
    legend.title = element_text(size = 8),
    plot.margin = unit(c(1, 1, 1, 1), "cm")  # Top, right, bottom, left margins
  ) + 
  labs(
    x = "Time (hrs)", 
    y = "\U001D6AB\u00B0C from torpor CBT",
    color = "VPM"
  ) + 
  xlim(-3, 5)

p_H278




#################################
#H298 July 26 Spontaneous arousal
#################################

#import data and genaral formatting
H298_jl26 <- read.csv("./bat/BAT_H298_20210726.csv")
H298_jl26 <- na.omit(H298_jl26)
names(H298_jl26)[1:2] <-c("datetime", "IPTT")
H298_jl26$ID <- paste("H298")
H298_jl26$ar_type <- paste("spontaneous")
H298_jl26$ar_group <- paste("full")
H298_jl26$datetime <- as.POSIXct(H298_jl26$datetime, format = "%Y-%m-%d %H:%M:%S", tz="UTC")
H298_jl26$unix <- as.numeric(H298_jl26$datetime)

#Adjust IPTT acording to post hoc calibration curves
H298_jl26$IPTT_adj <- H298_jl26$IPTT -1.4

#filter noise
H298_jl26<- filter(H298_jl26, IPTT_adj <39 &  IPTT_adj >8.6) 

#remove errors
H298_jl26 <- H298_jl26[-c(2264,26970,26996,27075,27320,27499,27712,27736), ] #errors in dataframe seen manually in plot

#rollmean smoothing for iBAT temperature (~60 sec/12 obs )
H298_jl26$IPTT_adj<- rollmean(H298_jl26$IPTT_adj, k=12, fill = NA)


#get tb from  R environment (dets. anipill_import_all.R)
apH298 <- filter(allcoreT, ID == "H298" ) 


#import ventilation rate H298
VH298 <- read_excel("./ventilation/Ventilation H298 20210726.xlsx", 
                    col_types = c("date", "numeric", 
                                  "text"))

#Genaral formatting of VF data
VH298$ID <- paste("H298")
VH298$datetime <- as.POSIXct(VH298$datetime, format = "%Y-%m-%d %H:%M:%S", tz="UTC")
VH298$unix <- as.numeric(VH298$datetime)

#Convert Ventilation/second to ventilation/minute
VH298$min <- (VH298$vent)*60
#calculate 120 sec rollmean smoothing
VH298$sma <- rollmean(VH298$min, k= 120, fill = NA)

#ggplot(VH298, aes(datetime, sma))+
  #geom_line()

#Join data
H298_jl26 <- H298_jl26 %>% 
  full_join( apH298)%>%
  full_join(VH298)%>%
  full_join(amb)

#make sure data is ordered according to datetime
H298_jl26 <- H298_jl26[order(H298_jl26$unix),]

#filter to arousal timespan for cleaner data
H298_jl26 <- H298_jl26%>%
  filter(.,unix>as.numeric(as.POSIXct("2021/07/26 00:30")) & unix< as.numeric(as.POSIXct("2021/07/26 21:00")))



#clean environment
remove(apH278, VH278)
gc()

#fill in missing observations for ambient temperatures
H298_jl26 <- H298_jl26 %>% 
  fill(amb, .direction = "updown")

#calculating mean torpid thermoregulatory distance from ambient temperature for present arousal (TRD)
TRD <-H298_jl26 %>% 
  select(IPTT_adj, amb)%>%
  filter(IPTT_adj <10)

TRD <- mean(TRD$IPTT_adj - TRD$amb)

#Calculating IPTT and Core body temperature distance from ambient temperature (TRD compensated)
H298_jl26$IPTT_diff <- H298_jl26$IPTT_adj - (H298_jl26$amb + TRD)
H298_jl26$ap_diff <- H298_jl26$ap_t - (H298_jl26$amb + TRD)

#zeroing unix time at 0.5C distance and converting to hours
H298_jl26$unix_0 <- (H298_jl26$unix - as.numeric(as_datetime("2021-07-26 08:45:50")))/3600


#Clean dataframe
H298_jl26 <- H298_jl26 %>% select("datetime","ID", "unix", "unix_0", "IPTT_adj","IPTT_diff",
                                  "ap_diff", "ap_t","sma","amb", "ar_type", "ar_group")

#Inspect data
summary(H298_jl26)
head(H298_jl26)
str(H298_jl26)


p_H298 <- ggplot(H298_jl26, aes(unix_0), na.rm = T)+
  geom_line(data=H298_jl26[!is.na(H298_jl26$IPTT_diff),], aes(y=IPTT_diff, colour=sma), na.rm = T, size=.7)+
  geom_line(data=H298_jl26[!is.na(H298_jl26$ap_diff),], aes(y=ap_diff), colour="steelblue", na.rm = T) +
  geom_line(data = H298_jl26[!is.na(H298_jl26$sma),], aes(y=sma/10), size=.1,  color="forestgreen", alpha= 0.6)+
  geom_line(data = H298_jl26[!is.na(H298_jl26$amb),], aes(y=amb-7.25), size=.1,  color="gray", alpha= 0.6)+
  ylim(c(-1,30))+theme_bw()+xlim(-2,5)

#(p_H298)


#################################
#H241 August 06 Spontaneous arousal
#################################

#import data and genaral formatting
H241_ag06 <- read.csv("./bat/20210807 _H241_BAT.csv")
H241_ag06 <- na.omit(H241_ag06)
names(H241_ag06)[1:2] <-c("datetime", "IPTT")
H241_ag06$ID <- paste("H241")
H241_ag06$ar_type <- paste("spontaneous")
H241_ag06$ar_group <- paste("full")
H241_ag06$datetime <- as.POSIXct(H241_ag06$datetime, format = "%Y-%m-%d %H:%M:%S", tz="UTC")
H241_ag06$unix <- as.numeric(H241_ag06$datetime)

#Adjust IPTT acording to post hoc calibration curves
H241_ag06$IPTT_adj <- H241_ag06$IPTT -1.7

#filter noise
H241_ag06<- filter(H241_ag06, IPTT_adj <39 &  IPTT_adj >8.35) 

#remove errors
H241_ag06 <- H241_ag06[-c(2264,26970,26996,27075,27320,27499,27712,27736), ] #errors in dataframe seen manually in plot

#rollmean smoothing for iBAT temperature (~60 sec/12 obs )
H241_ag06$IPTT_adj<- rollmean(H241_ag06$IPTT_adj, k=12, fill = NA)


#get tb from  R environment (dets. anipill_import_all.R)
apH241 <- filter(allcoreT, ID == "H241" ) 


#import ventilation rate H241
VH241 <- read_excel("./ventilation/Ventilation H241 20210806.xlsx", 
                    col_types = c("date", "numeric", 
                                  "text"))

#Genaral formatting of VF data
VH241$ID <- paste("H241")
VH241$datetime <- as.POSIXct(VH241$datetime, format = "%Y-%m-%d %H:%M:%S", tz="UTC")
VH241$unix <- as.numeric(VH241$datetime)

#Convert Ventilation/second to ventilation/minute
VH241$min <- (VH241$vent)*60
#calculate 120 sec rollmean smoothing
VH241$sma <- rollmean(VH241$min, k= 120, fill = NA)

ggplot(VH241, aes(datetime, sma))+
  geom_line()


#Join data
H241_ag06 <- H241_ag06 %>% 
  full_join(apH241)%>%
  full_join(VH241)%>%
  full_join(amb)

#make sure data is ordered according to datetime
H241_ag06 <- H241_ag06[order(H241_ag06$unix),]

#filter to arousal timespan for cleaner data
H241_ag06 <- H241_ag06%>%
  filter(.,unix>as.numeric(as.POSIXct("2021/08/05 00:30")) & unix< as.numeric(as.POSIXct("2021/08/06 21:00")))



#clean environment
remove(apH241, VH241)
gc()

#fill in missing observations for ambient temperatures
H241_ag06 <- H241_ag06 %>% 
  fill(amb, .direction = "updown")

#calculating mean torpid thermoregulatory distance from ambient temperature for present arousal (TRD)
TRD <-H241_ag06 %>% 
  select(IPTT_adj, amb)%>%
  filter(IPTT_adj <8.9)

TRD <- mean(TRD$IPTT_adj - TRD$amb)

#Calculating IPTT and Core body temperature distance from ambient temperature (TRD compensated)
H241_ag06$IPTT_diff <- H241_ag06$IPTT_adj - (H241_ag06$amb + TRD)
H241_ag06$ap_diff <- H241_ag06$ap_t - (H241_ag06$amb + TRD)

#zeroing unix time at 0.5C distance and converting to hours
H241_ag06$unix_0 <- (H241_ag06$unix - as.numeric(as_datetime("2021-08-06 02:21:58")))/3600


#Clean dataframe
H241_ag06 <- H241_ag06 %>% select("datetime","ID", "unix", "unix_0", "IPTT_adj","IPTT_diff",
                                  "ap_diff", "ap_t","sma","amb", "ar_type", "ar_group")

#Inspect data
summary(H241_ag06)
head(H241_ag06)
str(H241_ag06)


p_H241 <- ggplot(H241_ag06, aes(unix_0), na.rm = T)+
  geom_line(data=H241_ag06[!is.na(H241_ag06$IPTT_diff),], aes(y=IPTT_diff), colour="firebrick", na.rm = T, size=.7)+
  geom_line(data=H241_ag06[!is.na(H241_ag06$ap_diff),], aes(y=ap_diff), colour="steelblue", na.rm = T) +
  geom_line(data = H241_ag06[!is.na(H241_ag06$sma),], aes(y=sma/10), size=.1,  color="forestgreen", alpha= 0.6)+
  geom_line(data = H241_ag06[!is.na(H241_ag06$amb),], aes(y=amb-7.25), size=.1,  color="gray", alpha= 0.6)+
  ylim(c(-1,28))+theme_bw()+xlim(-4,5)
#
#(p_H241)


#################################
#H230 July 29 Spontaneous arousal
#################################

#import data and genaral formatting
H230_jl29 <- read.csv("./BAT/BAT_H230_20210729.csv")
H230_jl29 <- na.omit(H230_jl29)
names(H230_jl29)[1:2] <-c("datetime", "IPTT")
H230_jl29$ID <- paste("H230")
H230_jl29$ar_type <- paste("spontaneous")
H230_jl29$ar_group <- paste("full")
H230_jl29$datetime <- as.POSIXct(H230_jl29$datetime, format = "%Y-%m-%d %H:%M:%S", tz="UTC")
H230_jl29$unix <- as.numeric(H230_jl29$datetime)

#Adjust IPTT acording to post hoc calibration curves
H230_jl29$IPTT_adj <- H230_jl29$IPTT -2.15

#filter noise
H230_jl29<- filter(H230_jl29, IPTT_adj <39 &  IPTT_adj >8.6) 

#remove errors #not updated
#H230_jl29 <- H230_jl29[-c(2264,26970,26996,27075,27320,27499,27712,27736), ]  #errors in dataframe seen manually in plot

#rollmean smoothing for iBAT temperature (~60 sec/12 obs )
H230_jl29$IPTT_adj<- rollmean(H230_jl29$IPTT_adj, k=12, fill = NA)


#get tb from  R environment (dets. anipill_import_all.R)
apH216 <- filter(allcoreT, ID == "H230" ) 


#import ventilation rate H230
VH230 <- read_excel("./ventilation/Ventilation H230 20210729.xlsx", 
                             col_types = c("date", "numeric", 
                                           "text"))

#Genaral formatting of VF data
VH230$ID <- paste("H230")
VH230$datetime <- as.POSIXct(VH230$datetime, format = "%Y-%m-%d %H:%M:%S", tz="UTC")
VH230$unix <- as.numeric(VH230$datetime)

#Convert Ventilation/second to ventilation/minute
VH230$min <- (VH230$vent)*60
#calculate 120 sec rollmean smoothing
VH230$sma <- rollmean(VH230$min, k= 120, fill = NA)

ggplot(VH230, aes(datetime, sma))+
  geom_line()

#Join data
H230_jl29 <- H230_jl29 %>% 
  full_join( apH216)%>%
  full_join(VH230)%>%
  full_join(amb)

#make sure data is ordered according to datetime
H230_jl29 <- H230_jl29[order(H230_jl29$unix),]

#filter to arousal timespan for cleaner data
H230_jl29 <- H230_jl29%>%
  filter(.,unix>as.numeric(as.POSIXct("2021/07/28 22:30")) & unix< as.numeric(as.POSIXct("2021/07/29 21:00")))



#clean environment
remove(apH278, VH278)
gc()

#fill in missing observations for ambient temperatures
H230_jl29 <- H230_jl29 %>% 
  fill(amb, .direction = "updown")

#calculating mean torpid thermoregulatory distance from ambient temperature for present arousal (TRD)
TRD <-H230_jl29 %>% 
  select(IPTT_adj, amb)%>%
  filter(IPTT_adj <9)

TRD <- mean(TRD$IPTT_adj - TRD$amb)

#Calculating IPTT and Core body temperature distance from ambient temperature (TRD compensated)
H230_jl29$IPTT_diff <- H230_jl29$IPTT_adj - (H230_jl29$amb + TRD)
H230_jl29$ap_diff <- H230_jl29$ap_t - (H230_jl29$amb + TRD)

#zeroing unix time at 0.5C distance and converting to hours
#select closest time point where IPTT_diff == 0.5

p05 <- H230_jl29 %>% filter(IPTT_diff >0.495 & IPTT_diff <0.595) %>%
  select(unix)%>%
  summarise(mean=mean(unix))%>%
  as.numeric()

H230_jl29$unix_0 <- (H230_jl29$unix - p05)/3600




#Clean dataframe
H230_jl29 <- H230_jl29 %>% select("datetime","ID", "unix", "unix_0", "IPTT_adj","IPTT_diff",
                                  "ap_diff", "ap_t","sma","amb", "ar_type", "ar_group")


#Inspect data
summary(H230_jl29)
head(H230_jl29)
str(H230_jl29)


p_H230 <- ggplot(H230_jl29, aes(unix_0), na.rm = T)+
  geom_line(data=H230_jl29[!is.na(H230_jl29$IPTT_diff),], aes(y=IPTT_diff), colour="firebrick", na.rm = T, size=.7)+
  geom_line(data=H230_jl29[!is.na(H230_jl29$ap_diff),], aes(y=ap_diff), colour="steelblue", na.rm = T) +
  geom_line(data = H230_jl29[!is.na(H230_jl29$sma),], aes(y=sma/10), size=.1,  color="forestgreen", alpha= 0.6)+
  geom_line(data = H230_jl29[!is.na(H230_jl29$amb),], aes(y=amb-7.25), size=.1,  color="gray", alpha= 0.6)+
  scale_y_continuous(limits = c(-1,28), n.breaks = 20)+theme_bw()+xlim(-5,5)

#(p_H230)
###########################################################################



 #################################
#Full arousal merged With ventilation rate
#################################


fa <- H278_jl24 %>%
  full_join(H298_jl26)%>%
  full_join(H241_ag06)%>%
  full_join(H230_jl29)
library(scales)
custom_color <- function(x) {
  breaks <- c(5, 10, 15, 20, 30, 40, 50, 70, 90, 100)
  labels <- c("<= 5", "5-10", "10-15", "15-20", "20-30", "30-40", "40-50", "50-70", "70-90", "90-100", "> 100")
  return(cut(x, breaks = c(-Inf, breaks, Inf), labels = labels, right = TRUE, include.lowest = TRUE))
}



spectral_colors <- colorRampPalette(brewer.pal(11, "Spectral"))(10)

color_mapping <- c("<= 5" = "gray85", 
                   "2-10" = spectral_colors[1], 
                   "10-15" = spectral_colors[2], 
                   "15-20" = spectral_colors[3], 
                   "20-30" = spectral_colors[4], 
                   "30-40" = spectral_colors[5], 
                   "40-50" = spectral_colors[6], 
                   "50-70" = spectral_colors[7], 
                   "70-90" = spectral_colors[8], 
                   "90-100" = spectral_colors[9], 
                   "> 100" = "deeppink3")

reversed_spectral_colors <- rev(spectral_colors)

color_mapping <- c("<= 5" = "gray85", 
                   "5-10" = reversed_spectral_colors[1], 
                   "10-15" = reversed_spectral_colors[2], 
                   "15-20" = reversed_spectral_colors[3], 
                   "20-30" = reversed_spectral_colors[4], 
                   "30-40" = reversed_spectral_colors[5], 
                   "40-50" = reversed_spectral_colors[6], 
                   "50-70" = reversed_spectral_colors[7], 
                   "70-90" = reversed_spectral_colors[8], 
                   "90-100" = reversed_spectral_colors[9], 
                   "> 100" = "deeppink3")


fa <- fa %>% 
  fill(sma, .direction = "down")

# Now, add a new column for the labels
fa$color_labels <- custom_color(fa$sma)
unique(fa$color_labels)


str(fa)
summary(fa)


amb_avg <- fa %>%
  mutate(unix_0 = round(unix_0 * 6) / 6) %>%
  group_by(unix_0) %>%
  summarize(amb_avg = mean(amb, na.rm = TRUE), .groups = "drop")

head(amb_avg)

mean(amb_avg$amb_avg)



p_fa <- ggplot(fa, aes(unix_0))+
  geom_point(aes(y=IPTT_diff, color=color_labels), na.rm = T, size=1)+
  #geom_point(data= fa[!is.na(fa$ap_diff),], aes( y =ap_diff, color=ID), alpha = 0.7, size=2 ) +
  geom_line(data= amb_avg, aes(y=amb_avg-8.18), linewidth=0.75,  color="black", alpha= 0.5)+
  scale_y_continuous(limits = c(-2,28), n.breaks = 20, sec.axis =  sec_axis( trans=~.+8.18, name="IPTT(\u00B0C)", breaks = seq(-6,36, by =2) ))+
  theme_classic()+
  scale_x_continuous(breaks = seq(-3,5, by =1), limits = c(-3,5))+
  geom_hline(yintercept = 0.5 , alpha=0.5, size=.5, linetype=2, color='firebrick')+
  geom_hline(yintercept = 3 , alpha=0.5, size=.5, linetype=2, color='firebrick')+
  
  scale_color_manual(values = color_mapping) +
  theme_classic() +
  theme(
    text = element_text(family = "arial", size = 20),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(1, 1, 1, 1, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  labs(
    x = "Time (hrs)", 
    y = "\U001D6AB\u00B0C from torpor CBT",
    color = "ID"
  )
  
 
p_fa

#save the plot (uncomment)
#ggsave("Sp-Arousal-IPTT-VF.svg", plot =p_fa, width = 8, height = 6, path = "./graphix")





p2_fa <- ggplot(fa, aes(unix_0))+
  geom_point(aes(y=IPTT_diff, color=color_labels), na.rm = T, size=1)+
  #geom_point(data= fa[!is.na(fa$ap_diff),], aes( y =ap_diff, color=ID), alpha = 0.7, size=2 ) +
  #geom_line(data= amb_avg, aes(y=amb_avg-8.18), linewidth=0.75,  color="black", alpha= 0.5)+
  scale_y_continuous(limits = c(-0.2,6.2), sec.axis =  sec_axis( trans=~.+8.18, name="TiBAT(\u00B0C)", breaks = c(6:37)) )+
  theme_bw()+
  scale_x_continuous(breaks = seq(-3,5, by =1), limits = c(-1,1))+
  geom_hline(yintercept = 0.5 , alpha=0.5, size=.5, linetype=2, color='firebrick')+
  geom_hline(yintercept = 3 , alpha=0.5, size=.5, linetype=2, color='firebrick')+
  geom_hline(yintercept = 0 , alpha=0.75, size=.5, linetype=1, color='black')+
  geom_hline(yintercept = 6.2 , alpha=0.75, size=.5, linetype=1, color='black')+
  scale_color_manual(values = color_mapping)+
  theme_classic() +
  theme(legend.position = 'none',
    text = element_text(family = "arial", size = 20))+
  labs(
    x = "Time (hrs)", 
    y = "\U001D6AB\u00B0C from torpor CBT",
    color = "VF")

p2_fa

#save plot zoomed
#ggsave("Sp-Arousal-IPTT-VF-Zoomed.svg", plot =p2_fa, width = 6.5, height = 6, path = "./graphix")
#


#################################
#H028 spontaneous
head(H028)



H028 <- H028[order(cbat$datetime),]

f_H028 <- H028 %>%
  filter(datetime >= as.POSIXct("2023-05-23 05:00:00") & 
           datetime <= as.POSIXct("2023-05-23 21:00:00"))


f_H028$IPTT_adj <-f_H028$temp+0.5

summary(f_H028)

#fill in missing observations for ambient temperatures
f_H028 <-f_H028 %>% 
  fill(amb, .direction = "updown")


#calculating mean torpid thermoregulatory distance from ambient temperature for present arousal (TRD)
TRD <-f_H028 %>% 
  select(IPTT_adj, amb)%>%
  filter(IPTT_adj <10)

TRD <- mean(TRD$IPTT_adj - TRD$amb)

#Calculating IPTT and Core body temperature distance from ambient temperature (TRD compensated)
f_H028$IPTT_diff <- f_H028$IPTT_adj - (f_H028$amb + TRD)

#zeroing unix time at 0.5C distance and converting to hours
#select closest time point where IPTT_diff == 0.5

p05 <-f_H028 %>% filter(IPTT_diff >0.495 & IPTT_diff <0.595) %>%
  select(unix)%>%
  summarise(mean=mean(unix))%>%
  as.numeric()

f_H028$unix_0 <- (((f_H028$unix - p05)/3600))-1.7 #trd calc not work, adjust manually

p028<- ggplot(f_H028, aes(x = unix_0)) +
  geom_line(aes(y =IPTT_diff, color = "Temp")) +
  geom_point(aes(y =IPTT_diff, color = "Temp")) +
  geom_point(aes(y = amb, color = "TC")) +
  labs(title = "Temperature Data", y = "Temperature", x = "Date") +
  scale_color_manual(values = c("Temp" = "blue", "TC" = "red"), name = "Legend") +
  theme_minimal()


(p028)




#"some exaples of IPTT prfiles of A1 animals"
############################################################
#Arousal 0.5 H216 August 01
############################################################
#import data and general formatting
H216_05 <- read.csv("./BAT/20210801_H216_05.csv") #Arousal 05

H216_05 <- na.omit(H216_05)
names(H216_05)[1:2] <-c("datetime", "IPTT")
H216_05$ID <- paste("H216")
H216_05$ar_type <- paste("spontaneous")
H216_05$ar_group <- paste("A05")
H216_05$datetime <- as.POSIXct(H216_05$datetime, format = "%Y-%m-%d %H:%M:%S", tz="UTC")
H216_05$unix <- as.numeric(H216_05$datetime)

#Adjust IPTT acording to post hoc calibration curves
H216_05$IPTT_adj <- H216_05$IPTT -0.03

#filter noise
H216_05<- filter(H216_05, IPTT_adj <39 &  IPTT_adj >7.9) 

ggplot(H216_05, aes(unix , IPTT_adj))+
  geom_point()


#get tb from  R environment (dets. anipill_import_all.R)
apH216 <- filter(allcoreT, ID == "H216" ) 


#import ventilation rate H216
VH216 <- read_excel("./ventilation/20210801_ventilation_Arousal_05_H216.xlsx", 
                    col_types = c("date", "numeric", 
                                  "text"))

#Genaral formatting of VF data
VH216$ID <- paste("H216")
VH216$datetime <- as.POSIXct(VH216$datetime, format = "%Y-%m-%d %H:%M:%S", tz="UTC")
VH216$unix <- as.numeric(VH216$datetime)

#Convert Ventilation/second to ventilation/minute
VH216$min <- (VH216$vent)*60
#calculate 120 sec rollmean smoothing
VH216$sma <- rollmean(VH216$min, k= 120, fill = NA)


ggplot(VH216, aes(datetime, sma))+
  geom_line()

#Join data
H216_05 <- H216_05 %>% 
  full_join( apH216)%>%
  full_join(VH216)%>%
  full_join(amb)

#make sure data is ordered according to datetime
H216_05 <- H216_05[order(H216_05$unix),]

#fill missing sma ventilation rate values introduced by merge:
H216_05<- H216_05 %>% 
  fill(sma, .direction = "down")

#filter to arousal timespan for cleaner data
H216_05 <- H216_05%>%
  filter(.,unix>as.numeric(as.POSIXct("2021/07/31 22:30")) & unix< as.numeric(as.POSIXct("2021/08/01 04:00")))



#clean environment
remove(apH216, VH216)
gc()

#fill in missing observations for ambient temperatures
H216_05 <- H216_05 %>% 
  fill(amb, .direction = "updown")

#calculating mean torpid thermoregulatory distance from ambient temperature for present arousal (TRD)
TRD <-H216_05 %>% 
  select(IPTT_adj, amb)%>%
  filter(IPTT_adj <10 & IPTT_adj <= mean(H216_05$IPTT_adj, na.rm = T))

TRD <- mean(TRD$IPTT_adj - TRD$amb)

#Calculating IPTT and Core body temperature distance from ambient temperature (TRD compensated)
H216_05$IPTT_diff <- H216_05$IPTT_adj - (H216_05$amb + TRD)
H216_05$ap_diff <- H216_05$ap_t - (H216_05$amb + TRD)

#zeroing unix time at 0.5C distance and converting to hours
#select closest time point where IPTT_diff == 0.5
p05 <- H216_05 %>% filter(IPTT_diff >0.495 & IPTT_diff <0.595) %>%
  select(unix)%>%
  summarise(mean=mean(unix))%>%
  as.numeric(p05)

H216_05$unix_0 <- (H216_05$unix - p05)/3600



#Clean dataframe
H216_05 <- H216_05 %>% select("datetime","ID", "unix", "unix_0", "IPTT_adj","IPTT_diff",
                                  "ap_diff", "ap_t","sma","amb", "ar_type", "ar_group")


#Inspect data
summary(H216_05)
head(H216_05)
str(H216_05)


p_H216 <- ggplot(H216_05, aes(unix_0), na.rm = T)+
  geom_point( aes(y=IPTT_diff, color=sma), na.rm = T, size=1)+
  geom_line(data=H216_05[!is.na(H216_05$ap_diff),], aes(y=ap_diff), colour="steelblue", na.rm = T) +
  geom_line(data = H216_05[!is.na(H216_05$amb),], aes(y=amb-7.25), size=1,  color="black", alpha= 0.5)+
  scale_y_continuous(limits = c(-1,5), n.breaks = 20, sec.axis =  sec_axis( trans=~.+7.25, name="Ambient C", breaks = c(6:12)) )+
  theme_bw()+xlim(-3,1)+
  scale_color_distiller(palette = "Paired", limits= c(0,25))



(p_H216)


############################################################
#Arousal 0.5 H298 July 30 
############################################################
H298_05 <- read.csv("./BAT/20210730_H298_Arousal05.csv") #Arousal 05

H298_05 <- na.omit(H298_05)
names(H298_05)[1:2] <-c("datetime", "IPTT")
H298_05$ID <- paste("H216")
H298_05$ar_type <- paste("spontaneous")
H298_05$ar_group <- paste("A05")
H298_05$datetime <- as.POSIXct(H298_05$datetime, format = "%Y-%m-%d %H:%M:%S", tz="UTC")
H298_05$unix <- as.numeric(H298_05$datetime)

#Adjust IPTT according to post hoc calibration curves
H298_05$IPTT_adj <- H298_05$IPTT -1.30

#filter noise
H298_05<- filter(H298_05, IPTT_adj <39 &  IPTT_adj >7.9) 

#get tb from  R environment (dets. anipill_import_all.R)
apH298 <- filter(allcoreT, ID == "H298" ) 


ggplotly(ggplot(H298_05, aes(datetime , IPTT_adj))+
  geom_point(color="blue")+
  geom_point(data= apH298, aes(y=ap_t))+
  geom_point(data = amb, aes(y=amb)))


#import ventilation rate H216
VH298 <- read_excel("./ventilation/20210730_ventilation_Arousal_05_H298.xlsx", 
                    col_types = c("date", "numeric", 
                                  "text"))

#Genaral formatting of VF data
VH298$ID <- paste("H216")
VH298$datetime <- as.POSIXct(VH298$datetime, format = "%Y-%m-%d %H:%M:%S", tz="UTC")
VH298$unix <- as.numeric(VH298$datetime)

#Convert Ventilation/second to ventilation/minute
VH298$min <- (VH298$vent)*60
#calculate 120 sec rollmean smoothing
VH298$sma <- rollmean(VH298$min, k= 120, fill = NA)


ggplot(VH298, aes(datetime, sma))+
  geom_line()

#Join data
H298_05 <- H298_05 %>% 
  full_join( apH298)%>%
  full_join(VH298)%>%
  full_join(amb)

#make sure data is ordered according to datetime
H298_05 <- H298_05[order(H298_05$unix),]

#fill missing sma ventilation rate values introduced by merge:
H298_05<- H298_05 %>% 
  fill(sma, .direction = "down")

#filter to arousal timespan for cleaner data
H298_05 <- H298_05%>%
  filter(.,unix>as.numeric(as.POSIXct("2021/07/30 14:30")) & unix< as.numeric(as.POSIXct("2021/07/30 21:00")))



#clean environment
remove(apH298, VH298)
gc()

#fill in missing observations for ambient temperatures based on nearest observervation
H298_05 <- H298_05 %>% 
  fill(amb, .direction = "updown")

#calculating mean torpid thermoregulatory distance from ambient temperature for present arousal (TRD)
TRD <-H298_05 %>% 
  select(IPTT_adj, amb)%>%
  filter( IPTT_adj <9 & IPTT_adj> 8.7 & IPTT_adj <= mean(H298_05$IPTT_adj, na.rm = T) )

TRD <- mean(TRD$IPTT_adj - TRD$amb)


#Calculating IPTT and Core body temperature distance from ambient temperature (TRD compensated)
H298_05$IPTT_diff <- H298_05$IPTT_adj - (mean(H298_05$amb) + TRD)
H298_05$ap_diff <- H298_05$ap_t - (mean(H298_05$amb) + TRD)

#zeroing unix time at 0.5C distance and converting to hours
#select closest time point where IPTT_diff == 0.5
p05 <- H298_05 %>% filter(IPTT_diff >0.45 & IPTT_diff <0.58) %>%
  select(unix)%>%
  summarise(mean=mean(unix))%>%
  as.numeric(p05)

H298_05$unix_0 <- (H298_05$unix - p05)/3600 - 0.125 #manual correction with 0.125 because difficult ambient temps.



#Clean dataframe
H298_05 <- H298_05 %>% select("datetime","ID", "unix", "unix_0", "IPTT_adj","IPTT_diff",
                              "ap_diff", "ap_t","sma","amb", "ar_type", "ar_group")


#Inspect data
summary(H298_05)
head(H298_05)
str(H298_05)


p_H298 <- ggplot(H298_05, aes(unix_0), na.rm = T)+
  geom_point( aes(y=IPTT_diff, color=sma), na.rm = T, size=2)+
  geom_line(data=H298_05[!is.na(H298_05$ap_diff),], aes(y=ap_diff), colour="steelblue", na.rm = T) +
  theme_bw()+xlim(-5,1)+ylim(0,5)+
  scale_color_distiller(palette = "Paired", limits= c(0,20))



(p_H298)








