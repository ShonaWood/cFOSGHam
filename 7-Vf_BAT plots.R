library(tidyverse)
library(reshape2)
library(plyr)
library(plotly)
library(lubridate)
library(anytime)
library(scales)
library(gridExtra)
library(readxl)
require(smooth)
library(zoo) #for rollmean function

#Script for intergrating BAT IPTT reds and core Tb and Ventilatin feq data.

#import all core body temoeratures
allcoreT <- read.csv("./anipill_tablets/allcoreT.csv")
allcoreT$datetime <- as.POSIXct(allcoreT$datetime, format = "%Y-%m-%d %H:%M:%S", tz="UTC")


#import ventilation rate H278
VH278 <- read_excel("./ventilation/Ventilation H278 20210724.xlsx", 
                    col_types = c("date", "numeric", "numeric", 
                                  "text"))

#check notes
unique(VH278$notes)

#import iBAT temperature
H278_jul24 <- read.csv("./BAT/20210724_H278.csv")


#Genaral formatting
names(H278_jul24)[1:2] <-c("datetime", "IPTT")
H278_jul24$ID <- paste("H278")
H278_jul24$datetime <- as.POSIXct(H278_jul24$datetime, format = "%Y-%m-%d %H:%M:%S", tz="UTC")
H278_jul24$unix <- as.numeric(H278_jul24$datetime)

#reduce IPTT temperature with calibrated cold constant (see. calibration tests) and remove outliers/noise
H278_jul24$IPTT_adj <- H278_jul24$IPTT - 1.8
H278_jul24<- filter(H278_jul24, IPTT <39 &  IPTT >10.1)

#check data
head(H278_jul24)
summary(H278_jul24)  
summary(VH278)

#remove errors
H278_jul24 <- H278_jul24[-c(4524,10126,10193,10155),] #errors in dataframe

#Convert to minutes
VH278$min <- (VH278$vento+0.0001)*60

#calculate 20 sec roll means
VH278$sma <- rollmean(VH278$min, k= 20, fill = NA)

#rollmean smoothing for BAT temperature
H278_jul24$sma<- rollmean(H278_jul24$IPTT_adj, k=10, fill = NA)

#create plot 
pVH278 <- ggplot(VH278)+ #base data
  geom_point(data = H278_jul24, aes(datetime, (sma*6)-55), size= 1, alpha= 0.1,)+ #iBAT smoothed rollmean data
  geom_line(data= subset(allcoreT, ID %in% c("H278")), aes(datetime, y=(ap_t*6)-55), color="steelblue", size = 1)+ #Core tb
  geom_line(data = VH278[!is.na(VH278$sma),], aes(datetime, (sma)), size=.1,  color="firebrick", alpha= 0.6)+ #smoothed 20 sek rolling avg 
  scale_x_datetime(limits= as.POSIXct(strptime(c("24/07/21 14:00","24/07/21 20:00"),
                                        format = "%d/%m/%y %H:%M", tz="GMT"),
                                        date_breaks = "1 hours",
                                        labels = date_format("%b %d")))+
  scale_y_continuous(
  name = "Ventilation rate (VPM) 20 sec rolling avg", limits = c(-10,155),
  sec.axis = sec_axis( trans=~./6+6, name="BAT Temperature"))+
  theme_bw()+ggtitle("1")


# if annot is to be integr
 # geom_vline(xintercept = strptime(c("24/07/21 16:22:01"), format = "%d/%m/%y %H:%M:%S", tz="GMT"), alpha=0.5, size=.1, colour="forestgreen")+ #pufup
  #geom_vline(xintercept = strptime(c("24/07/21 16:23:25"), format = "%d/%m/%y %H:%M:%S", tz="GMT"), alpha=0.5, size=.1, colour="forestgreen")+ #pufup
  #geom_vline(xintercept = strptime(c("24/07/21 16:26:21"), format = "%d/%m/%y %H:%M:%S", tz="GMT"), alpha=0.5, size=.1, colour="forestgreen")+ #puffup
  #geom_vline(xintercept = strptime(c("24/07/21 16:31:33"), format = "%d/%m/%y %H:%M:%S", tz="GMT"), alpha=0.5, size=.1, colour="forestgreen")+ #puffup
  #geom_vline(xintercept = strptime(c("24/07/21 16:55:29"), format = "%d/%m/%y %H:%M:%S", tz="GMT"), alpha=0.5, size=.1, colour="forestgreen")+ #pufup
  #geom_vline(xintercept = strptime(c("24/07/21 17:39:20"), format = "%d/%m/%y %H:%M:%S", tz="GMT"), alpha=0.5, size=.1, colour="yellow")+ #pause for shivering
  #geom_vline(xintercept = strptime(c("24/07/21 17:44:19"), format = "%d/%m/%y %H:%M:%S", tz="GMT"), alpha=0.5, size=.1, colour="yellow")+ #pause for shivering
  #geom_vline(xintercept = strptime(c("24/07/21 17:46:00"), format = "%d/%m/%y %H:%M:%S", tz="GMT"), alpha=0.5, size=.1, colour="yellow")+ 
  #geom_vline(xintercept = strptime(c("24/07/21 17:49:40"), format = "%d/%m/%y %H:%M:%S", tz="GMT"), alpha=0.5, size=.1, colour="yellow")+  #pause for shivering
  #geom_vline(xintercept = strptime(c("24/07/21 17:27:57"), format = "%d/%m/%y %H:%M:%S", tz="GMT"), alpha=0.5, size=.1, colour="yellow") #small Pause body contraction 








#############################################################################
#New animal

VH241 <- read_excel("./ventilation/Ventilation H241 20210806.xlsx", 
                    col_types = c("date", "numeric", 
                                  "text"))
summary(VH241)
unique(VH241$notes)

H241_aug06 <- read.csv("./bat/20210807 _H241_BAT.csv")
names(H241_aug06)[1:2] <-c("datetime", "IPTT")
summary(H241_aug06)

H241_aug06$ID <- paste("H241")
H241_aug06$datetime <- as.POSIXct(strptime(H241_aug06$datetime, format = "%Y-%m-%d %H:%M:%S", tz="UTC"))
H241_aug06$unix <- as.numeric(H241_aug06$datetime)
H241_aug06$unix <- as.numeric(H241_aug06$datetime)
H241_aug06$IPTT_adj <- H241_aug06$IPTT - 1.7
H241_aug06<- filter(H241_aug06, IPTT_adj <39 &  IPTT_adj >8.35)#filer out bad reads

head(H241_aug06)
summary(H241_aug06)  
summary(VH278)

H241_aug06 <- H241_aug06[-c(2264,26970,26996,27075,27320,27499,27712,27736), ] #errors in dataframe


VH241$min <- (VH241$vent+0.0001)*60

VH241$sma <- rollmean(VH241$min, k= 20, fill = NA)

H241_aug06$sma<- rollmean(H241_aug06$IPTT_adj, k=2, fill = NA)


na.omit(VH241)



pVH241 <- ggplot(VH241)+
  geom_point(data = H241_aug06, aes(datetime, (sma*6)-55), size= 1, alpha= 0.1)+
  geom_line(data= subset(allcoreT, ID %in% c("H241")), aes(datetime, y=(ap_t*6)-55), color="steelblue", size = 1)+
  geom_line(data = VH241[!is.na(VH241$sma),], aes(datetime, (sma)), size=.1,  color="firebrick")+
  scale_x_datetime(limits= as.POSIXct(strptime(c("05/08/21 24:00","06/08/21 06:00"),
                                               format = "%d/%m/%y %H:%M", tz="GMT"),
                                      date_breaks = "1 hours",
                                      labels = date_format("%b %d")))+
  scale_y_continuous(
    name = "Ventilation rate (VPM) 20 sec rolling avg", limits = c(-10,170),
    sec.axis = sec_axis( trans=~./6+6, name="BAT Temperature"))+
  theme_bw()+ggtitle("2")





pVH241



##############################################################################
#new animal 

VH230 <- read_excel("./ventilation/Ventilation H230 20210729.xlsx", 
                        col_types = c("date", "numeric", 
                                      "text"))
summary(VH230)
unique(VH230$notes)

BAT230 <- read.csv("./BAT/BAT_H230_20210729.csv")
names(BAT230)[1:2] <-c("datetime", "IPTT")
summary(BAT230)

BAT230$ID <- paste("H241")
BAT230$datetime <- as.POSIXct(strptime(BAT230$datetime, format = "%Y-%m-%d %H:%M:%S", tz="UTC"))
BAT230$unix <- as.numeric(BAT230$datetime)
BAT230$IPTT_adj <- BAT230$IPTT - 2.15
BAT230<- filter(BAT230, IPTT_adj <39 &  IPTT_adj >8.5)#filer out bad reads

head(BAT230)
summary(BAT230)  
summary(VH230)

#BAT230 <- BAT230[-c(2264,26970,26996,27075,27320,27499,27712,27736), ] #errors in dataframe


VH230$min <- (VH230$vent+0.0001)*60

VH230$sma <- rollmean(VH230$min, k= 20, fill = NA)

BAT230$sma<- rollmean(BAT230$IPTT_adj, k=2, fill = NA)


pVH230 <- ggplot(VH230)+
  geom_point(data = BAT230, aes(datetime, (sma*6)-55), size= 1, alpha= 0.1)+
  geom_line(data= subset(allcoreT, ID %in% c("H230")), aes(datetime, y=(ap_t*6)-55), color="steelblue", size = 1)+
  geom_line(data = VH230[!is.na(VH230$sma),], aes(datetime, (sma)), size=.1,  color="firebrick")+
  scale_x_datetime(limits= as.POSIXct(strptime(c("28/07/21 23:00","29/07/21 05:00"),
                                               format = "%d/%m/%y %H:%M", tz="GMT"),
                                      date_breaks = "1 hours",
                                      labels = date_format("%b %d")))+
  scale_y_continuous(
    name = "Ventilation rate (VPM) 20 sec rolling avg", limits = c(-10,170),
    sec.axis = sec_axis( trans=~./6+6, name="BAT Temperature"))+
  theme_bw()+ggtitle("3")


pVH230




###############################################################################
VH300 <- read_excel("./ventilation/Ventilation H300 20210721.xlsx", 
                    col_types = c("date", "numeric", 
                                  "text"))
summary(VH300)
unique(VH300$notes)

H300 <- read.csv("./bat/20210721_H300_BAT.csv")
names(H300)[1:2] <-c("datetime", "IPTT")
summary(H300)

H300$ID <- paste("H241")
H300$datetime <- as.POSIXct(strptime(H300$datetime, format = "%Y-%m-%d %H:%M:%S", tz="UTC"))
H300$unix <- as.numeric(H300$datetime)
H300$unix <- as.numeric(H300$datetime)
H300$IPTT_adj <- H300$IPTT - 1.7
H300<- filter(H300, IPTT_adj <39 &  IPTT_adj >8.35)#filer out bad reads

head(H300)
summary(H300)  
summary(VH278)

#H300 <- H300[-c(2264,26970,26996,27075,27320,27499,27712,27736), ] #errors in dataframe


VH300$min <- (VH300$vent+0.0001)*60

VH300$sma <- rollmean(VH300$min, k= 20, fill = NA)

H300$sma<- rollmean(H300$IPTT_adj, k=2, fill = NA)



 pVH300<- ggplot(VH300)+
   geom_point(data = H300, aes(datetime, (sma*6)-55), size= 1, alpha= 0.1)+
   geom_line(data= subset(allcoreT, ID %in% c("H300")), aes(datetime, y=(ap_t*6)-55), color="steelblue", size = 1)+
   geom_line(data = VH300[!is.na(VH300$sma),], aes(datetime, (sma)), size=.1,  color="firebrick")+
   scale_x_datetime(limits= as.POSIXct(strptime(c("21/07/21 13:00","21/07/21 19:00"),
                                                format = "%d/%m/%y %H:%M", tz="GMT"),
                                       date_breaks = "1 hours",
                                       labels = date_format("%b %d")))+
   scale_y_continuous(
     name = "Ventilation rate (VPM) 20 sec rolling avg", limits = c(-10,170),
     sec.axis = sec_axis( trans=~./6+6, name="BAT Temperature"))+
   theme_bw()+ggtitle("4")
   
   

 pVH300


##############################################################################
 VH298 <- read_excel("./ventilation/Ventilation H298 20210726.xlsx", 
                     col_types = c("date", "numeric", 
                                   "text"))
 summary(VH298)
 unique(VH298$notes)
 
 H298 <- read.csv("./bat/BAT_H298_20210726.csv")
 names(H298)[1:2] <-c("datetime", "IPTT")
 H298 <- na.omit(H298)
 summary(H298)
 
 H298$ID <- paste("H298")
 H298$datetime <- as.POSIXct(strptime(H298$datetime, format = "%Y-%m-%d %H:%M:%S", tz="UTC"))
 H298$unix <- as.numeric(H298$datetime)
 H298$IPTT_adj <- H298$IPTT - 1.4
 H298<- filter(H298, IPTT_adj <39 &  IPTT_adj >8.35)#filer out bad reads
 
 head(H298)
 summary(H298)  
 summary(VH298)
 
 #H298 <- H298[-c(2264,26970,26996,27075,27320,27499,27712,27736), ] #errors in dataframe
 
 
 
 VH298$min <- (VH298$vent+0.0001)*60
 
 VH298$sma <- rollmean(VH298$min, k= 20, fill = NA)
 
 H298$sma<- rollmean(H298$IPTT_adj, k=2, fill = NA)
 
 
 
 pVH298<- ggplot(VH298)+
   geom_point(data = H298, aes(datetime, (sma*6)-53), size= 1, alpha= 0.1)+
   geom_line(data= subset(allcoreT, ID %in% c("H298")), aes(datetime, y=(ap_t*6)-53), color="steelblue", size = 1)+
   geom_line(data = VH298[!is.na(VH298$sma),], aes(datetime, (sma)), size=.1,  color="firebrick")+
   scale_x_datetime(limits= as.POSIXct(strptime(c("26/07/21 07:00","26/07/21 13:00"),
                                                format = "%d/%m/%y %H:%M", tz="GMT"),
                                       date_breaks = "1 hours",
                                       labels = date_format("%b %d")))+
   scale_y_continuous(
     name = "Ventilation rate (VPM) 20 sec rolling avg", limits = c(-10,170),
     sec.axis = sec_axis( trans=~./6+6, name="BAT Temperature"))+
   theme_bw()+ggtitle("5")
 
 
 
 pVH298
#################################################################################################
plot_final <-gridExtra:: grid.arrange(pVH278,pVH241,pVH230, pVH300 , pVH298, ncol=2 )

plot_final
 
 

