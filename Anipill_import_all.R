library(tidyverse)
library(reshape2)
library(plyr)
library(plotly)
library(lubridate)
library(anytime)
library(scales)
library(readr)

##ap_ID script for all monitors with 8 pills csv direct from upload##

##########################Tablet 1###########################################
setwd("./anipill_tablets")
T1raw <- read_csv ("HibExp_1_T1.csv", skip = 7, col_names = TRUE)
T1raw1 <- read_csv ("HibExp_1_T1.csv", skip = 9, col_names = FALSE)

#get the pill ap_ID and the data for the first animal
T1N1 <- names(T1raw [2])
t1n1 <- T1raw1[c(2:4)]

#tidy up the data frame
t1n1$ap_ID <- paste(T1N1)
colnames(t1n1)<- c("Date", "Time", "ap_t", "ap_ID")
t1n1$datetime <- paste(t1n1$Date, t1n1$Time)
t1n1$datetime <- as.POSIXct(t1n1$datetime, format = "%d.%m.%Y %H:%M:%S", tz="GMT")
t1n1$ID <- paste(c("H263"))
head(t1n1)

#Repeat for each animal
T1N2 <- names(T1raw [9])
t1n2 <- T1raw1[c(9:11)]
t1n2$ap_ID <- paste(T1N2)
colnames(t1n2)<- c("Date", "Time", "ap_t", "ap_ID")
t1n2$datetime <- paste(t1n2$Date, t1n2$Time)
t1n2$datetime <- as.POSIXct(t1n2$datetime, format = "%d.%m.%Y %H:%M:%S", tz="GMT")
t1n2$ID <- paste(c("H270"))


T1N3 <- names(T1raw [16])
t1n3 <- T1raw1[c(16:18)]
t1n3$ap_ID <- paste(T1N3)
colnames(t1n3)<- c("Date", "Time", "ap_t", "ap_ID")
t1n3$datetime <- paste(t1n3$Date, t1n3$Time)
t1n3$datetime <- as.POSIXct(t1n3$datetime, format = "%d.%m.%Y %H:%M:%S", tz="GMT")
t1n3$ID <- paste(c("H248"))


T1N4 <- names(T1raw [23])
t1n4 <- T1raw1[c(23:25)]
t1n4$ap_ID <- paste(T1N4)
colnames(t1n4)<- c("Date", "Time", "ap_t", "ap_ID")
t1n4$datetime <- paste(t1n4$Date, t1n4$Time)
t1n4$datetime <- as.POSIXct(t1n4$datetime, format = "%d.%m.%Y %H:%M:%S", tz="GMT")
t1n4$ID <- paste(c("H276"))

T1N5 <- names(T1raw [30])
t1n5 <- T1raw1[c(30:32)]
t1n5$ap_ID <- paste(T1N5)
colnames(t1n5)<- c("Date", "Time", "ap_t", "ap_ID")
t1n5$datetime <- paste(t1n5$Date, t1n5$Time)
t1n5$datetime <- as.POSIXct(t1n5$datetime, format = "%d.%m.%Y %H:%M:%S", tz="GMT")
t1n5$ID <- paste(c("H216"))

T1N6 <- names(T1raw [37])
t1n6 <- T1raw1[c(37:39)]
t1n6$ap_ID <- paste(T1N6)
colnames(t1n6)<- c("Date", "Time", "ap_t", "ap_ID")
t1n6$datetime <- paste(t1n6$Date, t1n6$Time)
t1n6$datetime <- as.POSIXct(t1n6$datetime, format = "%d.%m.%Y %H:%M:%S", tz="GMT")
t1n6$ID <- paste(c("H307"))

T1N7 <- names(T1raw [44])
t1n7 <- T1raw1[c(44:46)]
t1n7$ap_ID <- paste(T1N7)
colnames(t1n7)<- c("Date", "Time", "ap_t", "ap_ID")
t1n7$datetime <- paste(t1n7$Date, t1n7$Time)
t1n7$datetime <- as.POSIXct(t1n7$datetime, format = "%d.%m.%Y %H:%M:%S", tz="GMT")
t1n7$ID <- paste(c("H236"))

T1N8 <- names(T1raw [51])
t1n8 <- T1raw1[c(51:53)]
t1n8$ap_ID <- paste(T1N8)
colnames(t1n8)<- c("Date", "Time", "ap_t", "ap_ID")
t1n8$datetime <- paste(t1n8$Date, t1n8$Time)
t1n8$datetime <- as.POSIXct(t1n8$datetime, format = "%d.%m.%Y %H:%M:%S", tz="GMT")
t1n8$ID <- paste(c("H319"))

#Making into 1 dataframe
Tablet1 <- t1n1 %>% 
  full_join(t1n2,  by =c("datetime", "ap_t", "ap_ID","ID") )%>% 
  full_join(t1n3,  by =c("datetime", "ap_t", "ap_ID","ID") )%>%
  full_join(t1n4,  by =c("datetime", "ap_t", "ap_ID","ID") )%>%
  full_join(t1n5,  by =c("datetime", "ap_t", "ap_ID","ID") )%>%
  full_join(t1n6,  by =c("datetime", "ap_t", "ap_ID","ID") )%>%
  full_join(t1n7,  by =c("datetime", "ap_t", "ap_ID","ID") )%>%
  full_join(t1n8,  by =c("datetime", "ap_t", "ap_ID","ID") )%>%
  select(3:6)

#Clean up in data env
remove(t1n1, t1n2, t1n3, t1n4, t1n5, t1n6, t1n7, t1n8, T1raw, T1raw1)
summary(Tablet1)


ggplot(Tablet1, aes(datetime, ap_t, colour = ap_ID))+
  geom_point()
########################################Tablet 2###################################

T2raw <- read_csv ("HibExp_1_T2.csv", skip = 7, col_names = TRUE, show_col_types = FALSE)
T2raw1 <- read_csv ("HibExp_1_T2.csv", skip = 9, col_names = FALSE, show_col_types = FALSE)

T2N2 <- names(T2raw [9])
t2n2 <- T2raw1[c(9:11)]
t2n2$ap_ID <- paste(T2N2)
colnames(t2n2)<- c("Date", "Time", "ap_t", "ap_ID")
t2n2$datetime <- paste(t2n2$Date, t2n2$Time)
t2n2$datetime <- as.POSIXct(t2n2$datetime, format = "%d.%m.%Y %H:%M:%S", tz="GMT")
t2n2$ID <- paste(c("H275"))

Tablet2 <- t2n2%>%
  select(3:6)

#clean data env
remove(t2n2, T2raw, T2raw1)

#ggplotly(ggplot(Tablet2, aes(datetime, ap_t, colour = ID))+
# geom_point())

########################################Tablet 3###################################

T3raw <- read_csv ("HibExp_1_T3.csv", skip = 6, col_names = TRUE, show_col_types = FALSE) #just to get the ap_ID ID
T3raw1 <- read_csv ("HibExp_1_T3.csv", skip = 9, col_names = FALSE, show_col_types = FALSE)

#get the pill ap_ID and the data for the first animal
T3N1 <- names(T3raw [2]) #just to get the ap_ID ID
t3n1 <- T3raw1[c(2:4)]

#tidy up the data frame
t3n1$ap_ID <- paste(T3N1)
colnames(t3n1)<- c("Date", "Time", "ap_t", "ap_ID")
t3n1$datetime <- paste(t3n1$Date, t3n1$Time)
t3n1$datetime <- as.POSIXct(t3n1$datetime, format = "%d.%m.%Y %H:%M:%S", tz="GMT")
t3n1$ID <- paste(c("H299"))

#Repeat for each animal
T3N2 <- names(T3raw [9])
t3n2 <- T3raw1[c(9:11)]
t3n2$ap_ID <- paste(T3N2)
colnames(t3n2)<- c("Date", "Time", "ap_t", "ap_ID")
t3n2$datetime <- paste(t3n2$Date, t3n2$Time)
t3n2$datetime <- as.POSIXct(t3n2$datetime, format = "%d.%m.%Y %H:%M:%S", tz="GMT")
t3n2$ID <- paste(c("H298"))

T3N3 <- names(T3raw [16])
t3n3 <- T3raw1[c(16:18)]
t3n3$ap_ID <- paste(T3N3)
colnames(t3n3)<- c("Date", "Time", "ap_t", "ap_ID")
t3n3$datetime <- paste(t3n3$Date, t3n3$Time)
t3n3$datetime <- as.POSIXct(t3n3$datetime, format = "%d.%m.%Y %H:%M:%S", tz="GMT")
t3n3$ID <- paste(c("H301"))


T3N4 <- names(T3raw [23])
t3n4 <- T3raw1[c(23:25)]
t3n4$ap_ID <- paste(T3N4)
colnames(t3n4)<- c("Date", "Time", "ap_t", "ap_ID")
t3n4$datetime <- paste(t3n4$Date, t3n4$Time)
t3n4$datetime <- as.POSIXct(t3n4$datetime, format = "%d.%m.%Y %H:%M:%S", tz="GMT")
t3n4$ID <- paste(c("H264"))

T3N5 <- names(T3raw [30])
t3n5 <- T3raw1[c(30:32)]
t3n5$ap_ID <- paste(T3N5)
colnames(t3n5)<- c("Date", "Time", "ap_t", "ap_ID")
t3n5$datetime <- paste(t3n5$Date, t3n5$Time)
t3n5$datetime <- as.POSIXct(t3n5$datetime, format = "%d.%m.%Y %H:%M:%S", tz="GMT")
t3n5$ID <- paste(c("H273"))

T3N6 <- names(T3raw [37])
t3n6 <- T3raw1[c(37:39)]
t3n6$ap_ID <- paste(T3N6)
colnames(t3n6)<- c("Date", "Time", "ap_t", "ap_ID")
t3n6$datetime <- paste(t3n6$Date, t3n6$Time)
t3n6$datetime <- as.POSIXct(t3n6$datetime, format = "%d.%m.%Y %H:%M:%S", tz="GMT")
t3n6$ID <- paste(c("H243"))


#Making into 1 dataframe
Tablet3 <- t3n1 %>% 
  full_join(t3n2,  by =c("datetime", "ap_t", "ap_ID","ID") )%>% 
  full_join(t3n3,  by =c("datetime", "ap_t", "ap_ID","ID") )%>%
  full_join(t3n4,  by =c("datetime", "ap_t", "ap_ID","ID") )%>%
  full_join(t3n5,  by =c("datetime", "ap_t", "ap_ID","ID") )%>%
  full_join(t3n6,  by =c("datetime", "ap_t", "ap_ID","ID") )%>%
  select(3:6)

#Clean up in data env
remove(t3n1, t3n2, t3n3, t3n4, t3n5, t3n6, T3raw, T3raw1)

#ggplot(Tablet3, aes(datetime, ap_t, colour = ID))+
#geom_point()

#################################Tablet 4#################################
T4raw <- read_csv ("HibExp_1_T4.csv", skip = 7, col_names = TRUE, show_col_types = FALSE)
T4raw1 <- read_csv ("HibExp_1_T4.csv", skip = 9, col_names = FALSE, show_col_types = FALSE)

#get the pill ap_ID and the data for the first animal
T4N1 <- names(T4raw [2])
t4n1 <- T4raw1[c(2:4)]

#tidy up the data frame
t4n1$ap_ID <- paste(T4N1)
colnames(t4n1)<- c("Date", "Time", "ap_t", "ap_ID")
t4n1$datetime <- paste(t4n1$Date, t4n1$Time)
t4n1$datetime <- as.POSIXct(t4n1$datetime, format = "%d.%m.%Y %H:%M:%S", tz="GMT")
t4n1$ID <- paste(c("H300"))

#Repeat for each animal
T4N2 <- names(T4raw [9])
t4n2 <- T4raw1[c(9:11)]
t4n2$ap_ID <- paste(T4N2)
colnames(t4n2)<- c("Date", "Time", "ap_t", "ap_ID")
t4n2$datetime <- paste(t4n2$Date, t4n2$Time)
t4n2$datetime <- as.POSIXct(t4n2$datetime, format = "%d.%m.%Y %H:%M:%S", tz="GMT")
t4n2$ID <- paste(c("H250"))

T4N3 <- names(T4raw [16])
t4n3 <- T4raw1[c(16:18)]
t4n3$ap_ID <- paste(T4N3)
colnames(t4n3)<- c("Date", "Time", "ap_t", "ap_ID")
t4n3$datetime <- paste(t4n3$Date, t4n3$Time)
t4n3$datetime <- as.POSIXct(t4n3$datetime, format = "%d.%m.%Y %H:%M:%S", tz="GMT")
t4n3$ID <- paste(c("H224"))


T4N4 <- names(T4raw [23])
t4n4 <- T4raw1[c(23:25)]
t4n4$ap_ID <- paste(T4N4)
colnames(t4n4)<- c("Date", "Time", "ap_t", "ap_ID")
t4n4$datetime <- paste(t4n4$Date, t4n4$Time)
t4n4$datetime <- as.POSIXct(t4n4$datetime, format = "%d.%m.%Y %H:%M:%S", tz="GMT")
t4n4$ID <- paste(c("H278"))

T4N5 <- names(T4raw [30])
t4n5 <- T4raw1[c(30:32)]
t4n5$ap_ID <- paste(T4N5)
colnames(t4n5)<- c("Date", "Time", "ap_t", "ap_ID")
t4n5$datetime <- paste(t4n5$Date, t4n5$Time)
t4n5$datetime <- as.POSIXct(t4n5$datetime, format = "%d.%m.%Y %H:%M:%S", tz="GMT")
t4n5$ID <- paste(c("H327"))

T4N6 <- names(T4raw [37])
t4n6 <- T4raw1[c(37:39)]
t4n6$ap_ID <- paste(T4N6)
colnames(t4n6)<- c("Date", "Time", "ap_t", "ap_ID")
t4n6$datetime <- paste(t4n6$Date, t4n6$Time)
t4n6$datetime <- as.POSIXct(t4n6$datetime, format = "%d.%m.%Y %H:%M:%S", tz="GMT")
t4n6$ID <- paste(c("H277"))

T4N7 <- names(T4raw [44])
t4n7 <- T4raw1[c(44:46)]
t4n7$ap_ID <- paste(T4N7)
colnames(t4n7)<- c("Date", "Time", "ap_t", "ap_ID")
t4n7$datetime <- paste(t4n7$Date, t4n7$Time)
t4n7$datetime <- as.POSIXct(t4n7$datetime, format = "%d.%m.%Y %H:%M:%S", tz="GMT")
t4n7$ID <- paste(c("H254"))

T4N8 <- names(T4raw [51])
t4n8 <- T4raw1[c(51:53)]
t4n8$ap_ID <- paste(T4N8)
colnames(t4n8)<- c("Date", "Time", "ap_t", "ap_ID")
t4n8$datetime <- paste(t4n8$Date, t4n8$Time)
t4n8$datetime <- as.POSIXct(t4n8$datetime, format = "%d.%m.%Y %H:%M:%S", tz="GMT")
t4n8$ID <- paste(c("H230"))

#Making into 1 dataframe
Tablet4 <- t4n1 %>% 
  full_join(t4n2,  by =c("datetime", "ap_t", "ap_ID","ID") )%>% 
  full_join(t4n3,  by =c("datetime", "ap_t", "ap_ID","ID") )%>%
  full_join(t4n4,  by =c("datetime", "ap_t", "ap_ID","ID") )%>%
  full_join(t4n5,  by =c("datetime", "ap_t", "ap_ID","ID") )%>%
  full_join(t4n6,  by =c("datetime", "ap_t", "ap_ID","ID") )%>%
  full_join(t4n7,  by =c("datetime", "ap_t", "ap_ID","ID") )%>%
  full_join(t4n8,  by =c("datetime", "ap_t", "ap_ID","ID") )%>%
  select(3:6)

#Clean up in data env
remove(t4n1, t4n2, t4n3, t4n4, t4n5, t4n6, t4n7, t4n8, T4raw, T4raw1)

#ggplotly(ggplot(Tablet4, aes(datetime, ap_t, colour = ID))+
#geom_point())

###################################Tablet 5##########################
T5raw <- read_csv ("HibExp_1_T5.csv", skip = 7, col_names = TRUE, show_col_types = FALSE) #just to get the ap_ID ID
T5raw1 <- read_csv ("HibExp_1_T5.csv", skip = 9, col_names = FALSE, show_col_types = FALSE)

#get the pill ap_ID and the data for the first animal
T5N1 <- names(T5raw [2]) #just to get the ap_ID ID
t5n1 <- T5raw1[c(2:4)]

#tidy up the data frame
t5n1$ap_ID <- paste(T5N1)
colnames(t5n1)<- c("Date", "Time", "ap_t", "ap_ID")
t5n1$datetime <- paste(t5n1$Date, t5n1$Time)
t5n1$datetime <- as.POSIXct(t5n1$datetime, format = "%d/%m/%Y %H:%M:%S", tz="GMT")
t5n1$ID <- paste(c("H274"))



#Repeat for each animal
T5N2 <- names(T5raw [9])
t5n2 <- T5raw1[c(9:11)]
t5n2$ap_ID <- paste(T5N2)
colnames(t5n2)<- c("Date", "Time", "ap_t", "ap_ID")
t5n2$datetime <- paste(t5n2$Date, t5n2$Time)
t5n2$datetime <- as.POSIXct(t5n2$datetime, format = "%d/%m/%Y %H:%M:%S", tz="GMT")
t5n2$ID <- paste(c("H241"))

T5N3 <- names(T5raw [16])
t5n3 <- T5raw1[c(16:18)]
t5n3$ap_ID <- paste(T5N3)
colnames(t5n3)<- c("Date", "Time", "ap_t", "ap_ID")
t5n3$datetime <- paste(t5n3$Date, t5n3$Time)
t5n3$datetime <- as.POSIXct(t5n3$datetime, format = "%d/%m/%Y %H:%M:%S", tz="GMT")
t5n3$ID <- paste(c("H227"))


T5N4 <- names(T5raw [23])
t5n4 <- T5raw1[c(23:25)]
t5n4$ap_ID <- paste(T5N4)
colnames(t5n4)<- c("Date", "Time", "ap_t", "ap_ID")
t5n4$datetime <- paste(t5n4$Date, t5n4$Time)
t5n4$datetime <- as.POSIXct(t5n4$datetime, format = "%d/%m/%Y %H:%M:%S", tz="GMT")
t5n4$ID <- paste(c("H211"))

T5N5 <- names(T5raw [30])
t5n5 <- T5raw1[c(30:32)]
t5n5$ap_ID <- paste(T5N5)
colnames(t5n5)<- c("Date", "Time", "ap_t", "ap_ID")
t5n5$datetime <- paste(t5n5$Date, t5n5$Time)
t5n5$datetime <- as.POSIXct(t5n5$datetime, format = "%d/%m/%Y %H:%M:%S", tz="GMT")
t5n5$ID <- paste(c("H231"))

T5N6 <- names(T5raw [37])
t5n6 <- T5raw1[c(37:39)]
t5n6$ap_ID <- paste(T5N6)
colnames(t5n6)<- c("Date", "Time", "ap_t", "ap_ID")
t5n6$datetime <- paste(t5n6$Date, t5n6$Time)
t5n6$datetime <- as.POSIXct(t5n6$datetime, format = "%d/%m/%Y %H:%M:%S", tz="GMT")
t5n6$ID <- paste(c("H245"))


#Making into 1 dataframe
Tablet5 <- t5n1 %>% 
  full_join(t5n2,  by =c("datetime", "ap_t", "ap_ID","ID") )%>% 
  full_join(t5n3,  by =c("datetime", "ap_t", "ap_ID","ID") )%>%
  full_join(t5n4,  by =c("datetime", "ap_t", "ap_ID","ID") )%>%
  full_join(t5n5,  by =c("datetime", "ap_t", "ap_ID","ID") )%>%
  full_join(t5n6,  by =c("datetime", "ap_t", "ap_ID","ID") )%>%
  select(3:6)

#Clean up in data env
remove(t5n1, t5n2, t5n3, t5n4, t5n5, t5n6, T5raw, T5raw1)
rm(list = ls.str(mode = 'character'))


###################All tablets#############################

allcoreT <- Tablet1 %>% 
  full_join(Tablet2,  by =c("datetime", "ap_t", "ap_ID","ID") )%>% 
  full_join(Tablet3,  by =c("datetime", "ap_t", "ap_ID","ID") )%>%
  full_join(Tablet4,  by =c("datetime", "ap_t", "ap_ID","ID") )%>%
  full_join(Tablet5,  by =c("datetime", "ap_t", "ap_ID","ID") )

remove(Tablet1, Tablet2, Tablet3, Tablet4, Tablet5)

allcoreT$unix <- as.numeric(allcoreT$datetime)


#####################################To add ambient ap_t######################

Tempisbjorn <- read_csv("Tempisolat.csv", na = "empty")
Tempisbjorn$ID <- paste("amb")
names(Tempisbjorn)[1:3] <-c("datetime", "ap_t", "ID")
Tempisbjorn$datetime <- as.POSIXct(Tempisbjorn$datetime, format = "%Y/%m/%d %H:%M:%S", tz="GMT")
Tempisbjorn$datetime <- Tempisbjorn$datetime - dhours(0)
Tempisbjorn$unix <- as.numeric(Tempisbjorn$datetime)
Tempisbjorn$temp_corr <- Tempisbjorn$ap_t + 0.88
head(Tempisbjorn)




write.csv(allcoreT,"allcoreT.csv")

