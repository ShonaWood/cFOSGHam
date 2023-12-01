
library(tidyverse)
library(reshape2)
library(plyr)
library(plotly)
library(lubridate)
library(anytime)
library(scales)
library(readr)
library(zoo)


#scrip for plottig  core Tb
###############################PLOT ALL######################################

allcoreT <- read.csv("./anipill_tablets/allcoreT.csv")

#ensure format
allcoreT$datetime <- as.POSIXct(allcoreT$datetime, format= "%Y-%m-%d %H:%M:%S", tz="GMT") 
head(allcoreT)

#set lims to experimental period or as desired
lims <- as.POSIXct(strptime(c("19/04/21 10:00","17/08/21 10:00"), format = "%d/%m/%y %H:%M", tz="GMT"))



#import environmental data
#######################################################################
Tempisolat <- read_csv("./env/Tempisolat.csv", na = "empty") # room "isolat"
Tempisbjorn <- read_csv("./env/Tempisbjorn.csv", na = "empty") #room "isbjorn"

Tempisbjorn$ID <- paste("amb")
names(Tempisbjorn)[1:3] <-c("datetime", "rt", "ID")
Tempisbjorn$datetime <- as.POSIXct(Tempisbjorn$datetime, format = "%Y/%m/%d %H:%M:%S", tz="GMT")
Tempisbjorn$datetime <- Tempisbjorn$datetime - dhours(0)
Tempisbjorn$unix <- as.numeric(Tempisbjorn$datetime)
Tempisbjorn$tc <- Tempisbjorn$rt + 0.88 #calb value against anipill
Tempisbjorn <- Tempisbjorn %>% filter(tc>20)

Tempisolat$ID <- paste("amb")
names(Tempisolat)[1:3] <-c("datetime", "rt", "ID")
Tempisolat$datetime <- as.POSIXct(Tempisolat$datetime, format = "%Y/%m/%d %H:%M:%S", tz="GMT")
Tempisolat$datetime <- Tempisolat$datetime - dhours(0)
Tempisolat$unix <- as.numeric(Tempisolat$datetime)
Tempisolat$tc <- Tempisolat$rt + 0.88 #calb value against anipill


Tamb <- full_join(Tempisbjorn, Tempisolat)

remove(Tempisbjorn, Tempisolat)

Tamb %>% filter(rt<10)%>% summarise(mean(rt))

Tamb <- Tamb %>%
  mutate(datetime = floor_date(datetime, unit='minutes') + 
           (minute(datetime) %% 5)*60 - 
           (minute(datetime) %% 5)*60 %% 300)


Tamb <- Tamb%>%distinct(datetime, .keep_all = TRUE)


#ggplot(Tamb, aes(x = datetime, y=tc))+
#  geom_line()








#######################################################################
#

#######################################

# make sure datetime columns sre same POSIXct
allcoreT$datetime <- as.POSIXct(allcoreT$datetime, format= "%Y-%m-%d %H:%M:%S", tz="GMT") 
Tamb$datetime <- as.POSIXct(Tamb$datetime, format= "%Y-%m-%d %H:%M:%S", tz="GMT") 

# Define the SPc (week 0) date as POSIXct for consistency
SPc <- as.POSIXct("2021-05-27", tz = "GMT")

# Calc start_date and end_date in POSIXct format
start_date <- SPc - weeks(5)  
end_date <- SPc + weeks(9)    

# Gen weekly breaks from start_date to end_date
breaks <- seq(from = start_date, to = end_date, by = "week")

#the labs, Week 0 is SPc
labels <- paste( -5:9)

#representative hammie:
p <- allcoreT %>%
  filter(ID == 'H248') %>%
  ggplot(aes(x=datetime, y=ap_t)) +
  geom_line(color="black", alpha=0.9, linewidth = 0.5) +
  geom_line(data = Tamb, aes(x=datetime, y=rt), color='steelblue', alpha=0.7) +
  scale_x_datetime(name = "Weeks", 
                   breaks = breaks, 
                   labels = labels, 
                   limits = c(start_date, end_date)) +
  ylab(expression(paste("Tb (°C)"))) +
  theme_classic() +
  theme(
    text = element_text(family = "Arial", face = "bold", size = 25),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(1, 1, 1, 1, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line = element_line(color = "black")
  )

p

#save the plot (uncomment)
#ggsave("./graphix/248-Tb-exp.svg",  width = 17, height = 5, plot =p, dpi = 240 )



##################################################

#With lines as photperiod, look in wide form
p2 <- ggplot(subset(allcoreT, ID %in% c("H248")), aes(x=datetime, y=ap_t))+
       scale_x_datetime( limits= lims, date_breaks = "1 week", date_labels = "%W")+
       geom_line(color="black", alpha=0.9, linewidth = 1)+theme_classic()+
  geom_line(data = Tamb, aes(y=rt), color='steelblue', alpha=0.7)+
  labs(x="", y= "Core Body Temperature (C)")+
  geom_vline(xintercept = strptime(c("08/04/21 02:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=2.5)+
  geom_vline(xintercept = strptime(c("09/04/21 02:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=2.5)+
  geom_vline(xintercept = strptime(c("10/04/21 02:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=2.5)+
  geom_vline(xintercept = strptime(c("11/04/21 02:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=2.5)+
  geom_vline(xintercept = strptime(c("12/04/21 02:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=2.5)+
  geom_vline(xintercept = strptime(c("13/04/21 02:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=2.5)+
  geom_vline(xintercept = strptime(c("14/04/21 02:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=2.5)+
  geom_vline(xintercept = strptime(c("15/04/21 02:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=2.5)+
  geom_vline(xintercept = strptime(c("16/04/21 02:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=2.5)+
  geom_vline(xintercept = strptime(c("17/04/21 02:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=2.5)+
  geom_vline(xintercept = strptime(c("18/04/21 02:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=2.5)+
  geom_vline(xintercept = strptime(c("19/04/21 02:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=2.5)+
  geom_vline(xintercept = strptime(c("20/04/21 02:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=2.5)+
  geom_vline(xintercept = strptime(c("21/04/21 02:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=2.5)+
  geom_vline(xintercept = strptime(c("22/04/21 02:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=2.5)+
  geom_vline(xintercept = strptime(c("23/04/21 02:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=2.5)+
  geom_vline(xintercept = strptime(c("24/04/21 02:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=2.5)+
  geom_vline(xintercept = strptime(c("25/04/21 02:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=2.5)+
  geom_vline(xintercept = strptime(c("26/04/21 02:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=2.5)+
  geom_vline(xintercept = strptime(c("27/04/21 02:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=2.5)+
  geom_vline(xintercept = strptime(c("28/04/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("29/04/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("30/04/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("01/05/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("02/05/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("03/05/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("04/05/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("05/05/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("06/05/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("07/05/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("08/05/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("09/05/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("10/05/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("11/05/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("12/05/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("13/05/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("14/05/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("15/05/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("16/05/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("17/05/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("18/05/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("19/05/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("20/05/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("21/05/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("22/05/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("23/05/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("24/05/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("25/05/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("26/05/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("27/05/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("28/05/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("29/05/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("30/05/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("31/05/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("01/06/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("02/06/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("03/06/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("04/06/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("05/06/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("06/06/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("07/06/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("08/06/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+  
  geom_vline(xintercept = strptime(c("09/06/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+  
  geom_vline(xintercept = strptime(c("10/06/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("11/06/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("12/06/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("13/06/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("14/06/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("15/06/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("16/06/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("17/06/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("18/06/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("19/06/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("20/06/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("21/06/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("22/06/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("23/06/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("24/06/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("25/06/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("26/06/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("27/06/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("28/06/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("29/06/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("30/06/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("1/07/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("2/07/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("3/07/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("4/07/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("5/07/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("6/07/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("7/07/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("8/07/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("9/07/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("10/07/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("11/07/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("12/07/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("13/07/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("14/07/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("15/07/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("16/07/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("17/07/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("18/07/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("19/07/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("20/07/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("21/07/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("22/07/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("23/07/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("24/07/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("25/07/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("26/07/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("27/07/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("28/07/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("29/07/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("30/07/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)+
  geom_vline(xintercept = strptime(c("31/07/21 01:00"), format = "%d/%m/%y %H:%M", tz="GMT"), alpha=0.1, size=5)
p2








