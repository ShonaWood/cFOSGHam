library(tidyverse)
library(plotly)
library(lubridate)
library(anytime)
library(latticeExtra)

setwd("C:/Users/fma017/OneDrive - UiT Office 365/1. PhD/Scripts/data")
light2 <- read_csv("C:/Users/fma017/OneDrive - UiT Office 365/1. PhD/Scripts/data/Room2April24-2021.csv")
temp2 <-read_csv("C:/Users/fma017/OneDrive - UiT Office 365/1. PhD/Scripts/data/TempRoom2021.csv")


light2 <- light2 %>% 
  rename(
    datetime = `Date and Time`,
    secs = `Time (seconds)`,
    lx = `Room2 (lx)`
  )

temp2 <- temp2 %>%
  rename(
    datetime ="Time",
    temp ="Room 2"
  )

  light2$datetime <- light2$datetime + minutes(120)

head(light2)
head(temp2)

room2 <- full_join(light2, temp2, na.rm=T)
room2 <- room2 %>%
  arrange(datetime) 
room2$datetime<- as.POSIXct(room2$datetime, format = "%d.%m.%Y %H:%M", tz="GMT")

head(room2)
 
room2$lx <- as.numeric(room2$lx)
room2$temp <- as.numeric(room2$temp)

head(room2)

ggplot(room2, aes(x=datetime, y=temp))+
  geom_point()

lims4 <- as.POSIXct(strptime(c("19/04/21 15:00:01","22/04/21 08:59:01"), format = "%d/%m/%y %H:%M", tz="GMT"))

p<-ggplot(room2, aes(x=datetime)) +
  geom_point( aes(y=lx), col="firebrick", size =0.5) + 
  geom_point( aes(y=temp*100), col="steelblue", size =0.5) +
  scale_y_continuous(name = "Lx", sec.axis = sec_axis(~./100,name="temp"))+
  scale_x_datetime(date_breaks = "12 hours",limits = lims4)
p


# --> construct separate plots for each series
obj1 <- xyplot(lx ~ datetime, room2, type = "p" , lwd=2)
obj2 <- xyplot(temp ~ datetime, room2, type = "p", lwd=2)

obj1

# --> Make the plot with second y axis AND legend:
doubleYScale(obj1, obj2, text = c("Lux", "Temperature") , add.ylab2 = TRUE)
  