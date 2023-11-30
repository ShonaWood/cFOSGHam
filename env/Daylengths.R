library(tidyverse)
library(plotly)

setwd("C:/Users/fma017/OneDrive - UiT Office 365/1. PhD/scrips/data")

N71D90 <- read_csv("C:/Users/fma017/OneDrive - UiT Office 365/1. PhD/71N D90.csv")
N60d035 <-read_csv("C:/Users/fma017/OneDrive - UiT Office 365/1. PhD/60Nd305Enh70.csv")





N71D90 <- N71D90 %>% 
  rename(
    datetime = `Date and Time`,
    secs = `Time (seconds)`,
    lx = `71N D90 (lx)`
  )

head(N71D90)


N71D90$datetime<- as.POSIXct(N71D90$datetime, format = "%d.%m.%Y %H:%M", tz="GMT")

plot<- ggplot(N71D90)+
  geom_line(aes(datetime, lx))+
  theme_bw()

ggplotly(plot)
