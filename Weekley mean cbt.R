library(lubridate)
require(scales)
library(data.table)
library(reshape2)
library(lubridate)
library(viridis)
library(readxl)
library(gplots)
library(RColorBrewer)
library(tmaptools)
library(ggsignif)
library(plotly)
library(MASS)
library(ggplot2)
library(dplyr)

#import all core body temperatures
allcoreT <- read.csv("./anipill_tablets/allcoreT.csv")
allcoreT$datetime <- as.POSIXct(allcoreT$datetime, format = "%Y-%m-%d %H:%M:%S", tz="UTC")

#filter core body temperatures to experimental duration
allcoreT<- allcoreT%>% filter(datetime >= as.Date("2021-04-21") & datetime <= as.Date("2021-08-24"))%>%
  na.omit(allcoreT)

#Definition of time periods:
noise<-as.numeric(as.POSIXct("2021-04-20 00:00:00"))
SP22 <-as.numeric(as.POSIXct("2021-04-28 00:00:00"))
SP22w4 <-as.numeric(as.POSIXct("2021-05-26 00:00:00"))
SP8w1 <-as.numeric(as.POSIXct("2021-06-03 00:00:00"))
SP8w2<-as.numeric(as.POSIXct("2021-06-10 00:00:00"))
SP8w3<-as.numeric(as.POSIXct("2021-06-17 00:00:00"))
SP8w4<-as.numeric(as.POSIXct("2021-06-24 00:00:00"))
SP8w5<-as.numeric(as.POSIXct("2021-07-01 00:00:00"))
SP8w6<-as.numeric(as.POSIXct("2021-07-08 00:00:00"))
SP8w7<-as.numeric(as.POSIXct("2021-07-15 00:00:00"))
SP8w8<-as.numeric(as.POSIXct("2021-07-22 00:00:00"))
SP8w9<-as.numeric(as.POSIXct("2021-07-29 00:00:00"))
SP8w10<-as.numeric(as.POSIXct("2021-08-05 00:00:00"))
SP8w11<-as.numeric(as.POSIXct("2021-08-12 00:00:00"))
SP8w12<-as.numeric(as.POSIXct("2021-08-19 00:00:00"))
SP8w13<-as.numeric(as.POSIXct("2021-08-26 00:00:00"))


#Assign time periods to experiment and define State based on body temperature
Period_states <-allcoreT %>% 
  mutate(Period= case_when(
    unix<=noise ~ "Pre-op",
    unix<=SP22 ~ "LP",
    unix<=SP22w4 ~ "SP22",
    unix<= SP8w1 ~ "SP8w1",
    unix<= SP8w2 ~ "SP8w2",
    unix<= SP8w3 ~ "SP8w3",
    unix<= SP8w4 ~ "SP8w4",
    unix<= SP8w5 ~ "SP8w5",
    unix<= SP8w6 ~ "SP8w6",
    unix<= SP8w7 ~ "SP8w7",
    unix<= SP8w8 ~ "SP8w8",
    unix<= SP8w9 ~ "SP8w9",
    unix<= SP8w10 ~ "SP8wz10",
    unix<= SP8w11 ~ "SP8wz11",
    unix<= SP8w12 ~ "SP8wz12",
    unix<= SP8w13 ~ "SP8wz13",
    TRUE ~ NA_character_
    ))%>% 
  group_by(Period, ID)%>% 
  mutate(State= case_when(
    ap_t<= 11.75 ~ "Torpid",
    ap_t<= 28.5 ~ "Entrance/Arousal",
    ap_t<= 39 ~ "Euthermic",
    TRUE ~ NA_character_))%>%
  mutate(Week = case_when(
    unix<= (SP8w1 - 7*604800) ~ -6,
    unix<= (SP8w1 - 6*604800) ~ -5,
    unix<= (SP8w1 - 5*604800) ~ -4,
    unix<= (SP8w1 - 4*604800) ~ -3,
    unix<= (SP8w1 - 3*604800) ~ -2,
    unix<= (SP8w1 - 2*604800) ~ -1,
    unix<= (SP8w1 - 1*604800) ~ 0,
    unix<= SP8w1 ~ 1,
    unix<= SP8w2 ~ 2,
    unix<= SP8w3 ~ 3,
    unix<= SP8w4 ~ 4,
    unix<= SP8w5 ~ 5,
    unix<= SP8w6 ~ 6,
    unix<= SP8w7 ~ 7,
    unix<= SP8w8 ~ 8,
    unix<= SP8w9 ~ 9,
    unix<= SP8w10 ~ 10,
    unix<= SP8w11 ~ 11,
    unix<= SP8w12 ~ 12,
    unix<= SP8w13 ~ 13,
    TRUE ~ NA_real_
  ))

#inspect
summary(Period_states)


#Extract time of day information and convert to numeric format
Period_states$tod <- hms::as_hms(Period_states$datetime) 
Period_states$tod <- as.numeric(Period_states$tod)/60^2


#Assign day night values based on lights on/off (lights on 09:00-17:00)
Period_states <- Period_states %>%
  mutate(DayNight = case_when(
    tod <9 ~ 'Night',
    tod >17 ~ 'Night',
    TRUE ~ 'Day'))


# Managed to create both states and time periods. Can now calculate mean BT per state per individual:
# Can also calculate mean torpor bout duration 

# Calculate mean_apt without considering the DayNight variable
mean_apt <- Period_states %>%
  group_by(ID, State, Week) %>%
  summarise(mean_apt = mean(ap_t, na.rm = TRUE), .groups = "drop")

# Calculate other summary statistics with the DayNight variable
mean_DN_apt <- Period_states %>%
  group_by(Period, ID, State, DayNight, Week) %>% 
  summarise(
    mean_BTind = mean(ap_t, na.rm = TRUE),
    median_BTind = median(ap_t, na.rm = TRUE),
    std_BTind = sd(ap_t, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  na.omit()

# Join the two data frames
Mean_BT_Period_states <- left_join(mean_DN_apt, mean_apt, by = c("ID", "State", "Week"))

#Calculate period means
Mean_BT_group_Period_states<- Period_states%>%
  group_by(Period,State, DayNight, Week) %>% 
  summarise(mean_BTind = mean(ap_t, na.rm=T),
            median_BTind = median(ap_t, na.rm=T),
            std_BTind = sd(ap_t, na.rm = T))%>%
  na.omit(Period_states)


#Show plot of mean euthermic body temperatures during experimental weeks (VIOLIN)
mean_w_cbt <- ggplot(subset(Mean_BT_Period_states, State %in% c("Euthermic")), aes(Period, y=mean_BTind, fill= factor(DayNight)))+
  geom_violin(position = position_dodge(width = 0.9), scale = "width", alpha=0.6)+
  geom_point(color="black", size=0.4, alpha=0.9, position = position_jitterdodge(dodge.width=0.9, jitter.width = .07)) +
  stat_summary(fun = mean, na.rm = TRUE, 
               geom = "crossbar",
               size = .5, color = "black", 
               position = position_dodge(width = .9)) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option = "D", direction = -1) +
  theme_classic() +
  theme(
    legend.position= c(0.95, 0.1),
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 0.75, hjust=0.9)) +
  xlab("")+ ylab(expression(paste("Body temperature ",(degree~C))))+
  ylim(c(25,40))

mean_w_cbt

#ggsave("./graphix/mean weekley core body temp.png", plot = mean_w_cbt, dpi = 240 )




#Mean resting euthermic body temperatures during experimental weeks
MR_w_cbt <- Mean_BT_Period_states %>%
  filter(DayNight == 'Day' & State == "Euthermic") %>%
  ggplot() +
  geom_line(aes(Week, y = mean_BTind, group = ID, color = ID), alpha=0.5) +
  scale_color_manual(values = rep("black", length(unique(Mean_BT_Period_states$ID)))) +  # Set all lines to black
  scale_x_continuous(breaks = seq(-6, 9, 1), limits = c(-5, 9)) +
  xlab("Weeks") +
  ylab(expression(paste("Body Temperature (°C)"))) +
  ylim(c(31, 37)) +
  theme_classic() +
  theme(
    text = element_text(family = "arial", face = "bold", size = 16),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(1, 1, 1, 1, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line = element_line(color = "black")
  )

MR_w_cbt

#save plot (uncomment)
#ggsave("./graphix/Resting_CBT_reset-2.svg",  width = 8, height = 6, plot =MR_w_cbt, dpi = 240 )

 ##########################################################################
#Statistical test for differences between day and night body temperature:
##########################################################################
library(dplyr)
#LP
LP_day <- Mean_BT_Period_states %>% filter(State=="Euthermic", DayNight== "Day", Period == "LP")%>% ungroup()%>% select(mean_BTind, ID)
LP_night <- Mean_BT_Period_states %>% filter(State=="Euthermic", DayNight== "Night", Period == "LP")%>% ungroup()%>% select(mean_BTind, ID)
#Shapiro-Wilk normality test 
shapiro.test(LP_day$mean_BTind) # => p-value = 0.6141
shapiro.test(LP_night$mean_BTind)
#paired T-test
t.test(LP_day$mean_BTind, LP_night$mean_BTind, paired = T)

#SP22
SP22_day <- Mean_BT_Period_states %>% filter(State=="Euthermic", DayNight== "Day", Period == "SP22")%>% ungroup()%>% select(mean_BTind, ID)
SP22_night <- Mean_BT_Period_states %>% filter(State=="Euthermic", DayNight== "Night", Period == "SP22")%>% ungroup()%>% select(mean_BTind, ID)
#Shapiro-Wilk normality test 
shapiro.test(SP22_day$mean_BTind) 
shapiro.test(SP22_night$mean_BTind)
#paired T-test
t.test(SP22_day$mean_BTind, SP22_night$mean_BTind, paired = T)

#SP8w1
SP8w1_day <- Mean_BT_Period_states %>% filter(State=="Euthermic", DayNight== "Day", Period == "SP8w1")%>% ungroup()%>% select(mean_BTind, ID)
SP8w1_night <- Mean_BT_Period_states %>% filter(State=="Euthermic", DayNight== "Night", Period == "SP8w1")%>% ungroup()%>% select(mean_BTind, ID)
#Shapiro-Wilk normality test 
shapiro.test(SP8w1_day$mean_BTind) 
shapiro.test(SP8w1_night$mean_BTind)
#paired T-test
t.test(SP8w1_day$mean_BTind, SP8w1_night$mean_BTind, paired = T)

#SP8w2
SP8w2_day <- Mean_BT_Period_states %>% filter(State=="Euthermic", DayNight== "Day", Period == "SP8w2")%>% ungroup()%>% select(mean_BTind, ID)
SP8w2_night <- Mean_BT_Period_states %>% filter(State=="Euthermic", DayNight== "Night", Period == "SP8w2")%>% ungroup()%>% select(mean_BTind, ID)
#Shapiro-Wilk normality test 
shapiro.test(SP8w2_day$mean_BTind) 
shapiro.test(SP8w2_night$mean_BTind)
#paired T-test
t.test(SP8w2_day$mean_BTind, SP8w2_night$mean_BTind, paired = T)


#SP8w3
SP8w3_day <- Mean_BT_Period_states %>% filter(State=="Euthermic", DayNight== "Day", Period == "SP8w3")%>% ungroup()%>% select(mean_BTind, ID)
SP8w3_night <- Mean_BT_Period_states %>% filter(State=="Euthermic", DayNight== "Night", Period == "SP8w3")%>% ungroup()%>% select(mean_BTind, ID)
#Shapiro-Wilk normality test 
shapiro.test(SP8w3_day$mean_BTind) 
shapiro.test(SP8w3_night$mean_BTind)
#paired T-test
t.test(SP8w3_day$mean_BTind, SP8w3_night$mean_BTind, paired = T)


#SP8w4
SP8w4_day <- Mean_BT_Period_states %>% filter(State=="Euthermic", DayNight== "Day", Period == "SP8w4")%>% ungroup()%>% select(mean_BTind, ID)
SP8w4_night <- Mean_BT_Period_states %>% filter(State=="Euthermic", DayNight== "Night", Period == "SP8w4")%>% ungroup()%>% select(mean_BTind, ID)
#Shapiro-Wilk normality test 
shapiro.test(SP8w4_day$mean_BTind) 
shapiro.test(SP8w4_night$mean_BTind)
#paired T-test
t.test(SP8w4_day$mean_BTind, SP8w4_night$mean_BTind, paired = T)

#SP8w5
SP8w5_day <- Mean_BT_Period_states %>% filter(State=="Euthermic", DayNight== "Day", Period == "SP8w5")%>% ungroup()%>% select(mean_BTind, ID)
SP8w5_night <- Mean_BT_Period_states %>% filter(State=="Euthermic", DayNight== "Night", Period == "SP8w5")%>% ungroup()%>% select(mean_BTind, ID)
#Shapiro-Wilk normality test 
shapiro.test(SP8w5_day$mean_BTind) 
shapiro.test(SP8w5_night$mean_BTind)
#paired T-test
t.test(SP8w5_day$mean_BTind, SP8w5_night$mean_BTind, paired = T)

#SP8w6
SP8w6_day <- Mean_BT_Period_states %>% filter(State=="Euthermic", DayNight== "Day", Period == "SP8w6")%>% ungroup()%>% select(mean_BTind, ID)
SP8w6_night <- Mean_BT_Period_states %>% filter(State=="Euthermic", DayNight== "Night", Period == "SP8w6")%>% ungroup()%>% select(mean_BTind, ID)
#Shapiro-Wilk normality test 
shapiro.test(SP8w6_day$mean_BTind) 
shapiro.test(SP8w6_night$mean_BTind)
#paired T-test
t.test(SP8w6_day$mean_BTind, SP8w6_night$mean_BTind, paired = T)

#SP8w7
SP8w7_day <- Mean_BT_Period_states %>% filter(State=="Euthermic", DayNight== "Day", Period == "SP8w7")%>% ungroup()%>% select(mean_BTind, ID)
SP8w7_night <- Mean_BT_Period_states %>% filter(State=="Euthermic", DayNight== "Night", Period == "SP8w7")%>% ungroup()%>% select(mean_BTind, ID)
#Shapiro-Wilk normality test 
shapiro.test(SP8w7_day$mean_BTind) 
shapiro.test(SP8w7_night$mean_BTind)
#paired T-test
t.test(SP8w7_day$mean_BTind, SP8w7_night$mean_BTind, paired = T)


#SP8w
SP8w8_day <- Mean_BT_Period_states %>% filter(State=="Euthermic", DayNight== "Day", Period == "SP8w8")%>% ungroup()%>% select(mean_BTind, ID)
SP8w8_night <- Mean_BT_Period_states %>% filter(State=="Euthermic", DayNight== "Night", Period == "SP8w8")%>% ungroup()%>% select(mean_BTind, ID)
#Shapiro-Wilk normality test 
shapiro.test(SP8w8_day$mean_BTind) 
shapiro.test(SP8w8_night$mean_BTind)
#paired T-test
t.test(SP8w8_day$mean_BTind, SP8w8_night$mean_BTind, paired = T)


#SP8w9
SP8w9_day <- Mean_BT_Period_states %>% filter(State=="Euthermic", DayNight== "Day", Period == "SP8w9")%>% ungroup()%>% select(mean_BTind, ID)
SP8w9_night <- Mean_BT_Period_states %>% filter(State=="Euthermic", DayNight== "Night", Period == "SP8w9")%>% ungroup()%>% select(mean_BTind, ID)
#Shapiro-Wilk normality test 
shapiro.test(SP8w9_day$mean_BTind) 
shapiro.test(SP8w9_night$mean_BTind)
#paired T-test
t.test(SP8w9_day$mean_BTind, SP8w9_night$mean_BTind, paired = F)


#SP8wz10
SP8wz10_day <- Mean_BT_Period_states %>% filter(State=="Euthermic", DayNight== "Day", Period == "SP8wz10")%>% ungroup()%>% select(mean_BTind, ID)
SP8wz10_night <- Mean_BT_Period_states %>% filter(State=="Euthermic", DayNight== "Night", Period == "SP8wz10")%>% ungroup()%>% select(mean_BTind, ID)
#Shapiro-wzilk normality test 
shapiro.test(SP8wz10_day$mean_BTind) 
shapiro.test(SP8wz10_night$mean_BTind)
#paired T-test
t.test(SP8wz10_day$mean_BTind, SP8wz10_night$mean_BTind, paired = F)

#SP8wz11
SP8wz11_day <- Mean_BT_Period_states %>% filter(State=="Euthermic", DayNight== "Day", Period == "SP8wz11")%>% ungroup()%>% select(mean_BTind, ID)
SP8wz11_night <- Mean_BT_Period_states %>% filter(State=="Euthermic", DayNight== "Night", Period == "SP8wz11")%>% ungroup()%>% select(mean_BTind, ID)
#Shapiro-wzilk normality test 
shapiro.test(SP8wz11_day$mean_BTind) 
shapiro.test(SP8wz11_night$mean_BTind)
#paired T-test
t.test(SP8wz11_day$mean_BTind, SP8wz11_night$mean_BTind, paired = T)

#SP8wz12
SP8wz12_day <- Mean_BT_Period_states %>% filter(State=="Euthermic", DayNight== "Day", Period == "SP8wz12")%>% ungroup()%>% select(mean_BTind, ID)
SP8wz12_night <- Mean_BT_Period_states %>% filter(State=="Euthermic", DayNight== "Night", Period == "SP8wz12")%>% ungroup()%>% select(mean_BTind, ID)
#Shapiro-wzilk normality test 
shapiro.test(SP8wz12_day$mean_BTind) 
shapiro.test(SP8wz12_night$mean_BTind)
#paired T-test
t.test(SP8wz12_day$mean_BTind, SP8wz12_night$mean_BTind, paired = F)

#not possible with T test with 2 observations for week 13


################################################################################
#% Animal expressing 1st torpor bout
################################################################################

nT_SP8w4 <- Period_states %>% filter(Period %in% c("SP8w4") & State == "Torpid" )%>%
  summarize(n_torpid=n_distinct(ID))%>%as.numeric()

nT_SP8w5 <- Period_states %>% filter(Period %in% c("SP8w4","SP8w5") & State == "Torpid" )%>%
  summarize(n_torpid=n_distinct(ID))%>%as.numeric() + 1   #+1: H222 faulty anipill, deep torpor confirmed by IPTT record 

nT_SP8w6 <- Period_states %>% filter(Period %in% c("SP8w4","SP8w5", "SP8w6") & State == "Torpid" )%>%
  summarize(n_torpid=n_distinct(ID))%>%as.numeric() +1 #H222 as above
 #
nT_SP8w7 <- Period_states %>% filter(Period %in% c("SP8w4","SP8w5", "SP8w6","SP8w7") & State == "Torpid" )%>%
  summarize(n_torpid=n_distinct(ID))%>%as.numeric() +1 #H222 as above

nT_SP8w8 <- Period_states %>% filter(Period %in% c("SP8w4","SP8w5", "SP8w6", "SP8w7", "SP8w8") & State == "Torpid" )%>%
  summarize(n_torpid=n_distinct(ID))%>%as.numeric() +4   #+4: H222 as above. H264, H268, H245 faulty anipills, torpid according to manual check and IR-recordings at week 8

nT_SP8w9 <- Period_states %>% filter(Period %in% c("SP8w4","SP8w5", "SP8w6", "SP8w7", "SP8w8","SP8w9") & State == "Torpid" )%>%
  summarize(n_torpid=n_distinct(ID))%>%as.numeric() +4 #as above

nT_SP8w10 <-Period_states %>% filter(Period %in% c("SP8w4","SP8w5", "SP8w6", "SP8w7", "SP8w8","SP8w9", "SP8w10") & State == "Torpid" )%>%
  summarize(n_torpid=n_distinct(ID))%>%as.numeric() +4 #as above

nT_SP8w11 <- Period_states %>% filter(Period %in% c("SP8w4","SP8w5", "SP8w6", "SP8w7", "SP8w8","SP8w9", "SP8w10", "SP8w11") & State == "Torpid" )%>%
  summarize(n_torpid=n_distinct(ID))%>%as.numeric() +4 #As above

nT_SP8w12 <- Period_states %>% filter(Period %in% c("SP8w4","SP8w5", "SP8w6", "SP8w7", "SP8w8","SP8w9", "SP8w10", "SP8w11", "SP8w12") & State == "Torpid"  )%>%
  summarize(n_torpid=n_distinct(ID))%>%as.numeric() +4 #As above

nT_SP8w13 <- Period_states %>% filter(State == "Torpid"  )%>%
  summarize(n_torpid=n_distinct(ID))%>%as.numeric() +4 #As above


percent_torpor <- c(0,0,0,nT_SP8w4/30,nT_SP8w5/30,nT_SP8w6/30,nT_SP8w7/30,nT_SP8w8/30,nT_SP8w9/30,nT_SP8w10/30,nT_SP8w11/30,nT_SP8w12/30,nT_SP8w13/30 )

n_weeks_SP8c <- c(1:13)

frac_torpor <- data.frame(percent_torpor,n_weeks_SP8c)
  

Step_plot <- ggplot(frac_torpor, aes(n_weeks_SP8c, percent_torpor))+
 geom_step(linewidth=2, color="dodgerblue4")+ theme_bw()+
  scale_x_continuous(n.breaks = 13)+
  labs(x= expression(degree), y="% Expressed deep torpor")+
  theme(text = element_text(family = "serif", size=15))

Step_plot

ggsave("./graphix/Percent deep torpor.png",  Step_plot, dpi = 300 )



################################################################################
#Animation of CBT change. NB! Make take some time and processing power! Un comment to run commands
library(gganimate)
library(gifski)


get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}


#Period_states$density <- get_density(Period_states$tod, Period_states$ap_t, n = 600)

#make sure data is ordered according to datetime
#Period_states <- Period_states[order(Period_states$unix),]

#Period_states$floordate <- floor_date(Period_states$datetime, unit = "day")

#a <- Period_states %>% 
 # ggplot(aes(tod, ap_t, color=density))+
# geom_point(size=3, alpha=0.7)+
#  scale_color_viridis() + ylim(c(5,37)) +coord_polar()

#a


#GIF1 <- a+transition_time(floordate)+
#  labs(title = "{frame_time}") +
#  shadow_wake(wake_length = 0.5)

#GIF1

#animate(animacion, width = 700, height = 432, fps = 25, duration = 15, rewind = FALSE)

