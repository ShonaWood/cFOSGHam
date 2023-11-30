library(readr)
library(lubridate)
library(zoo)
library(plotly)
library(dplyr)
library(tidyverse)

#Plot for calculating average IPTT temp and Tb during arousal 

#Imp and format ambient temps
Tempisolat <- read_csv("./env/Tempisolat.csv", na = "empty")
Tempisbjorn <- read_csv("./env/Tempisbjorn.csv", na = "empty")

Tempisbjorn$ID <- paste("amb")
names(Tempisbjorn)[1:3] <-c("datetime", "rt", "ID")
Tempisbjorn$datetime <- as.POSIXct(Tempisbjorn$datetime, format = "%Y/%m/%d %H:%M:%S", tz="GMT")
Tempisbjorn$datetime <- Tempisbjorn$datetime - dhours(0)
Tempisbjorn$unix <- as.numeric(Tempisbjorn$datetime)
Tempisbjorn$tc <- Tempisbjorn$rt + 0.88
Tempisbjorn <- Tempisbjorn %>% filter(tc>20)

Tempisolat$ID <- paste("amb")
names(Tempisolat)[1:3] <-c("datetime", "rt", "ID")
Tempisolat$datetime <- as.POSIXct(Tempisolat$datetime, format = "%Y/%m/%d %H:%M:%S", tz="GMT")
Tempisolat$datetime <- Tempisolat$datetime - dhours(0)
Tempisolat$unix <- as.numeric(Tempisolat$datetime)
Tempisolat$tc <- Tempisolat$rt + 0.88


Tamb <- full_join(Tempisbjorn, Tempisolat)

remove(Tempisbjorn, Tempisolat)

Tamb %>% filter(rt<10)%>% summarise(mean(rt))


#Imp and format all core Tb
allcoreT <- read.csv("./anipill_tablets/allcoreT.csv")
allcoreT$datetime <- as.POSIXct(allcoreT$datetime, format= "%Y-%m-%d %H:%M:%S", tz="GMT") 
head(allcoreT)

lims <- as.POSIXct(strptime(c("01/07/21 10:00","17/08/21 10:00"), format = "%d/%m/%y %H:%M", tz="GMT"))



#Imp and format IPTT temps 
BAT1 <- read.csv("./BAT/BAT temperatures-ALL-nr1-2021-08-16 12_45_19.csv")
BAT2 <- read.csv("./BAT/BAT temperatures-ALL-nr2-2021-08-16 12_44_24.csv")


summary(BAT1)
summary(BAT2)

#rmove redundat ruuvi temps
BAT1 <- BAT1[,-which(names(BAT1) %in% c("RUUVISOLAT.mean", "Ambient.temperature"))]
BAT2 <- BAT2[,-which(names(BAT2) %in% c("RUUVISOLAT.mean", "Ambient.temperature"))]

head(BAT1)
head(BAT2)

#need to cast into vetical long format
BAT1_long <- BAT1 %>%
  gather(key = "ID", value = "t_bat", -Time) %>%
  rename(Datetime = Time)

BAT2_long <- BAT2 %>%
  gather(key = "ID", value = "t_bat", -Time) %>%
  rename(Datetime = Time)

#to merge the long form IPTT datas
cbat <- rbind(BAT1_long, BAT2_long)

#rmove NA DATA
cbat <- cbat[!is.na(cbat$t_bat),]

#clean up remove old
remove("BAT1","BAT2","BAT1_long", "BAT2_long")

#freememory
gc()

#make sure datime is right format
cbat$Datetime <- as.POSIXct(cbat$Datetime, format="%Y-%m-%d %H:%M:%S", tz="UTC")

#check sum data
summary(cbat)

#Very high extremes need to filter
# Filtering out noisy readings
cbat <- cbat %>%
  filter(t_bat > 6 & t_bat < 39)

#add the unix time for easy handel
cbat$unix <- as.numeric(cbat$Datetime)

#Plotting the data with ambient temperature
 #ggplot() +
  #geom_point(data = cbat, aes(x = unix, y = t_bat, color = ID, group = ID), alpha = 0.5) +
#  geom_line(data = Tamb, aes(x = unix, y = tc, color = ID, group = ID)) +
#  theme_minimal() +
#  ylim(c(5,40)) +
#  labs(title = "Temperature Readings of Bats and Ambient over Time",
#       x = "Datetime",
#       y = "Temperature",
#       color = "ID") +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#filtering the dataframes to the expterimental timeperiod for less data handing
cbat_filtered <- cbat %>%
  filter(Datetime >= lims[1] & Datetime <= lims[2])%>%
  rename(datetime=Datetime)

Tamb_filtered <- Tamb %>%
  filter(datetime >= lims[1] & datetime <= lims[2])

allcoreT_filtered <- allcoreT %>%
  filter(datetime >= lims[1] & datetime <= lims[2])

#rounding data dttm for easier handling
cbat_filtered$datetime <- round_date(cbat_filtered$datetime, unit="5 seconds")
allcoreT_filtered$datetime <- round_date(allcoreT_filtered$datetime, unit="5 seconds")


#removal of some columns to ease merge
if("unix" %in% names(cbat_filtered)) cbat_filtered$unix <- NULL
if("unix" %in% names(Tamb_filtered)) Tamb_filtered$unix <- NULL
if("rt" %in% names(Tamb_filtered)) Tamb_filtered$rt <- NULL

#Faster way: Looping through and remove them from allcoreT_filtered
for (col in c("unix", "X", "ap_ID")) {
  if (col %in% names(allcoreT_filtered)) {
    allcoreT_filtered[[col]] <- NULL
  }
}


#and merging to join to full frame
cdf_1 <- full_join( Tamb_filtered, allcoreT_filtered, by = c("datetime", "ID"))
cdf_1 <- cdf_1[order(cdf_1$datetime),]
cdf_1 <- cdf_1 %>%
  arrange(datetime) %>%
  fill(tc, .direction = "down")


cdf <- full_join(cdf_1,cbat_filtered,by = c("datetime", "ID"))


#make sure data is ordered according to datetime
cdf <- cdf[order(cdf$datetime),]

# Fill in missing values for 'tc'  abt ap_t based on the last observation  "Last Observation Carried Forward" (LOCF).
cdf <- cdf %>%
  arrange(datetime) %>%
  fill(tc, .direction = "down")

summary(cdf)


#inspect data
pl <- ggplot(subset(cdf, ID %in% c("H248")), aes(x=datetime)) +
  scale_x_datetime(limits=lims, date_breaks="1 week", date_labels="%W") +
  geom_point(aes(y=t_bat), color="black", alpha=0.9, size=2) +
  geom_line(data = subset(cdf, ID %in% c("H298") & !is.na(ap_t)), aes(y=ap_t), color="firebrick", alpha=0.7) +
  theme_bw()

#(pl)
#clean environment
remove(allcoreT, allcoreT_filtered, cbat, cbat_filtered, cdf_1, Tamb, Tamb_filtered)
#free space
gc()

###################################################################################
#apply filter made from manual picking of suitable temperature traces to plot together. See cbt_shiny
#Need to do manual as scripted methods for finding correct ponts did not work. Simpley going through the animals and picking suitable tracks prooved best and fastest. 

filter <- read.csv("./BAT/ID_time_filter_BAT_HibExp1_corrected.csv")
filter$start_time <- as.POSIXct(filter$start_time,format = "%Y/%m/%d %H:%M:%S", tz="GMT")
filter$end_time <- as.POSIXct(filter$end_time, format = "%Y/%m/%d %H:%M:%S",tz="GMT")
filter$correction_value <- as.numeric(as.character(filter$correction_value))

# Initialize an empty dataframe to store the filtered data
cdf_final <- data.frame()

# Loop through each row of the filter dataframe
for (i in 1:nrow(filter)) {
  specific_ID <- filter$ID[i]
  start_t <- filter$start_time[i]
  end_t <- filter$end_time[i]
  temp_data <- cdf %>%
    filter(ID == specific_ID & datetime >= start_t & datetime <= end_t)
  cdf_final <- rbind(cdf_final, temp_data)
}

# Adding back the ambient temperature data
amb_data <- cdf %>% filter(ID == "amb")
cdf_final <- full_join(cdf_final, amb_data)

# Create a new column 'tc_bat' initialized with the original 't_bat' values
cdf_final$tc_bat <- cdf_final$t_bat

# Iterating through each row of the filter dataframe
for (i in 1:nrow(filter)) {
  specific_ID <- filter$ID[i]
  correction <- filter$correction_value[i]
  cdf_final$tc_bat[cdf_final$ID == specific_ID] <- cdf_final$t_bat[cdf_final$ID == specific_ID] + correction
}

# View the first few rows of the updated dataframe
summary(cdf_final)

unique(cdf_final$ID)
gc()





###############################################################################
#Final manipulations to get plot together

# plotts to inspect individul data
p <- cdf_final%>% filter(ID=="H248")%>%
  ggplot( aes(x = datetime)) +
  geom_point(aes(y = tc_bat, color = ID), size = 0.5) +
  geom_point(aes(y= (ap_t)), color='firebrick', alpha=0.7) +
  geom_point(aes(y = tc, color = "amb"), size = 0.5, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Temperature Readings over Time",
       x = "Datetime",
       y = "Temperature (°C)",
       color = "ID") +
  ylim(c(5, 40)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#ggplotly(p)


closest_to_20 <- cdf_final %>%
  filter(!ID == 'amb') %>%
  group_by(ID) %>%
  arrange(abs(tc_bat - 17)) %>%
  slice(1) %>%
  select(ID, datetime, tc_bat)

# Display the time points closest to 20 degrees for each ID
closest_to_20

#closest_to_20_unique to Unix timestamps
closest_to_20$unix_anchor <- as.numeric(closest_to_20$datetime)

#Unix timestamps as anchor points for each ID
cdf_final <- merge(cdf_final, closest_to_20[, c("ID", "unix_anchor")], by = "ID", all.x = TRUE)

# Convert the datetime  to Unix timestamps and subtract the anchor point timestamp to get the relative time difference in hours
cdf_final$unix <- as.numeric(cdf_final$datetime)
cdf_final$hours_from_anchor <- (cdf_final$unix - cdf_final$unix_anchor) / 3600 # Convert seconds to hours


# Apply LOWESS smoothing to filer out noisy reads
cdf_final <- cdf_final %>%
  arrange(ID, datetime) %>%
  group_by(ID) %>%
  mutate(
    tc_bat_sm = stats::lowess(tc_bat, f = 0.5)$y
  ) %>%
  ungroup()

#filter out animal unsuited for model (incomplete or corrupted data)
cdf_final <- cdf_final %>% filter(!ID %in% c('H248'))

# Interpolating the ap_t column for each ID to aid TRD calculation 
cdf_final <- cdf_final %>%
  filter(!ID == 'amb') %>%
  group_by(ID, datetime) %>%
  mutate(avg_ap_t = mean(ap_t, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(ID) %>%
  mutate(ap_t_interp = ifelse(is.na(avg_ap_t), na.approx(avg_ap_t, x = datetime, na.rm = TRUE, rule = 2), avg_ap_t))


cdf_final<- cdf_final %>%
  filter(abs(hours_from_anchor) <= 10)

cdf_final <- cdf_final %>%
  group_by(ID) %>%
  mutate(
    # Calculate TRD for each ID
    TRD = {
      valid_indices <- ap_t < 9.5 & ap_t > 6 & ap_t <= mean(ap_t, na.rm = TRUE) #filter valid temperatures for calculation
      mean_diff <- mean(ap_t[valid_indices] - tc[valid_indices], na.rm = TRUE) #calculation of average distance from Ta
      mean_diff  
    },
    mean_ambient = mean(tc, na.rm = TRUE),  
    IPTT_diff = tc_bat_sm - (mean_ambient + TRD),
    ap_diff = ap_t_interp - (mean_ambient + TRD)
  ) %>%
  ungroup()


cdf_final$IPTT_diff[cdf_final$hours_from_anchor >3] <- NA #cut-off for clarity

#calculats moving avg across IDs, Adjust line three to ajust interval averaging
cdf_final_10min_avg <- cdf_final %>%
  filter(hours_from_anchor <= 2) %>%
  mutate(hours_from_anchor= round(hours_from_anchor * 6) / 6) %>% #every 10 mins
  group_by(hours_from_anchor) %>%
  summarize(
    IPTT_diff_avg = mean(IPTT_diff, na.rm = TRUE),
    IPTT_diff_sem =(sd(IPTT_diff, na.rm = TRUE)),
    ap_diff_avg = mean(ap_diff, na.rm = TRUE),
    ap_diff_sem = sd(ap_diff, na.rm = TRUE))%>%
  ungroup()

cdf_final_10min_avg$IPTT_diff_avg[cdf_final_10min_avg$hours_from_anchor > 3] <- NA #cut-off for clarity

#sanity chacks
head(cdf_final_10min_avg)
head(cdf_final)
unique(cdf_final$ID)

#mooving avg Ta 
tc_avg <- cdf_final %>%
  filter(hours_from_anchor <= 10) %>%
  mutate(hours_from_anchor = round(hours_from_anchor * 6) / 6) %>%
  group_by(hours_from_anchor) %>%
  summarize(tc_avg = mean(tc, na.rm = TRUE)) %>%
  ungroup()

summary(tc_avg)



#plotting the data using the relatrive times
p <- ggplot(data = cdf_final_10min_avg, aes(x=hours_from_anchor)) +
  geom_ribbon( aes(ymin = IPTT_diff_avg - IPTT_diff_sem, ymax = IPTT_diff_avg + IPTT_diff_sem), fill = 'grey85', alpha = 0.75) +
  geom_ribbon( aes(ymin = ap_diff_avg - ap_diff_sem, ymax = ap_diff_avg + ap_diff_sem), fill = 'grey85', alpha =0.75) +
  geom_line( aes(y=IPTT_diff_avg), color= 'firebrick', linewidth=1.2, alpha=0.75)+
  geom_line( aes(y=ap_diff_avg), color= 'darkgreen', linewidth=1.2, alpha=0.75)+
  geom_line( data=tc_avg, aes(y = mean(tc_avg - (8.128+mean(!is.na(cdf_final$TRD))))), linewidth = 0.75, color = "grey50", alpha = 0.75) + #added to display average mean TRD
  scale_color_brewer(palette = "Spectral")+
  scale_x_continuous(breaks = seq(-4,3, by =1), limits = c(-4,3))+
  scale_y_continuous(limits = c(-2,28), n.breaks = 20,
                   sec.axis =  sec_axis( trans=~.+8.18, name="TiBAT(\u00B0C)", breaks = seq(-6,36, by =2) ))+
  theme_classic() +
  theme(
    text = element_text(family = "arial", size = 20),
    legend.position = "none",
    #plot.title = element_text(hjust = 0.5),
    plot.margin = margin(1, 1, 1, 1, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  labs(
    x = "Time (hrs)", 
    y = "\U001D6AB\u00B0C from torpor Tb",
    color = "ID"
  )

p

#saving the plot (uncomment)
ggsave("BAT-Tb-10min_average.svg", plot =p, width = 8, height = 6, path = "./graphix")


#calcs for max rewarmign speed: 
head(cdf_final)
subset_cdf <- cdf_final %>% 
  filter(IPTT_diff > 10, IPTT_diff < 24, hours_from_anchor<1.1)

#make linear modeler on data subset for RRmax
lm_model <- lm(IPTT_diff ~ hours_from_anchor, data = subset_cdf)
coef <- coef(lm_model)
eqn <- paste0("y = ", round(coef[1], 2), " + ", round(coef[2], 2), "x")


x_range <- seq(min(subset_cdf$hours_from_anchor) - 0.2, max(subset_cdf$hours_from_anchor) + 1.3, by = 0.1)
y_values <- coef(lm_model)[1] + coef(lm_model)[2] * x_range
line_data <- data.frame(hours_from_anchor = x_range, IPTT_diff = y_values)


p <- p + geom_point(data = subset_cdf, aes(x = hours_from_anchor, y = IPTT_diff), color = 'gray30')+
  geom_smooth(method = "lm", data = subset_cdf, aes(x = hours_from_anchor, y = IPTT_diff), color = "blue", se = FALSE)+ 
  annotate("text", x = -3, y = 25, label = eqn, color = "blue", size = 5, hjust = 0)+
  geom_line(data = line_data, aes(x = hours_from_anchor, y = IPTT_diff), color = "blue", size = 1)

p


