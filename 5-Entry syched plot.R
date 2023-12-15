library(ggplot2)
library(dplyr)
library(lubridate)

#Read dat
df <- read.csv('./anipill_tablets/allcoreT.csv')

#Make sure convert 'datetime' to datetime format
df$datetime <- as.POSIXct(df$datetime, format = "%Y-%m-%d %H:%M:%S")

#filter to exp time
df <- df[df$datetime >= as.POSIXct('2021-06-01') & df$datetime <= as.POSIXct('2021-08-01'), ]


#Identify hamsters that have ever reached below 10c
valid_hamsters <- df %>% 
  group_by(ID) %>% 
  filter(any(ap_t < 10, na.rm = TRUE)) %>% 
  summarise()%>%
  filter(!ID %in% c('H254', 'H263', 'H275', 'H216', 'H307'))#Filter away because suspect forced arousals


set.seed(123)  # Setting a seed
selected_hamsters <- sample(valid_hamsters$ID, 18) #all valid hammies

selected_hamsters

# Define a function to find the first anchor point for each hamster, ie first time entering MDT, not test drop
find_anchor_point <- function(ap_t, datetime) {
  below_10_time <- min(datetime[ap_t < 10], na.rm = TRUE)
  if (is.infinite(below_10_time)) {
    return(NA)
  }
  anchor_time <- max(datetime[datetime < below_10_time & ap_t > 25], na.rm = TRUE)
  return(anchor_time)
}
anchor_points_list <- lapply(selected_hamsters, function(hamster) {
  hamster_data <- df[df$ID == hamster, ]
  return(find_anchor_point(hamster_data$ap_t, hamster_data$datetime))
})

# Convert the list to a named vector
anchor_points <- unlist(anchor_points_list)
names(anchor_points) <- selected_hamsters

#checks what trhe ancorpoints are
print(as.POSIXct(anchor_points, origin="1970-01-01", tz="UTC"))

get_windowed_data <- function(hamster_id, df, anchor_points) {
  start_time <- anchor_points[hamster_id] - (7*24)*3600  # hours in seconds (unix)
  end_time <- anchor_points[hamster_id] + 1000*3600  # hours in seconds
  windowed_data <- df[df$ID == hamster_id & df$unix >= start_time & df$unix <= end_time, ]
  return(windowed_data)
}


#empty data frame for store the plotting data for the hamsters
all_plot_data <- data.frame()

#Gets the data from selected time period and calcs time from anchorpoint. 
for (hamster in selected_hamsters) {
  hamster_data <- get_windowed_data(hamster, df, anchor_points)
  hamster_data$time_from_anchor <- (hamster_data$unix - anchor_points[hamster]) / 3600  # Convert to hours
  all_plot_data <- rbind(all_plot_data, hamster_data)
}

all_plot_data <- na.omit(all_plot_data)



library(viridis)

# Plot the data 
plot <- ggplot(all_plot_data, aes(x = time_from_anchor, y = ap_t, color = ID)) +
  geom_line(linewidth=0.6,  alpha=0.75) +
  labs(x = "Hours",
       y = "Temperature (°C)") +
  scale_color_manual(values = rep("gray25", length(unique(all_plot_data$ID)))) +  # Set all lines to black
  #scale_color_viridis_d(option = 'H', alpha=0.85 )+
  #scale_color_brewer(palette = "Spectral")+
  scale_x_continuous(breaks = seq(-70,90, by =10), limits = c(-76,85))+
  scale_y_continuous(breaks = seq(5,40, by =5))+
  theme_classic()+
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
    )
plot

#ggsave("Entry_synch_BW.svg", plot =plot, width = 9, height = 6, path = "./graphix")


# Plot the data zoomed in on 0
plot2 <- ggplot(all_plot_data, aes(x = time_from_anchor, y = ap_t )) +
  geom_point(size=0.5,  alpha=0.7 ) +
  labs(x = "Hours",
       y = "Temperature (°C)") +
  scale_x_continuous(limits = c(-8,14) ,breaks = seq(-10,24, by =2))+
  geom_smooth(color='darkblue')+
  #scale_color_manual(values = rep("gray25", length(unique(all_plot_data$ID)))) +  # Set all lines to black
  #scale_color_viridis_d(option = 'H',alpha=0.85 )+
  #scale_color_brewer(palette = "Spectral")+
  scale_y_continuous(breaks = seq(5,40, by =5))+
  theme_classic()+
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
  )
plot2

#Save the plot (uncomment)
#ggsave("Entry_synch_BW-Zoomed.svg", plot =plot2, width = 7, height = 6, path = "./graphix")

# Plot the data zoomed in on arousal
plot3 <- ggplot(all_plot_data, aes(x = time_from_anchor, y = ap_t, color = ID)) +
  geom_line(linewidth=0.6,  alpha=0.8 ) +
  labs(x = "Hours",
       y = "Temperature (°C)") +
  scale_color_viridis_d(option = 'H')+
  #scale_color_brewer(palette = "Spectral")+
  scale_x_continuous(limits = c(45,80) ,breaks = seq(45,80, by =2))+
  scale_y_continuous(breaks = seq(5,40, by =5))+
  theme_bw()+
  theme(
    text = element_text(family = "Arial", size = 13),
    axis.text = element_text(color = "black"), 
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank(),
    legend.position = 'none', #comment out if wanted
    #legend.position = c(1.085, 1), # Move legend to top right corner, comment in if wanted
    legend.justification = c(1, 1), # Justify legend to the top right corner
    legend.key.size = unit(1, "lines"), # Adjust key size in the legend
    legend.text = element_text(size = 5),
    legend.title = element_text(size = 8),
    plot.margin = unit(c(1, 1.5, 1, 1), "cm")  # Top, right, bottom, left margins
  )
plot3


#Calc cooling rate at selected range
head(all_plot_data)
subset_apd <- all_plot_data%>% 
  dplyr::filter(ap_t > 19, ap_t < 27, time_from_anchor>-4, time_from_anchor<10 )


#make linear modeler on data subset for RRmax
lm_model <- lm(ap_t~ time_from_anchor, data = subset_apd)
coef <- coef(lm_model)
eqn <- paste0("y = ", round(coef[2], 2),'x', " + ", round(coef[1], 2))


x_range <- seq(min(subset_apd$time_from_anchor) - 3, max(subset_apd$time_from_anchor) +3, by = 0.1)
y_values <- coef(lm_model)[1] + coef(lm_model)[2] * x_range
line_data <- data.frame(time_from_anchor = x_range, ap_t = y_values)


p <- plot2 +
  geom_point(data = subset_apd, aes(x = time_from_anchor, y = ap_t), color = 'gray30')+
  geom_smooth(method = "lm", data = subset_apd, aes(x = time_from_anchor, y = ap_t), color = "firebrick", se = FALSE)+ 
  annotate("text", x = 4, y = 28, label = eqn, color = "gray30", size = 5, hjust = 0)+
  geom_line(data = line_data, aes(x = time_from_anchor, y = ap_t), color = "firebrick", size = 1)

p

ggsave("Entry-zoomed-regrssion.svg", plot =p, width = 7, height = 6, path = "./graphix")




