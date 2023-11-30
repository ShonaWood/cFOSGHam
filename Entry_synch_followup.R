#trynbg to add ech bout for analysis. Related to Entry sunched plot
#currently not working as expected:

apd2<- all_plot_data%>%
  dplyr::filter(time_from_anchor>85)

ggplot(apd2, aes(x = time_from_anchor, y = ap_t, color = ID)) +
  geom_line(linewidth=0.6,  alpha=0.75) +xlim(85,200)

head(apd2)


# Define a function to find the anchor point for each hamster
find_anchor_point <- function(ap_t, datetime) {
  below_10_time <- min(datetime[ap_t < 10], na.rm = TRUE)
  
  # If no time is found where temperature drops below 10°C, return NA
  if (is.infinite(below_10_time)) {
    return(NA)
  }
  
  anchor_time <- max(datetime[datetime < below_10_time & ap_t > 25], na.rm = TRUE)
  return(anchor_time)
}

# Calculate anchor points for each of the selected hamsters
anchor_points_list <- lapply(selected_hamsters, function(hamster) {
  hamster_data <- apd2[apd2$ID == hamster, ]
  return(find_anchor_point(hamster_data$ap_t, hamster_data$datetime))
})

# Convert the list to a named vector
anchor_points <- unlist(anchor_points_list)
names(anchor_points) <- selected_hamsters

#checks what trhe ancorpoints are
print(as.POSIXct(anchor_points, origin="1970-01-01", tz="UTC"))

get_windowed_data <- function(hamster_id, apd2, anchor_points) {
  # Define the window: 6 hours before to 36 hours after the anchor point
  start_time <- anchor_points[hamster_id] - (7*24)*3600  # hours in seconds (unix)
  end_time <- anchor_points[hamster_id] + 1000*3600  # hours in seconds
  
  # Filter the data
  windowed_data <- apd2[apd2$ID == hamster_id & apd2$unix >= start_time & apd2$unix <= end_time, ]
  
  # Return the filtered data
  return(windowed_data)
}


#empty data frame for store the plotting data for the hamsters
apd2_pl <- data.frame()

#Gets the data from selected time period and calcs time from anchorpoint. 
for (hamster in selected_hamsters) {
  hamster_data <- get_windowed_data(hamster, apd2, anchor_points)
  
  # Calculates time from anchor in hours
  hamster_data$time_from_anchor <- (hamster_data$unix - anchor_points[hamster]) / 3600  # Convert to hours
  apd2_pl <- rbind(apd2_pl, hamster_data)  # Corrected to append to apd2_pl
}

apd2_pl <- na.omit(apd2_pl)

# Plot the data 
ggplot(apd2_pl, aes(x = time_from_anchor, y = ap_t, color = ID)) +
  geom_line(linewidth=0.6,  alpha=0.75) 

ggplot(apd2_pl, aes(x = time_from_anchor, y = ap_t, color = ID)) +
  geom_line(linewidth=0.6,  alpha=0.75) +xlim(-10,150)
################################################################################
library(dplyr)
apd3<- apd2_pl%>%
  dplyr::filter(time_from_anchor>40)

ggplot(apd3, aes(x = time_from_anchor, y = ap_t, color = ID)) +
  geom_line(linewidth=0.6,  alpha=0.75) +xlim(40,200)

dfilter<- function(data, .group) {
  # Find the first point where ap_t drops below 10°C
  first_below_10_index <- which(data$ap_t < 15)[1]
  
  # If no such point is found, or if it's the last point in the data, return the original data
  if (is.na(first_below_10_index) || first_below_10_index == nrow(data)) {
    return(data)
  }
  
  # Check if ap_t exceeds 30°C within the next 80 hours from the first below 10°C point
  max_hours_check <- 80  # Maximum hours to check
  time_limit <- data$time_from_anchor[first_below_10_index] + max_hours_check
  if (any(data$ap_t[first_below_10_index:nrow(data)] > 30 & data$time_from_anchor[first_below_10_index:nrow(data)] <= time_limit)) {
    # Find the first occurrence where ap_t exceeds 30°C after first_below_10_index
    first_above_30_index <- which(data$ap_t > 30 & data$time_from_anchor > data$time_from_anchor[first_below_10_index])[1]
    
    # If no such point is found, return the original data
    if (is.na(first_above_30_index)) {
      return(data)
    }
    
    # Filter out data points below 30°C before this point
    data <- data %>% dplyr::filter(!(time_from_anchor < data$time_from_anchor[first_above_30_index] & ap_t < 30))
  }
  
  return(data)
}

# Apply the function to each ID
apd3 <- apd2_pl %>%
  group_by(ID) %>%
  group_modify(~ dfilter(.x, .y))

# Convert the result back to a data frame if it's a grouped_df
apd3 <- as.data.frame(apd3)

ggplot(apd3, aes(x = time_from_anchor, y = ap_t, color = ID)) +
  geom_line(linewidth=0.6,  alpha=0.75) +xlim(40,200)

head(apd3)


# Define a function to find the anchor point for each hamster
find_anchor_point <- function(ap_t, datetime) {
  below_10_time <- min(datetime[ap_t < 10], na.rm = TRUE)
  
  # If no time is found where temperature drops below 10°C, return NA
  if (is.infinite(below_10_time)) {
    return(NA)
  }
  
  anchor_time <- max(datetime[datetime < below_10_time & ap_t > 25], na.rm = TRUE)
  return(anchor_time)
}

# Calculate anchor points for each of the selected hamsters
anchor_points_list <- lapply(selected_hamsters, function(hamster) {
  hamster_data <- apd3[apd3$ID == hamster, ]
  return(find_anchor_point(hamster_data$ap_t, hamster_data$datetime))
})

# Convert the list to a named vector
anchor_points <- unlist(anchor_points_list)
names(anchor_points) <- selected_hamsters

#checks what trhe ancorpoints are
print(as.POSIXct(anchor_points, origin="1970-01-01", tz="UTC"))

get_windowed_data <- function(hamster_id, apd3, anchor_points) {
  # Define the window: 6 hours before to 36 hours after the anchor point
  start_time <- anchor_points[hamster_id] - (7*24)*3600  # hours in seconds (unix)
  end_time <- anchor_points[hamster_id] + 1000*3600  # hours in seconds
  
  # Filter the data
  windowed_data <- apd3[apd3$ID == hamster_id & apd3$unix >= start_time & apd3$unix <= end_time, ]
  
  # Return the filtered data
  return(windowed_data)
}


#empty data frame for store the plotting data for the hamsters
apd3_pl <- data.frame()

#Gets the data from selected time period and calcs time from anchorpoint. 
for (hamster in selected_hamsters) {
  hamster_data <- get_windowed_data(hamster, apd3, anchor_points)
  
  # Calculates time from anchor in hours
  hamster_data$time_from_anchor <- (hamster_data$unix - anchor_points[hamster]) / 3600  # Convert to hours
  apd3_pl <- rbind(apd3_pl, hamster_data)  # Corrected to append to apd3_pl
}

apd3_pl <- na.omit(apd3_pl)

# Plot the data 
ggplot(apd3_pl, aes(x = time_from_anchor, y = ap_t, color = ID)) +
  geom_line(linewidth=0.6,  alpha=0.75) 
######################################################################### 
#########################################################################  
apd4<- apd3_pl%>%
  dplyr::filter(time_from_anchor>50)

ggplot(apd4, aes(x = time_from_anchor, y = ap_t, color = ID)) +
  geom_line(linewidth=0.6,  alpha=0.75) +xlim(40,200)

dfilter<- function(data, .group) {
  # Find the first point where ap_t drops below 10°C
  first_below_10_index <- which(data$ap_t < 15)[1]
  
  # If no such point is found, or if it's the last point in the data, return the original data
  if (is.na(first_below_10_index) || first_below_10_index == nrow(data)) {
    return(data)
  }
  
  # Check if ap_t exceeds 30°C within the next 80 hours from the first below 10°C point
  max_hours_check <- 80  # Maximum hours to check
  time_limit <- data$time_from_anchor[first_below_10_index] + max_hours_check
  if (any(data$ap_t[first_below_10_index:nrow(data)] > 30 & data$time_from_anchor[first_below_10_index:nrow(data)] <= time_limit)) {
    # Find the first occurrence where ap_t exceeds 30°C after first_below_10_index
    first_above_30_index <- which(data$ap_t > 30 & data$time_from_anchor > data$time_from_anchor[first_below_10_index])[1]
    
    # If no such point is found, return the original data
    if (is.na(first_above_30_index)) {
      return(data)
    }
    
    # Filter out data points below 30°C before this point
    data <- data %>% dplyr::filter(!(time_from_anchor < data$time_from_anchor[first_above_30_index] & ap_t < 30))
  }
  
  return(data)
}

# Apply the function to each ID
apd4 <- apd2_pl %>%
  group_by(ID) %>%
  group_modify(~ dfilter(.x, .y))

# Convert the result back to a data frame if it's a grouped_df
apd4 <- as.data.frame(apd4)

ggplot(apd4, aes(x = time_from_anchor, y = ap_t, color = ID)) +
  geom_line(linewidth=0.6,  alpha=0.75) +xlim(40,200)

head(apd4)


# Define a function to find the anchor point for each hamster
find_anchor_point <- function(ap_t, datetime) {
  below_10_time <- min(datetime[ap_t < 10], na.rm = TRUE)
  
  # If no time is found where temperature drops below 10°C, return NA
  if (is.infinite(below_10_time)) {
    return(NA)
  }
  
  anchor_time <- max(datetime[datetime < below_10_time & ap_t > 25], na.rm = TRUE)
  return(anchor_time)
}

# Calculate anchor points for each of the selected hamsters
anchor_points_list <- lapply(selected_hamsters, function(hamster) {
  hamster_data <- apd4[apd4$ID == hamster, ]
  return(find_anchor_point(hamster_data$ap_t, hamster_data$datetime))
})

# Convert the list to a named vector
anchor_points <- unlist(anchor_points_list)
names(anchor_points) <- selected_hamsters

#checks what trhe ancorpoints are
print(as.POSIXct(anchor_points, origin="1970-01-01", tz="UTC"))

get_windowed_data <- function(hamster_id, apd4, anchor_points) {
  # Define the window: 6 hours before to 36 hours after the anchor point
  start_time <- anchor_points[hamster_id] - (7*24)*3600  # hours in seconds (unix)
  end_time <- anchor_points[hamster_id] + 1000*3600  # hours in seconds
  
  # Filter the data
  windowed_data <- apd4[apd4$ID == hamster_id & apd4$unix >= start_time & apd4$unix <= end_time, ]
  
  # Return the filtered data
  return(windowed_data)
}


#empty data frame for store the plotting data for the hamsters
apd4_pl <- data.frame()

#Gets the data from selected time period and calcs time from anchorpoint. 
for (hamster in selected_hamsters) {
  hamster_data <- get_windowed_data(hamster, apd4, anchor_points)
  
  # Calculates time from anchor in hours
  hamster_data$time_from_anchor <- (hamster_data$unix - anchor_points[hamster]) / 3600  # Convert to hours
  apd4_pl <- rbind(apd4_pl, hamster_data)  # Corrected to append to apd4_pl
}

apd4_pl <- na.omit(apd4_pl)

# Plot the data 
ggplot(apd4_pl, aes(x = time_from_anchor, y = ap_t, color = ID)) +
  geom_line(linewidth=0.6,  alpha=0.75) 
