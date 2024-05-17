rm(list = ls())
# including the library
library(httr)
library(jsonlite)
library(ggplot2)
library(lubridate) # For date-time manipulation
library(zoo)
library(tidyverse)

choose_lower_percentil <- 0.15
choose_upper_percentil <- 0.85

# call the URL with the GET method
price_info <- GET("https://apis.smartenergy.at/market/v1/price")
# convert to cleaner format
data <- fromJSON(rawToChar(price_info$content))


# Assuming your data is stored in a list called 'data'
# Extracting relevant information from the list
date <- as.POSIXct(data$data$date, format = "%Y-%m-%dT%H:%M:%S")
value <- data$data$value

# Creating a data frame
df <- data.frame(date, value)
# Convert 'date' column to POSIXct class
df$date <- as.POSIXct(df$date)

# Calculate hourly moving average
hourly_ma <- rollmean(df$value, k = 4, fill = NA)

# Add hourly moving average to the data frame
df <- cbind(df, hourly_ma)

# Calculate the maximum observed value
max_observed_value <- max(df$value)

# Extracting the most appearing day
most_appearing_day <- as.Date(df$date[which.max(table(as.Date(df$date)))])
most_appearing_day <- format(most_appearing_day, "%Y-%m-%d")

# Calculate the lowerpercentile value
lower_percentile <- quantile(df$hourly_ma, choose_lower_percentil, na.rm = T)

# Calculate the upperpercentile value
upper_percentile <- quantile(df$hourly_ma, choose_upper_percentil, na.rm = T)



# Filter the dataframe to include only rows where hourly_ma is less than or equal to the lower 25th percentile
green_ribbon_df <- df[df$hourly_ma <= lower_percentile,]

# Filter out NA values
green_ribbon_df <- green_ribbon_df[!is.na(green_ribbon_df$hourly_ma),]


# Plotting using ggplot2
graph <- ggplot(df, aes(x = date)) +
  geom_bar(data = df[df$hourly_ma <= lower_percentile,], stat = 'identity', aes(y = max_observed_value + 1, fill = hourly_ma <= lower_percentile) , alpha=0.2) +
  geom_bar(data = df[df$hourly_ma <= lower_percentile,], stat = 'identity', aes(y = -2, fill = hourly_ma <= lower_percentile) , alpha=0.2) +
  geom_bar(data = df[df$hourly_ma >= upper_percentile,], stat = 'identity', aes(y = max_observed_value + 1, fill = hourly_ma <= lower_percentile) , alpha=0.2) +
  geom_bar(data = df[df$hourly_ma >= upper_percentile,], stat = 'identity', aes(y = -2, fill = hourly_ma <= lower_percentile) , alpha=0.2) +
  geom_line(aes(y = value, color = "Original"), size = 0.7) +
  geom_line(aes(y = hourly_ma, color = "Moving Average"), size = 1) +

  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1) +  # Adding bold line at 0
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H'", minor_breaks = "1 hour") +  # Setting x-axis labels every 2 hours
  scale_y_continuous(breaks = seq(-2, max_observed_value + 1, by = 2)) +  # Setting y-axis labels at every 2 units
  geom_hline(yintercept = lower_percentile, linetype = "solid", color = "green", size = 1) + # Horizontal green line at lower 25th percentile
  geom_hline(yintercept = upper_percentile, linetype = "solid", color = "red", size = 1) + # Horizontal red line at upper 75th percentile
  labs(x = paste("hour"), y = paste(data$unit),
       title = paste("Graph of ", data$tariff, most_appearing_day )) +  # Adding most appearing day to title
  scale_color_manual(values = c("Original" = "blue", "Moving Average" = "red")) +  # Specifying colors for the lines
  scale_fill_manual(name = "", values=c("red", "green"), labels=c("High cost","Low cost"))+
  theme_minimal() +
  theme(legend.position = "bottom",  # Positioning legend below the plot
        legend.title = element_blank(),  # Removing legend title
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 12),  # Increasing the size of axis text
        axis.title = element_text(size = 14))  # Increasing the size of axis labels

# Save the graph to PDF
ggsave(paste0("./daily_charts/",data$tariff,"_",most_appearing_day,".pdf"), graph, width = 11.7, height = 8.3) # A5 dimensions in inches (landscape orientation)
df_save <- df %>% mutate(unit=data$unit) %>% dplyr::select(-hourly_ma)
write.csv(df_save, paste0("./daily_price/",data$tariff,"_",most_appearing_day,".csv"), row.names = F)
