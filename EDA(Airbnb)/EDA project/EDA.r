#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("skimr")
#install.packages("mice")
#install.packages("randomForest")
#install.packages("corrplot")
#install.packages("ggcorrplot")
#install.packages("ggplot2")
#library(ggplot2)
#library(ggthemes)

library(tidyverse)
library(dplyr)
library(skimr)
library(mice)
library(randomForest)
library(corrplot)
library(ggcorrplot)
library(VIM)
library(dplyr)
library(ggplot2)
library(viridis)

path_loc <- "C:\\DISK 1\\semster 3\\EDA\\EDA"
setwd(path_loc)



#loading my Dataset
airbnb_data <- read.csv("data/NYC-Airbnb-2023.csv")
View(airbnb_data)

#head of my dataset
head(airbnb_data)

#copy my dataset into the another variable 
nyc_list <- airbnb_data
View(nyc_list)
class(nyc_list)
#Summary of the dataset
summary(nyc_list)
head(nyc_list)

#no of rows and column of my dataset
cat("Number of rows:", nrow(nyc_list), "\nNumber of columns:", ncol(nyc_list))
#column names of dataframe
column_names <- colnames(nyc_list)
column_names
View(nyc_list)

#Replace missing values with NA
nyc_list[nyc_list==""] <- NA
skim_without_charts(nyc_list)
# Assuming your dataset is named nyc_df
null_values_per_column <- colSums(is.na(nyc_list))

# Display the number of null values per column
print(null_values_per_column)

#strucutre of
str(nyc_list)

# Calculate the percentage of missing values in each column
missing_percentage <- colMeans(is.na(nyc_list)) * 100
# Print the missing percentage for each column
print(missing_percentage)

# Removing rows with missing values in listing_name or host_name
nyc_list <- nyc_list[complete.cases(nyc_list$name, nyc_list$host_name), ]

head(nyc_list)
# Data Wrangling
clean_df <- nyc_list %>% 
  rename(list_id = id,
         listing_name = name,
         area = neighbourhood_group,
         geo_location = neighbourhood,
         host_list_count = calculated_host_listings_count,
         reviews_per_year = number_of_reviews_ltm,
         reviews_per_month_pct = reviews_per_month,
         last_date_review=last_review) %>% 
  select(-license, -reviews_per_month_pct,-last_date_review)


head(clean_df)

# Assuming your dataset is named nyc_df
null_values_per_column <- colSums(is.na(clean_df))

# Display the number of null values per column
print(null_values_per_column)

#Attributes of my dataset
column_names <- names(clean_df)
print(column_names)

#seeing my final clean data
skim_without_charts(clean_df)
view(clean_df)


# Count missing values for each variable
missing_values <- colSums(is.na(clean_df))

# Print the result
print(missing_values)

# Check for missing values in the entire dataset
any_missing <- any(is.na(clean_df))

# Print the result
if (any_missing) {
  print("There are missing values in the dataset.")
} else {
  print("There are no missing values in the dataset.")
}






#Visualization  

# Mean Price Distribution by Area Groups and Room Types


#visualization each column
column_names <- colnames(clean_df)
# Display the column names
print(column_names)


#Average price of each Room_type
av_price_room <- clean_df %>% 
  group_by(room_type) %>% 
  summarise(price = mean(price))
# Rename the columns
column_names <- c("Room_Type", "Price")
df_av_room <- data.frame(av_price_room) %>% 
  setNames(column_names)
# Print the resulting data frame
print(df_av_room)

#Average price of Area group
av_price_neighb <- clean_df %>% 
  group_by(area) %>% 
  summarise(price = mean(price))

# Rename the columns
column_names <- c("Area", "Price")
df_av_neighb <- data.frame(av_price_neighb) %>% 
  setNames(column_names)

# Print the resulting data frame
print(df_av_neighb)



# visualization in bar plot


# Set the size for each figure
width <- 12
height <- 6
# Increase font size
font_size <- 12

# Plotting Average Price by Room Type
plot1 <- ggplot(df_av_room, aes(x = Room_Type, y = Price, fill = Room_Type)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Average Price by Room Type", x = "Room Type", y = "Average Price") +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(size = font_size))

# Save the first plot with the specified size
ggsave("plot1.png", plot1, width = width, height = height)

# Plotting Average Price by Neighbourhood Group
plot2 <- ggplot(df_av_neighb, aes(x = Area, y = Price, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Average Price by Area Group", x = "Area Group", y = "Average Price") +
  theme_minimal() +
  theme(text = element_text(size = font_size))

# Save the second plot with the specified size
ggsave("plot2.png", plot2, width = width, height = height)
print(plot1)
print(plot2)



#Making box plot by using the price Distribution by Room Type
ggplot(clean_df, aes(x = room_type, y = price)) +
  geom_boxplot(aes(fill = room_type)) +
  scale_fill_manual(values = c("Entire home/apt" = "blue", 
                               "Hotel room" = "red", 
                               "Private room" = "green", 
                               "Shared room" = "purple")) +
  scale_y_log10(limits = c(1, 10000), labels = scales::comma) +
  geom_hline(yintercept = mean(clean_df$price), color = "purple", linetype = 6) +
  annotate("text", x = 1,
           y = median(clean_df$price[clean_df$room_type == "Entire home/apt"]), 
           label = round(median(clean_df$price[clean_df$room_type == "Entire home/apt"]), 2), 
           size = 5, color = "white") +
  annotate("text", x = 2, 
           y = median(clean_df$price[clean_df$room_type == "Hotel room"]), 
           label = round(median(clean_df$price[clean_df$room_type == "Hotel room"]), 2), 
           size = 5, color = "black") +
  annotate("text", x = 3, 
           y = median(clean_df$price[clean_df$room_type == "Private room"]), 
           label = round(median(clean_df$price[clean_df$room_type == "Private room"]), 2), 
           size = 5, color = "white") +
  annotate("text", x = 4, 
           y = median(clean_df$price[clean_df$room_type == "Shared room"]), 
           label = round(median(clean_df$price[clean_df$room_type == "Shared room"]), 2), 
           size = 5, color = "green") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Room Type",
       y = "Price",
       title = "Price Distribution by Room Type :",
       caption = "Data Analyst : JP")




#Geographical Distribution of Listings by Neighbourhood Group

# Extract location information
location <- clean_df[, c("latitude", "longitude", "area")]

# Scatter plot of latitude and longitude colored by neighborhood_group
ggplot(location, aes(x = longitude, y = latitude, color = area)) +
  geom_point(alpha = 0.6, size = 2) + 
  labs(x = 'Longitude', y = 'Latitude', 
       title = 'Geographical Distribution of Listings by Neighbourhood Group',
       subtitle = 'New York Airbnb 2023 Dataset') +  
  scale_color_discrete(name = 'Neighbourhood Group') +
  theme_minimal() +  
  theme(legend.position = 'right',  
        plot.title = element_text(size = 16, face = 'bold'),
        plot.subtitle = element_text(size = 12, color = 'gray'),  
        axis.text = element_text(size = 9), 
        legend.text = element_text(size = 9), 
        legend.title = element_text(size = 10, face = 'bold'))  


#Room Type availability

room_365 <- aggregate(availability_365 ~ room_type, data = clean_df, sum)

# Rename columns
colnames(room_365) <- c("room_type", "availability")

# Print or use the resulting data frame as needed
room_365

# Rename columns
colnames(room_365) <- c("room_type", "availability")

# Define a custom color palette
custom_palette <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00")  

# Create a bar plot using ggplot2 with a custom color palette
bar_room_365 <- ggplot(room_365, aes(x = room_type, y = availability, fill = room_type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_palette) +
  labs(title = "Availability by Room Type",
       x = "Room Type",
       y = "Availability") +
  theme_minimal()

# Display the plot
print(bar_room_365)



# Top Five Listings by Host names

host_list <- clean_df %>%
  group_by(host_name) %>%
  summarise(hostings = sum(host_list_count)) %>%
  arrange(desc(hostings)) %>%
  slice_head(n = 5)

# Display the result
print(host_list)


# Rename columns
colnames(host_list) <- c("name", "hostings")

# Create a bar plot using ggplot2 with advanced customization
bar_host_list <- ggplot(host_list, aes(x = reorder(name, -hostings), y = hostings, fill = name)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Top 5 Hosts by Total Listings",
       x = "Host Name",
       y = "Total Listings",
       caption = "Data source: Airbnb 2023 (kaggle)") +
  theme_minimal() +
  geom_text(aes(label = hostings), vjust = -0.5, size = 4, color = "black") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "none") 

# Display the plot
print(bar_host_list)





# Average number of reviews by area group
nbg_nights1 <- aggregate(number_of_reviews ~ area, data = clean_df, FUN = mean)
nbg_nights1

library(ggplot2)

# Create a bar plot
ggplot(nbg_nights1, aes(x = area, y = number_of_reviews, fill = area)) +
  geom_bar(stat = 'identity', position = 'dodge', color = 'white', width = 0.7) +
  scale_fill_manual(values = c('blue', 'green', 'red', 'cyan', 'black')) +
  labs(y = 'Average number of reviews', title = 'Average number of reviews by Area group') +
  geom_text(aes(label = sprintf('%.2f', number_of_reviews)), vjust = -0.5, size = 3) +
  theme_minimal()



#Average Minimum number of nights based on Area Group

nbg_group1 <- clean_df %>%
  group_by(area) %>%
  summarize(mean_minimum_nights = mean(minimum_nights))

# Print the result
print(nbg_group1)

bar_colors <- c('blue', 'green', 'red', 'cyan', 'black')

# Create a bar plot
ggplot(nbg_group1, aes(x = area, y = mean_minimum_nights, fill = area)) +
  geom_bar(stat = 'identity', position = 'dodge', color = 'white', width = 0.7) +
  scale_fill_manual(values = bar_colors) +
  labs(y = 'Average Minimum number of nights', title = 'Average Minimum number of nights based on Area Group') +
  geom_text(aes(label = sprintf('%.2f', mean_minimum_nights)), vjust = -0.5, size = 3) +
  theme_minimal()


#Average Minimum number of nights by room type

room_group1 <- clean_df %>%
  group_by(room_type) %>%
  summarize(mean_minimum_nights = mean(minimum_nights))

# Print the result
print(room_group1)

bar_colors <- c('blue', 'green', 'red', 'cyan', 'black')

# Create a bar plot
ggplot(room_group1, aes(x = room_type, y = mean_minimum_nights, fill = room_type)) +
  geom_bar(stat = 'identity', position = 'dodge', color = 'white', width = 0.7) +
  scale_fill_manual(values = bar_colors) +
  labs(y = 'Minimum number of nights', title = 'Average Minimum number of nights by room type') +
  geom_text(aes(label = sprintf('%.2f', mean_minimum_nights)), vjust = -0.5, size = 3) +
  theme_minimal()


