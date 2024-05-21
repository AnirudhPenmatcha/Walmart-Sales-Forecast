library(readr)
library(dplyr)

df_store <- read_csv("C:/Users/aathi/Downloads/dataset/features/stores.csv")
df_features = read_csv("C:/Users/aathi/Downloads/dataset/features/features.csv") #external information
df_train = read_csv("C:/Users/aathi/Downloads/dataset/features/train.csv") #external information

# Display the first three rows of the data frame
head(df_store, 3)

head(df_features, 3)


df <- inner_join(inner_join(df_train, df_features, by = c("Store", "Date")), df_store, by = "Store")

# Print the first 5 rows of the resulting data frame
head(df, 5)
# removing duplicate column
df$IsHoliday_y <- NULL

names(df)[names(df) == "IsHoliday.x"] <- "IsHoliday"

head(df, 5)

# check for non-zero and zero values for weekly sales
filtered_df <- subset(df, Weekly_Sales <= 0)
filtered_df

# total rows in dataframe = 421570
# total rows with missing or zero or negative values = 1348
# perventage of rows with missing values= 1348/ 421570= 0.31%

# removing them 
df <- subset(df, Weekly_Sales > 0)
df

# Assuming your data frame is named 'df'
colnames(df)[colnames(df) == "IsHoliday_x"] <- "IsHoliday"

library(ggplot2)

# Create a bar plot
ggplot(df, aes(x = IsHoliday, y = Weekly_Sales, fill = IsHoliday)) +
  geom_bar(stat = "identity") +
  labs(x = "Holiday", y = "Weekly Sales") +
  ggtitle("Holiday vs. Weekly Sales")

# Assuming your data frame is named 'df'
# Assuming 'Date' is in Date format, if not, convert it using as.Date(df$Date, format = "your_date_format")

# Create a subset for rows where 'IsHoliday' is true
df_holiday <- subset(df, IsHoliday == TRUE)

# Get unique dates from the subset
unique_dates <- unique(df_holiday$Date)

# Print unique dates
print(unique_dates)

#create different rows in the dataset for the unique holidays
df$Super_Bowl <- ifelse(df$Date %in% as.Date(c('2010-02-12', '2011-02-11', '2012-02-10')), TRUE, FALSE)
df$Thanksgiving <- ifelse(df$Date %in% as.Date(c('2010-11-26', '2011-11-25')), TRUE, FALSE)
df$Labor_Day <- ifelse(df$Date %in% as.Date(c('2010-09-10', '2011-09-09', '2012-09-07')), TRUE, FALSE)
df$Christmas <- ifelse(df$Date %in% as.Date(c('2010-12-31', '2011-12-30')), TRUE, FALSE)

#THANKSGIVING AVERAGE SALES VS AVERAGE WEEKLY SALES
# Calculate the average weekly sales for both TRUE and FALSE values
average_sales <- tapply(df$Weekly_Sales, df$Thanksgiving, mean)

# Create a data frame for plotting
plot_data <- data.frame(
  Thanksgiving = factor(names(average_sales)),
  Average_Weekly_Sales = average_sales
)

# Create a bar plot
ggplot(plot_data, aes(x = Thanksgiving, y = Average_Weekly_Sales, fill = Thanksgiving)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Thanksgiving", y = "Average Weekly Sales") +
  scale_fill_manual(values = c("FALSE" = "blue", "TRUE" = "red"), name = "Thanksgiving") +
  ggtitle("Thanksgiving vs. Average Weekly Sales")

#LABOUR DAY AVERAGE SALES VS AVERAGE WEEKLY SALES

# Calculate the average weekly sales for both TRUE and FALSE values for Labor Day
average_sales_labor_day <- tapply(df$Weekly_Sales, df$Labor_Day, mean)

# Create a data frame for plotting for Labor Day
plot_data_labor_day <- data.frame(
  Holiday = factor(names(average_sales_labor_day)),
  Average_Weekly_Sales = average_sales_labor_day
)

# Create a bar plot for Labor Day
ggplot(plot_data_labor_day, aes(x = Holiday, y = Average_Weekly_Sales, fill = Holiday)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Labor Day", y = "Average Weekly Sales") +
  scale_fill_manual(values = c("FALSE" = "blue", "TRUE" = "red"), name = "Labor Day") +
  ggtitle("Labor Day vs. Average Weekly Sales")

# Calculate the average weekly sales for both TRUE and FALSE values for Christmas
average_sales_christmas <- tapply(df$Weekly_Sales, df$Christmas, mean)

# Create a data frame for plotting for Christmas
plot_data_christmas <- data.frame(
  Holiday = factor(names(average_sales_christmas)),
  Average_Weekly_Sales = average_sales_christmas
)

# Create a bar plot for Christmas
ggplot(plot_data_christmas, aes(x = Holiday, y = Average_Weekly_Sales, fill = Holiday)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Christmas", y = "Average Weekly Sales") +
  scale_fill_manual(values = c("FALSE" = "blue", "TRUE" = "red"), name = "Christmas") +
  ggtitle("Christmas vs. Average Weekly Sales")


# install.packages("scales")
library(scales)

# Calculate the average weekly sales by store type
average_sales_by_store <- aggregate(Weekly_Sales ~ Type, data = df, FUN = mean)

# Store types
store_types <- average_sales_by_store$Type

# Average weekly sales data
average_sales_data <- average_sales_by_store$Weekly_Sales

# Plotting a pie chart with percentage labels
pie(average_sales_data, labels = paste0(store_types, "\n", percent(average_sales_data / sum(average_sales_data))),
    col = c("blue", "orange", "green"), cex = 1.2, border = "white",
    main = "Average Weekly Sales by Store Type", cex.lab = 1.5)

# Create a boxplot
ggplot(df_store, aes(x = Type, y = Size)) +
  geom_boxplot() +
  labs(title = "Boxplot of Size by Type", x = "Type", y = "Size")

# Calculate average weekly sales for each combination of 'month' and 'year'
df_avg <- df %>%
  group_by(month, year) %>%
  summarise(Avg_Weekly_Sales = mean(Weekly_Sales, na.rm = TRUE))

# Create a scatter plot for 'month' and 'year' against average weekly sales
ggplot(df_avg, aes(x = month, y = Avg_Weekly_Sales, color = factor(year))) +
  geom_line() +
  labs(title = "Average Weekly Sales by Month and Year", x = "Month", y = "Average Weekly Sales") +
  scale_color_manual(values = c("blue", "red", "green")) +  # Set your desired color for each year
  theme_minimal()

# df_filtered <- df %>%
#   filter(Weekly_Sales <= 400000)
# df_filtered
# ggplot(df_filtered, aes(x = CPI, y = Weekly_Sales)) +
#   geom_line() +
#   labs(title = "Weekly Sales Against Fuel Prices", x = "Fuel Price", y = "Weekly Sales") +
#   scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with commas
#   theme_minimal()
# 
# ggplot(df_filtered, aes(x = CPI, y = Weekly_Sales)) +
#   geom_line() +
#   labs(title = "Weekly Sales Against Fuel Prices", x = "Fuel Price", y = "Weekly Sales") +
#   scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with commas
#   theme_minimal()
