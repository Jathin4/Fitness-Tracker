# Install necessary packages (if not already installed)
#if(!require(dplyr)) install.packages("dplyr", dependencies=TRUE)
#if(!require(ggplot2)) install.packages("ggplot2", dependencies=TRUE)
#if(!require(lubridate)) install.packages("lubridate", dependencies=TRUE)

# Load libraries
library(dplyr)
library(ggplot2)
library(lubridate)

# Define the path to your data


#data_path <- 'C:/Users/chari/Downloads'
# Load, clean, and preprocess the data
fitness_data <- read.csv('fitness_data.csv') %>%
  mutate(Date = as_date(Date, format = "%Y-%m-%d"),
         Weekday = weekdays(Date),
         Month = month(Date, label = TRUE))

# Perform exploratory data analysis (EDA) and visualizations

# Summary statistics
print(summary(fitness_data))

# Distribution of steps
ggplot(fitness_data, aes(x = Steps)) +
  geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
  labs(title = "Distribution of Steps", x = "Steps", y = "Frequency") +
  theme_minimal()

# Average steps per day
avg_steps_per_day <- fitness_data %>%
  group_by(Date) %>%
  summarise(Average_Steps = mean(Steps, na.rm = TRUE))

ggplot(avg_steps_per_day, aes(x = Date, y = Average_Steps)) +
  geom_line(color = "blue") +
  labs(title = "Average Steps per Day", x = "Date", y = "Average Steps") +
  theme_minimal()

# Steps per weekday
steps_per_weekday <- fitness_data %>%
  group_by(Weekday) %>%
  summarise(Average_Steps = mean(Steps, na.rm = TRUE))

ggplot(steps_per_weekday, aes(x = Weekday, y = Average_Steps)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Average Steps per Weekday", x = "Weekday", y = "Average Steps") +
  theme_minimal()

# Steps per month
steps_per_month <- fitness_data %>%
  group_by(Month) %>%
  summarise(Average_Steps = mean(Steps, na.rm = TRUE))

ggplot(steps_per_month, aes(x = Month, y = Average_Steps)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Average Steps per Month", x = "Month", y = "Average Steps") +
  theme_minimal()

# Linear regression model
model <- lm(Steps ~ Weekday + Month, data = fitness_data)
print(summary(model))

# Visualize the regression model
ggplot(fitness_data, aes(x = Weekday, y = Steps)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Steps vs Weekday with Regression Line", x = "Weekday", y = "Steps") +
  theme_minimal()
