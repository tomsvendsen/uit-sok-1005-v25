# Load necessary libraries
library(dplyr) # For data manipulation
library(lubridate) # For date handling
library(ggplot2) # For plotting
library(tidyr) # For data reshaping

# Read the data from the provided URL
data <- read.csv("https://raw.githubusercontent.com/uit-sok-1005-v23/uit-sok-1005-v23.github.io/main/storedata.csv")

# Convert Order_Date to Date type
data$Order_Date <- as.Date(data$Order_Date, format = "%Y-%m-%d")

# Task 1.1: Table 1 - Total Sales by month for Region 1 and Region 9 in Customer_Segment Corporate and Consumer

# Filter data for last 3 months of 2017 and required regions and segments
data_filtered_1 <- data %>%
  filter(year(Order_Date) == 2017, month(Order_Date) %in% 10:12, Region %in% c("Region 1", "Region 9"), Customer_Segment %in% c("Corporate", "Consumer"))

# Group by month, Region, and Customer_Segment, and calculate total sales
table_1 <- data_filtered_1 %>%
  mutate(Month = month(Order_Date)) %>%
  group_by(Month, Region, Customer_Segment) %>%
  summarise(Total_Sales = sum(Sales), .groups = 'drop')

print(table_1)

# Task 1.2: Figure 1 - Plot of monthly total Sales in Region 1 and Region 13 for 2015, 2016, and 2017

# Filter data for relevant regions and years
data_filtered_2 <- data %>%
  filter(Region %in% c("Region 1", "Region 13"), year(Order_Date) %in% c(2015, 2016, 2017))

# Group by Year, Month, and Region, and calculate total sales
data_filtered_2 <- data_filtered_2 %>%
  mutate(Year = year(Order_Date), Month = month(Order_Date), YearMonth = ym(paste0(year(Order_Date), "-", month(Order_Date)))) %>%
  group_by(YearMonth, Region) %>%
  summarise(Total_Sales = sum(Sales), .groups = 'drop')

# Plot data
figure_1 <- ggplot(data_filtered_2, aes(x = YearMonth, y = Total_Sales, color = Region)) +
  geom_line() +
  labs(title = "Monthly Total Sales in Region 1 and Region 13 (2015-2017)", x = "Month", y = "Total Sales", color = "Region")

print(figure_1)

# Task 1.3: Table 2 - Identify months where Region 13 sales > Region 1

# Pivot data to wide format for comparison
data_wide <- data_filtered_2 %>%
  pivot_wider(names_from = Region, values_from = Total_Sales, values_fill = 0)

# Filter rows where Region 13 sales > Region 1
comparison_table_2 <- data_wide %>%
  filter(`Region 13` > `Region 1`) %>%
  select(YearMonth)

print(comparison_table_2)

# Task 1.4: Table 3 - Average Profit per Customer_Segment and Product_Category in 2017 (excluding Regions 3, 5, 8)

# Filter data for 2017 and exclude certain regions
data_filtered_3 <- data %>%
  filter(year(Order_Date) == 2017, !Region %in% c("Region 3", "Region 5", "Region 8"))

# Group by Customer_Segment and Product_Category, and calculate average profit
table_3 <- data_filtered_3 %>%
  group_by(Customer_Segment, Product_Category) %>%
  summarise(Average_Profit = mean(Profit), .groups = 'drop')

# Identify segment with the highest average profit
highest_profit_segment <- table_3 %>%
  filter(Average_Profit == max(Average_Profit))

print(table_3)
print(highest_profit_segment)

# Load Data from Website
url <- "https://www.motor.no/aktuelt/motors-store-vintertest-av-rekkevidde-pa-elbiler/217132"
webpage <- read_html(url) # Read the HTML content of the webpage
table <- html_table(html_nodes(webpage, "div table")[[1]]) # Extract the first table from the webpage

# Clean and Prepare Data
cleaned_data <- table %>%
  slice(-1) %>% # Remove the first row as it contains headers
  mutate(
    WLTP = as.numeric(str_extract(X2, "^\\d+")), # Extract the numeric part before '/' in column X2 (WLTP range)
    Consumption = as.numeric(str_replace(str_extract(X2, "(?<=/)[\\d,\\.]+"), ",", ".")), # Extract and replace comma with dot in numeric part after '/' in column X2 (consumption)
    STOP = as.numeric(str_extract(X3, "\\d+")) # Extract numeric values from column X3 (actual range/STOP)
  ) %>%
  filter(!is.na(WLTP) & !is.na(STOP) & !is.na(Consumption)) # Remove rows with missing values

# Task 2.a: Plot Data with 45-degree Line
plot1 <- ggplot(cleaned_data, aes(x = WLTP, y = STOP)) +
  geom_point(color = "blue") + # Plot data points
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + # Add 45-degree line (ideal line)
  labs(
    title = "WLTP vs. Actual Mileage (STOP) - Winter Test",
    x = "WLTP Range (km)",
    y = "Actual Range (STOP) (km)",
    caption = "Red dashed line represents ideal 1:1 relationship"
  ) 
print(plot1)

# Task 2.b_1: Linear Regression
model <- lm(STOP ~ WLTP, data = cleaned_data) # Fit linear regression model
summary(model) # Display model summary

# Extract slope and intercept
intercept <- coef(model)[1]
slope <- coef(model)[2]
# Q: After ???running??? the code, how do you interpret the two values on the customized line?
# A1: Intercept is the predicted value of STOP when WLTP is 0.
# A2: Slope is for every 1 km increase in WLTP the actual range increases by 0.87 km on average.

# Task 2.b_2: Plot Data with Regression Line and 45-degree Line
plot2 <- ggplot(cleaned_data, aes(x = WLTP, y = STOP)) +
  geom_point(color = "blue") + # Plot data points
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") + # Add 45-degree line
  geom_smooth(method = "lm", se = FALSE, color = "green") + # Add linear regression line
  labs(
    title = "WLTP vs. Actual Mileage (STOP) with Linear Regression",
    x = "WLTP Range (km)",
    y = "Actual Range (STOP) (km)",
    caption = paste(
      "Red dashed line: Expected (1:1), Green line: Linear regression (Intercept:",
      round(intercept, 2), ", Slope:", round(slope, 2), ")"
    )
  ) 
print(plot2)
