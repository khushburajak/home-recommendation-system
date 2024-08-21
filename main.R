# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lubridate)

# URLs for the datasets
url_2020 <- "http://prod2.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-2020.txt"
url_2021 <- "http://prod2.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-2021.txt"
url_2022 <- "http://prod2.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-2022.txt"
url_2023 <- "http://prod2.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-2023.txt"

# Read the data
data_2020 <- read.csv(url_2020, header = FALSE)
data_2021 <- read.csv(url_2021, header = FALSE)
data_2022 <- read.csv(url_2022, header = FALSE)
data_2023 <- read.csv(url_2023, header = FALSE)

# Combine the data
combined_data <- rbind(data_2020, data_2021, data_2022, data_2023)

# Assign column names based on the structure
colnames(combined_data) <- c("TransactionID", "Price", "Date", "Postcode", "PropertyType", 
                             "NewBuild", "Tenure", "PrimaryAddressableObjectName", 
                             "SecondaryAddressableObjectName", "Street", "Locality", 
                             "Town", "District", "County", "PPDCategoryType", "RecordStatus")

# Convert data types
combined_data$Date <- as.Date(combined_data$Date)
combined_data$Price <- as.numeric(combined_data$Price)

# Summary statistics
summary(combined_data)

# Histogram of Prices
ggplot(combined_data, aes(x = Price)) +
  geom_histogram(binwidth = 50000, fill = "blue", color = "black") +
  labs(title = "Distribution of Property Prices", x = "Price", y = "Frequency")

# Average Price by Property Type
avg_price_by_type <- combined_data %>%
  group_by(PropertyType) %>%
  summarize(AveragePrice = mean(Price, na.rm = TRUE))

ggplot(avg_price_by_type, aes(x = PropertyType, y = AveragePrice)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  labs(title = "Average Price by Property Type", x = "Property Type", y = "Average Price")

# Trend of Prices Over Time
ggplot(combined_data, aes(x = Date, y = Price)) +
  geom_line(color = "red") +
  labs(title = "Trend of Property Prices Over Time", x = "Date", y = "Price")

# Average Price by County
avg_price_by_county <- combined_data %>%
  group_by(County) %>%
  summarize(AveragePrice = mean(Price, na.rm = TRUE))

ggplot(avg_price_by_county, aes(x = reorder(County, -AveragePrice), y = AveragePrice)) +
  geom_bar(stat = "identity", fill = "purple", color = "black") +
  coord_flip() +
  labs(title = "Average Price by County", x = "County", y = "Average Price")

# Simple linear regression example
model <- lm(Price ~ PropertyType + County, data = combined_data)

# Summary of the model
summary(model)

# Predicting property prices
predictions <- predict(model, newdata = combined_data)

# Plotting actual vs predicted prices
ggplot(combined_data, aes(x = Price, y = predictions)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Actual vs Predicted Property Prices", x = "Actual Price", y = "Predicted Price")





# Average Price by County
avg_price_by_county <- combined_data %>%
  group_by(County) %>%
  summarize(AveragePrice = mean(Price, na.rm = TRUE))

# Creating a graph structure
library(igraph)

# Convert data to igraph format
graph_data <- graph_from_data_frame(as_data_frame(avg_price_by_county), directed = FALSE)

# Visualize the graph (network) of counties
plot(graph_data, vertex.label = V(graph_data)$County, vertex.size = 10, edge.arrow.size = 0.5,
     layout = layout_with_fr(graph_data), main = "Network of Counties based on Property Prices")

