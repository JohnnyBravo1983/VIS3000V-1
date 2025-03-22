# Read the dataset
dataset <- read.csv("D:/R/VIS3000V-1/combined_clean.csv")

# Testing if the data shows up and receiving some information about it
test_data <- function() {
head(dataset)  
tail(dataset)  
summary(dataset)
str(dataset)
}

test_data()


# Convert sales_date and modify_date to date format
dataset$sales_date <- as.POSIXct(dataset$sales_date, format = "%Y-%m-%dT%H:%M:%SZ")
dataset$modify_date <- as.POSIXct(dataset$modify_date, format = "%Y-%m-%dT%H:%M:%SZ")

avg_sale <- function() {
  # Total sale and average tatal price
total_sales <- sum(dataset$total_price)
avg_total_price <- mean(dataset$total_price)

#https://stackoverflow.com/questions/15589601/print-string-and-variable-contents-on-the-same-line-in-r
cat("Total sales: \n", total_sales)
cat("Average: ", avg_total_price)
}

avg_sale()




# Transaction amount per month function
#https://www.w3schools.com/r/tryr.asp?filename=demo_function
transaction_per_month <- function() {   
  monthly_sales <- dataset %>%
    mutate(month = format(as.Date(sales_date, format="%Y-%m-%dT%H:%M:%SZ"), "%Y-%m")) %>%
    group_by(month) %>%
    summarise(
      total_transactions = n(),
      total_sales_month = sum(total_price),
      avg_price_month = mean(total_price)
    ) %>%
    arrange(month)

  # Show the results
  print(monthly_sales)
}

# Calling the function
transaction_per_month()



# Check if there were any sales in june 2018
checkIf_june_sale <- function() {
  library(dplyr)

june_sales <- dataset %>%
  filter(format(as.Date(sales_date), "%Y-%m") == "2018-06")

#Shsow results
cat("June sales:\n")
print(june_sales)
}

checkIf_june_sale()



library(ggplot2)

# Refined sales distribution analysis with better x-axis labels
sales_distribution_analysis <- function() {
  # Calculate key statistics
  min_price <- min(dataset$total_price, na.rm = TRUE)
  first_quartile <- quantile(dataset$total_price, 0.25, na.rm = TRUE)
  median_price <- median(dataset$total_price, na.rm = TRUE)
  mean_price <- mean(dataset$total_price, na.rm = TRUE)
  third_quartile <- quantile(dataset$total_price, 0.75, na.rm = TRUE)
  max_price <- max(dataset$total_price, na.rm = TRUE)
  
  # Print the statistics to the console
  cat("Sales Statistics:\n")
  cat("Minimum Price: ", min_price, "\n")
  cat("1st Quartile: ", first_quartile, "\n")
  cat("Median Price: ", median_price, "\n")
  cat("Mean Price: ", mean_price, "\n")
  cat("3rd Quartile: ", third_quartile, "\n")
  cat("Maximum Price: ", max_price, "\n\n")
  
  # Create a histogram to visualize the sales distribution
  sales_distribution_plot <- ggplot(dataset, aes(x = total_price)) + 
    geom_histogram(binwidth = 250, fill = "skyblue", color = "black", alpha = 0.7) +
    labs(title = "Sales Distribution", 
         x = "Total Price", 
         y = "Frequency") +
    scale_x_continuous(breaks = seq(0, max(dataset$total_price), by = 250)) + # Adjust x-axis labels
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), # Title styling
      axis.title = element_text(size = 12), # Axis title styling
      axis.text = element_text(size = 10),  # Axis label styling
      panel.grid.major = element_line(color = "grey90"), # Grid styling
      panel.grid.minor = element_line(color = "grey95")
    )
  
  # Print the plot
  print(sales_distribution_plot)
}

# Run the sales distribution analysis
sales_distribution_analysis()






# Find the transaction with the minimum price
min_sale <- dataset %>% filter(total_price == min(dataset$total_price))
print(min_sale)

# Remove duplicates based on product name and sort by total_price
cheapest_products <- dataset %>%
  distinct(product_name, .keep_all = TRUE) %>%  # Keep only the first occurrence of each product
  arrange(total_price) %>%
  head(5)  # Get the first 5 cheapest products

# Print the result
print(cheapest_products)




# Function to find products with the highest and lowest sales
find_product_sales <- function() {
  # Group by product and calculate total sales and number of transactions
  product_sales <- dataset %>%
    group_by(product_name) %>%
    summarise(
      total_sales = sum(total_price),
      transactions = n()
    ) %>%
    arrange(desc(total_sales))

  # Products with the highest sales
  top_products <- head(product_sales, 5)
  cat("Top 5 products with highest sales:\n")
  print(top_products)

  # Products with the lowest sales
  bottom_products <- tail(product_sales, 5)
  cat("Top 5 products with lowest sales:\n")
  print(bottom_products)
}

# Run the function
find_product_sales()


# Function to analyze sales per category using category_id but displaying category_name
sales_per_category <- function() {
  
  # Group by category_id but include category_name for readability
  category_sales <- dataset %>%
    group_by(category_id, category_name) %>%
    summarise(
      total_sales = sum(total_price),     # Total sales
      total_transactions = n(),           # Number of transactions
      avg_price = mean(total_price)       # Average price per category
    ) %>%
    arrange(desc(total_sales))           # Sort by total sales
  
  # Print the results
  print(category_sales)
  
  return(category_sales)
}

# Call the function to analyze sales per category
category_sales_data <- sales_per_category()


library(scales)  # Importer scales-pakken for å formatere tall

# Function to create a bar plot of sales per category
plot_sales_per_category <- function(category_sales_data) {
  
  # Bar chart for total sales per category
  ggplot(category_sales_data, aes(x = reorder(category_name, total_sales), y = total_sales)) + 
    geom_bar(stat = "identity", fill = "skyblue", color = "black") +
    coord_flip() +  # Swap x and y axes for better readability
    labs(title = "Total Sales per Category",
         x = "Category",
         y = "Total Sales") +
    scale_y_continuous(labels = scales::comma) +  # Format y-axis to show full numbers
    theme_minimal()
}

# Plot sales per category
plot_sales_per_category(category_sales_data)








#View the dataset table
View(dataset)

#List of columns
colnames(dataset)











