# Preprocessing
# Read the Coca_cola_historical_data from a CSV file into a data frame
coca_cola <- read.csv("C:/Users/Hp/Downloads/Coca_Cola_historical_data.csv", header = TRUE, sep = ",")

# Create a new data frame containing only the columns of interest
child_coca_cola <- data[,c("Date","Open","High","Low","Close","Volume")]

# Check the ratio of missing data in variables
na_ratio <- colMeans(is.na(child_coca_cola))
print(na_ratio)

# Function of removing outliers
removing_outliers <- function(df, col) {
  x <- df[[col]]
  quartiles  <- quantile(x, probs = c(0.25, 0.75))
  iqr <- IQR(x)
  
  Lower <- quartiles[1]- 1.5*iqr
  Upper <- quartiles[2] + 1.5*iqr
  
  df[x >= Lower & x <= Upper, , drop = FALSE]
}
# Removing outliers in each columns 
child_coca_cola <- removing_outliers(child_data, "Open")
child_coca_cola <- removing_outliers(child_data, "High")
child_coca_cola <- removing_outliers(child_data, "Low")
child_coca_cola <- removing_outliers(child_data, "Close")
child_coca_cola <- removing_outliers(child_data, "Volume")

# Removing the time in Date column
removing_time <- function(bien) {
  as.Date(substr(bien,1, 10 ))
}
child_coca_cola$Date <- as.Date(sapply(child_coca_cola$Date, removing_time))
