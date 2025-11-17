# 1. Thư viện 
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)
library(forecast)
library(lmtest)
library(tseries)
library(aTSA)
library(randomForest)
library(nortest)

# 2. Đọc và tiền xử lý dữ liệu
Coca_data <- read.csv("C:/Users/Hp/Downloads/Coca_Cola_historical_data.csv")
head(Coca_data, 10)

# Chuyển Date sang dạng Date
Coca_data$Date <- ymd_hms(Coca_data$Date) %>% as.Date()

# Chọn các cột cần thiết và sắp xếp theo ngày
data_clean <- Coca_data %>%
  select(Date, Open, High, Low, Close, Volume) %>%
  arrange(Date)

# Kiểm tra dữ liệu khuyết
na_ratio <- colMeans(is.na(data_clean))
print(na_ratio)

# 3. Kiểm tra ngoại lai cho Close
Q1 <- quantile(data_clean$Close, 0.25, na.rm = TRUE)
Q3 <- quantile(data_clean$Close, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower <- Q1 - 1.5 * IQR
upper <- Q3 + 1.5 * IQR

data_clean2 <- data_clean
data_clean2$outlier_flag <- ifelse(data_clean2$Close < lower | data_clean2$Close > upper, 1, 0)
num_outliers <- sum(data_clean2$outlier_flag, na.rm = TRUE)
total_obs <- nrow(data_clean2)
pct_outliers <- num_outliers / total_obs * 100
cat("Số ngoại lai:", num_outliers, " / Tổng:", total_obs, "=>", round(pct_outliers,2), "%\n")

# 4. Thống kê mô tả
# Thống kê cơ bản
summary(data_clean)

# Vẽ histogram cho các biến quan trọng
ggplot(data_clean, aes(x=Close)) +
  geom_histogram(bins=30, fill="steelblue", alpha=0.7) +
  theme_minimal() +
  labs(title="Phân bố giá Close", x="Close", y="Số lượng")

ggplot(data_clean2, aes(x=Volume)) +
  geom_histogram(bins=30, fill="orange", alpha=0.7) +
  theme_minimal() +
  labs(title="Phân bố Volume", x="Volume", y="Số lượng") +

# Boxplot để kiểm tra ngoại lai
boxplot(data_clean2$Close, main="Boxplot giá Close")
boxplot(data_clean2$Volume, main="Boxplot Volume")

# Lập ma trận tương quan
eda_df <- data_clean
numeric_df <- eda_df[, sapply(eda_df, is.numeric)]
  corr_matrix <- cor(numeric_df, use = "complete.obs")
  melted_corr_matrix <- melt(corr_matrix)
  plt <- ggplot(data = melted_corr_matrix, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    geom_text(aes(label = sprintf("%.2f", value)), color = "white", size = 4) +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1, 1), name = "Correlation") +
    theme_minimal() + 
    labs(title = "Ma trận tương quan của các cột dữ liệu") +
  print(plt)
