#Importing my excel dataset
library(readxl)
dataset <- read_excel("C:\\Users\\Fonis\\Downloads\\dataset.xlsx")
developing <-  read_excel("C:\\Users\\Fonis\\Downloads\\developing.xlsx")
developed <-  read_excel("C:\\Users\\Fonis\\Downloads\\developed.xlsx")
#Inspect dataset
names(dataset)
head(dataset)
tail(dataset)
str(dataset)
summary(dataset)
summary(developing)
summary(developed)
#convert year to character variable
# List of dataset names
dataset_names <- c("dataset", "developing", "developed")

# Function to convert "Year" column to character
convert_year_to_character <- function(data) {
  data$Year <- as.character(data$Year)
  return(data)
}

# Apply the function to each dataset
for (name in dataset_names) {
  assign(name, convert_year_to_character(get(name)))
}
summary(dataset)
#Data preprocessing (loading all necessary libraries)
library(datarium)
library(tidyverse)
library(corrplot)
library(rcompanion)
#check for missing data
any(is.na(dataset))
#rename variables for easy coding
# List of dataset names
dataset_names <- c("dataset", "developing", "developed")
# Variable renaming code
rename_variables <- function(data) {
  names(data) <- c("Year", "Year_Code", "Country_Name", "Country_Code", 
                   "GDP", "RDE", "EMP", "UET", "INT", "EXP", "IMP", 
                   "EIT", "TEA", "FFE", "GSP", "CPI", "PSI")
  return(data)
}

# Apply renaming code to each dataset
for (name in dataset_names) {
  assign(name, rename_variables(get(name)))
}
#replace missing data using mean
dataset_names <- c("dataset", "developing", "developed")
# List of columns with missing values
columns_with_missing <- c("RDE", "INT", "FFE", "GSP", "CPI", "PSI")
# Function to replace missing values with mean for specified columns
replace_missing_with_mean <- function(data, columns) {
  for (col in columns) {
    missing_values <- is.na(data[[col]])
    if (any(missing_values)) {
      mean_value <- mean(data[[col]], na.rm = TRUE)
      data[[col]][missing_values] <- mean_value
    }
  }
  return(data)
}

install.packages("writexl")
library(writexl)
excel <- "C:\\Users\\Fonis\\Documents\\asdv assessment\\dataset.xlsx"
write_xlsx(dataset, path = excel)
# Apply the function to each dataset
for (name in dataset_names) {
  assign(name, replace_missing_with_mean(get(name), columns_with_missing))
}
any(is.na(dataset))
summary(dataset)
#Carrying out comprehensive descriptive statistical analysis
numericd <- sapply(dataset, is.numeric)
numeric_data <-dataset[, numericd]
library(ggplot2)
library(tidyr)
library(dplyr)
#calculating mean, median and mode
colMeans(numeric_data, na.rm = TRUE)
apply(numeric_data, 2, median, na.rm = TRUE)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
apply(numeric_data, 2, Mode)
# Standard Deviation
apply(numeric_data, 2, sd, na.rm = TRUE)
apply(developed_numeric, 2, sd, na.rm = TRUE)
apply(developing_numeric, 2, sd, na.rm = TRUE)
# Skewness and Kurtosis
library(e1071)
library(moments)
skewness_values <- apply(numeric_data, 2, skewness, na.rm = TRUE)
wskewness_values <- apply(developing_numeric, 2, skewness, na.rm = TRUE)
iskewness_values <- apply(developed_numeric, 2, skewness, na.rm = TRUE)
print(iskewness_values)
print(wskewness_values)
library(ggplot2)
plot(density(developed_numeric$GDP))
plot(density(developing_numeric$GDP))
kurtosis_values <- apply(numeric_data, 2, kurtosis, na.rm = TRUE)
kurtosis1 <- apply(developed_numeric,2, kurtosis, na.rm = TRUE)
print(kurtosis1)
kurtosis2 <- apply(developing_numeric,2,kurtosis, na.rm = TRUE)
print(kurtosis2)
print(skewness_values)
print(kurtosis_values)
#check normality of data
library(datarium)
library(ggplot2)
library(qqplotr)
library(stats)
#Shapiro_Wilk Test
shapiro_test_results <- apply(numeric_data, 2, shapiro.test)
print("Shapiro-Wilk Test Results:")
print(shapiro_test_results)
#carry out Q-Qplots for the variables
#carry out Q-Qplots for the variables
shapiro_test_results <- lapply(numeric_data, shapiro.test)
par(mfrow = c(4, 4))  # Set up a 4x4 grid for the plots

for (variable_name in names(shapiro_test_results)) {
  qqplot_title <- paste("Q-Q Plot for ", variable_name)
  
  # Create a new PNG file for each Q-Q plot
  qqplot_filename <- paste("qqplot_", variable_name, ".png", sep = "")
  png(filename = qqplot_filename)
  
  # Create Q-Q plot
  qqnorm(numeric_data[[variable_name]], main = qqplot_title)
  qqline(numeric_data[[variable_name]], col = 2)
  
  # Save the plot
  dev.off()
}

# Reset the plotting parameters to default
par(mfrow = c(1, 1))
#create histogram
dir.create("histograms")
for (i in seq_along(shapiro_test_results)) {
  png(filename = paste("histograms/histogram_", names(numeric_data)[i], ".png"))
  hist(numeric_data[, i], main = paste("Histogram for ", names(numeric_data)[i]))
  dev.off()
}
#normalise data
#use cube root transformation
transformed_data <- sign(numeric_data) * abs(numeric_data)^(1/3)
# Shapiro-Wilk test on transformed data
shapiro_test_results <- lapply(transformed_data, shapiro.test)
print("Shapiro-Wilk Test:")
print(shapiro_test_results)
#test for correlation
round(cor(numeric_data), digits = 2)
library(corrplot)
corrplot(cor(numeric_data), method = "number", type = "upper")
#perform wilcox test
numeric_data_subset <- numeric_data[, sapply(numeric_data, is.numeric)]
#hypothesis testing
developed_numeric <- developed[sapply(developed, is.numeric)]
developing_numeric <- developing[sapply(developing, is.numeric)]
d <-lapply(developed_numeric, shapiro.test)
round(cor(developed_numeric), digits = 2)
corrplot(cor(developed_numeric), method = "number", type = "upper")
round(cor(developing_numeric), digits = 2)
corrplot(cor(developing_numeric), method = "number", type = "upper")
dataset$Year <- as.factor(dataset$Year)
dataset$Country_Name <- as.factor(dataset$Country_Name)
developed$Year<- as.factor(developed$Year)
developed$Country_Name <- as.factor(developed$Country_Name)
developing$Year <- as.factor(developing$Year)
developing$Country_Name <- as.factor(developing$Country_Name)
summary(dataset)
#hypothesis one
model <- lm(GDP ~ TEA, data = numeric_data)
summary(model)
test_result <- summary(model)$coefficients["TEA", c("t value", "Pr(>|t|)")]
test_result
#hypothesis 2
hypo2 <- lm(GDP~ RDE, data = numeric_data)
summary(hypo2)
test_result <- summary(hypo2)$coefficients["RDE", c("t value", "Pr(>|t|)")]
test_result
#Regression analysis
#For this analysis India was chosen
library(dplyr)
dataseta <- filter(dataset, Country_Name == "India")
cor_matrix <- round(cor(dataset[5:17]), digits = 2)
print(cor_matrix)
develop <- filter(developed, Country_Name == "Canada")
#model buiding
model1 <- lm(GDP ~ INT, data = dataset)
summary(model1)
model2 <- lm(GDP ~ INT + RDE, data = dataset)
summary(model2)
model3 <- lm(GDP ~ RDE + GSP, data = dataset)
summary(model3)
#add entrepreneurship indicator
model4 <- lm(GDP ~ RDE + GSP + TEA, data = dataset)
summary(model4)
model5 <- lm(GDP ~ RDE + GSP + EIT, data = dataset)
summary(model5)
model6 <- lm(GDP ~ + RDE + GSP + EIT + TEA, data = dataset)
summary(model6)
model7 <- lm(GDP ~ RDE + GSP + EIT + UET + TEA, data = dataset)
summary(model7)
model8 <- lm(GDP ~ EXP + EMP + TEA+ GSP + RDE, data = dataset)
summary(model8)
model9 <- lm(GDP ~ EXP + EMP + TEA+ GSP + IMP, data = develop)
summary(model9)
#check mlr conditions
library(car)
plot(model8)
colnames(dataset)
pairs(dataset[,c(5,6,7,10,13,15)], lower.panel = NULL, pch = 19, cex = 0.2)
plot(model8, 1)
plot(model8, 2)
plot(model8,3)
vif(model9)
plot(model8)
colnames(dataset)
pairs(dataset[,c(5,6,7,10,13,11)], lower.panel = NULL, pch = 19, cex = 0.2)
plot(model9, 1)
plot(model9, 2)
plot(model9,3)
vif(model9)
