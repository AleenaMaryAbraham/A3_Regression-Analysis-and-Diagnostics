# Setting the working directory
setwd("C:/Users/Aleena Mary Abraham/OneDrive/Desktop/SCMA632_2025/R")

# Function to install and load libraries
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Load required libraries
libraries <- c("dplyr", "readr", "readxl", "tidyr", "ggplot2", "BSDA","glue")
lapply(libraries, install_and_load)

# 2. Reading the dataset into R ####
data <- read.csv("../Data/NSSO68.csv")
dim(data)

unique(data$Religion)

# Filtering for ARP
ARP <- data %>%
  filter(state == "10")
# Display dataset info
cat("Dataset Information:\n")
print(names(ARP))
print(head(ARP))
print(dim(ARP))

# Finding missing values
missing_info <- colSums(is.na(ARP))
cat("Missing Values Information:\n")
print(missing_info)

# Sub-setting the data
ARPNEW <- ARP %>%
  select(foodtotal_q, MPCE_MRP, Age, Meals_At_Home, Possess_ration_card, 
         Education, No_of_Meals_per_day, emftt_q, emftt_v)

# Check for missing values in the subset
cat("Missing Values in Subset:\n")
print(colSums(is.na(ARPNEW)))

dim(ARPNEW)

# Impute missing values with mean for specific columns
columns_to_impute <- c("Education", "No_of_meals_per_day", "Meals_At_Home", 
                       "Possess_ration_card")

impute_with_mean <- function(column) {
  if (any(is.na(column))) {
    column[is.na(column)] <- mean(column, na.rm = TRUE)
  }
  return(column)
}

ARPNEW$emftt_q <- impute_with_mean(ARPNEW$emftt_q)
ARPNEW$emftt_v <- impute_with_mean(ARPNEW$emftt_v)

is.infinite.data <- sapply(ARPNEW, is.infinite)
ARPNEW[is.infinite.data] <- NA

ARPNEW <- na.omit(ARPNEW)

# Checking for missing values after imputation
cat("Missing Values After Imputation:\n")
print(colSums(is.na(ARPNEW)))

ARP$Religion

unique(ARP$Religion)
str(ARP$Religion)

# Fitting a probit regression to identify non-vegetarians. 

religion_mapping <- c("Hinduism", "Islam", "Christianity","Jainism","Others")
ARP$Religion <- factor(ARP$Religion, labels = religion_mapping)
table(ARP$Religion)

columns <- c('emftt_v','emftt_q')
data1 <- ARP[columns]
data1$target <- ifelse(data1$emftt_v>0,1,0) 
probit_modet <- glm(target~., data = data1, family = binomial(link = "probit"))
summary(probit_modet)

# Performorming a Tobit regression analysis on "NSSO68.csv" 
df_ARP = data[data$state_1 == 'ARP',]
vars <- c("foodtotal_q", "MPCE_MRP", "Age", "Meals_At_Home", "Possess_ration_card", 
          "Education", "No_of_Meals_per_day", "emftt_v", "emftt_q")

df_ARP_p = df_ARP[vars]
names(df_ARP_p)

df_ARP_p$price = df_ARP_p$emftt_v / df_ARP_p$emftt_q
names(df_ARP_p)

summary(df_ARP_p)

head(table(df_ARP_p$emftt_q))

dim(df_ARP_p)

names(ARP)

#  dependent variable and independent variables
y <- ARP$foodtotal_v
X <- ARP[, c("sauce_jam_v", "Othrprocessed_v", "Beveragestotal_v", "fv_tot")]

#  data for Tobit regression
y_tobit <- pmin(pmax(y, 0), 1)  
X_tobit <- cbind(1, X) 

options(repos = c(CRAN = "https://cloud.r-project.org"))
install.packages("censReg")
library(censReg)
# Fitting the Tobit model
X_tobit_df <- as.data.frame(X_tobit)
model <- censReg(y_tobit ~ ., data = X_tobit_df[, -1])

# Printing model summary
summary(model)

library(car)
# Check for multicollinearity using Variance Inflation Factor (VIF)
vif(model) # VIF Value more than 8 its problematic

# Extract the coefficients from the model
coefficients <- coef(model)

# Construct the equation
equation <- paste0("y = ", round(coefficients[1], 2))
for (i in 2:length(coefficients)) {
  equation <- paste0(equation, " + ", round(coefficients[i], 6), "*x", i-1)
}

# Print the equation
print(equation)
