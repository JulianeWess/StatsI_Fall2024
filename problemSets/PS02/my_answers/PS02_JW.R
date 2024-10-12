#PS02_JW

setwd("C:/Users/julia/OneDrive/Desktop/R Data")
getwd()

library(readr)  
library(ggplot2)  
library(broom)  

#exercise 2

url <- "https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv"
data <- read_csv(url)

head(data)

model <- lm(water ~ reserved, data = data)

summary(model)


#exercise 1 


observed <- matrix(c(14, 6, 7, 7, 7, 1), nrow = 2, byrow = TRUE)
colnames(observed) <- c("Not_Stopped", "Bribe_requested", "Stopped_warning")
rownames(observed) <- c("Upper_class", "Lower_class")

print("Observed counts:")
print(observed)


row_totals <- rowSums(observed)
col_totals <- colSums(observed)
grand_total <- sum(observed)


print("Row totals:")
print(row_totals)
print("Column totals:")
print(col_totals)
print("Grand total:")
print(grand_total)


expected <- matrix(0, nrow = 2, ncol = 3)  
for (i in 1:2) {
  for (j in 1:3) {
    expected[i, j] <- (row_totals[i] * col_totals[j]) / grand_total
  }
}


print("Expected counts:")
print(expected)

chi_squared_values <- (observed - expected)^2 / expected
print("Chi-squared components for each cell:")
print(chi_squared_values)


chi_squared_stat <- sum(chi_squared_values)
print("Chi-squared statistic:")
print(chi_squared_stat)



df <- (nrow(observed) - 1) * (ncol(observed) - 1)
print("Degrees of freedom:")
print(df)

p_value <- pchisq(chi_squared_stat, df = df, lower.tail = FALSE)
print("P-value:")
print(p_value)


standardized_residuals <- (observed - expected) / sqrt(expected)
print("Standardized Residuals for each cell:")
print(standardized_residuals)

