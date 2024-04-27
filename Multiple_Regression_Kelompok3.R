# Multiple Linear Regression
## Kelompok 3
## Ryan F F H - 2C2220007
## Betrand D - 2C2220001
## Lifa M - 2C2220014

# Data
data <- data.frame(
  y = c(25.5, 31.2, 25.9, 38.4, 18.4, 26.7, 26.4, 25.9, 32, 25.2, 39.7, 35.7, 26.5),
  x1 = c(1.74, 6.32, 6.22, 10.52, 1.19, 1.22, 4.1, 6.32, 4.08, 4.15, 10.15, 1.72, 1.7),
  x2 = c(5.3, 5.42, 8.41, 4.63, 11.6, 5.85, 6.62, 8.72, 4.42, 7.6, 4.83, 3.12, 5.3),
  x3 = c(10.8, 9.4, 7.2, 8.5, 9.4, 9.9, 8, 9.1, 8.7, 9.2, 9.4, 7.6, 8.2)
)

print(data)

# Regression Model
model <- lm(y ~ x1 + x2 + x3, data)
model

# Summary
summary(model)

# Prediction
new_data <- data.frame(
  const = 1,
  x1 = c(3),
  x2 = c(8),
  x3 = c(9)
)

# Predict with Confidence Interval
predictions <- predict(model, newdata = new_data, interval = "prediction")
predictions_summary <- cbind(new_data, predictions)
print(predictions_summary)

# ANOVA
anova_table <- data.frame(
  Sumber = c("Regression", "Residual", "Total"),
  DK = c(length(model$coefficients) - 1, length(model$residuals) - length(model$coefficients), length(model$residuals) - 1),
  JK = c(
    sum((fitted(model) - mean(data$y))^2),
    sum(residuals(model)^2),
    sum((data$y - mean(data$y))^2)
  ),
  RJK = c(
    sum((fitted(model) - mean(data$y))^2) / (length(model$coefficients) - 1),
    sum(residuals(model)^2) / (length(model$residuals) - length(model$coefficients)),
    sum((data$y - mean(data$y))^2) / (length(model$residuals) - 1)
  ),
  F = c(
    (sum((fitted(model) - mean(data$y))^2) / (length(model$coefficients) - 1)) / (sum(residuals(model)^2) / (length(model$residuals) - length(model$coefficients))),
    NA,
    NA
  )
)

print(anova_table)
