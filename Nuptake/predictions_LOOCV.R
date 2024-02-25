#Leave One Out Cross Validation

library(dplyr)
library(caret)

set.seed(123)

#specify the cross-validation method
ctrl <- trainControl(method = "LOOCV")


# NDVI vs N uptake --------------------------------------------------------

#without 2022/05/10
Nuptake_NDVI <- Nuptake_NDVI[-5,]

#fit a regression model and use LOOCV to evaluate performance
LOOCV_model <- train(log(mean_Nuptake) ~ meanNDVI, data = Nuptake_NDVI, method = "lm", trControl = ctrl)
LOOCV_model

# Predict using LOOCV model
predictions <- predict(LOOCV_model, newdata = Nuptake_NDVI, type = "raw")
print(predictions)

# Create a data frame with actual and predicted values
#must do exp(predictions) to undo the logarithmic transformation and get true Nuptake values
results_NDVI <- data.frame(Actual = Nuptake_NDVI$mean_Nuptake, Predicted = exp(predictions))

# Calculate R-squared and RMSE
r_squared <- cor(Nuptake_NDVI$mean_Nuptake, results_NDVI$Predicted)^2
rmse <- sqrt(mean((Nuptake_NDVI$mean_Nuptake - results_NDVI$Predicted)^2))

# Scatter plot
library(ggplot2)

Nuptake_NDVI_prediction <- results_NDVI %>%
  ggplot(aes(x = Actual, y = Predicted)) +
  geom_point(size = 3, ) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Actual", y = "Predicted", title = "NDVI Actual vs. Predicted Values") +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15),
        plot.title = element_text (margin = margin (b = 20), size = 22),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13))+
  annotate(
    "text",
    x = min(results_NDVI$Actual)+1,
    y = max(results_NDVI$Predicted)-0.5,
    label = paste(
      "R² =", round(r_squared, 3),
      "\nRMSE =", round(rmse, 3)
    ),
    hjust = 0, vjust = 1, color = "black", size = 6
  )

Nuptake_NDVI_prediction

ggsave("Nuptake_NDVI_prediction_no05-10-2022.png", Nuptake_NDVI_prediction, width = 7, height = 10, dpi = 350)

# NDRE vs N uptake --------------------------------------------------------

#without 2022/05/10
Nuptake_NDRE <- Nuptake_NDRE[-5,]


#fit a regression model and use LOOCV to evaluate performance
LOOCV_model <- train(log(mean_Nuptake) ~ meanNDRE, data = Nuptake_NDRE, method = "lm", trControl = ctrl)
LOOCV_model

# Predict using LOOCV model
predictions <- predict(LOOCV_model, newdata = Nuptake_NDRE, type = "raw")
print(predictions)

# Create a data frame with actual and predicted values
#must do exp(predictions) to undo the logarithmic transformation and get true Nuptake values
results_NDRE <- data.frame(Actual = Nuptake_NDRE$mean_Nuptake, Predicted = exp(predictions))

# Calculate R-squared and RMSE
r_squared <- cor(Nuptake_NDRE$mean_Nuptake, results_NDRE$Predicted)^2
rmse <- sqrt(mean((Nuptake_NDRE$mean_Nuptake - results_NDRE$Predicted)^2))

# Scatter plot
library(ggplot2)

Nuptake_NDRE_prediction <- results_NDRE %>%
  ggplot(aes(x = Actual, y = Predicted)) +
  geom_point(size = 3, ) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Actual", y = "Predicted", title = "NDRE Actual vs. Predicted Values") +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15),
        plot.title = element_text (margin = margin (b = 20), size = 22),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13))+
  annotate(
    "text",
    x = min(results_NDRE$Actual)+1,
    y = max(results_NDRE$Predicted)-0.5,
    label = paste(
      "R² =", round(r_squared, 3),
      "\nRMSE =", round(rmse, 3)
    ),
    hjust = 0, vjust = 1, color = "black", size = 6
  )

Nuptake_NDRE_prediction

ggsave("Nuptake_NDRE_prediction_no05-10-2022.png", Nuptake_NDRE_prediction, width = 7, height = 10, dpi = 350)

# MCARI vs N uptake --------------------------------------------------------

#without 2022/05/10
Nuptake_MCARI <- Nuptake_MCARI[-5,]


#fit a regression model and use LOOCV to evaluate performance
LOOCV_model <- train(log(mean_Nuptake) ~ meanMCARI, data = Nuptake_MCARI, method = "lm", trControl = ctrl)
LOOCV_model

# Predict using LOOCV model
predictions <- predict(LOOCV_model, newdata = Nuptake_MCARI, type = "raw")
print(predictions)

# Create a data frame with actual and predicted values
#must do exp(predictions) to undo the logarithmic transformation and get true Nuptake values
results_MCARI <- data.frame(Actual = Nuptake_MCARI$mean_Nuptake, Predicted = exp(predictions))

# Calculate R-squared and RMSE
r_squared <- cor(Nuptake_MCARI$mean_Nuptake, results_MCARI$Predicted)^2
rmse <- sqrt(mean((Nuptake_MCARI$mean_Nuptake - results_MCARI$Predicted)^2))

# Scatter plot
library(ggplot2)

Nuptake_MCARI_prediction <- results_MCARI %>%
  ggplot(aes(x = Actual, y = Predicted)) +
  geom_point(size = 3, ) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Actual", y = "Predicted", title = "MCARI Actual vs. Predicted Values") +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15),
        plot.title = element_text (margin = margin (b = 20), size = 22),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13))+
  annotate(
    "text",
    x = min(results_MCARI$Actual)+1,
    y = max(results_MCARI$Predicted)-0.5,
    label = paste(
      "R² =", round(r_squared, 3),
      "\nRMSE =", round(rmse, 3)
    ),
    hjust = 0, vjust = 1, color = "black", size = 6
  )

Nuptake_MCARI_prediction

ggsave("Nuptake_MCARI_prediction_no05-10-2022.png", Nuptake_MCARI_prediction, width = 7, height = 10, dpi = 350)

# EVI vs N uptake --------------------------------------------------------

#without 2022/05/10
Nuptake_EVI <- Nuptake_EVI[-5,]


#fit a regression model and use LOOCV to evaluate performance
LOOCV_model <- train(log(mean_Nuptake) ~ meanEVI, data = Nuptake_EVI, method = "lm", trControl = ctrl)
LOOCV_model

# Predict using LOOCV model
predictions <- predict(LOOCV_model, newdata = Nuptake_EVI, type = "raw")
print(predictions)

# Create a data frame with actual and predicted values
#must do exp(predictions) to undo the logarithmic transformation and get true Nuptake values
results_EVI <- data.frame(Actual = Nuptake_EVI$mean_Nuptake, Predicted = exp(predictions))

# Calculate R-squared and RMSE
r_squared <- cor(Nuptake_EVI$mean_Nuptake, results_EVI$Predicted)^2
rmse <- sqrt(mean((Nuptake_EVI$mean_Nuptake - results_EVI$Predicted)^2))

# Scatter plot
library(ggplot2)

Nuptake_EVI_prediction <- results_EVI %>%
  ggplot(aes(x = Actual, y = Predicted)) +
  geom_point(size = 3, ) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Actual", y = "Predicted", title = "EVI Actual vs. Predicted Values") +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15),
        plot.title = element_text (margin = margin (b = 20), size = 22),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13))+
  annotate(
    "text",
    x = min(results_EVI$Actual)+1,
    y = max(results_EVI$Predicted)-0.5,
    label = paste(
      "R² =", round(r_squared, 3),
      "\nRMSE =", round(rmse, 3)
    ),
    hjust = 0, vjust = 1, color = "black", size = 6
  )

Nuptake_EVI_prediction

ggsave("Nuptake_EVI_prediction_no05-10-2022.png", Nuptake_EVI_prediction, width = 7, height = 10, dpi = 350)
