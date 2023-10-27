library(dplyr)
library(fuzzyjoin)
library(GGally)
library(ggplot2)
library(ggthemes)

Nuptake_VIs <- fuzzy_inner_join(mean_Nuptake, VI_ts, 
                                  by = c("date" = "date"),
                                  match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(date.x, date.y, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)
  
Nuptake_VIs <- Nuptake_VIs [-c(3,11), ]
  
Nuptake_VIs <- Nuptake_VIs %>%
  group_by(date.x)%>%
  summarise(`dry-tara` = first(`dry-tara`), `%N corr.` = first(`%N corr.`), Nuptake = first(mean_Nuptake), meanMCARI = first(meanMCARI), meanNDVI = first (meanNDVI), meanNDRE = first(meanNDRE), meanEVI = first(meanEVI), meanGNDVI = first (meanGNDVI))



Nuptake_VIs <- Nuptake_VIs[which(Nuptake_VIs$meanNDVI >= "0.1"),]

Nuptake_VIs <- Nuptake_VIs[which(Nuptake_VIs$date.x <= "2023-06-01"),]

ggpairs(data = Nuptake_VIs, columns = 2:9, title = "Nitrogen uptake data")

fit_1 <- lm(Nuptake ~ meanMCARI, data = Nuptake_VIs)
summary(fit_1)

fit_2 <- lm(Nuptake ~ meanEVI, data = Nuptake_VIs)
summary(fit_2)

fit_3 <- lm(Nuptake ~ meanNDVI, data = Nuptake_VIs)
summary(fit_3)




ggplot(data = Nuptake_VIs, aes(fit_2$residuals)) +
  geom_histogram(binwidth = 1, color = "black", fill = "purple4") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Histogram for Model Residuals")

ggplot(data = Nuptake_VIs, aes(x = meanNDVI, y = Nuptake)) +
  geom_point() +
  stat_smooth(method = "lm", col = "dodgerblue3") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to Data")

predict(fit_1, data.frame(meanNDVI = 0.641))
predict(fit_2, data.frame(meanNDVI = 0.641, meanNDRE = 0.394))
predict(fit_3, data.frame(meanNDVI = 0.641, meanMCARI = 0.177))
predict(fit_4, data.frame(meanNDRE = 0.571, meanMCARI = 0.099))
predict(fit_5, data.frame(meanNDVI = 0.641, meanNDRE = 0.394, meanMCARI = 0.177))

meanNDVI <- seq(0.6,1, by=0.05) # make vectors
meanNDRE <- seq(0.35, 0.8, by=0.05) 
meanMCARI <- seq(0.05, 0.4, by=0.05)

pred_grid <- expand.grid(meanNDRE = meanNDRE, meanMCARI = meanMCARI)
library(scatterplot3d)

#Next, we make predictions for volume based on the predictor variable grid
pred_grid$Nuptake2 <-predict(fit_4, new = pred_grid)

#Now we can make a 3d scatterplot from the predictor grid and the predicted volumes
fit_4_sp <- scatterplot3d(pred_grid$meanNDRE, pred_grid$meanMCARI, pred_grid$Nuptake2, angle = 60, color = "dodgerblue", pch = 1, ylab = "MCARI", xlab = "NDRE", zlab = "N uptake (g/m2)" )

#overlay our actual observations to see how well they fit
fit_4_sp$points3d(Nuptake_VIs$meanNDRE, Nuptake_VIs$meanMCARI, Nuptake_VIs$Nuptake, pch=16)

fit_6 <- lm(Nuptake ~ , data = Nuptake_VIs)
summary(fit_6) 





# Prediction vs real data -------------------------------------------------

#Obtain prediction data
Nuptake_predictions <- predict(fit_2, Nuptake_VIs)
print(Nuptake_predictions)

fit2_summary <- summary(fit_2)
r_squared <- fit2_summary$r.squared

# Plot predictions against real data
ggplot(aes(x = Nuptake_VIs$Nuptake, y = Nuptake_predictions))+
  geom_point()+
  geom_smooth()

Nuptake_realvspredictions <- data.frame (real_Nuptake  = Nuptake_VIs$Nuptake,
                  predicted_Nuptake = Nuptake_predictions)

Nuptake_realvspredictions %>%
  ggplot(aes(x = real_Nuptake, y = predicted_Nuptake))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(x = "Real Data", y = "Predictions", title = "EVI N uptake predictions vs real")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(margin = margin(t = 20), size = 12),
        axis.title.y = element_text(margin = margin(r = 20), size = 12),
        plot.title = element_text (margin = margin (b = 20), size = 20))+
  annotate("text",
           x = min(Nuptake_realvspredictions$real_Nuptake) + 0.5,
           y = max(Nuptake_realvspredictions$predicted_Nuptake) - 1.5,
           label = paste("R2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(Nuptake_realvspredictions$real_Nuptake, Nuptake_realvspredictions$predicted_Nuptake), 2)),
           hjust = 0, vjust = 1, color = "black", size = 4)



# Predict N uptake for all EVI values I have ------------------------------

EVI <- EVI[which(EVI$meanEVI > 0.1), ]

Nuptake_predictions <- predict(fit_2, EVI)
print(Nuptake_predictions)

EVI_Nprediction <- EVI
EVI_Nprediction$Nuptake_prediction <- Nuptake_predictions
EVI_Nprediction <- EVI_Nprediction[-c(45,46,47), ]

rm(EVI_Nuptake_realvspredictions)

EVI_Nuptake_realvspredictions <- fuzzy_inner_join(mean_Nuptake, EVI_Nprediction, 
                                 by = c("date" = "date"),
                                 match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(date.x, date.y, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)%>%
  group_by(date.x)%>%
  summarise(mean_Nuptake = first(mean_Nuptake), meanEVI = first(meanEVI))
