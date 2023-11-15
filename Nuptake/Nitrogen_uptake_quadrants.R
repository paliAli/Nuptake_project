# NDVI vs Nitrogen uptake by quadrant -------------------------------------------------
nitrogen_uptake <- nitrogen_uptake %>%
  mutate(`quadrant (Q)` = ifelse(is.na(`quadrant (Q)`), sample, `quadrant (Q)`))

mean_Nuptake_q <- nitrogen_uptake %>%
  group_by(date, `quadrant (Q)`) %>%
  summarize(`dry-tara` = mean(`dry-tara`), `%N corr.` = mean(`%N corr.`), mean_Nuptake = mean(Nitrogen_uptake_gperm2))%>%
  na.omit()
mean_Nuptake_q <- mean_Nuptake_q[which(mean_Nuptake_q$date <= "2023-06-01"),]


Nuptake_q_plot <- ggplot(mean_Nuptake_q, aes(date, mean_Nuptake, color = factor(`quadrant (Q)`), group = factor(`quadrant (Q)`))) + 
  geom_line(linewidth = 1) + 
  geom_point(size = 2)+
  ggtitle("Nitrogen uptake time series") + 
  xlab("date") + 
  ylab("N uptake (g/m2)")+
  theme_minimal()+
  scale_color_manual(values = index_colors, labels = index_labels) +
  scale_x_date(date_labels = "%b/%Y", date_breaks = "2 months") +
  theme(axis.text.x = element_text(size = 11, angle = 35),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        plot.title = element_text (margin = margin (b = 20), size = 20))

Nuptake_q_plot

library(fuzzyjoin)

merged_NDVI_nUptake <- fuzzy_inner_join(mean_Nuptake_q, NDVI_quadrants, 
                                        by = c("date", "quadrant (Q)" = "quadrant"),
                                        match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7)%>%
  mutate(date_difference = abs(difftime(date.x, date.y, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)%>%
  filter(`quadrant (Q)` == quadrant)

merged_NDVI_nUptake <- merged_NDVI_nUptake [-c(41:44), ]

merged_NDVI_nUptake <- merged_NDVI_nUptake %>% 
  group_by(date.x, `quadrant (Q)`)%>%
  summarize(mean_Nuptake = first(mean_Nuptake), meanNDVI = first(meanNDVI))

#Find outliers
ggplot(merged_NDVI_nUptake, aes(x = meanNDVI, y = `mean_Nuptake`, group = date.x))+
  geom_boxplot(outlier.colour="red", outlier.size = 2)
#outliers are from 2021-07-20 and 2021-09-02
#removed outlier from 2021-07-20 
merged_NDVI_nUptake <- merged_NDVI_nUptake [-7, ]

# Linear N uptake vs NDVI per quadrant -------------------------------------------------

linear_model <- lm(mean_Nuptake ~ meanNDVI, data = merged_NDVI_nUptake)
summary(linear_model)

linear_coef <- coef(linear_model)
intercept <- linear_coef[1]
slope <- linear_coef[2]

r_squared <- 1 - sum(residuals(linear_model)^2) / sum((merged_NDVI_nUptake$mean_Nuptake - mean(merged_NDVI_nUptake$mean_Nuptake))^2)


merged_NDVI_nUptake %>%
  ggplot(aes(x = meanNDVI, y = mean_Nuptake))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(x = "NDVI", y = "N uptake (g/m2)", title = "NDVI vs N uptake")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15),
        plot.title = element_text (margin = margin (b = 20), size = 22))+
  annotate("text",
           x = min(merged_NDVI_nUptake$meanNDVI) + 0.06, 
           y = max(merged_NDVI_nUptake$mean_Nuptake) - 0.9,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(merged_NDVI_nUptake$meanNDVI, merged_NDVI_nUptake$mean_Nuptake), 2)),
           hjust = 0, vjust = 1, color = "black", size = 6)

