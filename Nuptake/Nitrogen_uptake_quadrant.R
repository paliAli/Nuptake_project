nitrogen_uptake <- nitrogen_uptake %>%
  mutate(`quadrant (Q)` = ifelse(is.na(`quadrant (Q)`), sample, `quadrant (Q)`))

mean_Nuptake_q <- nitrogen_uptake %>%
  group_by(date, `quadrant (Q)`) %>%
  summarize(`dry-tara` = mean(`dry-tara`), `%N corr.` = mean(`%N corr.`), mean_Nuptake = mean(Nitrogen_uptake_gperm2), material = first(Material))%>%
  na.omit()

mean_Nuptake_q <- mean_Nuptake_q[which(mean_Nuptake_q$date <= "2023-07-15"),]

mean_Nuptake_q <- mean_Nuptake_q %>%
  mutate(material = recode(material, 'aboveground biomass' = 'winter wheat'),
         material = recode(material, 'mix' = 'grass'))


Nuptake_q_plot <- ggplot(mean_Nuptake_q, aes(date, mean_Nuptake, color = factor(`quadrant (Q)`), shape = factor(material), group = factor(`quadrant (Q)`))) + 
  geom_line(linewidth = 1) + 
  geom_point(size = 3)+
  ggtitle("Nitrogen uptake time series") + 
  xlab("date") + 
  ylab("N uptake (g/m2)")+
  theme_minimal()+
  scale_color_manual(values = index_colors, labels = index_labels) +
  scale_x_date(date_labels = "%b/%Y", date_breaks = "3 months") +
  scale_shape_manual(values = c(16, 17)) +
  theme(plot.margin = margin(8, 30, 5, 5),
        axis.text.x = element_text(size = 12, angle = 35),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(margin = margin(r = 20), size = 15),
        axis.title.x = element_text(size = 15),
        plot.title = element_text (margin = margin (b = 20), size = 30))+
  guides(color = guide_legend(title = "Quadrant",
                              keywidth = 1.5),
         shape = guide_legend(title = "Biomass type",))

Nuptake_q_plot

ggsave("Nuptake_quadrants.png", Nuptake_q_plot, width = 10, height = 5, dpi = 350)

# NDVI vs Nitrogen uptake by quadrant -------------------------------------------------

library(fuzzyjoin)

NDVI_quadrants$quadrant <- as.numeric(NDVI_quadrants$quadrant)

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

merged_NDVI_nUptake <- merged_NDVI_nUptake[which(merged_NDVI_nUptake$date.x <= "2023-06-01"),]

#Find outliers
outliers <- ggplot(merged_NDVI_nUptake, aes(x = meanNDVI, y = `mean_Nuptake`, group = date.x))+
  geom_boxplot(outlier.colour="red", outlier.size = 2)

#exporting boxplot
ggsave("NDVIvsNuptake_outliers.png", outliers, width = 8, height = 6, dpi = 350)

#outliers are from 2021-07-20 and 2021-09-02
#removed outlier from 2021-07-20 
merged_NDVI_nUptake <- merged_NDVI_nUptake [-c(7,10), ]

# Linear N uptake vs NDVI per quadrant -------------------------------------------------

linear_model <- lm(mean_Nuptake ~ meanNDVI, data = merged_NDVI_nUptake)
summary(linear_model)

linear_coef <- coef(linear_model)
intercept <- linear_coef[1]
slope <- linear_coef[2]

r_squared <- 1 - sum(residuals(linear_model)^2) / sum((merged_NDVI_nUptake$mean_Nuptake - mean(merged_NDVI_nUptake$mean_Nuptake))^2)


Nuptake_NDVI_q <- merged_NDVI_nUptake %>%
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

Nuptake_NDVI_q

ggsave("NDVIvsNuptake_q.png", Nuptake_NDVI_q, width = 6, height = 10, dpi = 350)

# NDRE vs Nuptake per quadrant --------------------------------------------
NDRE_quadrants$quadrant <- as.numeric(NDRE_quadrants$quadrant)

merged_NDRE_nUptake <- fuzzy_inner_join(mean_Nuptake_q, NDRE_quadrants, 
                                        by = c("date", "quadrant (Q)" = "quadrant"),
                                        match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7)%>%
  mutate(date_difference = abs(difftime(date.x, date.y, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)%>%
  filter(`quadrant (Q)` == quadrant)

merged_NDRE_nUptake <- merged_NDRE_nUptake [-c(41:44), ]

merged_NDRE_nUptake <- merged_NDRE_nUptake %>% 
  group_by(date.x, `quadrant (Q)`)%>%
  summarize(mean_Nuptake = first(mean_Nuptake), meanNDRE = first(meanNDRE))

merged_NDRE_nUptake <- merged_NDRE_nUptake[which(merged_NDRE_nUptake$date.x <= "2023-06-01"),]

#Find outliers
ggplot(merged_NDRE_nUptake, aes(x = meanNDRE, y = `mean_Nuptake`, group = date.x))+
  geom_boxplot(outlier.colour="red", outlier.size = 2)

merged_NDRE_nUptake <- merged_NDRE_nUptake [-c(7,10), ]

# Linear N uptake vs NDRE per quadrant -------------------------------------------------

linear_model <- lm(mean_Nuptake ~ meanNDRE, data = merged_NDRE_nUptake)
summary(linear_model)

linear_coef <- coef(linear_model)
intercept <- linear_coef[1]
slope <- linear_coef[2]

r_squared <- 1 - sum(residuals(linear_model)^2) / sum((merged_NDRE_nUptake$mean_Nuptake - mean(merged_NDRE_nUptake$mean_Nuptake))^2)


Nuptake_NDRE_q <- merged_NDRE_nUptake %>%
  ggplot(aes(x = meanNDRE, y = mean_Nuptake))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(x = "NDRE", y = "N uptake (g/m2)", title = "NDRE vs N uptake")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15),
        plot.title = element_text (margin = margin (b = 20), size = 22))+
  annotate("text",
           x = min(merged_NDRE_nUptake$meanNDRE) + 0.06, 
           y = max(merged_NDRE_nUptake$mean_Nuptake) - 0.9,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(merged_NDRE_nUptake$meanNDRE, merged_NDRE_nUptake$mean_Nuptake), 2)),
           hjust = 0, vjust = 1, color = "black", size = 6)

Nuptake_NDRE_q

ggsave("NDREvsNuptake_q.png", Nuptake_NDRE_q, width = 6, height = 10, dpi = 350)

# MCARI vs Nuptake per quadrant --------------------------------------------
MCARI_quadrants$quadrant <- as.numeric(MCARI_quadrants$quadrant)

merged_MCARI_nUptake <- fuzzy_inner_join(mean_Nuptake_q, MCARI_quadrants, 
                                        by = c("date", "quadrant (Q)" = "quadrant"),
                                        match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7)%>%
  mutate(date_difference = abs(difftime(date.x, date.y, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)%>%
  filter(`quadrant (Q)` == quadrant)

merged_MCARI_nUptake <- merged_MCARI_nUptake [-c(41:44), ]

merged_MCARI_nUptake <- merged_MCARI_nUptake %>% 
  group_by(date.x, `quadrant (Q)`)%>%
  summarize(mean_Nuptake = first(mean_Nuptake), meanMCARI = first(meanMCARI))

merged_MCARI_nUptake <- merged_MCARI_nUptake[which(merged_MCARI_nUptake$date.x <= "2023-06-01"),]

#Find outliers
ggplot(merged_MCARI_nUptake, aes(x = meanMCARI, y = `mean_Nuptake`, group = date.x))+
  geom_boxplot(outlier.colour="red", outlier.size = 2)

merged_MCARI_nUptake <- merged_MCARI_nUptake [-c(7,10), ]

# Linear N uptake vs MCARI per quadrant -------------------------------------------------

linear_model <- lm(mean_Nuptake ~ meanMCARI, data = merged_MCARI_nUptake)
summary(linear_model)

linear_coef <- coef(linear_model)
intercept <- linear_coef[1]
slope <- linear_coef[2]

r_squared <- 1 - sum(residuals(linear_model)^2) / sum((merged_MCARI_nUptake$mean_Nuptake - mean(merged_MCARI_nUptake$mean_Nuptake))^2)


Nuptake_MCARI_q <- merged_MCARI_nUptake %>%
  ggplot(aes(x = meanMCARI, y = mean_Nuptake))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(x = "MCARI", y = "N uptake (g/m2)", title = "MCARI vs N uptake")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15),
        plot.title = element_text (margin = margin (b = 20), size = 22))+
  annotate("text",
           x = min(merged_MCARI_nUptake$meanMCARI) + 0.01, 
           y = max(merged_MCARI_nUptake$mean_Nuptake) - 2.8,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(merged_MCARI_nUptake$meanMCARI, merged_MCARI_nUptake$mean_Nuptake), 2)),
           hjust = 0, vjust = 1, color = "black", size = 6)

ggsave("MCARIvsNuptake_q.png", Nuptake_MCARI_q, width = 6, height = 10, dpi = 350)

# GNDVI vs Nuptake per quadrant --------------------------------------------
GNDVI_quadrants$quadrant <- as.numeric(GNDVI_quadrants$quadrant)

merged_GNDVI_nUptake <- fuzzy_inner_join(mean_Nuptake_q, GNDVI_quadrants, 
                                         by = c("date", "quadrant (Q)" = "quadrant"),
                                         match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7)%>%
  mutate(date_difference = abs(difftime(date.x, date.y, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)%>%
  filter(`quadrant (Q)` == quadrant)

merged_GNDVI_nUptake <- merged_GNDVI_nUptake [-c(41:44), ]

merged_GNDVI_nUptake <- merged_GNDVI_nUptake %>% 
  group_by(date.x, `quadrant (Q)`)%>%
  summarize(mean_Nuptake = first(mean_Nuptake), meanGNDVI = first(meanGNDVI))

#Find outliers
ggplot(merged_GNDVI_nUptake, aes(x = meanGNDVI, y = `mean_Nuptake`, group = date.x))+
  geom_boxplot(outlier.colour="red", outlier.size = 2)


# Linear N uptake vs GNDVI per quadrant -------------------------------------------------

linear_model <- lm(mean_Nuptake ~ meanGNDVI, data = merged_GNDVI_nUptake)
summary(linear_model)

linear_coef <- coef(linear_model)
intercept <- linear_coef[1]
slope <- linear_coef[2]

r_squared <- 1 - sum(residuals(linear_model)^2) / sum((merged_GNDVI_nUptake$mean_Nuptake - mean(merged_GNDVI_nUptake$mean_Nuptake))^2)


Nuptake_GNDVI_q <- merged_GNDVI_nUptake %>%
  ggplot(aes(x = meanGNDVI, y = mean_Nuptake))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(x = "GNDVI", y = "N uptake (g/m2)", title = "GNDVI vs N uptake")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15),
        plot.title = element_text (margin = margin (b = 20), size = 22))+
  annotate("text",
           x = min(merged_GNDVI_nUptake$meanGNDVI) + 0.06, 
           y = max(merged_GNDVI_nUptake$mean_Nuptake) - 0.9,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(merged_GNDVI_nUptake$meanGNDVI, merged_GNDVI_nUptake$mean_Nuptake), 2)),
           hjust = 0, vjust = 1, color = "black", size = 6)

ggsave("GNDVIvsNuptake_q.png", Nuptake_GNDVI_q, width = 6, height = 10, dpi = 350)

# EVI vs Nuptake per quadrant --------------------------------------------
EVI_quadrants$quadrant <- as.numeric(EVI_quadrants$quadrant)

merged_EVI_nUptake <- fuzzy_inner_join(mean_Nuptake_q, EVI_quadrants, 
                                         by = c("date", "quadrant (Q)" = "quadrant"),
                                         match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7)%>%
  mutate(date_difference = abs(difftime(date.x, date.y, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)%>%
  filter(`quadrant (Q)` == quadrant)

merged_EVI_nUptake <- merged_EVI_nUptake [-c(41:44), ]

merged_EVI_nUptake <- merged_EVI_nUptake %>% 
  group_by(date.x, `quadrant (Q)`)%>%
  summarize(mean_Nuptake = first(mean_Nuptake), meanEVI = first(meanEVI))

merged_EVI_nUptake <- merged_EVI_nUptake[which(merged_EVI_nUptake$date.x <= "2023-06-01"),]

#Find outliers
ggplot(merged_EVI_nUptake, aes(x = meanEVI, y = `mean_Nuptake`, group = date.x))+
  geom_boxplot(outlier.colour="red", outlier.size = 2)

merged_EVI_nUptake <- merged_EVI_nUptake [-c(7,10), ]

# Linear N uptake vs NDRE per quadrant -------------------------------------------------

linear_model <- lm(mean_Nuptake ~ meanEVI, data = merged_EVI_nUptake)
summary(linear_model)

linear_coef <- coef(linear_model)
intercept <- linear_coef[1]
slope <- linear_coef[2]

r_squared <- 1 - sum(residuals(linear_model)^2) / sum((merged_EVI_nUptake$mean_Nuptake - mean(merged_EVI_nUptake$mean_Nuptake))^2)


Nuptake_EVI_q <- merged_EVI_nUptake %>%
  ggplot(aes(x = meanEVI, y = mean_Nuptake))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(x = "EVI", y = "N uptake (g/m2)", title = "EVI vs N uptake")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15),
        plot.title = element_text (margin = margin (b = 20), size = 22))+
  annotate("text",
           x = min(merged_EVI_nUptake$meanEVI) + 0.06, 
           y = max(merged_EVI_nUptake$mean_Nuptake) - 0.9,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(merged_EVI_nUptake$meanEVI, merged_EVI_nUptake$mean_Nuptake), 2)),
           hjust = 0, vjust = 1, color = "black", size = 6)

Nuptake_EVI_q

ggsave("EVIvsNuptake_q.png", Nuptake_EVI_q, width = 6, height = 10, dpi = 350)
