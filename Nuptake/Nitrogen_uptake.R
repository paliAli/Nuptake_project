
# Calculating nitrogen uptake per m2 --------------------------------------

nitrogen_uptake <- merged_data %>%
  dplyr::select (`date`,  `dry-tara`, `%N corr.`, `quadrant (Q)`, sample, Material)%>%
#the dry biomass weight is now in g/0.1m2 so I need to multiply by 10 to get g/m2
  mutate (Nitrogen_uptake_gperm2 = (`dry-tara` * 10) * (`%N corr.` / 100))%>%
  mutate (Nitrogen_uptake_kgperha = (`dry-tara` * 100) * (`%N corr.` / 100))


# Remove NAs (will keep some NAs in quadrant column because earlier samples werent divided in quadrants but harvest spots)
nitrogen_uptake <- nitrogen_uptake[!is.na(nitrogen_uptake$`%N corr.`), ]

write.csv (nitrogen_uptake, "nitrogen_uptake.csv", row.names = FALSE)

# N uptake time series ----------------------------------------------------

mean_Nuptake <- nitrogen_uptake %>%
  group_by(date) %>%
  summarize(`dry-tara` = mean(`dry-tara`), `%N corr.` = mean(`%N corr.`), mean_Nuptake = mean(Nitrogen_uptake_gperm2), material = first(Material))

mean_Nuptake <- mean_Nuptake %>%
  mutate(material = recode(material, 'aboveground biomass' = 'winter wheat'),
         material = recode(material, 'mix' = 'grass'))

Nuptake_plot <- ggplot(mean_Nuptake, aes(date, mean_Nuptake, shape = factor(material), color = factor(material))) + 
  geom_line(linewidth = 1) + 
  geom_point(size = 4)+
  ggtitle("Nitrogen uptake time series") + 
  xlab("date") + 
  ylab("N uptake (g/m2)")+
  theme_minimal()+
  scale_color_manual(values = index_colors, labels = index_labels) +
  scale_x_date(date_labels = "%b/%Y", date_breaks = "2 months") +
  theme(axis.text.x = element_text(size = 11, angle = 35),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        plot.title = element_text (margin = margin (b = 20), size = 20))+
  scale_shape_manual(values = c(16, 17)) +
  scale_color_manual(values = c("green3", "gold3")) +
  guides(shape = guide_legend(title = "Biomass type",
                              keywidth = 1.5),
         color = guide_legend(title = "Biomass type",))

Nuptake_plot

library(fuzzyjoin)
library(dplyr)
library(ggplot2)
library(ggthemes)

#the N uptake value for winter wheat after June is not going to be correct because the plant is already not uptaking the nitrogen and the plant is dry
#so it would not be correlated with VIs well

mean_Nuptake <- mean_Nuptake[which(mean_Nuptake$date <= "2023-06-01"),]

write.csv (mean_Nuptake, "mean_nitrogen_uptake.csv", row.names = FALSE)

# mean Nitrogen uptake vs NDVI--------------------------------

Nuptake_NDVI <- fuzzy_inner_join(mean_Nuptake, NDVI, 
                                   by = c("date" = "date"),
                                   match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(date.x, date.y, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)


#I have to manually select the dates where the date difference is lower for NDVI measured after the cut, I want to keep the NDVI from before the cut
Nuptake_NDVI <- Nuptake_NDVI [-11, ]

Nuptake_NDVI <- Nuptake_NDVI %>% 
    group_by(date.x)%>%
    summarise(mean_Nuptake = first(mean_Nuptake), meanNDVI = first(meanNDVI), material = first(material))


#probably lower NDVI due to flowering
#Nuptake_NDVI <- Nuptake_NDVI [-5, ]

# Linear N uptake vs NDVI -------------------------------------------------

linear_model <- lm(mean_Nuptake ~ meanNDVI, data = Nuptake_NDVI)
summary(linear_model)

linear_coef <- coef(linear_model)
intercept <- linear_coef[1]
slope <- linear_coef[2]

r_squared <- 1 - sum(residuals(linear_model)^2) / sum((Nuptake_NDVI$mean_Nuptake - mean(Nuptake_NDVI$mean_Nuptake))^2)


linear_Nuptake_NDVI_plot <- Nuptake_NDVI %>%
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
           x = min(Nuptake_NDVI$meanNDVI) + 0.06, 
           y = max(Nuptake_NDVI$mean_Nuptake) - 0.9,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(Nuptake_NDVI$meanNDVI, Nuptake_NDVI$mean_Nuptake), 2)),
           hjust = 0, vjust = 1, color = "black", size = 6)

linear_Nuptake_NDVI_plot

ggsave("NDVIvsNuptake.png", linear_Nuptake_NDVI_plot, width = 6, height = 10, dpi = 350)

# Nuptake vs NDRE ---------------------------------------------------------

Nuptake_NDRE <- fuzzy_inner_join(mean_Nuptake, NDRE, 
                                  by = c("date" = "date"),
                                  match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(date.x, date.y, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)

Nuptake_NDRE <- Nuptake_NDRE [-11, ]

Nuptake_NDRE <- Nuptake_NDRE%>%
  group_by(date.x)%>%
  summarise(mean_Nuptake = first(mean_Nuptake), meanNDRE = first(meanNDRE), material = first(material))

#Nuptake_NDRE <- Nuptake_NDRE [-5, ]


linear_model <- lm(mean_Nuptake ~ meanNDRE, data = Nuptake_NDRE)
summary(linear_model)

linear_coef <- coef(linear_model)
intercept <- linear_coef[1]
slope <- linear_coef[2]

r_squared <- 1 - sum(residuals(linear_model)^2) / sum((Nuptake_NDRE$mean_Nuptake - mean(Nuptake_NDRE$mean_Nuptake))^2)

linear_Nuptake_NDRE_plot <- Nuptake_NDRE %>%
  ggplot(aes(x = meanNDRE, y = mean_Nuptake))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(x = "NDRE", y = "", title = "NDRE vs N uptake")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15),
        plot.title = element_text (margin = margin (b = 20), size = 22))+
  annotate("text",
           x = min(Nuptake_NDRE$meanNDRE) + 0.02, 
           y = max(Nuptake_NDRE$mean_Nuptake) - 1,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(Nuptake_NDRE$meanNDRE, Nuptake_NDRE$mean_Nuptake), 2)),
           hjust = 0, vjust = 1, color = "black", size = 6)

linear_Nuptake_NDRE_plot

ggsave("NDREvsNuptake.png", linear_Nuptake_NDRE_plot, width = 6, height = 10, dpi = 350)

# Nuptake vs MCARI --------------------------------------------------------

Nuptake_MCARI <- fuzzy_inner_join(mean_Nuptake, MCARI, 
                                 by = c("date" = "date"),
                                 match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(date.x, date.y, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)

Nuptake_MCARI <- Nuptake_MCARI [-11, ]

Nuptake_MCARI <- Nuptake_MCARI %>%   
  group_by(date.x)%>%
  summarise(mean_Nuptake = first(mean_Nuptake), meanMCARI = first(meanMCARI), material = first(material))



linear_model <- lm(mean_Nuptake ~ meanMCARI, data = Nuptake_MCARI)
summary(linear_model)

linear_coef <- coef(linear_model)
intercept <- linear_coef[1]
slope <- linear_coef[2]

r_squared <- 1 - sum(residuals(linear_model)^2) / sum((Nuptake_MCARI$mean_Nuptake - mean(Nuptake_MCARI$mean_Nuptake))^2)

linear_Nuptake_MCARI_plot <- Nuptake_MCARI%>%
  ggplot(aes(x = meanMCARI, y = mean_Nuptake))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(x = "MCARI", y = "", title = "MCARI vs N uptake")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15),
        plot.title = element_text (margin = margin (b = 20), size = 22))+
  annotate("text",
           x = min(Nuptake_MCARI$meanMCARI) + 0.03, 
           y = max(Nuptake_MCARI$mean_Nuptake) - 0.4,
           label = paste("y =", format(slope, digits = 3), 
                         "*x +", 
                         format(intercept, digits = 3),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(Nuptake_MCARI$meanMCARI, Nuptake_MCARI$mean_Nuptake), 2)),
           hjust = 0, vjust = 1, color = "black", size = 6)

linear_Nuptake_MCARI_plot

ggsave("MCARIvsNuptake.png", linear_Nuptake_MCARI_plot, width = 6, height = 10, dpi = 350)

# EVI vs N uptake ---------------------------------------------------------

Nuptake_EVI <- fuzzy_inner_join(mean_Nuptake, EVI, 
                                by = c("date" = "date"),
                                match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(date.x, date.y, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)

Nuptake_EVI <- Nuptake_EVI [-11, ]

Nuptake_EVI <- Nuptake_EVI %>%
  group_by(date.x)%>%
  summarise(mean_Nuptake = first(mean_Nuptake), meanEVI = first(meanEVI), material = first(material))


linear_model <- lm(mean_Nuptake ~ meanEVI, data = Nuptake_EVI)
summary(linear_model)

linear_coef <- coef(linear_model)
intercept <- linear_coef[1]
slope <- linear_coef[2]

r_squared <- 1 - sum(residuals(linear_model)^2) / sum((Nuptake_EVI$mean_Nuptake - mean(Nuptake_EVI$mean_Nuptake))^2)

linear_Nuptake_EVI_plot <- Nuptake_EVI%>%
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
           x = min(Nuptake_EVI$meanEVI) + 0.08, 
           y = max(Nuptake_EVI$mean_Nuptake) - 0.8,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(Nuptake_EVI$meanEVI, Nuptake_EVI$mean_Nuptake), 2)),
           hjust = 0, vjust = 1, color = "black", size = 6)

linear_Nuptake_EVI_plot

ggsave("EVIvsNuptake.png", linear_Nuptake_EVI_plot, width = 6, height = 10, dpi = 350)

# GNDVI vs N uptake -------------------------------------------------------

Nuptake_GNDVI <- fuzzy_inner_join(mean_Nuptake, GNDVI, 
                                  by = c("date" = "date"),
                                  match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(date.x, date.y, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)

Nuptake_GNDVI <- Nuptake_GNDVI [-11, ]

Nuptake_GNDVI <- Nuptake_GNDVI %>%
  group_by(date.x)%>%
  summarise(mean_Nuptake = first(mean_Nuptake), meanGNDVI = first(meanGNDVI))

linear_model <- lm(mean_Nuptake ~ meanGNDVI, data = Nuptake_GNDVI)
summary(linear_model)

linear_coef <- coef(linear_model)
intercept <- linear_coef[1]
slope <- linear_coef[2]

r_squared <- 1 - sum(residuals(linear_model)^2) / sum((Nuptake_GNDVI$mean_Nuptake - mean(Nuptake_GNDVI$mean_Nuptake))^2)

linear_Nuptake_GNDVI_plot <- Nuptake_GNDVI%>%
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
           x = min(Nuptake_GNDVI$meanGNDVI) + 0.01, 
           y = max(Nuptake_GNDVI$mean_Nuptake) - 0.5,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(Nuptake_GNDVI$meanGNDVI, Nuptake_GNDVI$mean_Nuptake), 2)),
           hjust = 0, vjust = 1, color = "black", size = 6)

linear_Nuptake_GNDVI_plot

ggsave("GNDVIvsNuptake.png", linear_Nuptake_GNDVI_plot, width = 6, height = 10, dpi = 350)
