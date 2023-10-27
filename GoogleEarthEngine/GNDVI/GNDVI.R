library (data.table)
library(dplyr)

setwd("C:\\Users\\apavlackova\\Documents\\GoogleEarthEngine\\GNDVI")
GNDVI <- read.csv ("C:\\Users\\apavlackova\\Documents\\GoogleEarthEngine\\GNDVI\\GNDVI_scale10_cloud20.csv")
GNDVI <- GNDVI %>% na.omit() %>%
  rename(date = 'system.time_start')
  
GNDVI <- GNDVI %>% rename(meanGNDVI = meanNDVI)

GNDVI$date <- as.Date(GNDVI$date, format = "%b %d,%Y")

library(ggplot2)
library(ggthemes)

GNDVI <- GNDVI[which(GNDVI$meanGNDVI >= "0.1"),]

ggplot(data = GNDVI, aes(x = date, y = meanGNDVI))+
  geom_point(size =2, color = "darkgreen")+
  geom_line()+
  labs(title = "GNDVI",
       x = "Date",
       y = "GNDVI",) +
  theme_minimal() +
  scale_x_date(date_labels = "%b/%Y", date_breaks = "2 months")+
  theme_minimal()+
  theme(plot.margin = margin(8, 20, 5, 5),
        axis.text.x = element_text(size = 14, angle = 35),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15),
        plot.title = element_text (margin = margin (b = 20), size = 30))


#GNDVI vs LAI -------------------------------------------------------------

library(fuzzyjoin)

mean_LAI_GNDVI <- fuzzy_inner_join(mean_LAI_values, GNDVI, 
                                 by = c("Date" = "date"),
                                 match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(Date, date, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)

mean_LAI_GNDVI <- mean_LAI_GNDVI[-c(7,15), ]

mean_LAI_GNDVI <- mean_LAI_GNDVI %>% group_by(Date)%>%
  summarise(mean_LAI = first(mean_LAI), meanGNDVI = first(meanGNDVI))


mean_LAI_GNDVI <- mean_LAI_GNDVI [which(mean_LAI_GNDVI$Date <= "2023-06-01"), ]


linear_model <- lm(mean_LAI ~ meanGNDVI, data = mean_LAI_GNDVI)
summary(linear_model)

linear_coef <- coef(linear_model)
intercept <- linear_coef[1]
slope <- linear_coef[2]

r_squared <- 1 - sum(residuals(linear_model)^2) / sum((mean_LAI_GNDVI$mean_LAI - mean(mean_LAI_GNDVI$mean_LAI))^2)


linear_LAI_GNDVI_plot <- mean_LAI_GNDVI %>%
  ggplot(aes(x = meanGNDVI, y = mean_LAI))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(x = "GNDVI", y = "LAI", title = "GNDVI vs LAI")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(margin = margin(t = 20), size = 12),
        axis.title.y = element_text(margin = margin(r = 20), size = 12),
        plot.title = element_text (margin = margin (b = 20), size = 20))+
  annotate("text",
           x = min(mean_LAI_GNDVI$meanGNDVI) + 0.02, 
           y = max(mean_LAI_GNDVI$mean_LAI) - 0.3,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(mean_LAI_GNDVI$meanGNDVI, mean_LAI_GNDVI$mean_LAI), 2)),
           hjust = 0, vjust = 1, color = "black", size = 4)

linear_LAI_GNDVI_plot


# GNDVI vs CropHeight ------------------------------------------------------

CropHeight_GNDVI <- fuzzy_inner_join(mean_field_CropHeight, GNDVI, 
                                   by = c("Date" = "date"),
                                   match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(Date, date, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)

CropHeight_GNDVI <- CropHeight_GNDVI [-4, ]

CropHeight_GNDVI <- CropHeight_GNDVI %>%  group_by(Date)%>%
  summarise(`mean_height(cm)` = first(`mean_height(cm)`), meanGNDVI = first(meanGNDVI))


CropHeight_GNDVI <- CropHeight_GNDVI [which(CropHeight_GNDVI$Date <= "2023-06-01"), ]

linear_model <- lm(`mean_height(cm)` ~ meanGNDVI, data = CropHeight_GNDVI)
summary(linear_model)

linear_coef <- coef(linear_model)
intercept <- linear_coef[1]
slope <- linear_coef[2]

r_squared <- 1 - sum(residuals(linear_model)^2) / sum((CropHeight_GNDVI$`mean_height(cm)` - mean(CropHeight_GNDVI$`mean_height(cm)`))^2)


linear_CH_GNDVI_plot <- CropHeight_GNDVI %>%
  ggplot(aes(x = meanGNDVI, y = `mean_height(cm)`))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(x = "GNDVI", y = "Crop height (cm)", title = "GNDVI vs Crop height")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(margin = margin(t = 20), size = 12),
        axis.title.y = element_text(margin = margin(r = 20), size = 12),
        plot.title = element_text (margin = margin (b = 20), size = 20))+
  annotate("text",
           x = min(CropHeight_GNDVI$meanGNDVI) + 0.02, 
           y = max(CropHeight_GNDVI$`mean_height(cm)`) - 10,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(CropHeight_GNDVI$meanGNDVI, CropHeight_GNDVI$`mean_height(cm)`), 2)),
           hjust = 0, vjust = 1, color = "black", size = 4)

linear_CH_GNDVI_plot


# GNDVI vs N content -----------------------------------------------------

Ncontent_GNDVI <- fuzzy_inner_join(mean_Nuptake, GNDVI, 
                                 by = c("date" = "date"),
                                 match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(date.x, date.y, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)

Ncontent_GNDVI <- Ncontent_GNDVI[-c(3,11), ]

Ncontent_GNDVI <- Ncontent_GNDVI%>%
  group_by(date.x)%>%
  summarise(mean_Ncontent = first(`%N corr.`), meanGNDVI = first(meanGNDVI))


linear_model <- lm(mean_Ncontent ~ meanGNDVI, data = Ncontent_GNDVI)
summary(linear_model)

linear_coef <- coef(linear_model)
intercept <- linear_coef[1]
slope <- linear_coef[2]

r_squared <- 1 - sum(residuals(linear_model)^2) / sum((Ncontent_GNDVI$mean_Ncontent - mean(Ncontent_GNDVI$mean_Ncontent))^2)

Ncontent_GNDVI_plot <- Ncontent_GNDVI %>%
  ggplot(aes(x = meanGNDVI, y = mean_Ncontent))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(x = "GNDVI", y = "N content (%)", title = "GNDVI vs N content")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(margin = margin(t = 20), size = 12),
        axis.title.y = element_text(margin = margin(r = 20), size = 12),
        plot.title = element_text (margin = margin (b = 20), size = 20))+
  annotate("text",
           x = min(Ncontent_GNDVI$meanGNDVI) + 0.05, 
           y = max(Ncontent_GNDVI$mean_Ncontent) - 0.3,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(Ncontent_GNDVI$meanGNDVI, Ncontent_GNDVI$mean_Ncontent), 2)),
           hjust = 0, vjust = 1, color = "black", size = 4)

Ncontent_GNDVI_plot



# GNDVI vs biomass weight --------------------------------------------------

biomass_GNDVI <- fuzzy_inner_join(mean_Nuptake, GNDVI, 
                                by = c("date" = "date"),
                                match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(date.x, date.y, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)

biomass_GNDVI <- biomass_GNDVI [-c(3,11),]

biomass_GNDVI<- biomass_GNDVI %>%  
  group_by(date.x)%>%
  summarise(biomass_weight = first(`dry-tara`), meanGNDVI = first(meanGNDVI))%>%
  na.omit()


linear_model <- lm(biomass_weight ~ meanGNDVI, data = biomass_GNDVI)
summary(linear_model)

linear_coef <- coef(linear_model)
intercept <- linear_coef[1]
slope <- linear_coef[2]

r_squared <- 1 - sum(residuals(linear_model)^2) / sum((biomass_GNDVI$biomass_weight - mean(biomass_GNDVI$biomass_weight))^2)

biomass_GNDVI_plot <- biomass_GNDVI %>%
  ggplot(aes(x = meanGNDVI, y = biomass_weight))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(x = "GNDVI", y = "biomass weight (g)", title = "GNDVI vs biomass weight")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(margin = margin(t = 20), size = 12),
        axis.title.y = element_text(margin = margin(r = 20), size = 12),
        plot.title = element_text (margin = margin (b = 20), size = 20))+
  annotate("text",
           x = min(biomass_GNDVI$meanGNDVI) + 0.02, 
           y = max(biomass_GNDVI$biomass_weight) - 10,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(biomass_GNDVI$meanGNDVI, biomass_GNDVI$biomass_weight), 2)),
           hjust = 0, vjust = 1, color = "black", size = 4)

biomass_GNDVI_plot
