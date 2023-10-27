library (data.table)
library(dplyr)

setwd("C:\\Users\\apavlackova\\Documents\\GoogleEarthEngine\\NDRE")

NDRE <- read.csv ("C:\\Users\\apavlackova\\Documents\\GoogleEarthEngine\\NDRE\\NDRE_scale10_cloud20.csv")
NDRE <- NDRE %>% na.omit() %>%
  rename(date = 'system.time_start')

NDRE$date <- as.Date(NDRE$date, format = "%b %d,%Y")


library(ggplot2)
library(ggthemes)

NDRE <- NDRE [which(NDRE$meanNDRE >= "0.1"), ]

# Timeseries plot ---------------------------------------------------------

NDRE_plot <- ggplot(data = NDRE, aes(x = date, y = meanNDRE))+
  geom_point(size =2, color = "darkgreen")+
  geom_line()+
  labs(title = "NDRE",
       x = "Date",
       y = "NDRE",) +
  theme_minimal() +
  scale_x_date(date_labels = "%b/%Y", date_breaks = "2 months")+
  theme_minimal()+
  theme(plot.margin = margin(8, 20, 5, 5),
        axis.text.x = element_text(size = 14, angle = 35),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15),
        plot.title = element_text (margin = margin (b = 20), size = 30))

NDRE_plot

# NDRE vs LAI -------------------------------------------------------------

mean_LAI_NDRE <- fuzzy_inner_join(mean_LAI_values, NDRE, 
                                   by = c("Date" = "date"),
                                   match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(Date, date, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)

mean_LAI_NDRE <- mean_LAI_NDRE [-c(7,15), ]

mean_LAI_NDRE <- mean_LAI_NDRE %>% group_by(Date)%>%
  summarise(mean_LAI = first(mean_LAI), meanNDRE = first(meanNDRE))

mean_LAI_NDRE <- mean_LAI_NDRE [which(mean_LAI_NDRE$Date <= "2023-06-01"), ]

# Linear NDRE vs LAI

linear_model <- lm(mean_LAI ~ meanNDRE, data = mean_LAI_NDRE)
summary(linear_model)

linear_coef <- coef(linear_model)
intercept <- linear_coef[1]
slope <- linear_coef[2]

r_squared <- 1 - sum(residuals(linear_model)^2) / sum((mean_LAI_NDRE$mean_LAI - mean(mean_LAI_NDRE$mean_LAI))^2)


linear_LAI_NDRE_plot <- mean_LAI_NDRE %>%
  ggplot(aes(x = meanNDRE, y = mean_LAI))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(x = "NDRE", y = "LAI", title = "NDRE vs LAI")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(margin = margin(t = 20), size = 12),
        axis.title.y = element_text(margin = margin(r = 20), size = 12),
        plot.title = element_text (margin = margin (b = 20), size = 20))+
  annotate("text",
           x = min(mean_LAI_NDRE$meanNDRE) + 0.02, 
           y = max(mean_LAI_NDRE$mean_LAI) - 0.3,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(mean_LAI_NDRE$meanNDRE, mean_LAI_NDRE$mean_LAI), 2)),
           hjust = 0, vjust = 1, color = "black", size = 4)

linear_LAI_NDRE_plot

# NDRE vs CropHeight ------------------------------------------------------

CropHeight_NDRE <- fuzzy_inner_join(mean_field_CropHeight, NDRE, 
                                     by = c("Date" = "date"),
                                     match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(Date, date, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)

CropHeight_NDRE <- CropHeight_NDRE [-4, ]

CropHeight_NDRE <- CropHeight_NDRE %>%  group_by(Date)%>%
  summarise(`mean_height(cm)` = first(`mean_height(cm)`), meanNDRE = first(meanNDRE))


CropHeight_NDRE <- CropHeight_NDRE [which(CropHeight_NDRE$Date <= "2023-06-01"), ]


linear_model <- lm(`mean_height(cm)` ~ meanNDRE, data = CropHeight_NDRE)
summary(linear_model)

linear_coef <- coef(linear_model)
intercept <- linear_coef[1]
slope <- linear_coef[2]

r_squared <- 1 - sum(residuals(linear_model)^2) / sum((CropHeight_NDRE$`mean_height(cm)` - mean(CropHeight_NDRE$`mean_height(cm)`))^2)


linear_CH_NDRE_plot <- CropHeight_NDRE %>%
  ggplot(aes(x = meanNDRE, y = `mean_height(cm)`))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(x = "NDRE", y = "Crop height (cm)", title = "NDRE vs Crop height")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(margin = margin(t = 20), size = 12),
        axis.title.y = element_text(margin = margin(r = 20), size = 12),
        plot.title = element_text (margin = margin (b = 20), size = 20))+
  annotate("text",
           x = min(CropHeight_NDRE$meanNDRE) + 0.01, 
           y = max(CropHeight_NDRE$`mean_height(cm)`) - 10,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(CropHeight_NDRE$meanNDRE, CropHeight_NDRE$`mean_height(cm)`), 2)),
           hjust = 0, vjust = 1, color = "black", size = 4)



linear_CH_NDRE_plot


# NDRE vs N content -----------------------------------------------------

library(fuzzyjoin)

Ncontent_NDRE <- fuzzy_inner_join(mean_Nuptake, NDRE, 
                                   by = c("date" = "date"),
                                   match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(date.x, date.y, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)

Ncontent_NDRE <- Ncontent_NDRE[-c(3,11), ]

Ncontent_NDRE <- Ncontent_NDRE%>%
  group_by(date.x)%>%
  summarise(mean_Ncontent = first(`%N corr.`), meanNDRE = first(meanNDRE))


linear_model <- lm(mean_Ncontent ~ meanNDRE, data = Ncontent_NDRE)
summary(linear_model)

linear_coef <- coef(linear_model)
intercept <- linear_coef[1]
slope <- linear_coef[2]

r_squared <- 1 - sum(residuals(linear_model)^2) / sum((Ncontent_NDRE$mean_Ncontent - mean(Ncontent_NDRE$mean_Ncontent))^2)

Ncontent_NDRE_plot <- Ncontent_NDRE %>%
  ggplot(aes(x = meanNDRE, y = mean_Ncontent))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(x = "NDRE", y = "N content (%)", title = "NDRE vs N content")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(margin = margin(t = 20), size = 12),
        axis.title.y = element_text(margin = margin(r = 20), size = 12),
        plot.title = element_text (margin = margin (b = 20), size = 20))+
  annotate("text",
           x = min(Ncontent_NDRE$meanNDRE) + 0.12, 
           y = max(Ncontent_NDRE$mean_Ncontent) - 0.4,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(Ncontent_NDRE$meanNDRE, Ncontent_NDRE$mean_Ncontent), 2)),
           hjust = 0, vjust = 1, color = "black", size = 4)

Ncontent_NDRE_plot



# NDRE vs biomass weight --------------------------------------------------

biomass_NDRE <- fuzzy_inner_join(mean_Nuptake, NDRE, 
                                  by = c("date" = "date"),
                                  match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(date.x, date.y, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)

biomass_NDRE <- biomass_NDRE [-c(3,11),]
  
biomass_NDRE<- biomass_NDRE %>%  
  group_by(date.x)%>%
  summarise(biomass_weight = first(`dry-tara`), meanNDRE = first(meanNDRE))%>%
  na.omit()


linear_model <- lm(biomass_weight ~ meanNDRE, data = biomass_NDRE)
summary(linear_model)

linear_coef <- coef(linear_model)
intercept <- linear_coef[1]
slope <- linear_coef[2]

r_squared <- 1 - sum(residuals(linear_model)^2) / sum((biomass_NDRE$biomass_weight - mean(biomass_NDRE$biomass_weight))^2)

biomass_NDRE_plot <- biomass_NDRE %>%
  ggplot(aes(x = meanNDRE, y = biomass_weight))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(x = "NDRE", y = "biomass weight (g)", title = "NDRE vs biomass weight")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(margin = margin(t = 20), size = 12),
        axis.title.y = element_text(margin = margin(r = 20), size = 12),
        plot.title = element_text (margin = margin (b = 20), size = 20))+
  annotate("text",
           x = min(biomass_NDRE$meanNDRE) + 0.05, 
           y = max(biomass_NDRE$biomass_weight) - 0.5,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(biomass_NDRE$meanNDRE, biomass_NDRE$biomass_weight), 2)),
           hjust = 0, vjust = 1, color = "black", size = 4)

biomass_NDRE_plot

