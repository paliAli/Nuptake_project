library (data.table)
library(dplyr)

setwd("C:\\Users\\pavla\\OneDrive\\Documents\\Nuptake_project\\Nuptake_final\\GoogleEarthEngine")
EVI <- read.csv ("C:\\Users\\pavla\\OneDrive\\Documents\\Nuptake_project\\Nuptake_final\\GoogleEarthEngine\\EVI\\EVI_scale10_cloud50.csv")
EVI <- EVI %>% na.omit() %>%
  rename(date = 'system.time_start')

EVI$date <- as.Date(EVI$date, format = "%b %d,%Y")


library(ggplot2)
library(ggthemes)


ggplot(data = EVI, aes(x = date, y = meanEVI))+
  geom_point(size =2, color = "darkgreen")+
  geom_line()+
  labs(title = "EVI",
       x = "Date",
       y = "EVI",) +
  theme_minimal() +
  scale_x_date(date_labels = "%b/%Y", date_breaks = "2 months")+
  theme_minimal()+
  theme(plot.margin = margin(8, 20, 5, 5),
        axis.text.x = element_text(size = 14, angle = 35),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15),
        plot.title = element_text (margin = margin (b = 20), size = 30))


# EVI vs LAI -------------------------------------------------------------

library(fuzzyjoin)

mean_LAI_EVI <- fuzzy_inner_join(mean_LAI_values, EVI, 
                                  by = c("Date" = "date"),
                                  match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(Date, date, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)

mean_LAI_EVI <- mean_LAI_EVI [-c(7,15), ]

mean_LAI_EVI <- mean_LAI_EVI %>% group_by(Date)%>%
  summarise(mean_LAI = first(mean_LAI), meanEVI = first(meanEVI))


mean_LAI_EVI <- mean_LAI_EVI [which(mean_LAI_EVI$Date <= "2023-06-01"), ]



linear_model <- lm(mean_LAI ~ meanEVI, data = mean_LAI_EVI)
summary(linear_model)

linear_coef <- coef(linear_model)
intercept <- linear_coef[1]
slope <- linear_coef[2]

r_squared <- 1 - sum(residuals(linear_model)^2) / sum((mean_LAI_EVI$mean_LAI - mean(mean_LAI_EVI$mean_LAI))^2)


linear_LAI_EVI_plot <- mean_LAI_EVI %>%
  ggplot(aes(x = meanEVI, y = mean_LAI))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(x = "EVI", y = "LAI", title = "EVI vs LAI")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(margin = margin(t = 20), size = 12),
        axis.title.y = element_text(margin = margin(r = 20), size = 12),
        plot.title = element_text (margin = margin (b = 20), size = 20))+
  annotate("text",
           x = min(mean_LAI_EVI$meanEVI) + 0.02, 
           y = max(mean_LAI_EVI$mean_LAI) - 0.3,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(mean_LAI_EVI$meanEVI, mean_LAI_EVI$mean_LAI), 2)),
           hjust = 0, vjust = 1, color = "black", size = 4)

linear_LAI_EVI_plot

# EVI vs CropHeight ------------------------------------------------------

CropHeight_EVI <- fuzzy_inner_join(mean_field_CropHeight, EVI, 
                                    by = c("Date" = "date"),
                                    match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(Date, date, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)

CropHeight_EVI <- CropHeight_EVI [-4, ]

CropHeight_EVI <- CropHeight_EVI %>%  group_by(Date)%>%
  summarise(`mean_height(cm)` = first(`mean_height(cm)`), meanEVI = first(meanEVI))

CropHeight_EVI <- CropHeight_EVI [which(CropHeight_EVI$Date >= "2023-01-01"), ]


linear_model <- lm(`mean_height(cm)` ~ meanEVI, data = CropHeight_EVI)
summary(linear_model)

linear_coef <- coef(linear_model)
intercept <- linear_coef[1]
slope <- linear_coef[2]

r_squared <- 1 - sum(residuals(linear_model)^2) / sum((CropHeight_EVI$`mean_height(cm)` - mean(CropHeight_EVI$`mean_height(cm)`))^2)


linear_CH_EVI_plot <- CropHeight_EVI %>%
  ggplot(aes(x = meanEVI, y = `mean_height(cm)`))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(x = "EVI", y = "Crop height (cm)", title = "EVI vs Crop height")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(margin = margin(t = 20), size = 12),
        axis.title.y = element_text(margin = margin(r = 20), size = 12),
        plot.title = element_text (margin = margin (b = 20), size = 20))+
  annotate("text",
           x = min(CropHeight_EVI$meanEVI) + 0.1, 
           y = max(CropHeight_EVI$`mean_height(cm)`) - 10,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(CropHeight_EVI$meanEVI, CropHeight_EVI$`mean_height(cm)`), 2)),
           hjust = 0, vjust = 1, color = "black", size = 4)

linear_CH_EVI_plot


# EVI vs N content -----------------------------------------------------

Ncontent_EVI <- fuzzy_inner_join(mean_Nuptake, EVI, 
                                  by = c("date" = "date"),
                                  match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(date.x, date.y, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)

Ncontent_EVI <- Ncontent_EVI[-c(3,11), ]

Ncontent_EVI <- Ncontent_EVI%>%
  group_by(date.x)%>%
  summarise(mean_Ncontent = first(`%N corr.`), meanEVI = first(meanEVI))


linear_model <- lm(mean_Ncontent ~ meanEVI, data = Ncontent_EVI)
summary(linear_model)

linear_coef <- coef(linear_model)
intercept <- linear_coef[1]
slope <- linear_coef[2]

r_squared <- 1 - sum(residuals(linear_model)^2) / sum((Ncontent_EVI$mean_Ncontent - mean(Ncontent_EVI$mean_Ncontent))^2)

Ncontent_EVI_plot <- Ncontent_EVI %>%
  ggplot(aes(x = meanEVI, y = mean_Ncontent))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(x = "EVI", y = "N content (%)", title = "EVI vs N content")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(margin = margin(t = 20), size = 12),
        axis.title.y = element_text(margin = margin(r = 20), size = 12),
        plot.title = element_text (margin = margin (b = 20), size = 20))+
  annotate("text",
           x = min(Ncontent_EVI$meanEVI) + 0.25, 
           y = max(Ncontent_EVI$mean_Ncontent) - 0.3,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(Ncontent_EVI$meanEVI, Ncontent_EVI$mean_Ncontent), 2)),
           hjust = 0, vjust = 1, color = "black", size = 4)

Ncontent_EVI_plot



# EVI vs biomass weight --------------------------------------------------

biomass_EVI <- fuzzy_inner_join(mean_Nuptake, EVI, 
                                 by = c("date" = "date"),
                                 match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(date.x, date.y, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)

biomass_EVI <- biomass_EVI [-c(3,11),]

biomass_EVI<- biomass_EVI %>%  
  group_by(date.x)%>%
  summarise(biomass_weight = first(`dry-tara`), meanEVI = first(meanEVI))%>%
  na.omit()


linear_model <- lm(biomass_weight ~ meanEVI, data = biomass_EVI)
summary(linear_model)

linear_coef <- coef(linear_model)
intercept <- linear_coef[1]
slope <- linear_coef[2]

r_squared <- 1 - sum(residuals(linear_model)^2) / sum((biomass_EVI$biomass_weight - mean(biomass_EVI$biomass_weight))^2)

biomass_EVI_plot <- biomass_EVI %>%
  ggplot(aes(x = meanEVI, y = biomass_weight))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(x = "EVI", y = "biomass weight (g)", title = "EVI vs biomass weight")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(margin = margin(t = 20), size = 12),
        axis.title.y = element_text(margin = margin(r = 20), size = 12),
        plot.title = element_text (margin = margin (b = 20), size = 20))+
  annotate("text",
           x = min(biomass_EVI$meanEVI) + 0.08, 
           y = max(biomass_EVI$biomass_weight) - 10,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(biomass_EVI$meanEVI, biomass_EVI$biomass_weight), 2)),
           hjust = 0, vjust = 1, color = "black", size = 4)

biomass_EVI_plot
