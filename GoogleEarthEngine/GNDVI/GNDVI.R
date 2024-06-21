library (data.table)
library(dplyr)

setwd("C:\\Users\\pavla\\OneDrive\\Documents\\Nuptake_project\\Nuptake_final\\GoogleEarthEngine")
GNDVI <- read.csv ("C:\\Users\\pavla\\OneDrive\\Documents\\Nuptake_project\\Nuptake_final\\GoogleEarthEngine\\GNDVI\\GNDVI_scale10.csv")
GNDVI <- GNDVI %>% na.omit() %>%
  rename(date = 'system.time_start')
  

GNDVI$date <- as.Date(GNDVI$date, format = "%b %d,%Y")

library(ggplot2)
library(ggthemes)

GNDVI <- GNDVI[which(GNDVI$meanGNDVI >= "0.1"),]

GNDVI$material <- ifelse(GNDVI$date <= as.Date("2022-10-06"), "grass", "winter wheat")

GNDVI <- GNDVI [which(GNDVI$date <= "2023-06-01"), ]

GNDVI_plot <- ggplot(data = GNDVI, aes(x = date, y = meanGNDVI))+
  geom_line(linewidth = 1, alpha = 0.7)+
  geom_point(size =4, aes(shape = factor(material), color = factor(material)))+
  labs(x = "Date",
       y = "GNDVI",) +
  theme_minimal() +
  scale_x_date(date_labels = "%b/%Y", date_breaks = "3 months")+
  theme_minimal()+
  theme(plot.margin = margin(8, 30, 5, 5),
        axis.text.x = element_text(size = 12, angle = 35),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.position = "none")+
  scale_shape_manual(values = c(16, 17)) +
  scale_color_manual(values = c("green3", "gold2")) +
  guides(shape = guide_legend(title = "Biomass type",
                              keywidth = 1.5),
         color = guide_legend(title = "Biomass type",))

GNDVI_plot

ggsave("GNDVI_timeseries2.png", GNDVI_plot, width = 10, height = 5, dpi = 350)

# GNDVI for each quadrant --------------------------------------------------
library(purrr)

# Get the list of CSV files
file_list <- list.files(path = "C:\\Users\\pavla\\OneDrive\\Documents\\Nuptake_project\\Nuptake_final\\GoogleEarthEngine\\GNDVI\\quadrants", pattern = "*.csv")

# Read and process each file, adding the quadrant column
GNDVI_quadrants <- map_df(file_list, ~ {
  data <- read.csv(file.path("C:\\Users\\pavla\\OneDrive\\Documents\\Nuptake_project\\Nuptake_final\\GoogleEarthEngine\\GNDVI\\quadrants", .x))
  quadrant <- get_quadrant(.x)
  data$quadrant <- quadrant
  data
})

GNDVI_quadrants <- GNDVI_quadrants %>% rename(date = system.time_start)
GNDVI_quadrants <- na.omit(GNDVI_quadrants)
GNDVI_quadrants$date <- as.Date(GNDVI_quadrants$date, format = "%b %d,%Y")
GNDVI_quadrants <- arrange(GNDVI_quadrants, date)

GNDVI_quadrants <- GNDVI_quadrants [which(GNDVI_quadrants$meanGNDVI >= "0.1"), ]

typeof(GNDVI_quadrants$quadrant)
GNDVI_quadrants$quadrant <- as.factor(GNDVI_quadrants$quadrant)

#mean NDRE per quadrant
GNDVI_quadrants <- GNDVI_quadrants %>%
  group_by(date, quadrant) %>%
  summarize(meanGNDVI = mean(meanGNDVI))%>%
  na.omit()

GNDVI_quadrants_plot <- GNDVI_quadrants %>%
  ggplot(aes(x = date, y = meanGNDVI, color = quadrant))+
  geom_line(linewidth = 1) + 
  geom_point(size = 2, alpha = 0.6)+
  labs(x = "Date", y = "GNDVI", title = "Time series GNDVI for each quadrant")+
  theme_minimal()+
  scale_x_date(date_labels = "%b/%Y", date_breaks = "3 months")+
  theme(plot.margin = margin(8, 30, 5, 5),
        axis.text.x = element_text(size = 12, angle = 35),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(margin = margin(r = 20), size = 15),
        axis.title.x = element_text(size = 15),
        plot.title = element_text (margin = margin (b = 20), size = 30))+
  scale_color_manual(values = c("red", "blue", "green", "purple")) 

GNDVI_quadrants_plot

ggsave("GNDVI_quadrants.png", GNDVI_quadrants_plot, width = 10, height = 5, dpi = 350)

#GNDVI vs LAI -------------------------------------------------------------

library(fuzzyjoin)

mean_LAI_GNDVI <- fuzzy_inner_join(mean_LAI_values, GNDVI, 
                                 by = c("Date" = "date"),
                                 match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(Date, date, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)

mean_LAI_GNDVI <- mean_LAI_GNDVI[-18, ]

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
  labs(x = "GNDVI", y = "LAI")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15))+
  annotate("text",
           x = min(mean_LAI_GNDVI$meanGNDVI) + 0.02, 
           y = max(mean_LAI_GNDVI$mean_LAI) - 0.3,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(mean_LAI_GNDVI$meanGNDVI, mean_LAI_GNDVI$mean_LAI), 2)),
           hjust = 0, vjust = 1, color = "black", size = 6)

linear_LAI_GNDVI_plot

ggsave("GNDVIvsLAI.png", linear_LAI_GNDVI_plot, width = 6, height = 10, dpi = 350)


# GNDVI vs CropHeight ------------------------------------------------------

CropHeight_GNDVI <- fuzzy_inner_join(mean_field_CropHeight, GNDVI, 
                                   by = c("Date" = "date"),
                                   match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(Date, date, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)%>%  
  group_by(Date)%>%
  summarise(`mean_height(cm)` = first(`mean_height(cm)`), meanGNDVI = first(meanGNDVI))

#CropHeight_GNDVI <- CropHeight_GNDVI [-5, ]
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
  labs(x = "GNDVI", y = "Crop height (cm)")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15))+
  annotate("text",
           x = min(CropHeight_GNDVI$meanGNDVI) + 0.02, 
           y = max(CropHeight_GNDVI$`mean_height(cm)`) - 10,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(CropHeight_GNDVI$meanGNDVI, CropHeight_GNDVI$`mean_height(cm)`), 2)),
           hjust = 0, vjust = 1, color = "black", size = 6)

linear_CH_GNDVI_plot

ggsave("GNDVIvsCropHeight.png", linear_CH_GNDVI_plot, width = 6, height = 10, dpi = 350)

# GNDVI vs N content -----------------------------------------------------

Ncontent_GNDVI <- fuzzy_inner_join(mean_Nuptake, GNDVI, 
                                 by = c("date" = "date"),
                                 match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(date.x, date.y, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)

Ncontent_GNDVI <- Ncontent_GNDVI[-11, ]

Ncontent_GNDVI <- Ncontent_GNDVI%>%
  group_by(date.x)%>%
  summarise(mean_Ncontent = first(`%N corr.`), meanGNDVI = first(meanGNDVI))

Ncontent_GNDVI <- Ncontent_GNDVI [which(Ncontent_GNDVI$date.x <= "2023-06-01"), ]

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
  labs(x = "GNDVI", y = "N content (%)")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15))+
  annotate("text",
           x = min(Ncontent_GNDVI$meanGNDVI) + 0.08, 
           y = max(Ncontent_GNDVI$mean_Ncontent) - 0.3,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(Ncontent_GNDVI$meanGNDVI, Ncontent_GNDVI$mean_Ncontent), 2)),
           hjust = 0, vjust = 1, color = "black", size = 6)

Ncontent_GNDVI_plot

ggsave("GNDVIvsNcontent.png", Ncontent_GNDVI_plot, width = 6, height = 10, dpi = 350)


# GNDVI vs biomass weight --------------------------------------------------

biomass_GNDVI <- fuzzy_inner_join(mean_Nuptake, GNDVI, 
                                by = c("date" = "date"),
                                match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(date.x, date.y, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)

biomass_GNDVI <- biomass_GNDVI [-11,]

biomass_GNDVI<- biomass_GNDVI %>%  
  group_by(date.x)%>%
  summarise(biomass_weight = first(`dry-tara`), meanGNDVI = first(meanGNDVI))%>%
  na.omit()

biomass_GNDVI <- biomass_GNDVI [which(biomass_GNDVI$date.x <= "2023-06-01"), ]

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
  labs(x = "GNDVI", y = "biomass weight (g)")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15))+
  annotate("text",
           x = min(biomass_GNDVI$meanGNDVI) + 0.02, 
           y = max(biomass_GNDVI$biomass_weight) - 10,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(biomass_GNDVI$meanGNDVI, biomass_GNDVI$biomass_weight), 2)),
           hjust = 0, vjust = 1, color = "black", size = 6)

biomass_GNDVI_plot

ggsave("GNDVIvsBiomass.png", biomass_GNDVI_plot, width = 6, height = 10, dpi = 350)
