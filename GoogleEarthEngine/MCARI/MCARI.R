library (data.table)
library(dplyr)

setwd("C:\\Users\\pavla\\OneDrive\\Documents\\GitHub\\Nuptake_project\\GoogleEarthEngine")
MCARI <- read.csv ("C:\\Users\\pavla\\OneDrive\\Documents\\Nuptake_project\\Nuptake_final\\GoogleEarthEngine\\MCARI\\MCARI_scale10.csv")
MCARI <- MCARI %>% na.omit() %>%
  rename(date = 'system.time_start')

MCARI$date <- as.Date(MCARI$date, format = "%b %d,%Y")

library(ggplot2)
library(ggthemes)

MCARI <- MCARI [which(MCARI$meanMCARI >= 0.01), ]

MCARI$material <- ifelse(MCARI$date <= as.Date("2022-10-06"), "grass", "winter wheat")

MCARI <- MCARI [which(MCARI$date <= "2023-07-01"), ]
MCARI <- MCARI [which(MCARI$date >= "2021-03-01"), ]

MCARI_plot <- ggplot(data = MCARI, aes(x = date, y = meanMCARI))+
  geom_line(linewidth = 1, alpha = 0.7)+
  geom_point(size =4, aes(shape = factor(material), color = factor(material)))+
  labs(x = "Date",
       y = "MCARI",) +
  theme_minimal() +
  scale_x_date(date_labels = "%b/%Y", date_breaks = "3 months")+
  scale_y_continuous(breaks = seq(0.1, 0.7, by = 0.1)) +
  theme_minimal()+
  theme(plot.margin = margin(8, 30, 5, 5),
        axis.text.x = element_text(size = 12, angle = 35),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(margin = margin(r = 20), size = 15),
        axis.title.x = element_text(size = 15),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.position = "none")+
  scale_shape_manual(values = c(16, 17)) +
  scale_color_manual(values = c("green3", "gold2")) +
  guides(shape = guide_legend(title = "Biomass type",
                              keywidth = 1.5),
         color = guide_legend(title = "Biomass type",))

MCARI_plot

ggsave("MCARI_timeseries2.png", MCARI_plot, width = 10, height = 5, dpi = 350)

library(fuzzyjoin)

library(purrr)


# MCARI quadrants ---------------------------------------------------------

# Get the list of CSV files
file_list <- list.files(path = "C:\\Users\\pavla\\OneDrive\\Documents\\Nuptake_project\\Nuptake_final\\GoogleEarthEngine\\MCARI\\quadrants2", pattern = "*.csv")

# Read and process each file, adding the quadrant column
MCARI_quadrants <- map_df(file_list, ~ {
  data <- read.csv(file.path("C:\\Users\\pavla\\OneDrive\\Documents\\Nuptake_project\\Nuptake_final\\GoogleEarthEngine\\MCARI\\quadrants2", .x))
  quadrant <- get_quadrant(.x)
  data$quadrant <- quadrant
  data
})

MCARI_quadrants <- MCARI_quadrants %>% rename(date = system.time_start)
MCARI_quadrants <- na.omit(MCARI_quadrants)
MCARI_quadrants$date <- as.Date(MCARI_quadrants$date, format = "%b %d,%Y")
MCARI_quadrants <- arrange(MCARI_quadrants, date)

MCARI_quadrants <- MCARI_quadrants [which(MCARI_quadrants$meanMCARI >= "0.01"), ]

typeof(MCARI_quadrants$quadrant)
MCARI_quadrants$quadrant <- as.factor(MCARI_quadrants$quadrant)

#mean MCARI per quadrant
MCARI_quadrants <- MCARI_quadrants %>%
  group_by(date, quadrant) %>%
  summarize(meanMCARI = mean(meanMCARI))%>%
  na.omit()

MCARI_quadrants_plot <- MCARI_quadrants %>%
  ggplot(aes(x = date, y = meanMCARI, color = quadrant))+
  geom_line(linewidth = 1) + 
  geom_point(size = 2, alpha = 0.6)+
  labs(x = "Date", y = "MCARI", title = "Time series MCARI for each quadrant")+
  theme_minimal()+
  scale_x_date(date_labels = "%b/%Y", date_breaks = "3 months")+
  theme(plot.margin = margin(8, 30, 5, 5),
        axis.text.x = element_text(size = 12, angle = 35),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(margin = margin(r = 20), size = 15),
        axis.title.x = element_text(size = 15),
        plot.title = element_text (margin = margin (b = 20), size = 30))+
  scale_color_manual(values = c("red", "blue", "green", "purple")) 

MCARI_quadrants_plot

ggsave("MCARI_quadrants(1).png", MCARI_quadrants_plot, width = 10, height = 5, dpi = 350)

# MCARI vs LAI ------------------------------------------------------------

library(fuzzyjoin)
mean_LAI_MCARI <- fuzzy_inner_join(mean_LAI_values, MCARI, 
                             by = c("Date" = "date"),
                             match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(Date, date, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)

mean_LAI_MCARI <- mean_LAI_MCARI [-18, ]

mean_LAI_MCARI <- mean_LAI_MCARI %>% group_by(Date)%>%
  summarise(mean_LAI = first(mean_LAI), meanMCARI = first(meanMCARI))


mean_LAI_MCARI <- mean_LAI_MCARI [which(mean_LAI_MCARI$Date <= "2023-06-01"), ]

linear_model <- lm(mean_LAI ~ meanMCARI, data = mean_LAI_MCARI)
summary(linear_model)

linear_coef <- coef(linear_model)
intercept <- linear_coef[1]
slope <- linear_coef[2]

r_squared <- 1 - sum(residuals(linear_model)^2) / sum((mean_LAI_MCARI$mean_LAI - mean(mean_LAI_MCARI$mean_LAI))^2)

library(ggplot2)
library(ggthemes)


linear_LAI_MCARI_plot <- mean_LAI_MCARI %>%
  ggplot(aes(x = meanMCARI, y = mean_LAI))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(x = "MCARI", y = "LAI")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15))+
  annotate("text",
           x = min(mean_LAI_MCARI$meanMCARI) + 0.08, 
           y = max(mean_LAI_MCARI$mean_LAI),
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(mean_LAI_MCARI$meanMCARI, mean_LAI_MCARI$mean_LAI), 2)),
           hjust = 0, vjust = 1, color = "black", size = 6)

linear_LAI_MCARI_plot

ggsave("MCARIvsLAI.png", linear_LAI_MCARI_plot, width = 6, height = 10, dpi = 350)

# MCARI vs CropHeight -----------------------------------------------------

CropHeight_MCARI <- fuzzy_inner_join(mean_field_CropHeight, MCARI, 
                                    by = c("Date" = "date"),
                                    match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(Date, date, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)%>% 
  group_by(Date)%>%
  summarise(`mean_height(cm)` = first(`mean_height(cm)`), meanMCARI = first(meanMCARI))

CropHeight_MCARI <- CropHeight_MCARI [which(CropHeight_MCARI$Date <= "2023-06-01"), ]


linear_model <- lm(`mean_height(cm)` ~ meanMCARI, data = CropHeight_MCARI)
summary(linear_model)

linear_coef <- coef(linear_model)
intercept <- linear_coef[1]
slope <- linear_coef[2]

r_squared <- 1 - sum(residuals(linear_model)^2) / sum((CropHeight_MCARI$`mean_height(cm)` - mean(CropHeight_MCARI$`mean_height(cm)`))^2)


linear_CH_MCARI_plot <- CropHeight_MCARI %>%
  ggplot(aes(x = meanMCARI, y = `mean_height(cm)`))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(x = "MCARI", y = "Crop height (cm)")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15))+
  annotate("text",
           x = min(CropHeight_MCARI$meanMCARI) + 0.005, 
           y = max(CropHeight_MCARI$`mean_height(cm)`) - 10,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(CropHeight_MCARI$meanMCARI, CropHeight_MCARI$`mean_height(cm)`), 2)),
           hjust = 0, vjust = 1, color = "black", size = 6)


linear_CH_MCARI_plot

ggsave("MCARIvsCropHeight.png", linear_CH_MCARI_plot, width = 6, height = 10, dpi = 350)

# MCARI vs N content ------------------------------------------------------

Ncontent_MCARI <- fuzzy_inner_join(mean_Nuptake, MCARI, 
                                  by = c("date" = "date"),
                                  match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(date.x, date.y, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)

Ncontent_MCARI <- Ncontent_MCARI [-11,]

Ncontent_MCARI <- Ncontent_MCARI%>%
  group_by(date.x)%>%
  summarise(mean_Ncontent = first(`%N corr.`), meanMCARI = first(meanMCARI))

Ncontent_MCARI <- Ncontent_MCARI [which(Ncontent_MCARI$date.x <= "2023-06-01"), ]

linear_model <- lm(mean_Ncontent ~ meanMCARI, data = Ncontent_MCARI)
summary(linear_model)

linear_coef <- coef(linear_model)
intercept <- linear_coef[1]
slope <- linear_coef[2]

r_squared <- 1 - sum(residuals(linear_model)^2) / sum((Ncontent_MCARI$mean_Ncontent - mean(Ncontent_MCARI$mean_Ncontent))^2)

Ncontent_MCARI_plot <- Ncontent_MCARI %>%
  ggplot(aes(x = meanMCARI, y = mean_Ncontent))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(x = "MCARI", y = "N content (%)")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15))+
  annotate("text",
           x = min(Ncontent_MCARI$meanMCARI) + 0.12, 
           y = max(Ncontent_MCARI$mean_Ncontent) - 0.1,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(Ncontent_MCARI$meanMCARI, Ncontent_MCARI$mean_Ncontent), 2)),
           hjust = 0, vjust = 1, color = "black", size = 6)

Ncontent_MCARI_plot

ggsave("MCARIvsNcontent.png", Ncontent_MCARI_plot, width = 6, height = 10, dpi = 350)

# MCARI vs biomass weight -------------------------------------------------

biomass_MCARI <- fuzzy_inner_join(mean_Nuptake, MCARI, 
                                   by = c("date" = "date"),
                                   match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(date.x, date.y, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)

biomass_MCARI <- biomass_MCARI[-11,]

biomass_MCARI <- biomass_MCARI%>%
  group_by(date.x)%>%
  summarise(biomass_weight = first(`dry-tara`), meanMCARI = first(meanMCARI))%>%
  na.omit()

biomass_MCARI <- biomass_MCARI [which(biomass_MCARI$date.x <= "2023-06-01"), ]

linear_model <- lm(biomass_weight ~ meanMCARI, data = biomass_MCARI)
summary(linear_model)

linear_coef <- coef(linear_model)
intercept <- linear_coef[1]
slope <- linear_coef[2]

r_squared <- 1 - sum(residuals(linear_model)^2) / sum((biomass_MCARI$biomass_weight - mean(biomass_MCARI$biomass_weight))^2)

biomass_MCARI_plot <- biomass_MCARI %>%
  ggplot(aes(x = meanMCARI, y = biomass_weight))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(x = "MCARI", y = "biomass weight (g)")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15))+
  annotate("text",
           x = min(biomass_MCARI$meanMCARI) + 0.1, 
           y = max(biomass_MCARI$biomass_weight) - 3,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(biomass_MCARI$meanMCARI, biomass_MCARI$biomass_weight), 2)),
           hjust = 0, vjust = 1, color = "black", size = 6)

biomass_MCARI_plot

ggsave("MCARIvsBiomass.png", biomass_MCARI_plot, width = 6, height = 10, dpi = 350)
