library (data.table)
library(dplyr)

setwd("C:\\Users\\pavla\\OneDrive\\Documents\\GitHub\\Nuptake_project\\GoogleEarthEngine")

NDRE <- read.csv ("C:\\Users\\pavla\\OneDrive\\Documents\\Nuptake_project\\Nuptake_final\\GoogleEarthEngine\\NDRE\\NDRE_scale10.csv")
NDRE <- NDRE %>% na.omit()%>%
  rename(date = 'system.time_start')

NDRE$date <- as.Date(NDRE$date, format = "%b %d,%Y")


library(ggplot2)
library(ggthemes)

NDRE <- NDRE [which(NDRE$meanNDRE >= "0.1"), ]

NDRE$material <- ifelse(NDRE$date <= as.Date("2022-10-06"), "grass", "winter wheat")

NDRE <- NDRE [which(NDRE$date <= "2023-07-01"), ]

# Timeseries plot ---------------------------------------------------------


NDRE_plot <- ggplot(data = NDRE, aes(x = date, y = meanNDRE))+
  geom_line(linewidth = 1, alpha = 0.7)+
  geom_point(size =4, aes(shape = factor(material), color = factor(material)))+
  labs(x = "Date",
       y = "NDRE",) +
  theme_minimal() +
  scale_x_date(date_labels = "%b/%Y", date_breaks = "3 months")+
  scale_y_continuous(breaks = seq(0.3, 0.9, by = 0.1)) +
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

NDRE_plot

ggsave("NDRE_timeseries2.png", NDRE_plot, width = 10, height = 5, dpi = 350)

# NDRE for each quadrant --------------------------------------------------

# Get the list of CSV files
file_list <- list.files(path = "C:\\Users\\pavla\\OneDrive\\Documents\\Nuptake_project\\Nuptake_final\\GoogleEarthEngine\\NDRE\\quadrants", pattern = "*.csv")

# Read and process each file, adding the quadrant column
NDRE_quadrants <- map_df(file_list, ~ {
  data <- read.csv(file.path("C:\\Users\\pavla\\OneDrive\\Documents\\Nuptake_project\\Nuptake_final\\GoogleEarthEngine\\NDRE\\quadrants", .x))
  quadrant <- get_quadrant(.x)
  data$quadrant <- quadrant
  data
})

NDRE_quadrants <- NDRE_quadrants %>% rename(date = system.time_start)
NDRE_quadrants <- na.omit(NDRE_quadrants)
NDRE_quadrants$date <- as.Date(NDRE_quadrants$date, format = "%b %d,%Y")
NDRE_quadrants <- arrange(NDRE_quadrants, date)

NDRE_quadrants <- NDRE_quadrants [which(NDRE_quadrants$meanNDRE >= "0.1"), ]

typeof(NDRE_quadrants$quadrant)
NDRE_quadrants$quadrant <- as.factor(NDRE_quadrants$quadrant)

#mean NDRE per quadrant
NDRE_quadrants <- NDRE_quadrants %>%
  group_by(date, quadrant) %>%
  summarize(meanNDRE = mean(meanNDRE))%>%
  na.omit()

NDRE_quadrants_plot <- NDRE_quadrants %>%
  ggplot(aes(x = date, y = meanNDRE, color = quadrant))+
  geom_line(linewidth = 1) + 
  geom_point(size = 2, alpha = 0.6)+
  labs(x = "Date", y = "NDRE", title = "Time series NDRE for each quadrant")+
  theme_minimal()+
  scale_x_date(date_labels = "%b/%Y", date_breaks = "3 months")+
  theme(plot.margin = margin(8, 30, 5, 5),
        axis.text.x = element_text(size = 12, angle = 35),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(margin = margin(r = 20), size = 15),
        axis.title.x = element_text(size = 15),
        plot.title = element_text (margin = margin (b = 20), size = 30))+
  scale_color_manual(values = c("red", "blue", "green", "purple")) 

NDRE_quadrants_plot

ggsave("NDRE_quadrants(1).png", NDRE_quadrants_plot, width = 10, height = 5, dpi = 350)

# NDRE vs LAI -------------------------------------------------------------
library(fuzzyjoin)

mean_LAI_NDRE <- fuzzy_inner_join(mean_LAI_values, NDRE, 
                                   by = c("Date" = "date"),
                                   match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(Date, date, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)

mean_LAI_NDRE <- mean_LAI_NDRE [-18, ]

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
  labs(x = "NDRE", y = "LAI")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15))+
  annotate("text",
           x = min(mean_LAI_NDRE$meanNDRE) + 0.07, 
           y = max(mean_LAI_NDRE$mean_LAI) - 0.3,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(mean_LAI_NDRE$meanNDRE, mean_LAI_NDRE$mean_LAI), 2)),
           hjust = 0, vjust = 1, color = "black", size = 6)

linear_LAI_NDRE_plot

ggsave("NDREvsLAI.png", linear_LAI_NDRE_plot, width = 6, height = 10, dpi = 350)

# NDRE vs CropHeight ------------------------------------------------------

CropHeight_NDRE <- fuzzy_inner_join(mean_field_CropHeight, NDRE, 
                                     by = c("Date" = "date"),
                                     match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(Date, date, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)%>%  
  group_by(Date)%>%
  summarise(`mean_height(cm)` = first(`mean_height(cm)`), meanNDRE = first(meanNDRE))

#remove values around the cuts on 2022-05-14, probably lower NDVI due to flowering
CropHeight_NDRE <- CropHeight_NDRE [-5, ]
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
  labs(x = "NDRE", y = "Crop height (cm)")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15))+
  annotate("text",
           x = min(CropHeight_NDRE$meanNDRE) + 0.04, 
           y = max(CropHeight_NDRE$`mean_height(cm)`) - 10,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(CropHeight_NDRE$meanNDRE, CropHeight_NDRE$`mean_height(cm)`), 2)),
           hjust = 0, vjust = 1, color = "black", size = 6)



linear_CH_NDRE_plot

ggsave("NDREvsCropHeight.png", linear_CH_NDRE_plot, width = 6, height = 10, dpi = 350)


# NDRE vs N content -----------------------------------------------------

Ncontent_NDRE <- fuzzy_inner_join(mean_Nuptake, NDRE, 
                                   by = c("date" = "date"),
                                   match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(date.x, date.y, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)

Ncontent_NDRE <- Ncontent_NDRE[-11, ]

Ncontent_NDRE <- Ncontent_NDRE%>%
  group_by(date.x)%>%
  summarise(mean_Ncontent = first(`%N corr.`), meanNDRE = first(meanNDRE))

Ncontent_NDRE <- Ncontent_NDRE [which(Ncontent_NDRE$date.x <= "2023-06-01"), ]

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
  labs(x = "NDRE", y = "N content (%)")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15))+
  annotate("text",
           x = min(Ncontent_NDRE$meanNDRE) + 0.15, 
           y = max(Ncontent_NDRE$mean_Ncontent) - 0.2,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(Ncontent_NDRE$meanNDRE, Ncontent_NDRE$mean_Ncontent), 2)),
           hjust = 0, vjust = 1, color = "black", size = 6)

Ncontent_NDRE_plot

ggsave("NDREvsNcontent.png", Ncontent_NDRE_plot, width = 6, height = 10, dpi = 350)

# NDRE vs biomass weight --------------------------------------------------

biomass_NDRE <- fuzzy_inner_join(mean_Nuptake, NDRE, 
                                  by = c("date" = "date"),
                                  match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(date.x, date.y, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)

biomass_NDRE <- biomass_NDRE [-11,]
  
biomass_NDRE<- biomass_NDRE %>%  
  group_by(date.x)%>%
  summarise(biomass_weight = first(`dry-tara`), meanNDRE = first(meanNDRE))%>%
  na.omit()

biomass_NDRE <- biomass_NDRE [which(biomass_NDRE$date.x <= "2023-06-01"), ]

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
  labs(x = "NDRE", y = "biomass weight (g)")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15))+
  annotate("text",
           x = min(biomass_NDRE$meanNDRE) + 0.03, 
           y = max(biomass_NDRE$biomass_weight) - 5,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(biomass_NDRE$meanNDRE, biomass_NDRE$biomass_weight), 2)),
           hjust = 0, vjust = 1, color = "black", size = 6)

biomass_NDRE_plot

ggsave("NDREvsBiomass.png", biomass_NDRE_plot, width = 6, height = 10, dpi = 350)
