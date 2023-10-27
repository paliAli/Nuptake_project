library (data.table)
library(dplyr)

setwd("C:\\Users\\apavlackova\\Documents\\GoogleEarthEngine\\NDVI")

NDVI <- read.csv ("C:\\Users\\apavlackova\\Documents\\GoogleEarthEngine\\NDVI\\NDVI_scale10_cloud20.csv")
NDVI <- NDVI %>% na.omit()

NDVI$system.time_start <- as.Date(NDVI$system.time_start, format = "%b %d,%Y")
NDVI <- NDVI %>%
  rename(date = system.time_start)

library(ggplot2)
library(ggthemes)

NDVI <- NDVI [which(NDVI$meanNDVI >= "0.1"), ]

# Timeseries plot ---------------------------------------------------------

NDVI_plot <- ggplot(data = NDVI, aes(x = date, y = meanNDVI))+
  geom_point(size =2, color = "darkgreen")+
  geom_line()+
  labs(title = "NDVI",
       x = "Date",
       y = "NDVI",) +
  theme_minimal() +
  scale_x_date(date_labels = "%Y/%m", date_breaks = "2 months")+
  theme_minimal()+
  theme(plot.margin = margin(8, 20, 5, 5),
        axis.text.x = element_text(size = 11, angle = 35),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(margin = margin(r = 20), size = 15),
        plot.title = element_text (margin = margin (b = 20), size = 30))

NDVI_plot

# NDVI for each quadrant --------------------------------------------------


# Define a function to extract quadrant number from file name
get_quadrant <- function(filename) {
  if (grepl("Q1", filename)) return(1)
  if (grepl("Q2", filename)) return(2)
  if (grepl("Q3", filename)) return(3)
  if (grepl("Q4", filename)) return(4)
  return(NA)
}

# Get the list of CSV files
file_list <- list.files(path = "C:\\Users\\apavlackova\\Documents\\GoogleEarthEngine\\NDVI\\quadrants", pattern = "*.csv")

# Read and process each file, adding the quadrant column
NDVI_quadrants <- map_df(file_list, ~ {
  data <- read.csv(file.path("C:\\Users\\apavlackova\\Documents\\GoogleEarthEngine\\NDVI\\quadrants", .x))
  quadrant <- get_quadrant(.x)
  data$quadrant <- quadrant
  data
})

NDVI_quadrants <- arrange(NDVI_quadrants, date)
NDVI_quadrants <- na.omit(NDVI_quadrants)
NDVI_quadrants$date <- as.Date(NDVI_quadrants$date, format = "%m/%d/%Y")

NDVI_quadrants <- NDVI_quadrants [which(NDVI_quadrants$meanNDVI >= "0.1"), ]

NDVI_quadrants_plot <- NDVI_quadrants %>%
  ggplot(aes(x = date, y = meanNDVI))+
  geom_point()+
  geom_line()+
  labs(x = "Date", y = "NDVI", title = "Time series NDVI for each quadrant")+
  facet_wrap(~`quadrant (Q)`)+
  theme_minimal()+
  scale_x_date(date_labels = "%Y/%m", date_breaks = "6 months")+
  theme(axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = 20)),
        plot.title = element_text (margin = margin (b = 10), size = 15))

NDVI_quadrants_plot

# Plot LAI vs NDVI -----------------------------

library(fuzzyjoin)


LAI_NDVI <- fuzzy_inner_join(mean_LAI_values, NDVI, 
                             by = c("Date" = "date"),
                             match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(Date, date, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)

LAI_NDVI <- LAI_NDVI [-c(7,15), ]

LAI_NDVI <- LAI_NDVI %>% group_by(Date)%>%
  summarise(mean_LAI = first(mean_LAI), meanNDVI = first(meanNDVI))

LAI_NDVI <- LAI_NDVI [which(LAI_NDVI$Date <= "2023-06-01"), ]

linear_model <- lm(mean_LAI ~ meanNDVI, data = LAI_NDVI)
summary(linear_model)

linear_coef <- coef(linear_model)
intercept <- linear_coef[1]
slope <- linear_coef[2]

r_squared <- 1 - sum(residuals(linear_model)^2) / sum((LAI_NDVI$mean_LAI - mean(LAI_NDVI$mean_LAI))^2)


linear_LAI_NDVI_plot <- LAI_NDVI %>%
  ggplot(aes(x = meanNDVI, y = mean_LAI))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(x = "NDVI", y = "LAI", title = "NDVI vs LAI")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(margin = margin(t = 20), size = 12),
        axis.title.y = element_text(margin = margin(r = 20), size = 12),
        plot.title = element_text (margin = margin (b = 20), size = 20))+
  annotate("text",
           x = min(LAI_NDVI$meanNDVI) + 0.02, 
           y = max(LAI_NDVI$mean_LAI) - 0.3,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(LAI_NDVI$meanNDVI, LAI_NDVI$mean_LAI), 2)),
           hjust = 0, vjust = 1, color = "black", size = 4)


linear_LAI_NDVI_plot


# NDVI vs LAI for each quadrant --------------------------------------------------

NDVI_quadrants <- rename(NDVI_quadrants,c('quadrant (Q)'='quadrant'))

LAI_NDVI_quadrants <- fuzzy_inner_join(LAI_values, NDVI_quadrants, 
                                       by = c("Date" = "date"),
                                       match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 10) %>%
  filter(abs(difftime(Date, date, units = "days")) <= 10 & `quadrant (Q).x` == `quadrant (Q).y`) %>%
  mutate(date_difference = abs(difftime(Date, date, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)%>%
  group_by(Date, `quadrant (Q).x`)%>%
  summarise(LAI = first(LAI), meanNDVI = first(meanNDVI))%>%
  rename(`quadrant (Q)` = `quadrant (Q).x`)

LAI_NDVI_quadrants <- LAI_NDVI_quadrants [which(LAI_NDVI_quadrants$Date <= "2023-06-01"), ]


NDVI_LAI_plot <- LAI_NDVI_quadrants %>%
  ggplot(aes(x = meanNDVI, y = LAI))+
  geom_point()+
  geom_smooth (method = lm, se = FALSE)+
  labs(x = "NDVI", y = "LAI", title = "NDVI vs LAI for each quadrant")+
  facet_wrap(~`quadrant (Q)`)+
  theme_minimal()+
  theme(axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = 20)),
        plot.title = element_text (margin = margin (b = 10), size = 15))

NDVI_LAI_plot

# Plot CropHeight vs NDVI -------------------------------------------------

CropHeight_NDVI <- fuzzy_inner_join(mean_field_CropHeight, NDVI, 
                                    by = c("Date" = "date"),
                                    match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(Date, date, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)

CropHeight_NDVI <- CropHeight_NDVI [-4, ]

CropHeight_NDVI <- CropHeight_NDVI %>% group_by(Date)%>%
  summarise(`mean_height(cm)` = first(`mean_height(cm)`), meanNDVI = first(meanNDVI))


CropHeight_NDVI <- CropHeight_NDVI [which(CropHeight_NDVI$Date <= "2023-06-01"), ]

linear_model <- lm(`mean_height(cm)` ~ meanNDVI, data = CropHeight_NDVI)
summary(linear_model)

linear_coef <- coef(linear_model)
intercept <- linear_coef[1]
slope <- linear_coef[2]

r_squared <- 1 - sum(residuals(linear_model)^2) / sum((CropHeight_NDVI$`mean_height(cm)` - mean(CropHeight_NDVI$`mean_height(cm)`))^2)


linear_NDVI_CH_plot <- CropHeight_NDVI %>%
  ggplot(aes(x = meanNDVI, y = `mean_height(cm)`))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(x = "NDVI", y = "Crop height (cm)", title = "NDVI vs Crop height")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(margin = margin(t = 20), size = 12),
        axis.title.y = element_text(margin = margin(r = 20), size = 12),
        plot.title = element_text (margin = margin (b = 20), size = 20))+
  annotate("text",
           x = min(CropHeight_NDVI$meanNDVI) + 0.02, 
           y = max(CropHeight_NDVI$`mean_height(cm)`) - 20,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(CropHeight_NDVI$meanNDVI, CropHeight_NDVI$`mean_height(cm)`), 2)),
           hjust = 0, vjust = 1, color = "black", size = 4)

linear_NDVI_CH_plot

#CropHeight vs NDVI per quadrant

CropHeight_NDVI_quadrants <- fuzzy_inner_join(CropHeight_data_mean, NDVI, 
                                    by = c("Date" = "date"),
                                    match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 10) %>%
  mutate(date_difference = abs(difftime(Date, date, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)%>%
  group_by(Date, `quadrant (Q)`)%>%
  summarise(`mean_height(cm)` = first(`mean_height(cm)`), meanNDVI = first(meanNDVI))

CropHeight_NDVI_quadrants <- CropHeight_NDVI_quadrants [which(CropHeight_NDVI_quadrants$Date <= "2023-06-01"), ]

NDVI_CH_quadrant_plot <- CropHeight_NDVI_quadrants %>%
  ggplot(aes(x = meanNDVI, y = `mean_height(cm)`))+
  geom_point()+
  geom_smooth (method = lm, se = FALSE)+
  labs(x = "NDVI", y = "crop height (cm)", title = "NDVI vs crop height for each quadrant")+
  facet_wrap(~`quadrant (Q)`)+
  theme_minimal()+
  theme(axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = 20)),
        plot.title = element_text (margin = margin (b = 10), size = 15))

NDVI_CH_quadrant_plot



#combining NDVI and NDRE
library(gridExtra)

NDVI_NDRE <- grid.arrange(NDVI_plot, NDRE_plot, ncol = 1,heights = c(1, 1.25))

library(data.table)
library(purrr)


# NDVI vs N content -----------------------------------------------------

Ncontent_NDVI <- fuzzy_inner_join(mean_Nuptake, NDVI, 
                                 by = c("date" = "date"),
                                 match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(date.x, date.y, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)

Ncontent_NDVI <- Ncontent_NDVI[-c(3,11), ]

Ncontent_NDVI <- Ncontent_NDVI%>%
  group_by(date.x)%>%
  summarise(mean_Ncontent = first(`%N corr.`), meanNDVI = first(meanNDVI))


linear_model <- lm(mean_Ncontent ~ meanNDVI, data = Ncontent_NDVI)
summary(linear_model)

linear_coef <- coef(linear_model)
intercept <- linear_coef[1]
slope <- linear_coef[2]

r_squared <- 1 - sum(residuals(linear_model)^2) / sum((Ncontent_NDVI$mean_Ncontent - mean(Ncontent_NDVI$mean_Ncontent))^2)

Ncontent_NDVI_plot <- Ncontent_NDVI %>%
  ggplot(aes(x = meanNDVI, y = mean_Ncontent))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(x = "NDVI", y = "N content (%)", title = "NDVI vs N content")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(margin = margin(t = 20), size = 12),
        axis.title.y = element_text(margin = margin(r = 20), size = 12),
        plot.title = element_text (margin = margin (b = 20), size = 20))+
  annotate("text",
           x = min(Ncontent_NDVI$meanNDVI) + 0.1, 
           y = max(Ncontent_NDVI$mean_Ncontent) - 0.3,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(Ncontent_NDVI$meanNDVI, Ncontent_NDVI$mean_Ncontent), 2)),
           hjust = 0, vjust = 1, color = "black", size = 4)

Ncontent_NDVI_plot


# NDVI vs biomass weight --------------------------------------------------

biomass_NDVI <- fuzzy_inner_join(mean_Nuptake, NDVI, 
                                by = c("date" = "date"),
                                match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(date.x, date.y, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)

biomass_NDVI <- biomass_NDVI [-c(3,11),]

biomass_NDVI<- biomass_NDVI %>%  
  group_by(date.x)%>%
  summarise(biomass_weight = first(`dry-tara`), meanNDVI = first(meanNDVI))%>%
  na.omit()


linear_model <- lm(biomass_weight ~ meanNDVI, data = biomass_NDVI)
summary(linear_model)

linear_coef <- coef(linear_model)
intercept <- linear_coef[1]
slope <- linear_coef[2]

r_squared <- 1 - sum(residuals(linear_model)^2) / sum((biomass_NDVI$biomass_weight - mean(biomass_NDVI$biomass_weight))^2)

biomass_NDVI_plot <- biomass_NDVI %>%
  ggplot(aes(x = meanNDVI, y = biomass_weight))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)+
  labs(x = "NDVI", y = "biomass weight (g)", title = "NDVI vs biomass weight")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(margin = margin(t = 20), size = 12),
        axis.title.y = element_text(margin = margin(r = 20), size = 12),
        plot.title = element_text (margin = margin (b = 20), size = 20))+
  annotate("text",
           x = min(biomass_NDVI$meanNDVI) + 0.05, 
           y = max(biomass_NDVI$biomass_weight) - 0.5,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(biomass_NDVI$meanNDVI, biomass_NDVI$biomass_weight), 2)),
           hjust = 0, vjust = 1, color = "black", size = 4)

biomass_NDVI_plot
