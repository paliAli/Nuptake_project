library(data.table)
library(dplyr)

setwd("C:\\Users\\pavla\\OneDrive\\Documents\\GitHub\\Nuptake_project\\GoogleEarthEngine\\NDVI")

NDVI <- read.csv ("C:\\Users\\pavla\\OneDrive\\Documents\\GitHub\\Nuptake_project\\GoogleEarthEngine\\NDVI\\NDVI_scale10.csv")
NDVI <- NDVI %>% na.omit()

NDVI$system.time_start <- as.Date(NDVI$system.time_start, format = "%b %d,%Y")
NDVI <- NDVI %>%
  rename(date = system.time_start)

NDVI <- NDVI [which(NDVI$meanNDVI >= "0.1"), ]

library(ggplot2)
library(ggthemes) 

# Timeseries plot ---------------------------------------------------------

NDVI_plot <- ggplot(data = NDVI, aes(x = date, y = meanNDVI))+
  geom_point(size =2, color = "darkgreen")+
  geom_line()+
  labs(title = "NDVI",
       x = "Date",
       y = "NDVI",) +
  theme_minimal() +
  scale_x_date(date_labels = "%b/%Y", date_breaks = "3 months")+
  theme_minimal()+
  theme(plot.margin = margin(8, 30, 5, 5),
        axis.text.x = element_text(size = 12, angle = 35),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(margin = margin(r = 20), size = 15),
        axis.title.x = element_text(size = 15),
        plot.title = element_text (margin = margin (b = 20), size = 30))

NDVI_plot

ggsave("NDVI_timeseries.png", NDVI_plot, width = 10, height = 5, dpi = 350)

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
file_list <- list.files(path = "C:\\Users\\pavla\\OneDrive\\Documents\\Nuptake_project\\Nuptake_final\\GoogleEarthEngine\\NDVI\\quadrants2", pattern = "*.csv")

library(purrr)
# Read and process each file, adding the quadrant column
NDVI_quadrants <- map_df(file_list, ~ {
  data <- read.csv(file.path("C:\\Users\\pavla\\OneDrive\\Documents\\Nuptake_project\\Nuptake_final\\GoogleEarthEngine\\NDVI\\quadrants2", .x))
  quadrant <- get_quadrant(.x)
  data$quadrant <- quadrant
  data
})

NDVI_quadrants <- arrange(NDVI_quadrants, date)
NDVI_quadrants <- na.omit(NDVI_quadrants)
NDVI_quadrants <- NDVI_quadrants %>% rename(date = system.time_start)
NDVI_quadrants$date <- as.Date(NDVI_quadrants$date, format = "%b %d,%Y")

NDVI_quadrants <- NDVI_quadrants [which(NDVI_quadrants$meanNDVI >= "0.1"), ]

typeof(NDVI_quadrants$quadrant)
NDVI_quadrants$quadrant <- as.factor(NDVI_quadrants$quadrant)

#mean NDVI per quadrant
NDVI_quadrants <- NDVI_quadrants %>%
  group_by(date, quadrant) %>%
  summarize(meanNDVI = mean(meanNDVI))%>%
  na.omit()

NDVI_quadrants_plot <- NDVI_quadrants %>%
  ggplot(aes(x = date, y = meanNDVI, color = quadrant))+
  geom_line(linewidth = 1) + 
  geom_point(size = 2, alpha = 0.6)+
  labs(x = "Date", y = "NDVI", title = "Time series NDVI for each quadrant")+
  theme_minimal()+
  scale_x_date(date_labels = "%b/%Y", date_breaks = "3 months") +
  theme(plot.margin = margin(8, 30, 5, 5),
        axis.text.x = element_text(size = 12, angle = 35),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(margin = margin(r = 20), size = 15),
        axis.title.x = element_text(size = 15),
        plot.title = element_text (margin = margin (b = 20), size = 30))+
  scale_color_manual(values = c("red", "blue", "green", "purple")) 

NDVI_quadrants_plot

ggsave("NDVi_quadrants.png", NDVI_quadrants_plot, width = 10, height = 5, dpi = 350)

# Plot LAI vs NDVI -----------------------------

library(fuzzyjoin)


LAI_NDVI <- fuzzy_inner_join(mean_LAI_values, NDVI, 
                             by = c("Date" = "date"),
                             match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(Date, date, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)

LAI_NDVI <- LAI_NDVI [-18, ]

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
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15),
        plot.title = element_text (margin = margin (b = 20), size = 22))+
  annotate("text",
           x = min(LAI_NDVI$meanNDVI) + 0.05, 
           y = max(LAI_NDVI$mean_LAI) - 0.5,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(LAI_NDVI$meanNDVI, LAI_NDVI$mean_LAI), 2)),
           hjust = 0, vjust = 1, color = "black", size = 6)


linear_LAI_NDVI_plot

ggsave("NDVIvsLAI(2).png", linear_LAI_NDVI_plot, width = 6, height = 10, dpi = 350)

# Plot LAI vs NDVI (exponential)-----------------------------

exponential_model <- lm(log(mean_LAI) ~ meanNDVI, data = LAI_NDVI)
summary(exponential_model)

# Extract coefficients
exponential_coef <- coef(exponential_model)
intercept_exp <- exponential_coef[1]
slope_exp <- exponential_coef[2]

r_squared_exp <- 1 - sum(residuals(exponential_model)^2) / sum((log(LAI_NDVI$mean_LAI) - mean(log(LAI_NDVI$mean_LAI)))^2)


exponential_LAI_NDVI_plot <- LAI_NDVI %>%
  ggplot(aes(x = meanNDVI, y = mean_LAI))+
  geom_point(size = 3)+
  geom_smooth(method = lm, se = FALSE, formula = y ~ exp(x), color = "blue", aes(group = 1))+
  labs(x = "NDVI", y = "LAI", title = "NDVI vs LAI (exponential)")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15),
        plot.title = element_text (margin = margin (b = 20), size = 22),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 13))+
  annotate(
    "text",
    x = min(LAI_NDVI$meanNDVI) + 0.02,
    y = max(LAI_NDVI$mean_LAI) - 1,
    label = paste(
      "y = ",
      "exp(",
      format(slope_exp, digits = 2),
      "*x +", format(exp(intercept_exp), digits = 2),
      ")\nR2 =", round(r_squared_exp, 2)
    ),
    hjust = 0, vjust = 1, color = "black", size = 6
  )

exponential_LAI_NDVI_plot

ggsave("NDVIvsLAI_exp.png", exponential_LAI_NDVI_plot, width = 7, height = 10, dpi = 350)


# Plot CropHeight vs NDVI -------------------------------------------------

CropHeight_NDVI <- fuzzy_inner_join(mean_field_CropHeight, NDVI, 
                                    by = c("Date" = "date"),
                                    match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(Date, date, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)%>% 
  group_by(Date)%>%
  summarise(`mean_height(cm)` = first(`mean_height(cm)`), meanNDVI = first(meanNDVI))


CropHeight_NDVI <- CropHeight_NDVI [which(CropHeight_NDVI$Date <= "2023-06-01"), ]
#remove values around the cuts on 2022-05-14, probably lower NDVI due to flowering
CropHeight_NDVI <- CropHeight_NDVI [-5, ]

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
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15),
        plot.title = element_text (margin = margin (b = 20), size = 22))+
  annotate("text",
           x = min(CropHeight_NDVI$meanNDVI) + 0.02, 
           y = max(CropHeight_NDVI$`mean_height(cm)`) - 20,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(CropHeight_NDVI$meanNDVI, CropHeight_NDVI$`mean_height(cm)`), 2)),
           hjust = 0, vjust = 1, color = "black", size = 6)

linear_NDVI_CH_plot

ggsave("NDVIvsCropHeight(1).png", linear_NDVI_CH_plot, width = 6, height = 10, dpi = 350)

# NDVI vs N content -----------------------------------------------------

Ncontent_NDVI <- fuzzy_inner_join(mean_Nuptake, NDVI, 
                                 by = c("date" = "date"),
                                 match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(date.x, date.y, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)

Ncontent_NDVI <- Ncontent_NDVI[-11, ]

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
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15),
        plot.title = element_text (margin = margin (b = 20), size = 22))+
  annotate("text",
           x = min(Ncontent_NDVI$meanNDVI) + 0.14, 
           y = max(Ncontent_NDVI$mean_Ncontent) - 0.3,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(Ncontent_NDVI$meanNDVI, Ncontent_NDVI$mean_Ncontent), 2)),
           hjust = 0, vjust = 1, color = "black", size = 6)

Ncontent_NDVI_plot

ggsave("NDVIvsNcontent(1).png", Ncontent_NDVI_plot, width = 6, height = 10, dpi = 350)

# NDVI vs biomass weight --------------------------------------------------

biomass_NDVI <- fuzzy_inner_join(mean_Nuptake, NDVI, 
                                by = c("date" = "date"),
                                match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(date.x, date.y, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)

biomass_NDVI <- biomass_NDVI [-11,]

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
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15),
        plot.title = element_text (margin = margin (b = 20), size = 22))+
  annotate("text",
           x = min(biomass_NDVI$meanNDVI) + 0.07, 
           y = max(biomass_NDVI$biomass_weight) - 7,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(biomass_NDVI$meanNDVI, biomass_NDVI$biomass_weight), 2)),
           hjust = 0, vjust = 1, color = "black", size = 6)

biomass_NDVI_plot

ggsave("NDVIvsBiomass(1).png", biomass_NDVI_plot, width = 6, height = 10, dpi = 350)
