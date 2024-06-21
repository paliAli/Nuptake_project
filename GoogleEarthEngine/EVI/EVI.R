library (data.table)
library(dplyr)

setwd("C:\\Users\\pavla\\OneDrive\\Documents\\GitHub\\Nuptake_project\\GoogleEarthEngine")
EVI <- read.csv ("C:\\Users\\pavla\\OneDrive\\Documents\\GitHub\\Nuptake_project\\GoogleEarthEngine\\EVI\\EVI_scale10.csv")
EVI <- EVI %>% na.omit() %>%
  rename(date = 'system.time_start')

EVI$date <- as.Date(EVI$date, format = "%b %d,%Y")


library(ggplot2)
library(ggthemes)

EVI <- EVI[which(EVI$meanEVI >= "0.1"),]

EVI$material <- ifelse(EVI$date <= as.Date("2022-10-06"), "grass", "winter wheat")

EVI <- EVI [which(EVI$date <= "2023-07-01"), ]
EVI <- EVI [which(EVI$date >= "2021-03-01"), ] #aby byl zacatek a konec vsech time series stejnej


 EVI_plot <- ggplot(data = EVI, aes(x = date, y = meanEVI))+
  geom_line(linewidth = 1, alpha = 0.7)+
  geom_point(size =4, aes(shape = factor(material), color = factor(material)))+
  labs(x = "Date",
       y = "EVI",) +
  theme_minimal() +
  scale_x_date(date_labels = "%b/%Y", date_breaks = "3 months")+
  scale_y_continuous(breaks = seq(0.3, 1.0, by = 0.1)) +
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

EVI_plot

ggsave("EVI_timeseries2.png", EVI_plot, width = 10, height = 5, dpi = 350)

# EVI for each quadrant --------------------------------------------------
library(purrr)

# Get the list of CSV files
file_list <- list.files(path = "C:\\Users\\pavla\\OneDrive\\Documents\\Nuptake_project\\Nuptake_final\\GoogleEarthEngine\\EVI\\quadrants", pattern = "*.csv")

# Read and process each file, adding the quadrant column
EVI_quadrants <- map_df(file_list, ~ {
  data <- read.csv(file.path("C:\\Users\\pavla\\OneDrive\\Documents\\Nuptake_project\\Nuptake_final\\GoogleEarthEngine\\EVI\\quadrants", .x))
  quadrant <- get_quadrant(.x)
  data$quadrant <- quadrant
  data
})

EVI_quadrants <- EVI_quadrants %>% rename(date = system.time_start)
EVI_quadrants <- na.omit(EVI_quadrants)
EVI_quadrants$date <- as.Date(EVI_quadrants$date, format = "%b %d,%Y")
EVI_quadrants <- arrange(EVI_quadrants, date)

EVI_quadrants <- EVI_quadrants [which(EVI_quadrants$meanEVI >= "0.1"), ]

typeof(EVI_quadrants$quadrant)
EVI_quadrants$quadrant <- as.factor(EVI_quadrants$quadrant)

#mean NDRE per quadrant
EVI_quadrants <- EVI_quadrants %>%
  group_by(date, quadrant) %>%
  summarize(meanEVI = mean(meanEVI))%>%
  na.omit()

EVI_quadrants_plot <- EVI_quadrants %>%
  ggplot(aes(x = date, y = meanEVI, color = quadrant))+
  geom_line(linewidth = 1) + 
  geom_point(size = 2, alpha = 0.6)+
  labs(x = "Date", y = "EVI", title = "Time series EVI for each quadrant")+
  theme_minimal()+
  scale_x_date(date_labels = "%b/%Y", date_breaks = "3 months")+
  theme(plot.margin = margin(8, 30, 5, 5),
        axis.text.x = element_text(size = 12, angle = 35),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(margin = margin(r = 20), size = 15),
        axis.title.x = element_text(size = 15),
        plot.title = element_text (margin = margin (b = 20), size = 30))+
  scale_color_manual(values = c("red", "blue", "green", "purple")) 

EVI_quadrants_plot

ggsave("EVI_quadrants.png", EVI_quadrants_plot, width = 10, height = 5, dpi = 350)

# EVI vs LAI -------------------------------------------------------------

library(fuzzyjoin)

mean_LAI_EVI <- fuzzy_inner_join(mean_LAI_values, EVI, 
                                  by = c("Date" = "date"),
                                  match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(Date, date, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)

mean_LAI_EVI <- mean_LAI_EVI [-18, ]

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
  labs(x = "EVI", y = "LAI")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15))+
  annotate("text",
           x = min(mean_LAI_EVI$meanEVI) + 0.02, 
           y = max(mean_LAI_EVI$mean_LAI) - 0.3,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(mean_LAI_EVI$meanEVI, mean_LAI_EVI$mean_LAI), 2)),
           hjust = 0, vjust = 1, color = "black", size = 6)

linear_LAI_EVI_plot

ggsave("EVIvsLAI(2).png", linear_LAI_EVI_plot, width = 6, height = 10, dpi = 350)

# EVI vs LAI (exponential) -------------------------------------------------------------

exponential_model <- lm(log(mean_LAI) ~ meanEVI, data = mean_LAI_EVI)
summary(exponential_model)

# Extract coefficients
exponential_coef <- coef(exponential_model)
intercept_exp <- exponential_coef[1]
slope_exp <- exponential_coef[2]

r_squared_exp <- 1 - sum(residuals(exponential_model)^2) / sum((log(mean_LAI_EVI$mean_LAI) - mean(log(mean_LAI_EVI$mean_LAI)))^2)


exponential_LAI_EVI_plot <- mean_LAI_EVI %>%
  ggplot(aes(x = meanEVI, y = mean_LAI))+
  geom_point(size = 3)+
  geom_smooth(method = lm, se = FALSE, formula = y ~ exp(x), color = "blue", aes(group = 1))+
  labs(x = "EVI", y = "LAI", title = "EVI vs LAI (exponential)")+
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
    x = min(mean_LAI_EVI$meanEVI) + 0.02,
    y = max(mean_LAI_EVI$mean_LAI) - 1,
    label = paste(
      "y = ",
      "exp(",
      format(slope_exp, digits = 2),
      "*x +", format(exp(intercept_exp), digits = 2),
      ")\nR2 =", round(r_squared_exp, 2)
    ),
    hjust = 0, vjust = 1, color = "black", size = 6
  )

exponential_LAI_EVI_plot

ggsave("EVIvsLAI_exp.png", exponential_LAI_EVI_plot, width = 7, height = 10, dpi = 350)


# EVI vs CropHeight ------------------------------------------------------

CropHeight_EVI <- fuzzy_inner_join(mean_field_CropHeight, EVI, 
                                    by = c("Date" = "date"),
                                    match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(Date, date, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE) %>%  
  group_by(Date)%>%
  summarise(`mean_height(cm)` = first(`mean_height(cm)`), meanEVI = first(meanEVI))

CropHeight_EVI <- CropHeight_EVI [which(CropHeight_EVI$Date <= "2023-06-01"), ]


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
  labs(x = "EVI", y = "Crop height (cm)")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15))+
  annotate("text",
           x = min(CropHeight_EVI$meanEVI) + 0.1, 
           y = max(CropHeight_EVI$`mean_height(cm)`) - 10,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(CropHeight_EVI$meanEVI, CropHeight_EVI$`mean_height(cm)`), 2)),
           hjust = 0, vjust = 1, color = "black", size = 6)

linear_CH_EVI_plot

ggsave("EVIvsCropheight.png", linear_CH_EVI_plot, width = 6, height = 10, dpi = 350)

# EVI vs N content -----------------------------------------------------

Ncontent_EVI <- fuzzy_inner_join(mean_Nuptake, EVI, 
                                  by = c("date" = "date"),
                                  match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(date.x, date.y, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)

Ncontent_EVI <- Ncontent_EVI[-11, ]

Ncontent_EVI <- Ncontent_EVI%>%
  group_by(date.x)%>%
  summarise(mean_Ncontent = first(`%N corr.`), meanEVI = first(meanEVI))

Ncontent_EVI <- Ncontent_EVI [which(Ncontent_EVI$date.x <= "2023-06-01"), ]


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
  labs(x = "EVI", y = "N content (%)")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15))+
  annotate("text",
           x = min(Ncontent_EVI$meanEVI) + 0.25, 
           y = max(Ncontent_EVI$mean_Ncontent) - 0.3,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(Ncontent_EVI$meanEVI, Ncontent_EVI$mean_Ncontent), 2)),
           hjust = 0, vjust = 1, color = "black", size = 6)

Ncontent_EVI_plot

ggsave("EVIvsNcontent.png", Ncontent_EVI_plot, width = 6, height = 10, dpi = 350)

# EVI vs biomass weight --------------------------------------------------

biomass_EVI <- fuzzy_inner_join(mean_Nuptake, EVI, 
                                 by = c("date" = "date"),
                                 match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7) %>%
  mutate(date_difference = abs(difftime(date.x, date.y, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)

biomass_EVI <- biomass_EVI [-11,]

biomass_EVI<- biomass_EVI %>%  
  group_by(date.x)%>%
  summarise(biomass_weight = first(`dry-tara`), meanEVI = first(meanEVI))%>%
  na.omit()

biomass_EVI <- biomass_EVI [which(biomass_EVI$date.x <= "2023-06-01"), ]

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
  labs(x = "EVI", y = "biomass weight (g)")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15))+
  annotate("text",
           x = min(biomass_EVI$meanEVI) + 0.08, 
           y = max(biomass_EVI$biomass_weight) - 10,
           label = paste("y =", format(slope, digits = 2), 
                         "*x +", 
                         format(intercept, digits = 2),
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(biomass_EVI$meanEVI, biomass_EVI$biomass_weight), 2)),
           hjust = 0, vjust = 1, color = "black", size = 6)

biomass_EVI_plot

ggsave("EVIvsBiomass.png", biomass_EVI_plot, width = 6, height = 10, dpi = 350)
