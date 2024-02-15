library(ggplot2)
library(ggthemes)

ggplot()+
  geom_point(data = NDVI, aes(x = date, y = meanNDVI, color = "NDVI"), size = 2, alpha = 0.6) +
  geom_line(data = NDVI, aes(x = date, y = meanNDVI, color = "NDVI"), linewidth = 1) +
  geom_point(data = NDRE, aes(x = date, y = meanNDRE, color = "NDRE"), size = 2, alpha = 0.6) +  
  geom_line(data = NDRE, aes(x = date, y = meanNDRE, color = "NDRE"), linewidth = 1)+
  geom_point(data = MCARI, aes(x = date, y = meanMCARI,  color = "MCARI"), size = 2, alpha = 0.6) +
  geom_line(data = MCARI, aes(x = date, y = meanMCARI, color = "MCARI"), linewidth = 1) +  
  labs(title = "Vegetation indices time series plot",
       x = "Date",
       y = "",) +
  theme_minimal() +
  scale_x_date(date_labels = "%Y/%m", date_breaks = "2 months")+
  scale_color_discrete(name = "Vegetation\nindices",
                      breaks = c("NDVI", "NDRE", "MCARI"),
                      labels = c("NDVI", "NDRE",  "MCARI")) +
  theme(axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = 20)),
        plot.title = element_text (margin = margin (b = 20), size = 20))

library(dplyr)
library(tidyr)

VI_df <- inner_join(NDVI, NDRE, by = "date")
VI_df <- inner_join(VI_df, MCARI, by = "date")
VI_df <- inner_join(VI_df, EVI, by = "date")
VI_df <- inner_join(VI_df, GNDVI, by = "date")

#adding LAI

LAI_VI <- full_join(mean_LAI_values, VI_df, by = c("Date" = "date"))

LAI_VI <- fuzzy_inner_join(mean_LAI_values, VI_df, 
                              by = c("Date" = "date"),
                              match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 10) %>%
  mutate(date_difference = abs(difftime(Date, date, units = "days")))%>%
  arrange(date_difference, decreasing = FALSE)

LAI_VI <- LAI_VI [-18, ]
 
  LAI_VI <- LAI_VI %>% group_by(date)%>%
   summarise(mean_LAI = first(mean_LAI), meanNDVI = first(meanNDVI), meanNDRE = first(meanNDRE), meanMCARI = first(meanMCARI), meanEVI = first(meanEVI), meanGNDVI = first(meanGNDVI))

#adding CropHeight
LAI_CH_VI <- full_join(mean_field_CropHeight, LAI_VI, by = "Date")
LAI_CH_VI <- as.data.frame(LAI_CH_VI)

## Indexing the series 
VI_ts <- VI_df 
VI_ts[,2] <- VI_ts[,2]/mean(VI_ts[2:17,2])*100 
VI_ts[,3] <- VI_ts[,3]/mean(VI_ts[2:17,3])*100 
VI_ts[,4] <- VI_ts[,4]/mean(VI_ts[2:17,4])*100
VI_ts[,5] <- VI_ts[,5]/mean(VI_ts[2:17,5])*100 
VI_ts[,6] <- VI_ts[,6]/mean(VI_ts[2:17,6])*100 


## Indexing the series 
LAI_CH_VI_ts <- LAI_CH_VI
mean_value <- mean(LAI_CH_VI_ts[, 6], na.rm = TRUE) #calculate for each column
LAI_CH_VI_ts[,2] <- LAI_CH_VI_ts[,2]/mean_value*100 
LAI_CH_VI_ts[,3] <- LAI_CH_VI_ts[,3]/mean_value*100 
LAI_CH_VI_ts[,4] <- LAI_CH_VI_ts[,4]/mean_value*100 
LAI_CH_VI_ts[,5] <- LAI_CH_VI_ts[,5]/mean_value*100 
LAI_CH_VI_ts[,6] <- LAI_CH_VI_ts[,6]/mean_value*100 


# Reshape data to long format
VI_long <- VI_ts %>%
  gather(vegetation_index, values, -date)

VIs_timeseries <- ggplot(VI_long[which(VI_long$date >= "2021-01-24"),], aes(date, values, color = vegetation_index)) + 
  geom_line(linewidth = 1) + 
  geom_point(size = 2, alpha = 0.6)+
  ggtitle("Normalized vegetation indices time series") + 
  xlab("date") + 
  ylab("")+
  theme_minimal()+
  scale_color_manual(values = index_colors, labels = index_labels) +
  scale_x_date(date_labels = "%b/%Y", date_breaks = "2 months") +
  theme(axis.text.x = element_text(size = 11, angle = 35),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        plot.title = element_text (margin = margin (b = 20), size = 20),
        legend.title=element_blank(),
        legend.text = element_text(size = 11))

VIs_timeseries

ggsave("VIs_timeseries.png", VIs_timeseries, width = 10, height = 5, dpi = 350)

# Reshape data to long format
LAI_CH_VI_long <- LAI_CH_VI_ts %>%
  gather(variables, values, -Date)%>%
  arrange(Date)%>%
  na.omit()

# Define colors for different vegetation indices
index_colors <- c("purple", "gold", "brown2", "chartreuse2", "dodgerblue", "orange", "violet")

# Define custom labels for legend
index_labels <- c( mean_field_height = "Crop height", mean_LAI = "LAI", meanNDVI = "NDVI", meanNDRE = "NDRE", meanMCARI = "MCARI", meanEVI = "EVI", meanGNDVI = "GNDVI")

ggplot(data= LAI_CH_VI_long[which(LAI_CH_VI_long$Date>"2022-11-10"),],
       aes(Date, values, color = variables, linetype = variables)) + 
  geom_line(linewidth = 1) + 
  geom_point(size = 2, alpha = 0.6)+
  ggtitle("Normalized vegetation indices, LAI, Crop height") + 
  xlab("date") + 
  ylab("")+
  theme_minimal()+
  scale_linetype_manual(values = c("solid", "solid", "dashed", "dashed", "dashed")) +
  scale_color_manual(values = index_colors, labels = index_labels) +
  scale_x_date(date_labels = "%Y/%m", date_breaks = "1 month") +
  theme(axis.text.x = element_text(size = 11, angle = 35),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        plot.title = element_text (margin = margin (b = 20), size = 20),
        legend.title=element_blank(),
        legend.text = element_text(size = 11))+
  guides(linetype = FALSE)
