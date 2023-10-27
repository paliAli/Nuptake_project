
# LAI in time plot --------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggthemes)

y_breaks <- seq(0.2, 5, by = 0.3)

LAI_values$LAI <- as.numeric(LAI_values$LAI)
str (LAI_values)

#LAI time series plot divided into 4 plots for each quadrant
LAI_plot <- LAI_values %>%
  ggplot(aes(x = Date, y = LAI))+
  geom_point()+
  geom_line()+
  labs(x = "Date", y = " LAI", title = "Time series LAI for each quadrant")+
  facet_wrap(~`quadrant (Q)`)+
  theme_minimal()+
  scale_y_continuous(limits = c(0.2, 5), breaks = y_breaks)+
  theme(axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = 20)),
        plot.title = element_text (margin = margin (b = 10), size = 15))

LAI_plot

#LAI time series with all quadrants differentiated by color
custom_colors <- c("blue", "orange", "purple", "darkgreen")
LAI_values <- LAI_values %>%
  group_by(Date)%>%
  mutate(average = mean(LAI))


LAI_values %>%
  ggplot(aes(x = Date, y = LAI, color = factor(`quadrant (Q)`), group = factor(`quadrant (Q)`)))+
  geom_line(linewidth = 1, alpha = 0.5)+
  geom_point()+
  geom_line(aes(x = Date, y = average), color = 'black', linetype = 'dashed', linewidth = 1)+
  geom_point(aes(x = Date, y = average), color = 'black')+
  scale_color_manual(values = custom_colors)+
  labs(x = "Date", y = " LAI", title = "LAI", color = "Quadrant")+
  theme_minimal()+
  scale_y_continuous(limits = c(0.2, 5), breaks = y_breaks)+
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(margin = margin(t = 20), size = 12),
        axis.title.y = element_text(margin = margin(r = 20), size = 12),
        plot.title = element_text (margin = margin (b = 20), size = 20))+
  annotate(geom = "text", x = as.Date("2023-05-31"), y = 3.95, 
                    label = "average LAI")

# Crop height time plot ---------------------------------------------------

#for each quadrant
CropHeight_data_mean <- CropHeight_data_mean %>%
  group_by(Date)%>%
  mutate(average = mean(`mean_height(cm)`))


CropHeight_plot <- CropHeight_data_mean %>%
  ggplot(aes(x = Date, y = `mean_height(cm)`))+
  geom_point()+
  geom_line(linewidth = 1, alpha = 0.5)+
  geom_line(aes(x = Date, y = average), color = 'black', linetype = 'dashed', linewidth = 1)+
  geom_point(aes(x = Date, y = average), color = 'black')+
  labs(x = "Date", y = " Crop height (cm)", title = "Time series Crop height for each quadrant")+
  facet_wrap(~`quadrant (Q)`)+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(margin = margin(t = 20), size = 12),
        axis.title.y = element_text(margin = margin(r = 20), size = 12),
        plot.title = element_text (margin = margin (b = 20), size = 20))

CropHeight_plot

CropHeight_data_mean %>%
  ggplot(aes(x = Date, y = `mean_height(cm)`, color = factor(`quadrant (Q)`), group = factor(`quadrant (Q)`)))+
  geom_point()+
  geom_line(linewidth = 1, alpha = 0.5)+
  geom_line(aes(x = Date, y = average), color = 'black', linetype = 'dashed', linewidth = 1)+
  scale_color_manual(values = custom_colors)+
  labs(x = "Date", y = "Crop height (cm)", title = "Crop height", color = "Quadrant")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(margin = margin(t = 20), size = 12),
        axis.title.y = element_text(margin = margin(r = 20), size = 12),
        plot.title = element_text (margin = margin (b = 20), size = 20))+
  annotate(geom = "text", x = as.Date("2023-04-26"), y = 50, angle = 59, 
           label = "average Crop height")


#average crop height for the whole field
mean_field_CropHeight <- CropHeight_data_mean %>%
  group_by(Date) %>%
  summarize(mean_field_height = mean(`mean_height(cm)`))

mean_field_CropHeight %>%
  ggplot(aes(x = Date, y = `mean_field_height`))+
  geom_point()+
  geom_line(linewidth = 1)+
  labs(x = "Date", y = "Crop height (cm)", title = "Time series crop height", color = "Quadrant")+
  theme_minimal()+
  theme(axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = 20)),
        plot.title = element_text (margin = margin (b = 10), size = 15))


