library(data.table)
library(dplyr)

CropHeight_data <- read.csv("C:\\Users\\pavla\\OneDrive\\Documents\\Nuptake_project\\Nuptake_final\\CropHeight+LAI\\CropHeight\\CanopyHeight_wheat_2022-23.csv")
CropHeight_data$Date <- as.Date(CropHeight_data$Date, format = "%Y-%m-%d")
CropHeight_data_mean <- CropHeight_data %>%
  group_by(Date, transect)%>%
  summarise(`mean_height(cm)` = mean(`height.cm.`))

CropHeight_data_mean <- rename(CropHeight_data_mean,'quadrant (Q)'='transect')

#old data
old_CropHeight <- read.csv("C:\\Users\\pavla\\OneDrive\\Documents\\Nuptake_project\\Nuptake_final\\CropHeight+LAI\\CropHeight\\old_CropHeight.csv")

old_CropHeight$Date <- as.Date(old_CropHeight$Date, format = "%m/%d/%Y")
old_CropHeight <- rename(old_CropHeight, 'mean_height(cm)' = 'mean_height.cm.')

#combine all data together
CropHeight_data_mean <- full_join(CropHeight_data_mean, old_CropHeight, by = NULL, copy = FALSE)

mean_field_CropHeight <- CropHeight_data_mean %>% 
  group_by(Date)%>%
  summarise(`mean_height(cm)` = mean(`mean_height(cm)`))