setwd ("C:\\Users\\pavla\\OneDrive\\Documents\\Nuptake_project\\Nuptake_final")
file_names <- list.files(path = "C:\\Users\\pavla\\OneDrive\\Documents\\Nuptake_project\\Nuptake_final\\CropHeight+LAI\\LAI", pattern = "*.txt", full.names = TRUE)

#Define the function to extract data from a file

extract_data_from_file <- function(file_name) {
  data <- read.delim(file_name, header = FALSE)
  selected_rows <- c(2,16,30,44)
  extracted_data <- data[selected_rows, 6]
  return(extracted_data)
}

date_columns <- list() # Initialize a list to store the extracted data

for (file_name in file_names) {
  extracted_data <- extract_data_from_file(file_name)
  date_in_file_name <- gsub("[^0-9]", "", file_name) # Extract the date from the file name (assuming it's a numeric value)
  date_columns[[date_in_file_name]] <- extracted_data
}


print(file_names)
print(date_columns)

#Combine the extracted data and create a data frame

result_df <- as.data.frame(date_columns)

write.csv(result_df, "LAI_values.csv", row.names = FALSE)


# Reshape the dataset
library(tidyr)

LAI_values <- result_df %>%
  pivot_longer(cols = everything(), 
               names_to = "Date",
               names_pattern = "X(\\d+)",
               values_to = "LAI") %>%
  arrange(Date)%>%
  mutate(`quadrant (Q)` = rep(1:4, length.out = nrow(reshaped_data)))
LAI_values$Date <- as.Date(LAI_values$Date, format = "2%Y%m%d")
  

# Get mean values of LAI
LAI_values$LAI <- as.numeric(LAI_values$LAI)
class(LAI_values$LAI)

mean_LAI_values <- LAI_values %>%
  group_by(Date) %>%
  summarize(mean_LAI = mean(LAI)) %>%
  ungroup() %>%
  select(Date, mean_LAI)


# Get older data LAI ------------------------------------------------------

oldLAI <- read.csv("C:\\Users\\pavla\\OneDrive\\Documents\\Nuptake_project\\Nuptake_final\\CropHeight+LAI\\LAI\\oldLAIvalues.csv")
oldLAI$Date <- as.Date(oldLAI$Date, format = "%d.%m.%Y")


#merge all LAI together

mean_LAI_values <- full_join(mean_LAI_values, oldLAI, by = NULL, copy = FALSE)


# Merge with cropheight ---------------------------------------------------


CropHeight_LAI <- left_join(CropHeight_data_mean, LAI_values, by = c("Date", "quadrant (Q)")) 


