library(data.table)
library(purrr)

# Define a function to extract quadrant number from file name
get_quadrant <- function(filename) {
  if (grepl("Q1", filename)) return(1)
  if (grepl("Q2", filename)) return(2)
  if (grepl("Q3", filename)) return(3)
  if (grepl("Q4", filename)) return(4)
  return(NA)
}

setwd("C:\\Users\\apavlackova\\Documents\\GoogleEarthEngine\\MCARI\\quadrants")

# Get the list of CSV files
file_list <- list.files(path = "C:\\Users\\apavlackova\\Documents\\GoogleEarthEngine\\MCARI\\quadrants", pattern = "*.csv")

# Read and process each file, adding the quadrant column
MCARI_quadrants <- map_df(file_list, ~ {
  data <- read.csv(file.path("C:\\Users\\apavlackova\\Documents\\GoogleEarthEngine\\MCARI\\quadrants", .x))
  quadrant <- get_quadrant(.x)
  data$quadrant <- quadrant
  data
})

MCARI_quadrants <- arrange(MCARI_quadrants, date)
MCARI_quadrants <- na.omit(MCARI_quadrants)
MCARI_quadrants$date <- as.Date(MCARI_quadrants$date, format = "%d-%b-%y")


MCARI_quadrants_plot <- MCARI_quadrants %>%
  ggplot(aes(x = date, y = meanMCARI, color = factor(quadrant)))+
  geom_point(size = 2, alpha = 0.5)+
  geom_line(linewidth = 1)+
  labs(x = "Date", y = "MCARI", title = "MCARI quadrants")+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15),
        plot.title = element_text (margin = margin (b = 20), size = 20))+
  labs(colour="Quadrant")

MCARI_quadrants_plot
