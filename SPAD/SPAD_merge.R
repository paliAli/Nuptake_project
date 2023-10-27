setwd ("C:\\Users\\apavlackova\\Documents\\Fabio's data\\SPAD")
library (stringr)
library(tidyverse)
library(dplyr)
library(data.table)

#Loading the SPAD files
filelist = list.files(pattern = ".*.txt")

#Putting all the SPAD files into one dataset and obtaining the date from the file name
SPAD_data <- rbindlist(sapply(filelist, fread, simplify = FALSE),
                use.names = TRUE, idcol = "date")
SPAD_data$date <- str_extract (SPAD_data$date, "\\d{8}")


write.csv(SPAD_data, "SPAD_data.csv", row.names = FALSE)
SPAD_data <- fread ("C:\\Users\\apavlackova\\Documents\\Fabio's data\\SPAD\\SPAD_data.csv")

class (SPAD_data$FileName)

SPAD_data$date <- as.Date(SPAD_data$date, format = "%m/%d/%Y")


# Left_join SPAD and %N ---------------------------------------------------

SPAD_Nuptake <- left_join (SPAD_data, merged_data_CN, by = c('date','quadrant (Q)'))

#remove the duplicates

SPAD_Nuptake <- SPAD_Nuptake[!duplicated(SPAD_Nuptake$`SPAD_averaged`), ]
SPAD_Nuptake <- SPAD_Nuptake %>% na.omit()


# Mean SPAD per quadrant --------------------------------------------------

SPAD_Nuptake_mean <- SPAD_Nuptake %>%
  group_by(date, `quadrant (Q)`) %>%
  reframe(date = date, mean_SPAD = mean(`SPAD_averaged`), `%N corr.` = first(`%N corr.`))

SPAD_Nuptake_mean <- SPAD_Nuptake_mean[!duplicated(SPAD_Nuptake_mean$`mean_SPAD`), ]


# Creating a scatterplot --------------------------------------------------

library(ggplot2)
library(ggthemes)

linear_model <- nls(`%N corr.` ~ a + b * SPAD_averaged, 
                    data = SPAD_Nuptake, 
                    start = list(a = 1, b = 0.1))

linear_coef <- coef(linear_model)
a <- linear_coef["a"]
b <- linear_coef["b"]

#Get r squared
r_squared <- 1 - sum(residuals(linear_model)^2) / sum((SPAD_Nuptake$`%N corr.` - mean(SPAD_Nuptake$`%N corr.`))^2)


ggplot(SPAD_Nuptake_mean, aes(x = mean_SPAD, y = `%N corr.`)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) + #prida regression line 
  theme_minimal()+
  ggtitle ("Relationship between SPAD and N-content") +
  labs(x = "average SPAD",
       y = "N-content (%)")+
  theme(axis.title.x = element_text(margin = margin(t = 20), size = 12),
        axis.title.y = element_text(margin = margin(r = 20), size = 12),
        plot.title = element_text (margin = margin (b = 20), size = 20))+
  # Add regression equation and correlation coefficient as annotation
  annotate("text",
           x = min(SPAD_Nuptake_mean$mean_SPAD) + 0.1, 
           y = max(SPAD_Nuptake_mean$`%N corr.`) - 0.3,
           label = paste("y =", format(a, digits = 2), 
                         "+", 
                         format(b, digits = 2), "* x",
                         "\nR2 =", round(r_squared, 2),
                         "\nCorrelation:", round(cor(SPAD_Nuptake$SPAD_averaged, SPAD_Nuptake$`%N corr.`), 2)),
           hjust = 0, vjust = 1, color = "black", size = 4)


ggplot(SPAD_Nuptake, aes(x = SPAD_averaged, y = `%N corr.`, shape = factor(`quadrant (Q)`))) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1, aes(group = 1)) + #prida regression line 
  theme_minimal()+
  ggtitle ("Relationship between SPAD and N-content") +
  labs(x = "average SPAD",
       y = "N-content (%)")+
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15),
        plot.title = element_text (margin = margin (b = 20), size = 20))


# SPAD divided by quadrant ------------------------------------------------


ggplot(SPAD_Nuptake, aes(x = SPAD_averaged, y = `%N corr.`)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) + #prida regression line 
  theme_minimal()+
  ggtitle ("Relationship between SPAD and N-content") +
  labs(x = "average SPAD",
       y = "N-content (%)")+
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15),
        plot.title = element_text (margin = margin (b = 20), size = 20))+
  facet_wrap(~`quadrant (Q)`)


# SPAD divided by date ----------------------------------------------------

ggplot(SPAD_Nuptake, aes(x = SPAD_averaged, y = `%N corr.`)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) + #prida regression line 
  theme_minimal()+
  ggtitle ("Relationship between SPAD and N-content") +
  labs(x = "average SPAD",
       y = "N-content (%)")+
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(margin = margin(t = 20), size = 15),
        axis.title.y = element_text(margin = margin(r = 20), size = 15),
        plot.title = element_text (margin = margin (b = 20), size = 20))+
  facet_wrap(~`date`)

