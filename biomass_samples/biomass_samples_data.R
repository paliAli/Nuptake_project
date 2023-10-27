
# Importing biomass samples -----------------------------------------------

setwd ("/Users/apavlackova/Documents/Fabio's data/biomass_samples/Older data_grassland/csv/")
library(tidyverse)
library(data.table)
library(dplyr)
library(stringr)

map_df

# Inserting date column ---------------------------------------------------

dates <- str_extract (list.files (path="/Users/apavlackova/Documents/Fabio's data/biomass_samples/biomass_weights", pattern = "*.csv"), "\\d{4}\\d{2}\\d{2}")
biomass_samples$date <- as.Date(biomass_samples$date, format="%Y%m%d")
dates


# Obtaining biomass -------------------------------------------------------

csv_dir = choose.dir()
biomass_samples <- list.files(path= "C:\\Users\\apavlackova\\Documents\\Fabio's data\\biomass_samples\\biomass_weights", pattern = "*.csv", full.names = TRUE) %>%
  map_df(~fread(.)) %>% 
  dplyr::select(1:5)%>%
  mutate(date = rep(dates, each = 12))%>%
  dplyr::select(date, everything())

biomass_samples$date <- as.Date(biomass_samples$date, format = "%Y%m%d")

# Counting wet-tara -------------------------------------------------------

biomass_samples <- biomass_samples %>%
  mutate(wettara = `wet (g)` - `tara (g)`) %>%
  rename ('wet-tara' = wettara)

# Counting dry-tara -------------------------------------------------------

biomass_samples <- biomass_samples %>%
  mutate(drytara = `dry (g)` - `tara (g)`) %>%
  rename ('dry-tara' = drytara) #nemuzu mit sloupec obsahujici - v nazvu

# Counting Water content (%) ----------------------------------------------

biomass_samples <- biomass_samples %>%
  mutate('WC(%)' = ((`wet-tara` - `dry-tara`)/`wet-tara`)*100)

write.csv(biomass_samples, "biomass_samples.csv", row.names = FALSE)


# Residue and stubble -----------------------------------------------------

residue <- read.csv(choose.files())
residue <- residue %>%
  rename("date" = "ï..date")

residue$date <- as.Date(residue$date, format = "%d.%m.%Y")

stubble <- read.csv(choose.files())
stubble <- stubble %>%
  rename("date" = "ï..date")

stubble_residue <- bind_rows(stubble, residue, id = NULL)

stubble$date <- as.Date(stubble$date, format = "%d.%m.%Y")

stubble_residue <- stubble_residue %>%
  rename("WC(%)" = "WC....")

#older data -----------------------------
biomass_samples_old <- list.files (path="/Users/apavlackova/Documents/Fabio's data/biomass_samples/Older data_grassland/csv/", pattern = "*.csv") %>%
  map_df (~fread(.))


biomass_samples_old$Date <- as.Date(biomass_samples_old$Date, format = "%d.%m.%Y")
biomass_samples_old <- biomass_samples_old %>%
  mutate('WC(%)' = ((`wet-tara` - `dry-tara`)/`wet-tara`)*100)


# Joining all biomasss together --------------------------------------------------------
biomass_samples_all <- full_join(biomass_samples, biomass_samples_old, by = c("date" = "Date", 'tara (g)', 'wet (g)', 'dry (g)', 'wet-tara', 'dry-tara', 'WC(%)'))

#Changing harvest sample to sample
biomass_samples_all$sample <- ifelse(grepl("spot 1", biomass_samples_all$`Harvest Sample`), 1, biomass_samples_all$'sample')


# Adding harvest samples

harvest_samples <- list.files(path= "C:\\Users\\apavlackova\\Documents\\Fabio's data\\biomass_samples\\biomass_weights\\harvest_samples", pattern = "*.csv", full.names = TRUE) %>%
  map_df(~fread(.)) %>% 
  dplyr::select(1:6)%>%
  mutate(date = rep(dates, c(2, 4, 4)))%>%
  dplyr::select(date, everything())

dates <- str_extract (list.files (path="/Users/apavlackova/Documents/Fabio's data/biomass_samples/biomass_weights/harvest_samples", pattern = "*.csv"), "\\d{4}\\d{2}\\d{2}")
harvest_samples$date <- as.Date(harvest_samples$date, format="%Y%m%d")

harvest_samples <- harvest_samples %>%
  mutate('WC(%)' = ((`wet-tara` - `dry-tara`)/`wet-tara`)*100)


biomass_samples_all <- full_join(biomass_samples_all, harvest_samples, by = c("date" = "date", 'sample', 'tara (g)', 'wet (g)', 'dry (g)', 'wet-tara', 'dry-tara', 'WC(%)'))

# full join biomass_samples and isolab data -------------------------------

merged_data <- full_join (biomass_samples_all, merged_data_CN, by = c('date', 'quadrant (Q)', 'sample'))

merged_data <- full_join(merged_data, stubble_residue, by = c("date", "Material", "quadrant (Q)", "sample"))

#I have to manually move all the .x and .y into just one common column

merged_data$'WC(%)' <- ifelse(
  !is.na(merged_data$'WC(%).x'), 
  merged_data$'WC(%).x', 
  merged_data$'WC(%).y'
)

merged_data <- merged_data[, -c(4:9, 19:23)]
