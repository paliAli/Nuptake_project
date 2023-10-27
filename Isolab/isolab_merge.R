
# Importing naming convention ---------------------------------


setwd ("C:\\Users\\apavlackova\\Documents\\Fabio's data\\Isolab")
library(tidyverse)
library(dplyr)
library(data.table)

setwd("C:\\Users\\apavlackova\\Documents\\Fabio's data\\Isolab\\naming_convention_ISOLAB(new).csv")

naming_convention <- fread ("C:\\Users\\apavlackova\\Documents\\Fabio's data\\Isolab\\naming_convention_ISOLAB(new).csv") %>%
  dplyr::select (`Date`, 5:9)

# Import d15N -------------------------------------------------------------

d15N <- fread ("C:\\Users\\apavlackova\\Documents\\Fabio's data\\Isolab\\d15N2.csv")
  


# Import d13C -------------------------------------------------------------

d13C <- fread ("C:\\Users\\apavlackova\\Documents\\Fabio's data\\Isolab\\d13C2.csv")


# N+C ---------------------------------------------------------------------

N_C <- inner_join (d15N, d13C, by = 'ISOLAB IDENTITY')

# Merge the data ----------------------------------------------------------

merged_data_CN <- full_join(N_C, naming_convention, by = 'ISOLAB IDENTITY')%>%
  rename (`date` = `Date`, `quadrant (Q)` = `Quadrant`, `sample` = `Sample`, `Delta_N` = `Delta_corr.x`, `Delta_C` = `Delta_corr.y`)

merged_data_CN$date <- as.Date (merged_data_CN$date, format = "%d.%m.%Y")

merged_data_CN <- merged_data_CN[!(grepl("grass", merged_data_CN$Material) | grepl("legume", merged_data_CN$Material)), ] #only keep the mix material from the older samples

write.csv(merged_data_CN, "merged_data.csv", row.names = FALSE)


# full join biomass_samples and isolab data -------------------------------

merged_data <- full_join (biomass_samples_all, merged_data_CN, by = c('date', 'quadrant (Q)', 'sample'))

write.csv (merged_data, "iso_biomass_merged.csv", row.names = FALSE)
