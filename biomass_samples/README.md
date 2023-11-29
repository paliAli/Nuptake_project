# Biomass sampling
The biomass was sampled on the study site using a 0.1 m2 sampling square <br>
3 random samples were collected per quadrant (4 quadrants) -> **12 samples per date** 
  - the data can be found in subfolders 'Older data_grassland' and 'biomass_weights' as csv files
  - The csv files contain the date (either as a column or in the name of the file), wet and dry weight, and data in the subfolder 'biomass_weights' also contains information about the quadrant, sample, and water content

## R code structure
First, data from the subfolder 'biomass_weights' were obtained. <br>
- The data does not contain a date column so the date was obtained from the file name
- The data was loaded in R and the date column was added
- Additional values were calculated

Next, the data of stubble, residues, harvest samples, and older data were added and all data frames were merged using the full_join
- The older data contained a column called 'harvest sample' so I changed it to sample

Lastly, the resulting 'biomass_samples_all' was merged with data from the isolab
- there were some issues with the merging where there were duplicates of some rows for some reason
