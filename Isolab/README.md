# Theory
Isotopic laboratory analysis is a powerful tool used to determine the elemental composition of substances, allowing us to differentiate between various isotopes of an element. Nitrogen exists in 2 isotopes: Nitrogen-14 (99% of naturally occurring nitrogen) and Nitrogen-15. These isotopes have similar chemical properties but differ in their atomic mass due to the number of neutrons in their nuclei. Nitrogen-15 analysis involves determining the ratio of Nitrogen-15 to Nitrogen-14 within the nitrogen compounds present in the biomass. This ratio provides valuable information about various ecological and biological processes, such as identifying the sources of nitrogen uptake by plants. It provides information on the efficiency of nitrogen uptake at different growth stages and can help assess the efficiency of different fertilization practices. <br>
The total concentration of nitrogen within the biomass has been quantified.

The biomass samples have been analyzed in the isolab in 4 trays, from which I manually selected the values of isolab identity, delta N15 corr. and %N corr. and combined them in the files d15N2 and d13C2.

# Code explanation
In the R file "isolab_merge" I loaded the csv file "naming_convention_ISOLAB(new)" (only selecting specific columns) containing the information about the samples, and the files d15N2 and d13C2.
First I merged the d15N and d13C data frames using inner_join and then merged the result again with the naming convention. The resulting data frame "merged_data_CN" has been corrected - changed names of the columns, set values in date column as date, removed the values of grass and legumes (only kept mix material, because other data like biomass weights was not separated)

merged_data_CN was merged with biomass_samples data (contained in biomass_samples folder) - resulting data frame contains both values of nitrogen concentration and biomass weights

In the R file "d15N_timeseries" I first calculated the average delta 15 nitrogen (using merged_data_CN data frame that still contained all types of material, including grass and legume)
I created a time series plot showing the change in delta 15 N through time for all the vegetation types differentiated by color
I also plotted the delta 15 N changes for all 4 samples (differentiated by color) for each date for each vegetation type
