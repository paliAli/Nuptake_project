# Vegetation indices
I obtained 5 vegetation indices <b>(NDVI, NDRE, MCARI, EVI, GNDVI)</b> using Google Earth Engine from Sentinel 2 level 2-A images using the javascript code which can be found in the folder of each vegetation index. 
The vegetation indices values were calculated using following equations
+ NDVI = (B8 - B4)/(B8 + B4) where B8 corresponds to the NIR band and B 4 to the Red band
+ NDRE 
+ MCARI
+ EVI
+ GNDVI


The data was downloaded as a csv file (can be found in the folder) which is named using the format "nameofvegetationindex_scalex_cloudx", where "scale" means the resolution of individual pixels and "cloud" corresponds to the maximum cloud coverage of the image. Next I uploaded the data into R studio, processed it and correlated it with LAI, Crop height and nitrogen uptake.
The vegetation indices data is obtained in the time interval between 2021-01-24 and 2023-09-11, each dataset containing 51 values. NDVI was also obtained for each quadrant in the time interval between 2020-09-21 and 2023-06-28 

## Available data
Each VI has its own folder containing:
+ the csv file
+ code for its calculation in Google Eart Engine
+ R files named NDVI.R, NDRE.R, MCARI.R, EVI.R and GNDVI.R
+ a folder called results containing the created plots
+ folders NDVI and MCARI also obtain folder called quadrants which obtains 4 csv files with the VIs values for each quadrant.

# Explanation of the R code

NDVI.R
1. Upload data (the working directory and pathways have to be modified) from csv file and correct it - omit NAs, rename date column and set the values as date, remove values of NDVI lower than 0.1 because it is most likely influenced by soil reflectance or cloud cover and not a true value
2. Plot the time series using ggplot: x-axis is the date, y-axis is the mean value of NDVI
3. Obtain the values for each quadrant (csv files are in GoogleEarthEngine\\NDVI\\quadrants) using a function:
	- function get_quadrant checks the name of each file and returns the number of quadrant written in the name
	- create data frame NDVI_quadrants, which combines all 4 csv files and add column "quadrant" with the number of quadrant obtained using the get_quadrant function
4. Plot the time series using ggplot: x-axis is the date, y-axis is the mean value of VI for the quadrant. I used facet_wrap to separate the plot in 4 by the quadrant.
5. Use fuzzy_inner_join to merge mean LAI (without values in June) and NDVI values by date in interval of 7 days. Because the date interval is so big, some values will be matched 2 times -> create new column called date_difference, arrange the data from lowest to highest date difference and select first value so that I only keep the combination with lower date difference. !After the cut the NDVI values are lower so I have to remove the rows after the cut which would get matched with LAI values before the cut
6. Create linear_model of NDVI and LAI, obtain the intercept, slope and R squared
7. Plot linear regression - NDVI on x-axis, LAI on y-axis and show the equation and value of R squared on the plot
8. Repeat steps 5-7 but with NDVI_quadrants (can only use new values of LAI, because before it was not separated per quadrant)
9. Repeat steps 5-8 but with CropHeight instead of LAI
10. Fuzzy_inner_join to merge N uptake (the dataset contains N content, biomass weight and from them calculated N uptake) and NDVI in interval of 7 days !values of VIs after cut are artifically lower and do not correspond to the N uptake -> have to manually remove dates after cut which would otherwise match and plot correlation between N content and NDVI
11. Plot correlation between NDVI and N content, biomass weight

NDRE.R
1. Upload data (the working directory and pathways have to be modified) from csv file and correct it - omit NAs, rename date column and set the values as date, remove values of NDRE lower than 0.1 because it is most likely influenced by soil reflectance or cloud cover and not a true value
2. Plot the time series using ggplot: x-axis is the date, y-axis is the mean value of NDRE
3. Use fuzzy_inner_join to merge mean LAI (without values in June) and NDRE values by date in interval of 10 days. Because the date interval is so big, some values will be matched 2 times -> create new column called date_difference, arrange the data from lowest to highest date difference and select first value so that I only keep the combination with lower date difference.
4. Create linear_model of NDRE and LAI, obtain the intercept, slope and R squared
5. Plot linear regression - NDVI on x-axis, LAI on y-axis and show the equation and value of R squared on the plot
6. Repeat steps 3-5 but with CropHeight instead of LAI
7. Fuzzy_inner_join to merge N uptake (the dataset contains N content, biomass weight and from them calculated N uptake) and NDRE in interval of 7 days !values of VIs after cut are artifically lower and do not correspond to the N uptake -> have to manually remove dates after cut which would otherwise match and plot correlation between N content and NDRE
8. Plot correlation between NDRE and N content, biomass weight

MCARI.R, EVI.R, GNDVI.R
- same as NDRE

maybe do LAI and cropheight only until June? (Tried it for CH with EVI and GNDVI)

Nuptake: EVI.R, GNDVI.R
1. Upload data (the working directory and pathways have to be modified) from csv file and correct it - omit NAs, rename date column and set the values as date, remove values of GNDVI lower than 0.1 (also remove EVI from the same date - the value was abnormally big in this case) because it is most likely influenced by soil reflectance or cloud cover and not a true value
2. Plot the time series using ggplot
3. Use fuzzy_inner_join to merge N uptake and the VIs in interval of 7 days (+ remove the same dates after cuts as before) and plot correlation between N uptake and VIs
