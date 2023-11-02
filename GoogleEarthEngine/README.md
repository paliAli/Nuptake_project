# Vegetation Indices
I obtained 5 vegetation indices <b>(NDVI, NDRE, MCARI, EVI, GNDVI)</b> using Google Earth Engine from Sentinel 2 level 2-A images using the javascript code which can be found in the folder of each vegetation index. 
The vegetation indices values were calculated using following equations
+ NDVI = (NIR - Red)/(NIR + Red) 
+ NDRE = (NIR - VNIR)/(NIR + VNIR) 
+ MCARI = (VNIR - Red)-0.2* (VNIR - Green))* (VNIR / Red)
+ EVI = 2.5*(NIR - Red) / (NIR + 6 * Red - 7.5 * Blue + 1)
+ GNDVI = (NIR - Green)/(NIR + Green)

The data was downloaded as a csv file (can be found in the folder of each vegetation index (VI) which is named using the format "nameofvegetationindex_scalex", where "scale" means the resolution of individual pixels. Next I uploaded the data into R studio, processed it and correlated it with LAI, Crop height and nitrogen uptake.
The vegetation indices data is obtained in the time interval between 2021-01-01 and 2023-10-28, each dataset containing 51 values. NDVI and MCARI was also obtained for each of the four quadrants.
# Description of the Google Earth Engine Code

## Import of Data
+ Image collection 'Harmonized Sentinel-2 MSI: MultiSpectral Instrument, Level-2A' available directly from GEE, which I named sentinel
+ Shapefile of the field which is available in the GIS folder as 'manual_measurement_area', which I called Boundaries
  - For the VI values per quadrant I imported the shapefile of each quadrant which are available in the GIS/subsection folder
+ 'Sentinel-2: Cloud Probability' image collection containing the cloud probability of each image available from GEE, which I named s2cloudless

## Image Collection Filtering
1. I set the desired startDate and endDate (in this case 2021-01-01 and 2023-10-28) and filtered the images in sentinel image collection by the date, Boundaries and I also removed images with snow cover higher than 20 % (the information about snow cover is included in the image properties)
2. I filtered the s2cloudless image collection by the date and Boundaries

Since the s2cloudless only contains the cloud probability property of the image and not the spectral bands needed for the calculation of vegetation indices I combined it with the sentinel image collection by system:index. This way I obtained a s2cloudlessimages image collection obtaining both the information about cloud probability and spectral bands.

## Cloud Masking Process
Cloud cover can negatively affect the VIs values, therefore it is crucial to account for cloud cover in satellite imagery. To address this, I have implemented a cloud masking process using the Sentinel-2 data and the s2cloudless dataset. The process involves the following steps:

### 1. Image Collection Filtering
I first filter the images so that they meet my specific criteria. This includes:
+ date range 2021-01-01 to 2023-10-28
+ region of interest: I uploaded the shapefile "manual_measurements_area" located in the GIS folder and named it "Boundaries"
+ cloud cover percentage: I used the property of the images CLOUDY_PIXEL_COVERAGE, filltering out images which are more than 50 % covered by clouds
+ snow and ice percentage: I excluded images containing more than 20 % snow

### 2. s2cloudless Dataset Integration
+ I imported sentinel-2: Cloud Probability and filtered it by the same date and region as the sentinel 2 image collection
+ I joined the filtered s2cloudless dataset to the Sentinel-2 images using a common property 'system:index.'
The resulting s2cloudlessimages collection contains both information about the bands needed to calculate the VIs and the cloud probability

### 3. Cloud Probability Calculation and Application
+ I calculate the mean cloud probability within the Boundaries for each image. The cloud probability information is obtained from the 'probability' band in s2cloudless property of the image
+ The probability was not present for all the pictures so I had to first check its availability
+ I map the cloud probability on the image collection and filter out images with probability higher than 20 %

### 4. Cloud masking
To ensure the analysis is minimally impacted by clouds, I applied a cloud masking function that accounts for different cloud and shadow classes. This function is mapped to the images with low cloud probability to create 'maskedImages.'
+ The masking is done using the scl band of the images, selecting the values 3,7,8,9 which corresponds to clouds and cloud shadows

After the cloud mask was applied, there were multiple images for some of the days so I filtered them, only keeping the image with least cloud cover per day.

## Calculation of Vegetation Indices 
The vegetation indices were calculated using their specific formulas. Next, the average value of the VI for the Boundaries area was calculated and the data was then plotted and downloaded as csv file.

First, I used a different cloud masking process which I later changed because it was filtering out too many images. I kept the time-series plots, where the new ones have (2) next to their name.

# Available Data
Each VI has its own folder containing:
+ the csv file
+ code for its calculation in Google Eart Engine
+ R files named NDVI.R, NDRE.R, MCARI.R, EVI.R and GNDVI.R
+ a folder called results containing the created plots
+ folders NDVI and MCARI also obtain folder called quadrants which obtains 4 csv files with the VIs values for each quadrant.

# Structure of the R codes

After obtaining the values of vegetation indices from the satellite images, I loaded them into R studio where I further refined the data and plotted it. The process is as follows for the individual files:

## NDVI.R
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

## NDRE.R
1. Upload data (the working directory and pathways have to be modified) from csv file and correct it - omit NAs, rename date column and set the values as date, remove values of NDRE lower than 0.1 because it is most likely influenced by soil reflectance or cloud cover and not a true value
2. Plot the time series using ggplot: x-axis is the date, y-axis is the mean value of NDRE
3. Use fuzzy_inner_join to merge mean LAI (without values in June) and NDRE values by date in interval of 10 days. Because the date interval is so big, some values will be matched 2 times -> create new column called date_difference, arrange the data from lowest to highest date difference and select first value so that I only keep the combination with lower date difference.
4. Create linear_model of NDRE and LAI, obtain the intercept, slope and R squared
5. Plot linear regression - NDVI on x-axis, LAI on y-axis and show the equation and value of R squared on the plot
6. Repeat steps 3-5 but with CropHeight instead of LAI
7. Fuzzy_inner_join to merge N uptake (the dataset contains N content, biomass weight and from them calculated N uptake) and NDRE in interval of 7 days !values of VIs after cut are artifically lower and do not correspond to the N uptake -> have to manually remove dates after cut which would otherwise match and plot correlation between N content and NDRE
8. Plot correlation between NDRE and N content, biomass weight

## MCARI.R, EVI.R, GNDVI.R
- same as NDRE

# Observations
The values for NDVI and NDRE from 14.5.2022 was not accurate, possibly because of flowering? I did not include them in the correlation
However, for MCARI the values from Mai 2022 were improving the results of the correlation
