# Nuptake_project

# Theory
Nitrogen is a fundamental building block for plant growth, impacting yield, quality, and overall crop performance. To provide plants with this essential element, nitrogen is introduced into the soil through N-fertilizers. However, excessive nitrogen application can lead to the accumulation of this nutrient in the soil, resulting in N2O emissions or nitrate leaching. On the other hand, insufficient nitrogen input can lead to reduced crop yield, therefore it is crucial to optimize the fertilizer application rate. Knowing the nitrogen uptake of the crop can help us adjust the N-fertilizer input. Nitrogen uptake can be measured manually using destructive methods, in which the crop samples are obtained from the field and analyzed in the lab. Even though this method is very accurate, it is difficult and time-consuming. In contrast, satellite-derived vegetation indices provide quickly and easily field-scale crop information, which could potentially enable us to measure the nitrogen uptake of the crops in real time without the need for expensive instruments. 

# Goal
The main goal is to analyze the correlation between measured nitrogen uptake and vegetation indices and create a prediction model of plant nitrogen uptake.
I also tried to correlate the vegetation indices with crop height and LAI.

# Project Structure and Site Information
The project was done in two parts:

1. <b>Experimental part</b>
2. <b>Remote sensing and data analysis</b>

The site of interest is located in Oensingen, Switzerland. It is intensively managed field, which was a grassland in years 2021-2022, in year 2023 winter wheat was sown.

## Experimental part
During this part I obtained values of dry biomass weight and nitrogen concentration, which are needed to calculate the nitrogen uptake. This is done through biomass sampling and analysis of the samples in the laboratory.

### The Process:
+ Biomass sampling
  - the samples were taken using a sampling square of size 0.1 m<sup>2</sup>
  - 3 samples were taken per quadrant
+ LAI and crop height measurements (6 per quadrant)
+ Weighing of the biomass before and after drying, obtaining the <b>dry biomass weight</b>
+ Processing of the biomass
  - double milling and weighing for isolab
+ Analysis of the biomass in the laboratory, obtaining the <b>nitrogen concentration</b>

## Remote Sensing and Analysis
Remote sensing is the process of detecting and monitoring the physical characteristics of an area by measuring its reflected and emitted radiation at a distance, in this case from a satelite. With remote sensing, we can obtain information about the condition of the crops without any sampling and laboratory analysis.

Vegetation absorbs light in the red and blue parts of the electromagnetic spectrum for photosynthesis, while it reflects light in the near-infrared (NIR) spectrum. Non-vegetated surfaces, like soil and water, have different reflectance patterns. <br>
Plants with sufficient nitrogen content tend to have a higher chlorophyll content, which leads to increased absorption of red light and increased reflection of NIR light.
As a result, **well-nourished plants typically have a higher NDVI** because the NIR reflectance is greater relative to the red reflectance.
Conversely, nitrogen-deficient plants have lower chlorophyll content, causing reduced NIR reflectance compared to the red reflectance, resulting in a lower NDVI.

### Used Methods
The programming languages used were R and javascript.

Used software:

1. <b>Google Earth Engine</b>
   - I obtained values of vegetation indices using Sentinel-2 satelite images
   - The values were then exported as a csv file

2. <b>R studio</b>
   - Processing of the VIs values
   - Creating time-series plots
   - Correlation of VIs and LAI, crop height, dry biomass weight, N concentration, N uptake
   - Creating linear regression model
  
3. <b>GIS</b>
   - Creating shapefile of the field
