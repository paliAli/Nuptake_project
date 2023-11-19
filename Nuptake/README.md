# Nitrogen Uptake
Nitrogen uptake is the process by which plants absorb and assimilate nitrogen compounds from their surrounding environment, allowing them to build essential molecules such as proteins, chlorophyll, and nucleic acids. Nitrogen deficiencies in plants are often associated with reduced chlorophyll content, which is an essential pigment for photosynthesis. As a result, when plants lack sufficient nitrogen, their foliage tends to exhibit a paler, less vibrant green color. Plants that efficiently absorb nitrogen often exhibit higher levels of chlorophyll, leading to increased absorption of red light and increased reflection of near-infrared (NIR) light. By analyzing these spectral signatures, remote sensing can estimate nitrogen content in plants, providing insights into their health and nutritional status non-invasively.<br>
Knowing the nitrogen uptake of the crop can help us adjust the N-fertilizer input.

## N uptake calculation
Nitrogen uptake is calculated using the equation:

**Nuptake (g/m<sup>2</sup>) = (dry biomass weight) x (N concentration)/100**

The biomass weight and nitrogen concentration values were obtained in the experimental part of the project.
In this case, the dry biomass weight was also multiplied by 10, since the samples were taken using a sampling square of the size 0,1 m<sup>2</sup>.

# Correlation of Calculated Nitrogen Uptake and VIs
Values of each vegetation index were plotted against the calculated nitrogen uptake.
Most values of vegetation indices and nitrogen uptake were not obtained on the same day, mainly due to the limitation of satellite images which are only taken every second day, and due to some images being filtered out due to high cloud coverage. 

First, I used mean values for the whole field, but this resulted in only 11 data points!
### Next steps
- [x] Use mean values per quadrant and plot vegetation indices against nitrogen uptake
- [ ] Train a linear regression model

When using data per quadrant, nitrogen uptake values varied greatly for some dates, while vegetation indices stayed fairly constant throughout the whole field.
# Linear regression model
Separate the data in 70 % training data, 30 % testing data

# Structure of the R codes
The work was done in three R files: Nitrogen_uptake.R, Nitrogen_uptake_quadrant.R and VIs_Nuptake.R <br>

## Nitrogen_uptake.R
1. Calculate nitrogen uptake using the equation mentioned above for each sample
2. Obtain mean values per date
3. Create a time series of N uptake
4. Only keep values until June 2023
5. Use function fuzzy_inner_join to merge N uptake and NDVI by date within the interval of 7 days
6. Manually remove the row that would result in merging of pre-cut N uptake with NDVI values obtained after the cut
7. Create a linear model and plot the correlation between N uptake and NDVI
8. Repeat steps 5-7 for each vegetation index

## Nitrogen_uptake_quadrant.R
In earlier samplings, one sample was taken per quadrant, and the quadrant column was not present
1. Fill in the column quadrant for older values (copied the column sample)
2. Obtain mean N uptake values per quadrant and date
3. Only keep values until June 2023
4. Create a time series of N uptake for all 4 quadrants
5. Use fuzzy_inner_join to merge N uptake and NDVI by date and quadrant within the interval of 7 days
6. Manually remove the row that would result in merging of pre-cut N uptake with NDVI values obtained after the cut
7. Use geom_boxplot to identify outliers
8. *Remove the outliers (??)*
9. Create a linear model and plot the correlation between N uptake and NDVI
10. Repeat steps 5-7 for each vegetation index
