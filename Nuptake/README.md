# Nitrogen Uptake
Nitrogen uptake is the process by which plants absorb and assimilate these nitrogen compounds from their surrounding environment, allowing them to build essential molecules such as proteins, chlorophyll, and nucleic acids. Nitrogen deficiencies in plants are often associated with reduced chlorophyll content, which is an essential pigment for photosynthesis. As a result, when plants lack sufficient nitrogen, their foliage tends to exhibit a paler, less vibrant green color. Thanks to this change in chlorophyll content, multi-spectral analysis can be used to assess the nitrogen status of the crops.
Knowing the nitrogen uptake of the crop can help us adjust the N-fertilizer input.

## N uptake calculation
Nitrogen uptake is calculated using the equation:

**Nuptake (g/m<sup>2</sup>) = (dry biomass weight) x (N concentration)/100**

The biomass weight and nitrogen concentration values were obtained in the experimental part of the project.
In this case, the dry biomass weight was also multiplied by 10, since the samples were taken using a sampling square of the size 0,1 m<sup>2</sup>.

# Correlation of Calculated Nitrogen Uptake and VIs
Values of each vegetation index were plotted against the measured nitrogen uptake.
Most values of vegetation indices and nitrogen uptake were not obtained on the same day, mainly due to the limitation of satellite images which are only taken every second day and some of them were filtered out due to high cloud coverage. 

First, I used mean vales for the whole field, but this resulted in only 11 data points!
### Next steps
- [x] Use mean values per quadrant and plot vegetation indices against nitrogen uptake
- [ ] Train a linear regression model

# Linear regression model
Separate the data in 70 % training data, 30 % testing data
