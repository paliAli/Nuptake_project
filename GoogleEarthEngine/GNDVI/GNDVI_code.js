var startDate = '2021-01-01';
var endDate = '2023-10-28';


var images = sentinel
  .filter(ee.Filter.date(startDate, endDate))
  .filterBounds(Boundaries)
  .filter(ee.Filter.lt('SNOW_ICE_PERCENTAGE', 20));

var cloudless = s2cloudless
    .filter(ee.Filter.date(startDate, endDate))
    .filterBounds(Boundaries);

// Join the filtered s2cloudless collection to the SR collection by the 'system:index' property.
var s2cloudlessimages = ee.ImageCollection(ee.Join.saveFirst('s2cloudless').apply({
        'primary': images,
        'secondary': cloudless,
        'condition': ee.Filter.equals({
            'leftField': 'system:index',
            'rightField': 'system:index'
        })
    }));

print('s2cloudless', s2cloudlessimages);

// Function to calculate the mean cloud probability within the ROI
var calculateCloudProbability = function(image) {
  // Try to access the 'probability' band, and handle cases where it's not available
  var cloudProbability = ee.Image(image.get('s2cloudless')).select('probability');
  
  // Check if 'probability' band is available
  var hasProbabilityBand = cloudProbability.bandNames().contains('probability');
  
  // Calculate mean cloud probability if the band is available
  var meanCloudProbability = hasProbabilityBand
    ? cloudProbability.reduceRegion({
        reducer: ee.Reducer.mean(),
        geometry: Boundaries,
        scale: 10
      }).get('probability')
    : 1.0;  // Set to 1.0 as a placeholder for images without 'probability'
  
  return image.set('CloudProbability', meanCloudProbability);
};

// Apply the calculateCloudProbability function to the Sentinel-2 collection
var s2cloudlessCollection = s2cloudlessimages.map(calculateCloudProbability);

// Filter images based on high cloud probability within the ROI
var CloudProbabilityThreshold = 20;  // Adjust the threshold as needed
var CloudlessImages = s2cloudlessCollection.filter(ee.Filter.lt('CloudProbability', CloudProbabilityThreshold));

// Display the filtered images
print('Images with low cloud probability:', CloudlessImages);



function s2_clear_sky (image){
  // 1.Locate SCL product
  var scl = image.select('SCL');
  
  var cloud_shadow = scl.eq(3);
  var cloud_low = scl.eq(7);
  var cloud_medium = scl.eq(8);
  var cloud_high = scl.eq(9);
  
  var cloud_mask = cloud_shadow.add(cloud_low).add(cloud_medium).add(cloud_high);
  return image.updateMask(cloud_mask.not());
  
}

var maskedImages = CloudlessImages.map(s2_clear_sky);

print ('maskedImages', maskedImages);


var gndvi = function (image) {
  var ndv = image.normalizedDifference(['B8', 'B3']); // B8 is the NIR band and B3 is the green band
  return ndv.copyProperties(image, ['system:index', 'system:time_start', 'CLOUDY_PIXEL_PERCENTAGE']);
};

var gndviCollection = maskedImages.map(gndvi);
print ('allimages', gndviCollection);

// Create a daily composite with the least cloudy image per day
var days = ee.List.sequence(
  ee.Date(startDate).millis(),
  ee.Date(endDate).advance(1, 'day').millis(),
  24 * 60 * 60 * 1000
);

var dailyComposite = ee.ImageCollection.fromImages(
  days.map(function (date) {
    var dailyImages = maskedImages
      .filterDate(ee.Date(date), ee.Date(date).advance(1, 'day'))
      .sort('CLOUDY_PIXEL_PERCENTAGE');
    return dailyImages.first();
  })
);



var gndviCollection = dailyComposite.map(gndvi);
print ('gndviCollection', gndviCollection);

// Calculate mean NDVI for the 'boundaries' region
var meanGNDVI = gndviCollection.map(function (image) {
  var meanValue = image.clip(Boundaries).reduceRegion({
    reducer: ee.Reducer.mean(),
    geometry: Boundaries,
    scale: 10
  }).get('nd'); // Get the mean NDVI value from the dictionary
  return image.set('meanGNDVI', meanValue); // Convert to Feature and set 'meanNDVI' property
});

var nd = gndviCollection.mean().clip(Boundaries);
Map.addLayer(nd, { min: 0, max: 1, palette: ["ffffff","d2e917","6bc228","3f8d26","2c5a23"] }, 'NDVI');



var chart = ui.Chart.feature.byFeature(meanGNDVI, 'system:time_start', ['meanGNDVI'])
  .setOptions({
    title: 'Sentinel-2 GNDVI Time Series',
    vAxis: { title: 'GNDVI' },
    hAxis: { title: 'Date', format: 'YYYY-MM' },
  });

print(chart);
