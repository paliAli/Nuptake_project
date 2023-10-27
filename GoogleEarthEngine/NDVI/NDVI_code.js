var startDate = '2021-01-01';
var endDate = '2023-09-12';


function maskS2clouds(image) {
  var qa = image.select('QA60');

  // Bits 10 and 11 are clouds and cirrus, respectively.
  var cloudBitMask = 1 << 10;
  var cirrusBitMask = 1 << 11;

  // Both flags should be set to zero, indicating clear conditions.
  var mask = qa.bitwiseAnd(cloudBitMask).eq(0)
    .and(qa.bitwiseAnd(cirrusBitMask).eq(0));

  return image.updateMask(mask).divide(10000).set('system:time_start', image.get('system:time_start'))
  .set('CLOUDY_PIXEL_PERCENTAGE', image.get('CLOUDY_PIXEL_PERCENTAGE'));
}

var images = sentinel
  .filter(ee.Filter.date(startDate, endDate))
  .filterBounds(Boundaries)
  .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', 20))
  .map(maskS2clouds);


var ndvi = function (image) {
  var ndv = image.normalizedDifference(['B8', 'B4']); // B8 is the NIR band and B4 is the red band
  return ndv.copyProperties(image, ['system:index', 'system:time_start', 'CLOUDY_PIXEL_PERCENTAGE']);
};

var ndviCollection = images.map(ndvi);
print ('allimages', ndviCollection);

// Create a daily composite with the least cloudy image per day
var days = ee.List.sequence(
  ee.Date(startDate).millis(),
  ee.Date(endDate).advance(1, 'day').millis(),
  24 * 60 * 60 * 1000
);

var dailyComposite = ee.ImageCollection.fromImages(
  days.map(function (date) {
    var dailyImages = images
      .filterDate(ee.Date(date), ee.Date(date).advance(1, 'day'))
      .sort('CLOUDY_PIXEL_PERCENTAGE');
    return dailyImages.first();
  })
);



var ndviCollection = dailyComposite.map(ndvi);
print ('ndviCollection', ndviCollection);

// Calculate mean NDVI for the 'boundaries' region
var meanNDVI = ndviCollection.map(function (image) {
  var meanValue = image.clip(Boundaries).reduceRegion({
    reducer: ee.Reducer.mean(),
    geometry: Boundaries,
    scale: 10
  }).get('nd'); // Get the mean NDVI value from the dictionary
  return image.set('meanNDVI', meanValue); // Convert to Feature and set 'meanNDVI' property
});

var nd = ndviCollection.mean().clip(Boundaries);
Map.addLayer(nd, { min: 0, max: 1, palette: ["ffffff","d2e917","6bc228","3f8d26","2c5a23"] }, 'NDVI');
var nd = ndviCollection.mean().clip(Boundaries);
Map.addLayer(nd, { min: 0, max: 1, palette: ["red"] }, 'NDVI');


var chart = ui.Chart.feature.byFeature(meanNDVI, 'system:time_start', ['meanNDVI'])
  .setOptions({
    title: 'Sentinel-2 NDVI Time Series',
    vAxis: { title: 'NDVI' },
    hAxis: { title: 'Date', format: 'YYYY-MM' },
  });

print(chart);