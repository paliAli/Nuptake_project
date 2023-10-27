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

  return image.updateMask(mask).divide(10000).set('system:time_start', image.get('system:time_start'));
}

var images = sentinel
  .filter(ee.Filter.date(startDate, endDate))
  .filterBounds(boundaries)
  .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', 20))
  .map(maskS2clouds);


function EVI(image) {
var evi = image.expression(
    '2.5*(NIR - Red) / (NIR + 6*Red - 7.5*Blue + 1)',
    {
      'NIR': image.select('B8'),
      'Red': image.select('B4'),
      'Blue': image.select('B2')
    }
  ).rename('EVI');
  return evi.copyProperties(image, ['system:index', 'system:time_start', 'CLOUDY_PIXEL_PERCENTAGE']);
}

var eviCollection = images.map(EVI);
print ('allimages', eviCollection);

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




var eviCollection = dailyComposite.map(EVI);
print ('eviCollection', eviCollection);

// Calculate mean NDVI for the 'boundaries' region
var meanEVI = eviCollection.map(function (image) {
  var meanValue = image.clip(boundaries).reduceRegion({
    reducer: ee.Reducer.mean(),
    geometry: boundaries,
    scale: 10
  }).get('EVI'); // Get the mean NDVI value from the dictionary
  return image.set('meanEVI', meanValue); // Convert to Feature and set 'meanNDVI' property
});


var nd = eviCollection.mean().clip(boundaries);
Map.addLayer(nd, { min: -1, max: 1}, 'EVI');


var chart = ui.Chart.feature.byFeature(meanEVI, 'system:time_start', ['meanEVI'])
  .setOptions({
    title: 'Sentinel-2 EVI Time Series',
    vAxis: { title: 'EVI' },
    hAxis: { title: 'Date', format: 'YYYY-MM' },
  });

print(chart);
