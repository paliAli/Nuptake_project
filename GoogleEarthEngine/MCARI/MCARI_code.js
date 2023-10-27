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


function MCARI(image) {
var mcari = image.expression(
    '((VNIR - Red)-0.2* (VNIR - Green))* (VNIR / Red)',
    {
      'VNIR': image.select('B5'),
      'Red': image.select('B4'),
      'Green': image.select('B3')
    }
  ).rename('MCARI');
  return mcari.copyProperties(image, ['system:index', 'system:time_start', 'CLOUDY_PIXEL_PERCENTAGE']);
}

var mcariCollection = images.map(MCARI);
print ('allimages', mcariCollection);

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



var mcariCollection = dailyComposite.map(MCARI);
print ('mcariCollection', mcariCollection);

// Calculate mean NDVI for the 'boundaries' region
var meanMCARI = mcariCollection.map(function (image) {
  var meanValue = image.clip(boundaries).reduceRegion({
    reducer: ee.Reducer.mean(),
    geometry: boundaries,
    scale: 10
  }).get('MCARI'); // Get the mean NDVI value from the dictionary
  return image.set('meanMCARI', meanValue); // Convert to Feature and set 'meanNDVI' property
});

/*
var nd = ndreCollection.mean().clip(boundaries);
Map.addLayer(nd, { min: -1000, max: 1000}, 'CIR');
*/

var chart = ui.Chart.feature.byFeature(meanMCARI, 'system:time_start', ['meanMCARI'])
  .setOptions({
    title: 'Sentinel-2 MCARi Time Series',
    vAxis: { title: 'MCARI' },
    hAxis: { title: 'Date', format: 'YYYY-MM' },
  });

print(chart);