// 1. Import data ----

var aca_benthic = ee.Image("ACA/reef_habitat/v2_0")
  .select('reef_mask');
  
// 2. Decrease resolution ----

var aca_benthic_proj = aca_benthic.projection();

var aca_benthic_lower = aca_benthic.reproject(aca_benthic_proj, null, 500) // Change resolution in meters
    .reduceResolution({
      reducer: ee.Reducer.mean(),
      maxPixels: 1024}).int();

// 3. Convert to vector ----

var rectangleBoundsA = ee.Geometry.Rectangle(
  [-180, -50, -0.001, 50]
);

var rectangleBoundsB = ee.Geometry.Rectangle(
  [0.001, -50, 180, 50]
);

var rectangleBounds = ee.FeatureCollection([rectangleBoundsA, rectangleBoundsB]);

var aca_benthic_vector = aca_benthic_lower.reduceToVectors({
  geometry: rectangleBounds,
  crs: aca_benthic_lower.projection(),
  scale: 500,
  maxPixels:1e20,
  geometryType: 'polygon',
  eightConnected: false,
  labelProperty: 'zone',
  reducer: ee.Reducer.countEvery()
});

// 4. Data vizualisation ----

Map.addLayer(aca_benthic);
Map.addLayer(aca_benthic_lower, {palette: 'red'});
Map.addLayer(aca_benthic_vector, {palette: 'green'});

// 5. Export the data ----

Export.table.toDrive({
  collection:aca_benthic_vector,
  folder:"GEE",
  fileNamePrefix:"reef-aca",
  fileFormat:"SHP",
  description:"reef-aca"
});

// 6. Create 100 km reef buffer ----

var reef_buffer = function(feature) {
  return feature.buffer(100000); // 100 km  
};

var aca_benthic_buffer = aca_benthic_vector.map(reef_buffer)
  .union();

// 7. Data vizualisation ----

Map.addLayer(aca_benthic_buffer);

// 8. Export the data ----

Export.table.toDrive({
  collection:aca_benthic_buffer,
  folder:"GEE",
  fileNamePrefix:"reef-aca-buffer",
  fileFormat:"SHP",
  description:"reef-aca-buffer"
});
