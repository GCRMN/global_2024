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

var rectangleBoundsA = ee.Geometry.Rectangle([-100, 3, -60, 30]);

var aca_benthic_vector = aca_benthic_lower.reduceToVectors({
  geometry: rectangleBoundsA,
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
