// 1. Import data ----

var data_reefs = ee.FeatureCollection("users/jeremywicquart/global_2024/global_2024_reefs");

// 2. Create 100 km reef buffer ----

var reef_buffer = function(feature) {
  return feature.buffer(100000); // 100 km  
};

var data_reefs_buffer = data_reefs.map(reef_buffer);
  
// 2.1 Data vizualisation ----

Map.addLayer(data_reefs);
Map.addLayer(data_reefs_buffer);

// 2.2 Export the data ----

Export.table.toDrive({
  collection:data_reefs_buffer,
  folder:"GEE",
  fileNamePrefix:"reefs_buffer_100",
  fileFormat:"SHP",
  description:"reefs_buffer_100"
});

// 3. Create 50 km reef buffer ----

var reef_buffer = function(feature) {
  return feature.buffer(50000); // 50 km  
};

var data_reefs_buffer = data_reefs.map(reef_buffer);
  
// 3.1 Data vizualisation ----

Map.addLayer(data_reefs);
Map.addLayer(data_reefs_buffer);

// 3.2 Export the data ----

Export.table.toDrive({
  collection:data_reefs_buffer,
  folder:"GEE",
  fileNamePrefix:"reefs_buffer_50",
  fileFormat:"SHP",
  description:"reefs_buffer_50"
});

// 4. Create 20 km reef buffer ----

var reef_buffer = function(feature) {
  return feature.buffer(20000); // 20 km  
};

var data_reefs_buffer = data_reefs.map(reef_buffer);
  
// 4.1 Data vizualisation ----

Map.addLayer(data_reefs);
Map.addLayer(data_reefs_buffer);

// 4.2 Export the data ----

Export.table.toDrive({
  collection:data_reefs_buffer,
  folder:"GEE",
  fileNamePrefix:"reefs_buffer_20",
  fileFormat:"SHP",
  description:"reefs_buffer_20"
});

// 5. Create 5 km reef buffer ----

var reef_buffer = function(feature) {
  return feature.buffer(5000); // 5 km  
};

var data_reefs_buffer = data_reefs.map(reef_buffer);
  
// 5.1 Data vizualisation ----

Map.addLayer(data_reefs);
Map.addLayer(data_reefs_buffer);

// 5.2 Export the data ----

Export.table.toDrive({
  collection:data_reefs_buffer,
  folder:"GEE",
  fileNamePrefix:"reefs_buffer_5",
  fileFormat:"SHP",
  description:"reefs_buffer_5"
});

Export.table.toAsset({
  collection:data_reefs_buffer,
  description:"reefs_buffer_5",
  assetId:"users/jeremywicquart/global_2024/reefs_buffer_5"
});
