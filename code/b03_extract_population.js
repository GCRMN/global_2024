// A. Import data -------------------------------------------------------------------------------

// 1. Import GPW count ----

var data_pop = ee.ImageCollection("CIESIN/GPWv411/GPW_Population_Count")
                  .select('population_count');

// 2. Import buffer reef ----

var buffer_reef = ee.FeatureCollection("users/jeremywicquart/global_2024_reefs_buffer");

Map.addLayer(buffer_reef);

// 3. Import subregions ----

var data_subregion = ee.FeatureCollection("users/jeremywicquart/global_2024_subregion");

Map.addLayer(data_subregion);

// B. Population within 20 km from coral reefs ---------------------------------------------------

// 1. Empty Collection to fill ----

var ft = ee.FeatureCollection(ee.List([]));

// 2. Create function to extract population ----

var fill = function(img, ini) {
  // type cast
  var inift = ee.FeatureCollection(ini);

  // gets the values for the points in the current img
  var ft2 = img.reduceRegions({
    reducer: ee.Reducer.sum(),
    collection: buffer_reef,
    scale: 930,
    });
  
  // gets the date of the img
  var date = img.date().format();

  // writes the date in each feature
  var ft3 = ft2.map(function(f){return f.set("date", date)});

  // merges the FeatureCollections
  return inift.merge(ft3);
};

// 3. Apply the function ----

var data_results = ee.FeatureCollection(data_pop.iterate(fill, ft));

// 4. Export the data ----

Export.table.toDrive({
  collection:data_results,
  folder:"GEE",
  fileNamePrefix:"ind_human-pop_20km",
  fileFormat:"CSV",
  description:"ind_human-pop_20km",
  selectors:["area", "date", "sum"],
});

// C. Population per EEZ -----------------------------------------------------------------

// 1. Empty Collection to fill ----

var ft = ee.FeatureCollection(ee.List([]));

// 2. Create function to extract SST ----

var fill = function(img, ini) {
  // type cast
  var inift = ee.FeatureCollection(ini);

  // gets the values for the points in the current img
  var ft2 = img.reduceRegions({
    reducer: ee.Reducer.sum(),
    collection: data_area_eez,
    scale: 930,
    });
  
  // gets the date of the img
  var date = img.date().format();

  // writes the date in each feature
  var ft3 = ft2.map(function(f){return f.set("date", date)});

  // merges the FeatureCollections
  return inift.merge(ft3);
};

// 3. Apply the function ----

var data_results = ee.FeatureCollection(data_pop.iterate(fill, ft));

// 4. Export the data ----

Export.table.toDrive({
  collection:data_results,
  folder:"GEE",
  fileNamePrefix:"ind_human-pop_eez",
  fileFormat:"CSV",
  description:"ind_human-pop_eez",
  selectors:["area", "date", "sum"],
});
