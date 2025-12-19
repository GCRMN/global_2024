// A. Import data -------------------------------------------------------------------------------

// 1. Import GPW count ----

var data_pop = ee.ImageCollection("CIESIN/GPWv411/GPW_Population_Count")
                  .select('population_count');

// 2. Import buffer reef ----

var buffer_reef = ee.FeatureCollection("users/jeremywicquart/global_2024/reefs_buffer_5");

Map.addLayer(buffer_reef);

// B. Population within 5 km from coral reefs (subregional) -------------------------------------

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

var data_results = ee.FeatureCollection(data_pop.iterate(fill, ft))

// 4. Export the data ----

Export.table.toDrive({
  collection:data_results,
  folder:"GEE",
  fileNamePrefix:"ind_human-pop_5km_subregion",
  fileFormat:"CSV",
  description:"ind_human-pop_5km_subregion",
  selectors:["region", "subregion", "date", "sum"],
});

// C. Population within 5 km from coral reefs (regional) -------------------------------------

// 1. Merge subregions into regions ----

// 1.1 List of regions ----

var regions = buffer_reef.aggregate_array('region').distinct();

// 1.2 Create a function to merge the subregions ----

var FctMergeSubregions = function(region_name) {
  
  // Filter subregions in region
  var filtered = buffer_reef.filter(ee.Filter.eq('region', region_name));

  // Merge polygons
  var subregion_merged = filtered.geometry().dissolve();

  // Return the merged geometry
  return ee.Feature(subregion_merged).set('region', region_name);
  
};

// 1.3 Map over the function and create a FeatureCollection ----

var buffer_reef = ee.FeatureCollection(regions.map(FctMergeSubregions));

// 2. Apply the function ----

var data_results = ee.FeatureCollection(data_pop.iterate(fill, ft));

// 3. Export the data ----

Export.table.toDrive({
  collection:data_results,
  folder:"GEE",
  fileNamePrefix:"ind_human-pop_5km_region",
  fileFormat:"CSV",
  description:"ind_human-pop_5km_region",
  selectors:["region", "subregion", "date", "sum"],
});

// D. Population within 5 km from coral reefs (global) -------------------------------------

// 1. Merge subregions into regions ----

var buffer_reef = buffer_reef.geometry().dissolve();

// 2. Apply the function ----

var data_results = ee.FeatureCollection(data_pop.iterate(fill, ft));

// 3. Export the data ----

Export.table.toDrive({
  collection:data_results,
  folder:"GEE",
  fileNamePrefix:"ind_human-pop_5km_global",
  fileFormat:"CSV",
  description:"ind_human-pop_5km_global",
  selectors:["region", "subregion", "date", "sum"],
});
