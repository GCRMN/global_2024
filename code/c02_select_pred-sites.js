// 1. Import data ----

var data_reefs = ee.FeatureCollection("users/jeremywicquart/global_2024_reefs");

// 2. Create the random points over coral reefs ----

var site_coords = ee.FeatureCollection.randomPoints(
    {region: data_reefs, points: 15000, seed: 0, maxError: 1}
);
  
// 3. Add site_id ----

var removeProperty = function(feat, property) {
  var properties = feat.propertyNames();
  var selectProperties = properties.filter(ee.Filter.neq('item', property));
  return feat.select(selectProperties);
};

var site_coords = site_coords.map(function(feat) {
  feat = removeProperty(feat, 'id');
  return feat.set('site_id', ee.Feature(feat).id());
});

// 4. Add site type ----
  
var site_coords = site_coords.map(function (feature) {
      return feature.set("type", "pred"); // add type of points (pred = prediction)
});

// 5. Data vizualisation ----

Map.addLayer(data_reefs);
Map.addLayer(site_coords.style({color: 'red'}));

// 6. Export the data ----

// 6.1 As an SHP file (for R) ----

Export.table.toDrive({
  collection:site_coords,
  folder:"GEE",
  fileNamePrefix:"site-coords_pred",
  fileFormat:"SHP",
  description:"site-coords_pred"
});

// 6.2 As an asset (for GEE) ----

Export.table.toAsset({
  collection:site_coords,
  description:"global_2024_site-coords_pred"
});
