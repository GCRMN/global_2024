// 1. Load data ----

var polygons = ee.FeatureCollection('users/jeremywicquart/misc/gcrmn_regions');

var bedrock = ee.Image('NOAA/NGDC/ETOPO1').select('bedrock');

var seaMask = bedrock.lte(0).selfMask();

var seaVectors = seaMask.reduceToVectors({
  geometry: polygons.geometry(),
  scale: 5000,
  geometryType: 'polygon',
  eightConnected: true,
  maxPixels: 1e13
});

// 2. Remove inland portions of GCRMN regions ----

var polygonsAtSea = polygons.map(function(f) {
  var seaPart = seaVectors.filterBounds(f.geometry()).geometry().dissolve();
  var geom = f.geometry().intersection(seaPart, ee.ErrorMargin(100));
  
  return ee.Feature(geom)
    .copyProperties(f)
    .set('sea_area_m2', geom.area(1));
});

var polygons = polygonsAtSea.filter(ee.Filter.gt('sea_area_m2', 0));

// 3. Estimate the maritime area of each region ----

var resultat = polygons.map(function(feature) {
  var geom = feature.geometry();

  // Boîte englobante en coordonnées géographiques (EPSG:4326)
  var bbox = geom.bounds(1, 'EPSG:4326');

  // Les coordonnées du rectangle sont un anneau :
  // [[xmin, ymin], [xmax, ymin], [xmax, ymax], [xmin, ymax], [xmin, ymin]]
  var ring = ee.List(ee.List(bbox.coordinates().get(0)));

  var p0 = ee.List(ring.get(0)); // [xmin, ymin]
  var p2 = ee.List(ring.get(2)); // [xmax, ymax]

  var xmin = ee.Number(p0.get(0));
  var ymin = ee.Number(p0.get(1));
  var xmax = ee.Number(p2.get(0));
  var ymax = ee.Number(p2.get(1));

  var etendueLongDeg = xmax.subtract(xmin);
  var etendueLatDeg  = ymax.subtract(ymin);

  // Surface en m² -> millions de km²
  var surfaceM2 = geom.area(1);
  var surfaceMillionKm2 = surfaceM2.divide(1e12);

  return feature.set({
    xmin_deg: xmin,
    xmax_deg: xmax,
    ymin_deg: ymin,
    ymax_deg: ymax,
    etendue_longitudinale_deg: etendueLongDeg,
    etendue_latitudinale_deg: etendueLatDeg,
    surface_million_km2: surfaceMillionKm2
  });
});

// 4. Return the result ----

Export.table.toDrive({
  collection:resultat,
  folder:"GEE",
  fileNamePrefix:"data_maritime-area",
  fileFormat:"CSV",
  description:"data_maritime-area",
  selectors:["region", "surface_million_km2",
  "etendue_latitudinale_deg", "etendue_longitudinale_deg",
  "xmin_deg", "xmax_deg", "ymin_deg", "ymax_deg"]
});
