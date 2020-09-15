# Kiki Takeuchi
# Lab 06
# Flood Risk in Mission Creek: Past, Present, Future

#libraries
library(sf)
library(raster)
library(fasterize)
library(whitebox)
library(osmdata)
library(elevatr)

# Collecting Data
url = 'https://labs.waterdata.usgs.gov/api/nldi/linked-data/nwissite/USGS-11119750/basin'
basin = read_sf(url)

elev = get_elev_raster(basin, z = 13) %>%
  crop(elev, basin) %>%
  elev * 3.281
writeRaster(elev, "data/basin-elev.tif")

plot(basin)
plot(elev, add = TRUE)

bb = st_bbox(basin) %>%
  st_as_sfc() %>%
  st_as_sf() %>%
  st_transform(4326)

osm = opq(bb) %>%
  add_osm_feature(key = 'building') %>%
  osmdata_sf()
bldg_cent = osm$osm_polygons %>%
  st_centroid() %>%
  st_crop(basin)
# not sure how to crop to  basin

railway_pt = osm$osm_points

osm_w = opq(bb) %>%
  add_osm_feature(key = 'waterway', value = "stream") %>%
  osmdata_sf()
stream = osm_w$osm_lines %>%
  st_crop(basin)
# not sure how to crop to  basin
plot(stream$geometry)
plot(basin, add = TRUE)

# Terrain Analysis

# Hillshade
wbt_hillshade("data/basin-elev.tif", "data/basin-hillshade.tif")
r = raster("data/basin-hillshade.tif")
plot(r, box = FALSE, axes = FALSE, col = gray.colors(256, alpha = .5), legend = None)
plot(basin, add = TRUE)
plot(stream, col = "blue", add = TRUE)

# HAND
buffer = stream %>%
  st_transform(5070) %>%
  st_buffer(10) %>%
  fasterize(buffer, elev) %>%
  writeRaster(buffer, "data/basin-river-networks.tif")

wbt_breach_depressions("data/basin-elev.tif", "data/basin-cond-elev.tif")

wbt_elevation_above_stream("data/basin-cond-elev.tif", "data/basin-river-networks.tif", "data/basin-hand.tif")
hand = raster("data/basin-hand.tif")
river_networks = raster("data/basin-river-networks.tif")
offset_hand = hand + 3.69

# 2017 Impact Assessment

# Flood Inundation Map Library
