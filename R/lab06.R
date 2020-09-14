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
  mask(elev, basin) %>%
  elev * 3.281
writeRaster(elev, "data/basin-elev.tif")

bb = st_bbox(basin) %>%
  st_as_sfc() %>%
  st_transform(4326)

osm_b = opq(bb) %>%
  add_osm_feature(key = 'building') %>%
  osmdata_sf()
bldg_cent = osm_b$osm_b_polygons %>%
  st_centroid() %>%
  st_intersection(st_as_sfc(st_bbox(basin)))

osm_r = opq(bb) %>%
  add_osm_feature(key = 'building', value = "train_station")
railway = osm_r$osm_r_points

osm_w = opq(bb) %>%
  add_osm_feature(key = 'waterway', value = "stream") %>%
  osmdata_sf()
stream = osm_w$osm_w_lines %>%
  st_intersection(st_as_sfc(st_bbox(basin)))

# Terrain Analysis

# Hillshade
wbt_hillshade("data/basin-elev.tif", "data/basin-hillshade.tif")
r = raster("data/basin-hillshade.tif")
plot(r, box = FALSE, axes = FALSE, col = gray.colors(256, alpha = .5), legend = None)
plot(basin, box = FALSE, axes = FALSE, add = TRUE)
plot(stream, box = FALSE, axes = FALSE, col = "blue", add = TRUE)

# HAND
buffer = stream %>%
  st_transform(5070) %>%
  st_buffer(10) %>%
  st_transform(4326) %>%
  fasterize(buffer, elev) %>%
  writeRaster(buffer, "data/basin-river-networks.tif")

wbt_breach_depressions("data/basin-elev.tif", "data/basin-cond-elev.tif")

wbt_elevation_above_stream("data/basin-cond-elev.tif", "data/basin-river-networks.tif", "data/basin-hand.tif")
hand = raster("data/basin-hand.tif")
river_networks = raster("data/basin-river-networks.tif")
offset_hand = hand + 3.69
writeRaster(offset_hand, "data/basin-offset-hand.tif")


# 2017 Impact Assessment
offset_hand = raster("data/basin-offset-hand.tif")
threshold = function(x){ifelse(x > 10.02, NA, 1)}
m = calc(offset_hand, threshold)
flood = m * offset_hand

plot(r, box = FALSE, axes = FALSE, col = gray.colors(256, alpha = .5), legend = None)
plot(flood, box = FALSE, axes = FALSE, col = rev(blues9), add = TRUE)
plot(railway, box = FALSE, axes = FALSE, col = "green", cex = 1, pch = 16, add = TRUE)

depth = extract(flood, bldg_cent)
impact = depth %>%
  na.omit(depth)
count(impact)

non-impact =

plot(r, box = FALSE, axes = FALSE, col = gray.colors(256, alpha = .5), legend = None)
plot(flood, box = FALSE, axes = FALSE, col = rev(blues9), add = TRUE)
plot(impact, box = FALSE, axes = FALSE, col = "red", pch = 16, cex = .08, add = TRUE)
plot(non-impact, box = FALSE, axes = FALSE, col = "black", pch = 16, cex = .08, add = TRUE)
plot(railway, box = FALSE, axes = FALSE, col = "green", cex = 1, pch = 16, add = TRUE)

# Flood Inundation Map Library

sb = AOI::aoi_get("Santa Barbara")
sb_basin = basin %>%
  crop(basin, sb) %>%
  mask(basin, sb)
sb_flood = offset_hand %>%
  st_intersection(st_as_sfc(st_bbox(sb)))
sb_hillshade = r %>%
  st_intersection(st_as_sfc(st_bbox(sb)))

gifski::save_gif({
  for(i in 0:20){
    plot(sb_basin, box = FALSE, axes = FALSE)
    plot(sb_hillshade, box = FALSE, axes = FALSE, col = gray.colors(256, alpha = .5), legend = None, add = TRUE)
    plot(sb_flood, box = FALSE, axes = FALSE, col = rev(blues9), add = TRUE)}},
  gif_file = "data/mission-creek-film.gif",
  width = 600, height = 600,
  delay = .7, loop = TRUE)




