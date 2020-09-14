# Kiki Takeuchi
# Lab 06
# Flood Risk in Mission Creek: Past, Present, Future

#libraries
library(tidyverse)
library(sf)
library(raster)
library(fasterize)
library(whitebox)
library(knitr)
library(osmdata)
library(elevatr)

# Collecting Data
url = 'https://labs.waterdata.usgs.gov/api/nldi/linked-data/nwissite/USGS-11119750/basin'
basin = read_sf(url)

elev = get_elev_raster(basin, z = 13) %>%
  crop(basin) %>%
  mask(basin)
elev = elev * 3.281
writeRaster(elev, "data/mission-creek-elev.tif")
elev = raster("data/mission-creek-elev.tif")

bb = st_bbox(basin) %>%
  st_as_sfc() %>%
  st_transform(4326)

bldg = opq(bb) %>%
  add_osm_feature(key = 'building') %>%
  osmdata_sf()
bldg_cent = bldg$osm_polygons %>%
  st_centroid() %>%
  st_transform(crs(basin)) %>%
  st_intersection(basin)

rail = opq(bb) %>%
  add_osm_feature(key = 'railway', value = "station") %>%
  osmdata_sf()
railway = rail$osm_points %>%
  st_transform(crs(basin)) %>%
  st_intersection(basin)

water = opq(bb) %>%
  add_osm_feature(key = 'waterway', value = "stream") %>%
  osmdata_sf()
stream = water$osm_lines %>%
  st_transform(crs(basin)) %>%
  st_intersection(basin)

# Terrain Analysis

# Hillshade
wbt_hillshade("data/mission-creek-elev.tif", "data/mission-creek-hillshade.tif")
hillshade = raster("data/mission-creek-hillshade.tif")
plot(hillshade, box = FALSE, axes = FALSE, col = gray.colors(256, alpha = .5), legend = None, main = "Mission Creek Basin")
plot(basin, box = FALSE, axes = FALSE, add = TRUE)
plot(stream, box = FALSE, axes = FALSE, col = "blue", add = TRUE)

# HAND
buffer = stream %>%
  st_transform(5070) %>%
  st_buffer(10) %>%
  st_transform(4326) %>%
  fasterize(elev) %>%
  writeRaster("data/mission-creek-stream.tif")

wbt_breach_depressions("data/mission-creek-elev.tif", "data/mission-creek-cond-elev.tif")
wbt_elevation_above_stream("data/mission-creek-cond-elev.tif", "data/mission-creek-stream.tif", "data/mission-creek-hand.tif")

hand = raster("data/mission-creek-hand.tif")
river_networks = raster("data/mission-creek-stream.tif")
offset_hand = hand + 3.69
offset_hand[river_networks == 1] = 0
writeRaster(offset_hand, "data/mission-creek-offset-hand.tif")


# 2017 Impact Assessment
offset_hand = raster("data/mission-creek-offset-hand.tif")
offset_hand[offset_hand > 10.02] = NA

plot(hillshade, box = FALSE, axes = FALSE, col = gray.colors(256, alpha = .5), legend = FALSE)
plot(basin, add = TRUE)
plot(offset_hand, col = rev(blues9), add = TRUE)
plot(railway, col = "green", cex = 1, pch = 16, add = TRUE)

buildings = ifelse(!is.na(raster::extract(offset_hand, bldg_cent)), "red", "black")

plot(hillshade, box = FALSE, axes = FALSE, col = gray.colors(256, alpha = .5), main = paste(sum(buildings == "red"), "Buildings Impacted by the 2017 Mission Creek Flooding"), legend = FALSE)
plot(basin, add = TRUE)
plot(offset_hand, col = rev(blues9), add = TRUE)
plot(bldg_cent, col = buildings, pch = 16, cex = .08, add = TRUE)
plot(railway, col = "green", cex = 1, pch = 16, add = TRUE)

# Flood Inundation Map Library

sb = AOI::aoi_get("Santa Barbara")

sb_basin = basin %>%
  st_transform(crs(sb)) %>%
  st_intersection(sb)
sb_flood = raster("data/mission-creek-offset-hand.tif") %>%
  crop(sb)
sb_hillshade = hillshade %>%
  crop(sb)
bldg_sb = bldg_cent %>%
  st_transform(crs(sb))
  st_intersection(sb)
bldgs = ifelse(!is.na(raster::extract(sb_flood, bldg_sb)), "red", "black")

gifski::save_gif({
  for(i in 0:20){
    sb_flood[sb_flood > i] = NA

    plot(sb_hillshade, box = FALSE, axes = FALSE, col = gray.colors(256, alpha = .5), legend = FALSE, main = paste(sum(bldgs == "red"), "Imacted Buildings", i, "Foot Stage"))
    plot(sb_basin, add = TRUE)
    plot(sb_flood, col = rev(blues9), add = TRUE)
    plot(bldg_sb, col = bldgs, pch = 16, cex = .08, add = TRUE)
    }
  },gif_file = "data/mission-creek-film.gif",
    width = 600, height = 600,
    delay = .7, loop = TRUE)




