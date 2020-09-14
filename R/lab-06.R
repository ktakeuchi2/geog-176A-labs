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
  crop(basin) %>%
  mutate(elev = units::set_units("ft"),
         elev = units:: drop_units())
writeRaster(elev, "data/basin-elev.tif")

bldg = opq(bbox = basin) %>%
  add_osm_feature(key = 'building') %>%
  osmdata_sf()
bldg_cent = getbb(bldg, format_out = "polygon") %>%
  st_centroid(bldg) %>%
  clip(basin)
railway_pt =

stream = opq(bbox = basin) %>%
  add_osm_feature(key = "stream") %>%
  osmdata_sf()





# Terrain Analysis

# 2017 Impact Assessment

# Flood Inundation Map Library
