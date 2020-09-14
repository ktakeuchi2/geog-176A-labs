# title: Geography 176A
# author: Kiki Takeuchi
# subtitle: Lab 05: Rasters and Remote Sensing

library(tidyverse)
library(sf)
library(raster)

library(getlandsat)
library(mapview)
library(osmdata)

# Question 1

bb = read_csv("data/uscities.csv") %>%
  filter(city == "Palo") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(5070) %>%
  st_buffer(5000) %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_as_sf()


# 5070 is equal area projection
# st_as_sfc is a simple features collection
# st_as_sf makes into a simple features object
mapview(bb)

# detour
bbwgs = bb %>%
  st_transform(4326)

osm = opq(bbwgs) %>%
  add_osm_feature("building") %>%
  osmdata_sf()

mapview(osm$osm_polygons)

# Question 2

bbwgs = bb %>%
  st_transform(4326)
bb = st_bbox(bbwgs)

scenes = getlandsat::lsat_scenes()

down = scenes %>%
  filter(min_lat <= bb$ymin, max_lat >= bb$ymax,
         min_lon <= bb$xmin, max_lon >= bb$xmax,
         as.Date(acquisitionDate) == as.Date("2016-09-26"))
write.csv(down, file = "data/palo-flood.csv", row.names = FALSE)

meta = read_csv("data/palo-flood.csv")
files = lsat_scene_files(meta$download_url) %>%
  filter(grepl(paste0("B", 1:6, ".TIF$", collapse = "|"), file)) %>%
  arrange(file) %>%
  pull(file)

st = sapply(files, lsat_image)

b = stack(st) %>%
  setNames(c(paste0("band", 1:6)))

cropper = bbwgs %>%
  st_transform(crs(b))

r = crop(b, cropper)

# Question 3

### Question 3
par(mfrow = c(2, 2))
plotRGB(r, r = 4, g = 3, b = 2)
plotRGB(r, r = 5, g = 4, b = 3)
plotRGB(r, r = 6, g = 5, b = 4)
plotRGB(r, r = 5, g = 7, b = 1)

par(mfrow = c(2, 2))
plotRGB(r, r = 4, g = 3, b = 2, stretch = "lin")
plotRGB(r, r = 5, g = 4, b = 3, stretch = "lin")
plotRGB(r, r = 6, g = 5, b = 4, stretch = "lin")
plotRGB(r, r = 5, g = 7, b = 1, stretch = "lin")

par(mfrow = c(2, 2))
plotRGB(r, r = 4, g = 3, b = 2, stretch = "hist")
plotRGB(r, r = 5, g = 4, b = 3, stretch = "hist")
plotRGB(r, r = 6, g = 5, b = 4, stretch = "hist")
plotRGB(r, r = 5, g = 7, b = 1, stretch = "hist")

### Question 4

ndvi = (r$band5 - r$band4) / (r$band5 + r$band4)
ndwi = (r$band3 - r$band5) / (r$band3 + r$band5)
mndwi = (r$band3 - r$band6) / (r$band3 + r$band6)
wri = (r$band3 + r$band4) / (r$band5 + r$band6)
swi = 1 / sqrt(r$band2 - r$band6)

sw_rasters = stack(ndvi, ndwi, mndwi, wri, swi) %>%
  setNames(c("ndvi", "ndwi", "mndwi", "wri", "swi"))

palette = colorRampPalette(c("blue", "white", "red"))
plot(sw_rasters, col = palette(256))

##### Thresholding
thresh_ndvi = function(x){ifelse(x <= 0, 1, NA)}
thresh_ndwi = function(x){ifelse(x >= 0, 1, NA)}
thresh_wri = function(x){ifelse(x >= 1, 1, NA)}
thresh_swi = function(x){ifelse(x <= 5, 1, NA)}

flood_ndvi = calc(ndvi, thresh_ndvi)
flood_ndwi = calc(ndwi, thresh_ndwi)
flood_mndwi = calc(mndwi, thresh_ndwi)
flood_wri = calc(wri, thresh_wri)
flood_swi = calc(swi, thresh_swi)

flood = stack(flood_ndvi, flood_ndwi, flood_mndwi, flood_wri, flood_swi) %>%
  setNames(c("ndvi", "ndwi", "mndwi", "wri", "swi"))

plot(flood, col = "blue")
flood <- na.omit(flood)

### Question 5
set.seed(09072020)
dim(r)

values = values(r)
idx = which(!is.na(values))
v = na.omit(values)
vs = scale(v)

library(stats)

k12 = kmeans(vs, centers = 12)
kmeans_raster = flood$ndvi
values(kmeans_raster) = k12$cluster
plot(kmeans_raster)

k11 = kmeans(vs, centers = 11)
kmeans_raster = flood$ndvi
values(kmeans_raster) = k11$cluster
plot(kmeans_raster)

k10 = kmeans(vs, centers = 10)
kmeans_raster = flood$ndvi
values(kmeans_raster) = k10$cluster
plot(kmeans_raster)

k13 = kmeans(vs, centers = 13)
kmeans_raster = flood$ndvi
values(kmeans_raster) = k13$cluster
plot(kmeans_raster)

k9 = kmeans(vs, centers = 9)
kmeans_raster = flood$ndvi
values(kmeans_raster) = k9$cluster
plot(kmeans_raster)

flood_values = getValues(flood_ndvi)
kmeans_values = getValues(kmeans_raster)
table = table(flood_values, kmeans_values)
which.max(table)

mask = function(x){ifelse(x == 5, 1, NA)}
flood_mask = calc(kmeans_raster, mask)

flood = addLayer(flood, flood_mask)

### Question 6

s1 = cellStats(flood$ndvi, stat = 'sum') * 30^2
s2 = cellStats(flood$ndwi, stat = 'sum') * 30^2
s3 = cellStats(flood$mndwi, stat = 'sum') * 30^2
s4 = cellStats(flood$swi, stat = 'sum') * 30^2
s5 = cellStats(flood$wri, stat = 'sum') * 30^2
s6 = cellStats(flood_mask, stat = 'sum') * 30^2
comparison = cbind(method = c("ndvi", "ndwi", "mndwi", "wri", "swi", "kmeans_mask"), cell_area = c(s1, s2, s3, s4, s5, s6))


knitr::kable(comparison,
             caption = "Total Area of Flooded Cells",
             col.names = c("Band", "Total Flooded Cells")) %>%
  kableExtra::kable_styling("striped", full_width = TRUE)

total_flood = calc(flood, sum)
plot(total_flood, col = RColorBrewer::brewer.pal(9, "Blues"))

total_flood <- na.omit(total_flood)
flood = addLayer(flood, total_flood)

mapview(total_flood)

