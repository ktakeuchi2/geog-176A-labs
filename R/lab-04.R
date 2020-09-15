# ---
# title: "Geography 176A"
# author: "Kiki Takeuchi"
# subtitle: "Lab 04: Tesselations, Point in Polygon"
# output:
#  html_document:
#  theme: journal
# ---

# Question 1
library(sf)
library(tidyverse)
library(USAboundaries)

# 1.1
counties = USAboundaries::us_counties() %>%
  filter(!state_name %in% c("Hawaii", "Alaska", "Puerto Rico")) %>%
  st_transform(5070)

# 1.2
states = counties %>%
  group_by(state_name) %>%
  summarise()

county_cent = st_centroid(counties) %>%
  st_union()


# 1.3
v_grid = st_voronoi(county_cent) %>%
  st_cast() %>%
  st_as_sf() %>%
  mutate(id = 1:n())

t_grid = st_triangulate(county_cent) %>%
  st_cast() %>%
  st_as_sf() %>%
  mutate(id = 1:n())

sq_grid = st_make_grid(counties, n = 70) %>%
  st_as_sf() %>%
  mutate(id = 1:n())

hex_grid = st_make_grid(counties, n = 70, square = FALSE) %>%
  st_as_sf() %>%
  mutate(id = 1:n())

# 1.4
counties_u = counties %>%
  st_union()
plot(counties_u)

# 1.5
counties_simp = rmapshaper::ms_simplify(counties_u, keep = 0.1)
plot(counties_simp)
mapview::npts(counties_u)
mapview::npts(counties_simp)
3229 - 322

# original: 3229 points, simplified object: 322 points, removed 2907 points

# Consequences of doing this computationally are that we lose accuracy in the shape of our object, however, do not need to spend as much time or data space on unncessary points.

v_grid = st_intersection(v_grid, counties_simp)
t_grid = st_intersection(t_grid, counties_simp)

# 1.6
plot_tess = function(data, title) {
  ggplot() +
    geom_sf(data = data, fill = "white", col = "navy", size = 0.2) +
    theme_void() +
    labs(title = title, caption = paste("This Tesselation has:", nrow(data), "tiles")) +
    theme(plot.title = element_text(hjust = 0.5, color = "navy", face = "bold"))
}

# 1.7
plot_tess(counties, "Original County Data") +
  geom_sf(data = county_cent, col = "darkred", size = 0.2)

plot_tess(v_grid, "Voronoi Coverage") +
  geom_sf(data = county_cent, col = "darkred", size = 0.2)

plot_tess(t_grid, "Triangulated Coverage") +
  geom_sf(data = county_cent, col = "darkred", size = 0.2)

plot_tess(sq_grid, "Square Coverage")

plot_tess(hex_grid, "Hexagonal Coverage")

# Question 2

# 2.1
library(units)

sum_tess = function(data, text){
  mutate(area = st_area(data),
         area = units:: set_units("km^2"),
         area = drop_units())
  mutate(features = nrow(data),
         mean_area = sum(area)/nrow(data),
         st_dev = sd(data),
         tot_area = sum(area))
  tess_df = data.frame(text, features, mean_area, st_dev, tot_area)
  return(tess_df)}


# 2.2
sum_counties = sum_tess(counties, 'US Counties')
sum_v_grid = sum_tess(v_grid, 'Voronoi Tesselation')
sum_t_grid = sum_tess(t_grid, 'Triangulated Tesselation')
sum_hex_grid = sum_tess(hex_grid, 'Hexagonal Tesselation')
sum_sq_grid = sum_tess(sq_grid, 'Square Tesselation')

# 2.3
tess_summary = bind_rows(
  sum_tess(counties, "counties"),
  sum_tess(v_grid, "voronoi"),
  sum_tess(t_grid, "triangulation"),
  sum_tess(sq_grid, "square"),
  sum_tess(hex_grid, "hexagonal"))

# 2.4
knitr::kable(tess_summary,
             caption = "Tesselation Summary",
             col.names = c("Tesselation", "Number of Features", "Mean Area", "Standard Deviation of Features", "Total Area")) %>%
  kableExtra::kable_styling("striped", full_width = TRUE)

# 2.5


# Question 3

# 3.1
library(readxl)
dams <- read_excel("data/NID2019_U.xlsx") %>%
  filter(!is.na(LONGITUDE), !is.na(LATITUDE)) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  st_transform(5070)

# 3.2
pip = function(points, polygon, group){
  st_join(polygon, points) %>%
    st_drop_geometry() %>%
    count(get(group)) %>%
    setNames(c(group, "n")) %>%
    left_join(polygon, by = group) %>%
    st_as_sf()
}

# 3.3
counties_pip = pip(dams, counties, "geoid")
v_pip = pip(dams, v_grid, "id")
t_pip = pip(dams, t_grid, "id")
sq_pip = pip(dams, sq_grid, "id")
hex_pip = pip(dams, hex_grid, "id")

# 3.4
plot_pip = function(data, title){
  ggplot() +
    geom_sf(data = data, aes(fill = log(n)), col = NA, alpha = 0.9, size = 0.2) +
    scale_fill_viridis_c() +
    theme_void() +
    labs(title = title,
         caption = paste(sum(n), "Dams")) +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, color = "navy", face = "bold"))}

# 3.5
counties_pip = pip(dams, counties, "geoid") %>%
  plot_pip("Dams Per County")

v_pip = pip(dams, v_grid, "id") %>%
  plot_pip("Dams Per Viroinoi Tesselation")
t_pip = pip(dams, t_grid, "id") %>%
  plot_pip("Dams Per Triangulated Tesselation")
sq_pip = pip(dams, sq_grid, "id") %>%
  plot_pip("Dams Per Square Coverage")
hex_pip = pip(dams, hex_grid, "id") %>%
  plot_pip("Dams Per Hexagonal Coverage")

# 3.6


# Question 4

# 4.1
unique(dams$PURPOSES) %>%
  length

p_dams = dams %>%
  filter(grepl("P", dams$PURPOSES) == TRUE)
p_pip = pip(p_dams, v_grid, "id")

i_dams = dams %>%
  filter(grepl("I", dams$PURPOSES) == TRUE)
i_pip = pip(i_dams, v_grid, "id")

f_dams = dams %>%
  filter(grepl("F", dams$PURPOSES) == TRUE)
f_pip = pip(f_dams, v_grid, "id")

h_dams = dams %>%
  filter(grepl("H", dams$PURPOSES) == TRUE)
h_pip = pip(h_dams, v_grid, "id")

# 4.2
plot_pip(p_pip, "Fire Protection Dams Over Voronoi Tesselation")
plot_pip(i_pip, "Irrigation Dams Over Voronoi Tesselation")
plot_pip(f_pip, "Fish and Wildlife Dams Over Voronoi Tesselation")
plot_pip(h_pip, "Hydroelectric Dams Over Voronoi Tesselation")

# Extra Credit

major_rivers = read_sf("data/majorrivers_0_0") %>%
  filter(SYSTEM == "Mississippi") %>%
  st_transform(5070)

library(leaflet)

leaflet() %>%
  addTiles() %>%
  addMarkers(popup = major_rivers)
