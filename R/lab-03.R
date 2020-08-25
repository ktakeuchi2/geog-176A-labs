#########################
## Project: Lab 03 W
## Script purpose: Week 3
## Date: August 19
#########################

# libraries

# SPDS
library(tidyverse)
library(sf)
library(units)

# Data
library(USAboundaries)
library(rnaturalearth)

# Visualization
install.packages("ggrepel")
install.packages("gghighlight")

library(gghighlight)
library(ggrepel)
library(knitr)


region = data.frame(region = state.region,
                    state_name = state.name)

conus = USAboundaries::us_states() %>%
  left_join(region) %>%
  filter(!state_name == "Alaska", "Hawaii", "Puerto Rico") #???


plot(conus)
plot(conus$geometry)
plot(conus['aland'])

# cities is just a data.frame
# making into a spatial feature
# x coordinate is longitude, y coordinate is latitude
# lat and lng means geographic coordinate system -> use crs = 4326 for WGS

# cities = readr::read_csv("data/uscities.csv") %>%
#   st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
#   st_filter(conus, .predicate = st_intersects)
#
# st_crs(cities)
# # assumption that coordinate system is
#
# plot(conus$geometry)
# plot(cities$geometry, pch = 16, cex = 0.1, add = T)
#
# a = st_combine(conus) %>%
#   st_cast("MULTILINESTRING")
#
# ?st_distance
# # automatically uses greater circle distances, more accurate but takes longer to calculate
# # options: (which = "Euclidean") or
# # a = st_transform(a, 5070)
# # cities = st_transform(cities, 5070)
#
# a = st_transform(a, 5070)
# cities = st_transform(cities, 5070)
#
# cities = cities %>%
#   mutate(dist_to_state = st_distance(cities, a),
#          # st_distance will return values as unit object, modify units with units package
#          dist_to_state = units::set_units(dist_to_state, "km"),
#          dist_to_state = units::drop_units(dist_to_state))
#
# big_cities = cities %>%
#   group_by(state_name) %>%
#   slice_max(population, n = 1)
#
# ggplot() +
#   geom_sf(data = a) +
#   geom_sf(data = cities, aes(col = dist_to_state), size = 0.1) +
#   geom_sf(data = big_cities, color = "navy") +
#   scale_color_gradient(low = "grey", high = "darkred") +
#   ggthemes::theme_map() +
#   ggrepel::geom_label_repel(
#     data = big_cities,
#     aes(label = city, geometry = geometry),
#     stat = "sf_coordinates",
#     size = 3)
#
# ?geom_label_repel
#
# ggplot() +
#   geom_sf(data = a) +
#   # highlighting only cities with a pop meeting a certain condition and applying color scale
#   gghighlight::gghighlight(population > 1e4) +
#   geom_sf(data = cities, aes(col = dist_to_state), size = 0.1) +
#   geom_sf(data = big_cities, color = "navy") +
#   scale_color_gradient(low = "grey", high = "darkred")

eqdc = '+proj=eqdc +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'

region = data.frame(region = state.region,
                    state_name = state.name)

conus = USAboundaries::us_states(resolution = "low") %>%
  left_join(region) %>%
  filter(!state_name %in% c("Puerto Rico", "Alaska", "Hawaii")) %>%
  st_transform(eqdc)
plot(conus['aland'])

remotes::install_github("ropenscilabs/rnaturalearthdata")
countries = rnaturalearth::countries110 %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  filter(admin %in% c("Mexico", "United States of America", "Canada")) %>%
  st_transform(eqdc)

cities = readr::read_csv("data/uscities.csv") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_filter(conus, .predicate = st_intersects) %>%
  st_transform(eqdc)

plot(cities$geometry, pch = 16, cex = 0.2)

# filter(!state_name %in% c("Hawaii", "Puerto Rico", "Alaska")) %>%

# Question 2

# 2.1 - Distance to US border (km)
conus_u = st_union(conus) %>%
  st_cast("MULTILINESTRING") %>%
  st_transform(5070)

cities = st_transform(cities, 5070)

cities = cities %>%
  mutate(dist_border = st_distance(cities, conus_u),
         dist_border = units::set_units(dist_border, "km"),
         dist_border = units::drop_units(dist_border))

farthest_us_border = cities %>%
  slice_max(dist_border, n= 5) %>%
  select(city, state_name, dist_border)

knitr::kable(farthest_us_border,
             caption = "Cities Farthest from USA border",
             col.names = c("City", "State", "Distance to USA Border (km)", "Coordinates")) %>%
  kableExtra::kable_styling("striped", full_width = TRUE)

# 2.2 - Distance to States (km)
conus_c = st_combine(conus) %>%
  st_cast("MULTILINESTRING") %>%
  st_transform(5070)

cities = cities %>%
  mutate(dist_state = st_distance(cities, conus_c),
         dist_state = units::set_units(dist_state, "km"),
         dist_state = units::drop_units(dist_state))

farthest_state = cities %>%
  slice_max(dist_state, n= 5) %>%
  select(city, state_name, dist_state)

knitr::kable(farthest_state,
             caption = "Cities Farthest from State Borders",
             col.names = c("City", "State", "Distance to State Border (km)", "Coordinates")) %>%
  kableExtra::kable_styling("striped", full_width = TRUE)

# 2.3 - Distance to Mexico (km)
mexico = countries %>%
  filter(admin == "Mexico") %>%
  st_transform(5070)

cities = cities %>%
  mutate(dist_mexico = st_distance(cities, mexico),
         dist_mexico = units::set_units(dist_mexico, "km"),
         dist_mexico = units::drop_units(dist_mexico))

farthest_to_mexico = cities %>%
  slice_max(dist_mexico, n= 5) %>%
  select(city, state_name, dist_mexico)

knitr::kable(farthest_to_mexico,
             caption = "Cities Farthest from Mexican Border",
             col.names = c("City", "State", "Distance to Mexican Border (km)", "Coordinates")) %>%
  kableExtra::kable_styling("striped", full_width = TRUE)

# 2.4 - Distance to Canada (km)
canada = countries %>%
  filter(admin == "Canada") %>%
  st_transform(5070)

cities = cities %>%
  mutate(dist_canada = st_distance(cities, canada),
         dist_canada = units::set_units(dist_canada, "km"),
         dist_canada = units::drop_units(dist_canada))

farthest_to_canada = cities %>%
  slice_max(dist_canada, n= 5) %>%
  select(city, state_name, dist_canada)

knitr::kable(farthest_to_canada,
             caption = "Cities Farthest from Canadian Border",
             col.names = c("City", "State", "Distance to Canadian Border (km)", "Coordinates")) %>%
  kableExtra::kable_styling("striped", full_width = TRUE)

# Question 3

# 3.1

cities_top10 = cities %>%
  slice_max(population, n = 10)

ggplot() +
  geom_sf(data = countries, fill = "gray80") +
  geom_sf(data = conus_u, color = "red", cex = 1) +
  geom_sf(data = conus_c, color = "black") +
  geom_sf(data = cities_top10, aes(size = population), color = "red") +
  theme_linedraw() +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "slategray1")) +
  labs(title = "Data")

# 3.2
ggplot() +
  geom_sf(data = conus) +
  geom_sf(data = cities) +
  geom_sf(data = farthest_us_border) +
  labs(title = "Distance of US Cities from US Border")

# 3.3


# 3.4

# Question 4
