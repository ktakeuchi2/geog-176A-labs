## Lab 03 Final

library(tidyverse)
library(sf)
library(units)
library(USAboundaries)
library(rnaturalearth)
library(gghighlight)
library(ggrepel)
library(knitr)



### Question 1
##### *Creating Datasets for Boundaries of US States, USA, Canada, Mexico, and US Cities*

eqdc = '+proj=eqdc +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'

conus = USAboundaries::us_states(resolution = "low") %>%
  filter(!state_name %in% c("Puerto Rico", "Alaska", "Hawaii")) %>%
  st_transform(eqdc)

countries = rnaturalearth::countries110
countries = countries %>%
  st_as_sf() %>%
  filter(admin %in% c("Mexico", "United States of America", "Canada")) %>%
  st_transform(eqdc)

cities = readr::read_csv("data/uscities.csv") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  filter(!state_name %in% c("Hawaii", "Puerto Rico", "Alaska")) %>%
  st_transform(eqdc)

  ### Question 2:
  ##### *Calculating Distances of US cities to (1) the national US border, (2) the nearest state border, (3) the Mexican border, and (4) the Canadian border.

  ##### Distance to US Border (km)
conus_u = st_union(conus) %>%
  st_cast("MULTILINESTRING")

cities = cities %>%
  mutate(dist_border = st_distance(cities, conus_u),
         dist_border = units::set_units(dist_border, "km"),
         dist_border = units::drop_units(dist_border))

farthest_us_border = cities %>%
  select(city, state_name, dist_border) %>%
  arrange(-dist_border) %>%
  slice_max(dist_border, n = 5) %>%
  st_drop_geometry()

knitr::kable(farthest_us_border,
             caption = "Cities Farthest from USA border",
             col.names = c("City", "State", "Distance to USA Border (km)")) %>%
  kableExtra::kable_styling("striped", full_width = TRUE)

##### Distance to States (km)

conus_c = st_combine(conus) %>%
  st_cast("MULTILINESTRING")

cities = cities %>%
  mutate(dist_state = st_distance(cities, conus_c),
         dist_state = units::set_units(dist_state, "km"),
         dist_state = units::drop_units(dist_state))

farthest_state <- cities %>%
  select(city, state_name, dist_state) %>%
  arrange(-dist_state) %>%
  slice_max(dist_state, n= 5) %>%
  st_drop_geometry()


knitr::kable(farthest_state,
             caption = "Cities Farthest from State Borders",
             col.names = c("City", "State", "Distance to State Border (km)")) %>%
  kableExtra::kable_styling("striped", full_width = TRUE)

##### Distance to Mexico (km)
mexico = countries %>%
  filter(admin == "Mexico") %>%
  st_cast("MULTILINESTRING")

cities = cities %>%
  mutate(dist_mexico = st_distance(cities, mexico),
         dist_mexico = units::set_units(dist_mexico, "km"),
         dist_mexico = units::drop_units(dist_mexico))

farthest_mexico = cities %>%
  select(city, state_name, dist_mexico) %>%
  arrange(-dist_mexico) %>%
  slice_max(dist_mexico, n= 5) %>%
  st_drop_geometry()

knitr::kable(farthest_mexico,
             caption = "Cities Farthest from Mexican Border",
             col.names = c("City", "State", "Distance to Mexican Border (km)")) %>%
  kableExtra::kable_styling("striped", full_width = TRUE)


##### Distance to Canada (km)
canada = countries %>%
  filter(admin == "Canada") %>%
  st_cast("MULTILINESTRING")

cities = cities %>%
  mutate(dist_canada = st_distance(cities, canada),
         dist_canada = units::set_units(dist_canada, "km"),
         dist_canada = units::drop_units(dist_canada))

farthest_canada = cities %>%
  select(city, state_name, dist_canada) %>%
  arrange(-dist_canada) %>%
  slice_max(dist_canada, n= 5) %>%
  st_drop_geometry()

knitr::kable(farthest_canada,
             caption = "Cities Farthest from Canadian Border",
             col.names = c("City", "State", "Distance to Canadian Border (km)")) %>%
  kableExtra::kable_styling("striped", full_width = TRUE)

### Question 3
##### *Visualization*

cities_top10 = cities %>%
  slice_max(population, n = 10)

ggplot() +
  geom_sf(data = countries, fill = "gray80") +
  geom_sf(data = conus_u, color = "blue", cex = 1) +
  geom_sf(data = conus_c, color = "black") +
  geom_sf(data = cities_top10, aes(size = population), color = "red") +
  theme_linedraw() +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "slategray1"),
        legend.position = "none") +
  labs(title = "Top 10 Largest US Cities")

far_us_border = cities %>%
  select(city, state_name, dist_border) %>%
  arrange(-dist_border) %>%
  slice_max(dist_border, n = 5)

ggplot() +
  geom_sf(data = cities, aes(col = dist_border), size = 0.1) +
  scale_color_gradient(low = "grey", high = "darkblue") +
  geom_sf()
  ggrepel::geom_label_repel(data = far_us_border,
                            aes(label = city, geometry = geometry),
                            stat = "sf_coordinates",
                            size = 4) +
  labs(title = "Distance of US Cities from US Border",
       col = "Distance to US Border (km)") +
  ggthemes::theme_map()

