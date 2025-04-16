
#loading library packages
library(sf)
library(terra)
library(tmap)
library(spData)
library(tidyverse)
library(grid)
library(raster)


#****************************MAP-1***************************************

#data connection
oh2020 <- read_csv("./data/static_mapping/oh_counties_DP2020.csv")
counties <- sf::read_sf("./data/static_mapping/oh_counties.gpkg")
places <- sf::read_sf("./data/static_mapping/oh_places.gpkg")
parks <- sf::read_sf("./data/static_mapping/oh_parks.gpkg")
streams.river <- sf::read_sf("./data/static_mapping/oh_rivers.gpkg")
linearwater22 <- sf::read_sf("./data/static_mapping/tl_2022_39133_linearwater/tl_2022_39133_linearwater.shp")
linearwater23 <- sf::read_sf("./data/static_mapping/tl_2023_39153_linearwater/tl_2023_39153_linearwater.shp")
neoh_dem <- terra::rast("./data/static_mapping/neoh_dem.tif")


#**Ohio Scale**
#1
oh2020 <- dplyr::filter(oh2020,name!="Ohio")
joined.table <- left_join(counties, oh2020, by=c("GEOIDFQ"="geoid"))

#2 & 3
ohio <- tm_shape(joined.table) + 
  tm_polygons(fill = "medianage",lty=3,lwd=3,fill.scale = tm_scale_intervals(style = "equal", values = "viridis"))+
  tm_scalebar(breaks = c(0, 100, 200), text.size = 1, position = c("left", "top"))
ohio


#**Local Scale**
#data organization
selected.county <- counties %>% dplyr::filter(., NAME=="Summit" | NAME=="Portage")
sf::sf_use_s2(FALSE)
mun.county <- st_intersection(places,selected.county)
park.projected <- st_transform(parks, crs = st_crs(mun.county))
stream.projected <- st_transform(streams.river, crs = st_crs(mun.county))
park <- st_intersection(park.projected, selected.county)
stream <- st_intersection(stream.projected, selected.county)

#1: County and municipal boundaries
boundary <- tm_shape(selected.county) + tm_polygons()
municipalities <- tm_shape(mun.county) + tm_polygons(fill = "red", alpha= 0.4) + 
                  tm_labels("NAME", size=0.7) + tm_layout(inner.margins = c(0.05, 0.05, 0.05, 0.05))

#2: Park map within selected counties
parkmap <- tm_shape(park) + tm_polygons(fill = "FEATTYPE", palette = "brewer.greens",title = "Park Type")

#3: Linear water features (streams, rivers) in Portage AND Summit counties
linearwater22.park <- st_intersection(linearwater22, park)
linearwater22.intersected <- tm_shape(linearwater22.park) + tm_lines(lwd = 3, col = "blue")
linearwater23.park <- st_intersection(linearwater23, park)
linearwater23.intersected <- tm_shape(linearwater23.park) + tm_lines(lwd = 3, col = "blue")
stream.park <- st_intersection(stream.projected, park)
stream.map <- tm_shape(stream) + tm_lines(lwd = 10, col = "lightblue")
stream.intersected <- tm_shape(stream.park) + tm_lines(lwd = 3.5, col = "darkblue")

mapL3 <- boundary + municipalities + parkmap + linearwater22.intersected + linearwater23.intersected + stream.intersected 
mapL3 #After trying multiple codes, I failed to place all the municipal labels in the final map, only two names are showing


#**Adding DEM, organizing the data and Final map**
#DEM map with other map components
neoh_dem.projected <- project(neoh_dem, crs(counties)) #Takes time to project the crs
dem.counties <- terra::crop(neoh_dem.projected, selected.county)
dem <- tm_shape(dem.counties) + tm_raster(palette = terrain.colors(10), alpha = 0.5, title="Elevation") + tm_compass(position = c("right", "top"), size = 3) +
  tm_title("Municipal Areas, Parks, Water, and Elevation of Portage & Summit Counties",
           size = 5,fontface = "bold",
           position = c("center","top")) +
  tm_layout(legend.position = c("left", "top"),
    legend.title.size = 0.7,
    legend.title.color = "black",
    legend.text.size = 0.5, 
    legend.frame = FALSE
  ) +
  tm_scalebar(breaks = c(0, 20, 40, 60, 80), text.size = 0.8, position = c("left", "bottom"), size = 0.5)

#semi-transparent Portage and Summit counties
county <- tm_shape(selected.county) + tm_polygons(fill_alpha = .4)

#Adding them all
map1 <- dem + county + municipalities + parkmap + linearwater22.intersected + linearwater23.intersected + stream.intersected + tm_layout(asp = 0, inner.margins = c(0.25, 0.25, 0.25, 0.4), frame = TRUE)

#Locator map
ohio.c <- tm_shape(counties) + tm_polygons(palette = "gray80", border.col = "white") +
  tm_shape(selected.county) +tm_polygons(fill="red", border.col = "white") +
  tm_layout(frame = TRUE, legend.show = FALSE)

#Map dimension fixing
norm_dim = function(obj){
  bbox = st_bbox(obj)
  width = bbox[["xmax"]] - bbox[["xmin"]]
  height = bbox[["ymax"]] - bbox[["ymin"]]
  w = width / max(width, height)
  h = height / max(width, height)
  return(unit(c(w, h), "snpc"))
}

main_dim = norm_dim(selected.county)
ins_dim = norm_dim(counties)

main_vp = viewport(width = main_dim[1], height = main_dim[2])
ins_vp = viewport(width = ins_dim[1] * 0.5, height = ins_dim[2] * 0.5,
                  x = unit(1, "npc") - unit(0, "cm"), y = unit(0.5, "cm"),
                  just = c("right", "bottom")) 

#If possible, take a pause before running this code, as sometimes it runs out of time and couldn't plot the final map
#I tried to fit the map within map frame but it is not working, my assumption is that the map is considering the boundary of original dem data
grid.newpage()
print(map1, vp = main_vp)
pushViewport(main_vp)
print(ohio.c, vp = ins_vp)



#****************************MAP-2***************************************

#my idea is to create a map of flood risk zone/counties of Ohio; 
#that's why I am using county boundary, river stream and DEM data that I personally downloaded from USGS

#data connection, organization
#political boundary
ohio.boundary <- tm_shape(counties) + tm_polygons(palette = "gray80", border.col = "white", alpha = 0.5)
ohio.population <- tm_shape(joined.table) + 
  tm_polygons(fill = "poptotal",fill.scale = tm_scale_intervals(style = "jenks", values = "blue"),alpha = 0.5)

#water(river): creating buffer of 1000m from river to identify the flood risk zones
riverstream <- tm_shape(stream.projected) + tm_lines(lwd = 2, col = "darkblue")
river.buffer <- st_buffer(streams.river, dist = 1000)
river.buffer_projected <- st_transform(parks, crs = st_crs(counties))
river.buffered <- tm_shape(river.buffer_projected) + tm_polygons(col = "lightblue", alpha = 0.3)

#dem data: preparing data and identifying the low elevated zones (elevation less than 50) 
ohio_dem <- terra::rast("./data/static_mapping/ohio_dem_1.tif")
dem_ohio <- terra::crop(ohio_dem, counties) #Takes time to run
dem_ohio <- terra::mask(ohio_dem, counties)

dem2 <- tm_shape(dem_ohio) + tm_raster(palette = terrain.colors(10), alpha = 0.5, title="Elevation") + 
  tm_compass(position = c("right", "top"), size = 4) +
  tm_layout(
    title = "Flood risk counties with population distribution of Ohio",
    title.size = 1,
    title.position = c("left", "top"),
    legend.position = c("left", "bottom"),
    legend.title.size = 1,
    legend.title.color = "black",
    legend.text.size = 0.8, 
    legend.frame = FALSE,
    #scale.bar.size = 1
  ) +
  tm_scalebar(breaks = c(0, 25, 50, 75, 100), text.size = 0.8, position = c("right", "bottom"), size = 2)

low_elevation <- dem_ohio<50 #This part takes time to run
low_elev_zones <- as.polygons(low_elevation, dissolve = TRUE)
low_elev_poly_sf <- st_as_sf(low_elev_zones)

#identifying flood risk zones and counties with road buffer, low elevation data
#This part is taking a lot of time to run
flood_risk_area <- st_intersection(river.buffer_projected, low_elev_poly_sf)
flood_risk_counties <- st_intersection(counties, flood_risk_area)
floodrisk.counties <- tm_shape(flood_risk_counties) + 
                      tm_polygons(fill="red",alpha = 0.4)

#Adding all layers into one map [the map id taking too much time to plot]
map.ohio <- dem2 + ohio.population + floodrisk.counties + riverstream + river.buffered + tm_layout(asp = 0, inner.margins = c(0.2, 0.2, 0.25, 0.05), frame = TRUE)
map.ohio



