#loading packages
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
install.packages("sf")
library(sf)


#data connection
p.counties <- "./data/County_Boundaries.shp"
p.stations <- "./data/Non-Tidal_Water_Quality_Monitoring_Stations_in_the_Chesapeake_Bay.shp"

d.counties <- sf::read_sf(p.counties)
d.stations <- sf::read_sf(p.stations)


#geometry files ready
d.stations %>% sf::st_is_valid()
d.counties %>% sf::st_is_valid()
d.counties <- d.counties %>% sf::st_make_valid()
d.counties %>% sf::st_crs() == d.stations %>% sf::st_crs()

dx.counties <- d.counties %>% dplyr::select(STATEFP10,NAMELSAD10,COUNTYFP10,GEOID10,ALAND10,Shape_Area,geometry)
county.station <- sf::st_intersection(dx.counties,d.stations)
#view(county.station)


#Task-1.1
d1.counties <- d.counties %>% group_by(STATEFP10) %>% mutate(stateLand = sum(ALAND10,AWATER10))
d11.counties <- d1.counties %>% group_by(COUNTYFP10) %>% mutate(countyLandpercent = ((ALAND10/stateLand)*100))
view(d11.counties)
d11.counties %>% group_by(COUNTYFP10) %>% dplyr::select(STATEFP10,COUNTYFP10,countyLandpercent) %>% head() #partial answer

#Task-1.2
d1.counties <- d1.counties %>% group_by(COUNTYFP10) %>% mutate(countyWLand = (AWATER10/stateLand))
d1.counties %>% group_by(STATEFP10) %>% dplyr::select(STATEFP10,COUNTYFP10,countyWLand) %>% dplyr::filter(countyWLand == max (countyWLand))

#Task-1.3
d.counties %>% as_tibble() %>% group_by(STATEFP10) %>% summarise(statecounty = n())

#Task-1.4
d.stations %>% dplyr::slice_min(nchar(STATION_NA))


#Task-2.1
d.counties %>% ggplot(., aes(x = ALAND10,y=AWATER10,color = STATEFP10)) +
  geom_point() +
  theme_classic() +
  labs(title = "Land and Water Area of Each County",
       subtitle = "Statewise color variation",
       x = "Land Area",
       y = "Water Area")

#Task-2.2
d.stations %>% ggplot(., aes(x = Drainage_A)) +
  geom_histogram(fill = "dark red",bins=10) +
  theme_minimal() +
  labs(title = "Drainage Area of Stations",
       x = "Drainage area")

#Task-2.3
ggplot(county.station, aes(x = Drainage_A,fill=STATEFP10)) +
  geom_histogram(bins=10) +
  theme_minimal() +
  labs(title = "Drainage Area of Stations", 
       x = "Drainage area")


#Task-3
my_function <- function(numbers){
  mean(numbers)
  median(numbers)
  min(numbers)
  max(numbers)
  sort(numbers, decreasing = FALSE)
  
  if(is.numeric(numbers)==FALSE){
    return("Error: The data are not numeric")
  }
  
  else{
    return(
      list(  mean(numbers),
             median(numbers),
             min(numbers),
             max(numbers),
             sort(numbers, decreasing = FALSE)
      )
    )
  }
}

my_function(c(1,0,-1))
my_function(c(10,100,1000))
my_function(c(.1, .001, 1e8))
my_function(c("a", "b", "c"))


#Task-4.1
county.station %>% as_tibble() %>% group_by(STATEFP10) %>% summarise(statestations = n())

#Task-4.2
#Performing the task on Intersected dataset
#Calculating area using "st_area" function
ny.counties <- county.station %>% dplyr::filter(STATEFP10 == 36)
ny.counties %>% st_area()
#avg. area calculation using Shape_area variable 
county.station %>% dplyr::filter(STATEFP10 == 36)%>% group_by(STATEFP10) %>% summarise(avgAcounty = mean(Shape_Area))

#Performing the task on d.counties file
d.counties %>% dplyr::filter(STATEFP10 == 36)%>% group_by(STATEFP10) %>% summarise(avgAcounty = mean(Shape_Area))


#Task-4.3
avgDrainage <- county.station %>% group_by(STATEFP10) %>% summarise(avgAdrainage = mean(Drainage_A))
avgDrainage %>% dplyr::slice_max(avgAdrainage)

