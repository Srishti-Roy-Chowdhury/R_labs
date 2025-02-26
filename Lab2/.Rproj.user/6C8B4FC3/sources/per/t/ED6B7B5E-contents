#loading packages
library(tidyverse)
library(sf)
library(tmap)

#data connection
bmps <- read_csv("./data/BMPreport2016_landbmps.csv")
dams <- sf::read_sf("./data/Dam_or_Other_Blockage_Removed_2012_2017.shp")
streams <- sf::read_sf("./data/Streams_Opened_by_Dam_Removal_2012_2017.shp")
counties <- sf::read_sf("./data/County_Boundaries.shp")


#Task-1.1
bmps %>% group_by(StateAbbreviation) %>% summarize(totalCost = sum(Cost,na.rm = TRUE))

#Task-1.2
#Though there are multiple ways for data transformation, I found these following most suitable to present
bmps %>% dplyr::filter(., Unit=="Acres") %>%
  ggplot(., aes(x = log(Cost+1), y = log(TotalAmountCredited+1))) +
  geom_point() +
  geom_smooth() +
  theme_minimal()
#Or,
bmps %>% dplyr::filter(., Unit=="Acres") %>%
  ggplot(., aes(x = sqrt(Cost), y = TotalAmountCredited)) +
  geom_point() +
  geom_smooth() +
  theme_minimal()

#Task-1.3
bmps13 <- bmps %>% mutate(., trim_bmp=stringr::str_sub(BMPShortName, 1, 9))
bmps13 %>% filter(trim_bmp=="covercrop") %>% 
  ggplot(., aes(x = StateAbbreviation, y = TotalAmountCredited)) +
  geom_boxplot(aes(fill = StateAbbreviation))

#Task-1.4
dams %>% filter(YEAR!=0) %>%
  ggplot(., aes(x = YEAR, y = STATE)) +
  geom_point() 

#Task-1.5
joined_table <- left_join(dams, bmps, by = c("STATE" = "StateAbbreviation"))
joined_table %>% ggplot(., aes(x = Sector, y = Cost, color=STATE)) +
  geom_point() +
  theme_minimal() +
  labs(title = "State-wise cost of different sectors",
       x = "Sector",
       y = "Cost")


#Task-2.1
streams %>% dplyr::arrange(desc(LengthKM)) %>% head(5)

#Task-2.2
streams %>% sf::st_is_valid()
counties %>% sf::st_is_valid()
counties <- counties %>% sf::st_make_valid()
counties %>% sf::st_crs() == streams %>% sf::st_crs()

streams.counties <- sf::st_join(streams, counties)
streams.countie_sum <- streams.counties %>% group_by(COUNTYFP10) %>% summarise(TotalLength=sum(LengthKM))
streams.countie_sum %>% dplyr::arrange(desc(TotalLength)) %>% head(3)

#Task-2.3
bmps <- bmps %>% mutate(., FIPS.trimmed = stringr::str_sub(GeographyName, 1, 5))
counties.bmps <- left_join(counties, bmps, by = c("GEOID10" = "FIPS.trimmed"))
glimpse(counties.bmps)
#The map is taking to much time to produce
tm_shape(counties.bmps) + tm_polygons(fill = "Cost")

#Task-2.4
#To solve this question, I got help from online sources.
distance <- sf::st_distance(dams, streams)

nearest_distances <- apply(distance, 1, min)
nearest_stream_index <- apply(distance, 1, which.min)

nearest_features <- data.frame(
  damID = dams$UNIQUE_ID,
  streamHYDROID = streams$HYDROID[nearest_stream_index],
  nearest_stream_index= nearest_stream_index,
  distance = nearest_distances
)

print(nearest_features)

#Task-2.5
dams %>% group_by(STATE) %>% summarise(DamsCount=n())


