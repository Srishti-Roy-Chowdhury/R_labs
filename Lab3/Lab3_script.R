
#loading packages
library(spdep)
library(sf)
library(tidyverse)
library(tmap)


#***For uploading purpose, considering the size of the data I removed the original downloaded data from the data folder and also the subsets***
#*****To reduce file size, creating the subset of necessary data to upload*****
# county <- sf::read_sf("./data/ACS_2020_5YR_COUNTY.gdb/ACS_2020_5YR_COUNTY.gdb", layer = "ACS_2020_5YR_COUNTY")
# write_sf(county, "./data/county_shapefile.shp")
# age.sex <- sf::read_sf("./data/ACS_2020_5YR_COUNTY.gdb/ACS_2020_5YR_COUNTY.gdb", layer = "X01_AGE_AND_SEX") 
# write.csv(age.sex, "./data/age.sex.csv")
# county.metadata <- sf::read_sf("./data/ACS_2020_5YR_COUNTY.gdb/ACS_2020_5YR_COUNTY.gdb", layer = "COUNTY_METADATA_2020") 
# write.csv(county.metadata, "./data/county.metadata.csv")

#**Task 1: Making a separate shape file of four contiguous states**
# states4 <- county %>% dplyr::filter(STATEFP == "50"|STATEFP == "33"|STATEFP == "23"|STATEFP == "25")
# write_sf(states4, "./data/states4.shp")

#**Also comment out the last two-line code because of the size of "county" shapefile*** 
#**Starting the tasks from reading the datasets generated from the codes mentioned above***
#Reading and organizing datasets
states4 <- sf::read_sf("./data/states4.shp")
age.sex <- read_csv("./data/age.sex.csv")
age.sex <- age.sex %>% mutate(.,fixed_geoid = str_sub(GEOID, start = 8, end = -1))
states4 <- states4 %>% left_join(., age.sex, by = c("GEOID" = "fixed_geoid"))
#Plotting a basic map of four contiguous states
tm_shape(states4) +
  tm_borders() +
  tm_polygons(fill = "STATEFP") +
  tm_title("Map of New Hampshire, Massachusetts, Vermont, Maine")


#**Task 2: Normalizing Male and Female population by Total population**
states4.population <- states4 %>%  mutate(.,MalePercent=B01001e2/B01001e1, FemalePercent=B01001e26/B01001e1)


#**Task 3: Creating histogram of normalized male and female population**
states4.population %>% ggplot(., aes(x = MalePercent)) +
  geom_histogram(binwidth= 0.004, fill= "blue", color = "skyblue", alpha = 0.7) +
  theme_minimal() +
  labs(title = "State-wise Male distribution",
       x = "Male Percentage",
       y= "Frequency")
#Or,
states4.population %>% ggplot(., aes(x = FemalePercent)) +
  geom_histogram(binwidth= 0.004, fill= "blue", color = "skyblue", alpha = 0.7) +
  theme_minimal() +
  labs(title = "State-wise Female distribution",
       x = "Female Percentage",
       y= "Frequency")


#**Task 4: Creating choropleth map using the Jenks (Natural Breaks) classification scheme as it works well for non-uniformly distributed data.**
#I found it the most suitable after trying different schemes such as quantile, equal interval
tm_shape(states4.population) + 
  tm_borders() +
  tm_polygons(fill = "MalePercent", style = "jenks", palette = "brewer.greens") +
  tm_title("Choropleth Map of Normalized Male Population by County of Four States") +
  tm_layout(legend.title.size = 1, legend.position = c("right", "bottom"))


#**Task 5: Contiguity Based**
#Basic Map plotting and setting coordinate system
tm_shape(states4) + 
  tm_polygons()
sf::st_crs(states4)
states4.projected <- states4 %>% sf::st_transform(., "ESRI:102010")
tm_shape(states4.projected) +
  tm_polygons()

#5.1: Row-standardizing the W
neighbor <- spdep::poly2nb(states4.projected, queen = TRUE)  #Calculate Queen's case neighbors
lw <- nb2listw(neighbor, style="W", zero.policy=TRUE)

#5.2: Plotting a histogram of the number of neighbors
neighborhoods <- attr(lw$weights,"comp")$d
hist(neighborhoods)

#5.3: Calculating the average number of neighbors
avg_neighbors <- mean(neighborhoods)
print(paste("Average number of neighbors (Queen contiguity):", avg_neighbors))

#5.4: Making a Moran Plot using the var B01001e26[total female population]
F.lag <- lag.listw(lw, states4.projected$B01001e26)
moran.test(states4.projected$B01001e26, lw)
moran.plot(states4.projected$B01001e26, lw, zero.policy=TRUE, plot=TRUE)


#**Task 6: IDW Based**
#6.1: Developing W using the IDW method
#Checking and making the geometry compatible (calculating centroids) to create neighborhoods based on distance 
sf::st_geometry_type(states4.projected) 
states4.centroid <- sf::st_centroid(states4.projected)
#Defining neighbors based on all distances (0 to infinity)
neighbor_idw <- spdep::dnearneigh(states4.centroid, 0, Inf)
lw_idw <- nb2listwdist(neighbor_idw, states4.centroid, type="idw", style="W", zero.policy=TRUE)

#6.2: Plotting a histogram of the number of neighbors
neighborhoods_idw <- attr(lw_idw$weights, "comp")$d
hist(neighborhoods_idw)

#6.3: Calculating the average number of neighbors
avg_neighbors_idw <- mean(neighborhoods_idw)
print(paste("Average number of neighbors (IDW):", avg_neighbors_idw))

#6.4: Making a Moran Plot using the var B01001e26 [total female population]
F.lag.idw <- lag.listw(lw_idw, states4.centroid$B01001e26)
moran.test(states4.centroid$B01001e26, lw_idw)
moran.plot(states4.centroid$B01001e26, lw_idw, zero.policy = TRUE, plot = TRUE)


#**Bonus tasks**
#**B1**
#Creating dataset with the previously used variable and lag values
moran_df <- data.frame(
  unique_id = states4.projected$GEOID,
  B01001e26 = states4.projected$B01001e26,
  lag = F.lag
)
#Adding var to the dataset mentioning quadrants based on values
moran_df <- moran_df %>% mutate( quadrant = 
                                   ifelse(B01001e26 > mean(B01001e26, na.rm = TRUE) & lag > mean(lag, na.rm = TRUE), "HH",
                                          ifelse(B01001e26 > mean(B01001e26, na.rm = TRUE) & lag <= mean(lag, na.rm = TRUE), "HL",
                                                 ifelse(B01001e26 <= mean(B01001e26, na.rm = TRUE) & lag <= mean(lag, na.rm = TRUE), "LL", "LH"))))
#Checking new dataset
moran_df %>% head(5)
#Creating Moran plot
ggplot(moran_df, aes(x =B01001e26, y =lag , color = quadrant, shape = quadrant)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("HH" = "red", "HL" = "blue", "LL" = "green", "LH" = "orange")) +
  geom_hline(yintercept=mean(moran_df$lag), lty=2) + 
  geom_vline(xintercept=mean(moran_df$B01001e26), lty=2) +
  labs(title = "Moran Plot", x = "B01001e26", y = "Lagged B01001e26") +
  theme_minimal()


#**B2**
joined.tables <- states4.projected %>% left_join(., moran_df, by = c("GEOID" = "unique_id"))

tm_shape(joined.tables) + 
  tm_borders() +
  tm_polygons(fill = "quadrant") +
  tm_title("Choropleth Map of Moran Plot of the Counties of Four States")

