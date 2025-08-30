#
#SM, 3/31/2025

# Load the required libraries

library(dplyr)
library(sf)


#LOADING IN THE PARIS IRIS SHAPEFILE
iris_shapefilenew <- st_read("iris@datailedefrance/iris.shp", quiet = T)

#centroid calculation
iris_centroidsnew <- st_centroid(iris_shapefilenew$geometry)

# Extract latitude and longitude of centroids
centroid_coords <- st_coordinates(iris_centroidsnew)


# Add centroid coordinates back to the original dataframe
iris_shapefilenew$centroid_lon <- centroid_coords[, 1]
iris_shapefilenew$centroid_lat <- centroid_coords[, 2]


# Define origins as the iris codes -- 
iris_origins <- iris_shapefilenew %>% select(iris, code_iris, centroid_lon, centroid_lat, nom_com) 
#above code just dropping all unnecesary columns that are duplicated or not useful








