#Cycling Density and Cycling Access metrics with added control of distance to the city center 
#Sophie McCauley 
#August 30 2025


#calculating the distance to center for each IRIS area, to use as a control with the cycling access and density regressions 
center_coords <- data_frame("lon" = 2.348301, "lat" = 48.85398)
#center coordinates chosen from the "Point Zero" in Paris, used as a measure throughout th city's history to measure distance for Paris to the suburbs 

#calculate distance from centroids to center iris area 
iris_dist_to_center <- iris_origins %>% select(
"code_iris",
"centroid_lon",
"centroid_lat"
)
iris_dist_to_center$center_lon <- 2.348301
iris_dist_to_center$center_lat <- 48.85398

iris_dist_to_center$dist_center <- sqrt((
  (iris_dist_to_center$centroid_lon - iris_dist_to_center$center_lon)^2 + 
    (iris_dist_to_center$centroid_lat- iris_dist_to_center$center_lon)^2
) * 111320)/1000  # Approximate conversion from degrees to meters at Paris latitude

##########
# Haversine formula function in R
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  # Radius of the Earth in kilometers
  R <- 6371.0
  
  # Convert latitude and longitude from degrees to radians
  lat1_rad <- lat1 * (pi/180)
  lon1_rad <- lon1 * (pi/180)
  lat2_rad <- lat2 * (pi/180)
  lon2_rad <- lon2 * (pi/180)
  
  # Differences in coordinates
  dlon <- lon2_rad - lon1_rad
  dlat <- lat2_rad - lat1_rad
  
  # Haversine formula
  a <- sin(dlat/2)^2 + cos(lat1_rad) * cos(lat2_rad) * sin(dlon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  distance <- R * c
  
  return(distance)
}

# Paris city center coordinates
paris_lat <- 48.85398
paris_lon <- 2.348301

# Your coordinates (replace with your actual coordinates)
iris_lat <- iris_dist_to_center$centroid_lat  # Replace with your latitude
iris_lon <- iris_dist_to_center$centroid_lon  # Replace with your longitude

# Calculate distance
iris_dist_to_center$distance_tocenter <- haversine_distance(iris_lat, iris_lon, paris_lat, paris_lon)

colnames(iris_dist_to_center)[colnames(iris_dist_to_center) == "code_iris"] <- "iris_id"
colnames(iris_dist_to_center)[colnames(iris_dist_to_center) == "distance_tocenter"] <- "distance_tocenter"

iris_dist_to_center <- iris_dist_to_center %>% select(-"dist_center")

new_dist <- iris_dist_to_center %>% select("iris_id", "centroid_lon", "centroid_lat", "distance_tocenter")

#joining the distance to center data with the cycling access file 
cycling_density <- read.csv("iris_cycling_density_for_did.csv")
paris1421_cyclingaccess <- read.csv("iris_cycling_access_for_did.csv")

new_dist$iris_id <- as.numeric(new_dist$iris_id)

cycling_density_dist <- left_join(cycling_density, new_dist, by = "iris_id")

paris1421_cyclingaccess_dist <- left_join(paris1421_cyclingaccess, new_dist, by = "iris_id")

saveRDS(cycling_density_dist, file = "cycling_density_dist.RDS")
saveRDS(paris1421_cyclingaccess_dist, file = "paris1421_cyclingaccess_dist.RDS")

