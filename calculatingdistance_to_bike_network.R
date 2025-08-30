# First, project all IRIS centroids to their nearest cycling path for both years
all_points_2014 <- project_to_cycling_network(iris_origins, cycling_data14)
all_points_2021 <- project_to_cycling_network(iris_origins, cycling_data21)

# Calculate distance from original centroids to nearest cycling path
# This will be a key metric showing how cycling infrastructure coverage changed
iris_cycling_access <- data.frame(
  iris_id = iris_origins$id,
  
  # 2014 data
  x_2014 = iris_origins$lon,
  y_2014 = iris_origins$lat,
  x_proj_2014 = all_points_2014$lon,
  y_proj_2014 = all_points_2014$lat,
  
  # 2021 data
  x_2021 = iris_origins$lon,
  y_2021 = iris_origins$lat,
  x_proj_2021 = all_points_2021$lon,
  y_proj_2021 = all_points_2021$lat
)

# Calculate distances to cycling paths (in meters)
iris_cycling_access$dist_to_bike_2014 <- sqrt(
  (iris_cycling_access$x_2014 - iris_cycling_access$x_proj_2014)^2 + 
    (iris_cycling_access$y_2014 - iris_cycling_access$y_proj_2014)^2
) * 111320  # Approximate conversion from degrees to meters at Paris latitude

iris_cycling_access$dist_to_bike_2021 <- sqrt(
  (iris_cycling_access$x_2021 - iris_cycling_access$x_proj_2021)^2 + 
    (iris_cycling_access$y_2021 - iris_cycling_access$y_proj_2021)^2
) * 111320  # Approximate conversion from degrees to meters at Paris latitude

# Calculate the improvement in distance to cycling infrastructure
iris_cycling_access$dist_improvement <- iris_cycling_access$dist_to_bike_2014 - iris_cycling_access$dist_to_bike_2021
iris_cycling_access$pct_improvement <- (iris_cycling_access$dist_to_bike_2014 - iris_cycling_access$dist_to_bike_2021) / iris_cycling_access$dist_to_bike_2014 * 100

# Structure for DiD analysis
iris_cycling_did <- data.frame(
  iris_id = rep(iris_cycling_access$iris_id, 2),
  year = c(rep(2014, nrow(iris_cycling_access)), rep(2021, nrow(iris_cycling_access))),
  dist_to_bike = c(iris_cycling_access$dist_to_bike_2014, iris_cycling_access$dist_to_bike_2021)
)

iris_cycling_did$T2021 <- ifelse(iris_cycling_did$year == 2021, 1, 0)

# Save for DiD analysis
write.csv(iris_cycling_did, file.path(base_dir, "iris_cycling_access_for_did.csv"), row.names = FALSE)