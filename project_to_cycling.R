# First, let's create a function to project points to the nearest cycling path
project_to_cycling_network <- function(points_df, cycling_network_sf) {
  # Convert points to SF object
  points_sf <- st_as_sf(points_df, coords = c("lon", "lat"), crs = 4326)
  
  # For each point, find the nearest cycling path and get its closest point
  projected_points <- lapply(1:nrow(points_sf), function(i) {
    point <- points_sf[i,]
    # Find the nearest cycling path
    distances <- st_distance(point, cycling_network_sf)
    nearest_idx <- which.min(distances)
    nearest_path <- cycling_network_sf[nearest_idx,]
    
    # Project point to the nearest path
    projected_point <- st_nearest_points(point, nearest_path)
    
    # Extract the endpoint (the point on the line)
    projected_coords <- st_coordinates(projected_point)[2, ]
    
    # Create a data frame with the original ID and the projected coordinates
    data.frame(
      id = point$id,
      projected_lon = projected_coords["X"],
      projected_lat = projected_coords["Y"]
    )
  })
  
  # Combine results into a single data frame
  projected_df <- do.call(rbind, projected_points)
  
  # Return as a data frame suitable for r5r
  return(data.frame(
    id = projected_df$id,
    lon = projected_df$projected_lon,
    lat = projected_df$projected_lat
  ))
}
