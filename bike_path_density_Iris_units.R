# Load required packages
library(sf)       # For spatial operations
library(dplyr)    # For data manipulation
library(stats)    # For aggregate function

# Load required packages
library(sf)       # For spatial operations
library(dplyr)    # For data manipulation
library(stats)    # For aggregate function

base_dir <- getwd()

# Load cycling data if not already available
if (!exists("cycling_data14") || !inherits(cycling_data14, "sf")) {
  # Load the original GeoJSON files for cycling infrastructure
  cycling_data14 <- st_read("paris_cycling_2014.geojson")
  cycling_data21 <- st_read("paris_cycling_2021.geojson")
  
  # Make sure they use the same CRS
  st_crs(cycling_data14) <- 4326  # WGS84
  st_crs(cycling_data21) <- 4326  # WGS84
}

# Memory-efficient approach to calculate cycling infrastructure density
calculate_cycling_density_efficient <- function(iris_sf, cycling_sf, batch_size = 100) {
  # Ensure both datasets have the same CRS
  if (st_crs(iris_sf) != st_crs(cycling_sf)) {
    cycling_sf <- st_transform(cycling_sf, st_crs(iris_sf))
  }
  
  # Simplify geometries to reduce complexity
  iris_simplified <- st_simplify(iris_sf, dTolerance = 5)  # 5m tolerance
  cycling_simplified <- st_simplify(cycling_sf, dTolerance = 2)  # 2m tolerance
  
  # Calculate area of each IRIS polygon in km²
  iris_simplified$area_km2 <- as.numeric(st_area(iris_simplified)) / 1000000
  
  # Initialize results dataframe
  all_results <- data.frame(
    iris_id = iris_simplified$code_iris,
    iris_name = iris_simplified$nom_iris,
    area_km2 = iris_simplified$area_km2,
    cycling_length_km = 0
  )
  
  # Process in batches to save memory
  total_iris <- nrow(iris_simplified)
  num_batches <- ceiling(total_iris / batch_size)
  
  for (b in 1:num_batches) {
    cat(paste0("Processing batch ", b, " of ", num_batches, "...\n"))
    
    # Determine batch indices
    start_idx <- (b-1) * batch_size + 1
    end_idx <- min(b * batch_size, total_iris)
    batch_indices <- start_idx:end_idx
    
    # Process this batch of IRIS areas
    iris_batch <- iris_simplified[batch_indices, ]
    
    # Create spatial index to speed up operations
    iris_bbox <- st_bbox(iris_batch)
    cycling_candidates <- cycling_simplified[st_intersects(
      st_as_sfc(iris_bbox), 
      cycling_simplified, 
      sparse = FALSE
    )[1, ], ]
    
    # Only process if there are cycling paths in this region
    if (nrow(cycling_candidates) > 0) {
      # Perform intersection
      gc()  # Force garbage collection before heavy operation
      intersections <- st_intersection(cycling_candidates, iris_batch)
      
      # Calculate lengths and aggregate by IRIS
      if (nrow(intersections) > 0) {
        intersections$length_km <- as.numeric(st_length(intersections)) / 1000
        
        # Aggregate by IRIS ID
        if ("code_iris" %in% names(intersections)) {
          length_by_iris <- aggregate(
            length_km ~ code_iris,
            data = intersections,
            FUN = sum
          )
          
          # Update results
          for (i in 1:nrow(length_by_iris)) {
            iris_id <- length_by_iris$code_iris[i]
            idx <- which(all_results$iris_id == iris_id)
            if (length(idx) > 0) {
              all_results$cycling_length_km[idx] <- length_by_iris$length_km[i]
            }
          }
        }
      }
    }
    
    # Free memory
    rm(iris_batch, cycling_candidates)
    if (exists("intersections")) rm(intersections)
    gc()
  }
  
  # Calculate density
  all_results$cycling_density_km_per_km2 <- all_results$cycling_length_km / all_results$area_km2
  
  return(all_results)
}

# Load IRIS shapefile if not already loaded
if (!exists("iris_shapefile") || !inherits(iris_shapefile, "sf")) {
  iris_shapefile_path <- file.path(getwd(), "iris@datailedefrance/iris.shp")
  iris_shapefile <- st_read(iris_shapefile_path, quiet = TRUE)
}

# Calculate density for 2014 (smaller dataset)
cat("Calculating cycling density for 2014...\n")
density_2014 <- calculate_cycling_density_efficient(iris_shapefile, cycling_data14, batch_size = 100)

# Calculate density for 2021 (larger dataset)
cat("Calculating cycling density for 2021...\n")
density_2021 <- calculate_cycling_density_efficient(iris_shapefile, cycling_data21, batch_size = 50)  # Smaller batch size for larger dataset

# Add year
density_2014$year <- 2014
density_2021$year <- 2021

# Calculate change metrics
density_2021$length_change_km <- density_2021$cycling_length_km - density_2014$cycling_length_km
density_2021$density_change <- density_2021$cycling_density_km_per_km2 - density_2014$cycling_density_km_per_km2
density_2021$pct_length_change <- ifelse(
  density_2014$cycling_length_km > 0,
  (density_2021$cycling_length_km - density_2014$cycling_length_km) / density_2014$cycling_length_km * 100,
  ifelse(density_2021$cycling_length_km > 0, 100, 0)
)

# Create a long-format dataset for DiD analysis
density_did <- rbind(
  density_2014[, c("iris_id", "iris_name", "area_km2", "cycling_length_km", "cycling_density_km_per_km2", "year")],
  density_2021[, c("iris_id", "iris_name", "area_km2", "cycling_length_km", "cycling_density_km_per_km2", "year")]
)

# Add T2021 indicator for DiD
density_did$T2021 <- ifelse(density_did$year == 2021, 1, 0)

# Create a wide-format dataset for easier comparison
density_wide <- merge(
  density_2014[, c("iris_id", "cycling_length_km", "cycling_density_km_per_km2")],
  density_2021[, c("iris_id", "cycling_length_km", "cycling_density_km_per_km2", 
                   "length_change_km", "density_change", "pct_length_change")],
  by = "iris_id",
  suffixes = c("_2014", "_2021")
)

# Save outputs
write.csv(density_did, file.path(base_dir, "iris_cycling_density_for_did.csv"), row.names = FALSE)
write.csv(density_wide, file.path(base_dir, "iris_cycling_density_comparison.csv"), row.names = FALSE)

# Merge with the previous distance analysis
if (exists("iris_cycling_did")) {
  # First, reshape iris_cycling_did to wide format for easier merging
  iris_cycling_wide <- reshape(
    iris_cycling_did,
    idvar = "iris_id",
    timevar = "year",
    direction = "wide"
  )
  
  # Rename columns to be more descriptive
  names(iris_cycling_wide) <- gsub("dist_to_bike.", "dist_to_bike_", names(iris_cycling_wide))
  
  # Merge with density wide data
  combined_data <- merge(iris_cycling_wide, density_wide, by = "iris_id", all = TRUE)
  
  # Save combined dataset
  write.csv(combined_data, file.path(base_dir, "iris_cycling_combined_metrics.csv"), row.names = FALSE)
}

# Print summary statistics
cat("Summary of cycling infrastructure density metrics:\n")
cat("Average cycling infrastructure length in 2014 (km per IRIS):", mean(density_2014$cycling_length_km), "\n")
cat("Average cycling infrastructure length in 2021 (km per IRIS):", mean(density_2021$cycling_length_km), "\n")
cat("Average cycling infrastructure density in 2014 (km/km²):", mean(density_2014$cycling_density_km_per_km2), "\n")
cat("Average cycling infrastructure density in 2021 (km/km²):", mean(density_2021$cycling_density_km_per_km2), "\n")
cat("Average increase in cycling infrastructure length (km):", mean(density_2021$length_change_km), "\n")
cat("Average increase in cycling infrastructure density (km/km²):", mean(density_2021$density_change), "\n")

