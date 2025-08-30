#testing different methods for geojson file conversion to pbf file 
library(sf)
library(tidyselect)
library(dplyr)


#METHOD 1: convert the data frames into geojson files-->osm files --> pbf files
paris_cyclinggeojson<- st_read("amenagements-cyclables (1).geojson", quiet = T)


paris_geojson_yearsep <- tidyr::separate_wider_delim(paris_cyclinggeojson,
                                                 cols = "date_export", 
                                                 delim = "-", 
                                                 names = "Year",
                                                 too_many = "debug")

paris_geojson_yearsep<- data.frame(paris_geojson_yearsep)
#making the 2014 subset
paris_geojson2014 <- subset(paris_geojson_yearsep, Year %in% "2014")
paris_geojson2014 <- tidyr::separate_wider_delim(paris_geojson2014,
                                                     cols = "geo_point_2d", 
                                                     delim = ",", 
                                                     names = "lon",
                                                     too_many = "debug")
colnames(paris_geojson2014)[colnames(paris_geojson2014) == "geo_point_2d_remainder"] <- "lat"

#making the 2021 subset
up_to_21 <- c("2014",
              "2015",
              "2016",
              "2017",
              "2018",
              "2019",
              "2020",
              "2021")
paris_geojson2021 <- subset(paris_geojson_yearsep, Year %in% up_to_21)
paris_geojson2021 <- tidyr::separate_wider_delim(paris_geojson2021,
                                                 cols = "geo_point_2d", 
                                                 delim = ",", 
                                                 names = "lon",
                                                 too_many = "debug")
colnames(paris_geojson2021)[colnames(paris_geojson2021) == "geo_point_2d_remainder"] <- "lat"

#clean the paris_geojson data set more for only columns needed
paris_geojson2014 <- subset(paris_geojson2014, select = -c(date_export_ok,date_export_pieces,date_export_remainder,geo_point_2d_ok,geo_point_2d_pieces ))
paris_geojson2021 <- subset(paris_geojson2021, select = -c(date_export_ok,date_export_pieces,date_export_remainder,geo_point_2d_ok,geo_point_2d_pieces ))

#clean the longitude and latitude columns 
paris_geojson2014$lat <- gsub("^.{0,9}", "", paris_geojson2014$lat)
paris_geojson2014$lon <- gsub("^.{0,9}", "", paris_geojson2014$lon)
paris_geojson2021$lat <- gsub("^.{0,9}", "", paris_geojson2021$lat)
paris_geojson2021$lon <- gsub("^.{0,9}", "", paris_geojson2021$lon)
paris_geojson2014$lat <- stringr::str_sub(paris_geojson2014$lat, end=-2)
paris_geojson2021$lat <- stringr::str_sub(paris_geojson2021$lat, end=-2)

# Make sure the length is calculated and preserved
# why might the lengths be slightly off in the hundredths decimal place?
paris_geojson2014final <- paris_geojson2014 %>%
  mutate(length_m = st_length(geometry))

paris_geojson2021final <- paris_geojson2021 %>%
  mutate(length_m = st_length(geometry))

#writing the geojson files out to save

st_write(paris_geojson2014final, "paris_cycling_2014.geojson", delete_dsn = TRUE)
st_write(paris_geojson2021final, "paris_cycling_2021.geojson", delete_dsn = TRUE)

#validating geojson files 
list.files(pattern = "*.geojson")
library(sf)
# Try to read the file to see if it's valid
tryCatch({
  cycling_data <- st_read("paris_cycling_2014.geojson")
  print("File is readable with sf")
}, error = function(e) {
  print(paste("Error reading file:", e$message))
})

#testing that my file is actually a geojson file (answered, yes)
geojson_data <- geojsonio::geojson_read("paris_cycling_2014.geojson", what = "sp")
geojsonio::geojson_read("paris_cycling_2014.geojson", what = "sp")



                    

                        
                       


 
