################################################################################
###                                                                          ###
###             TO CLEAN INCORRECT OCCURRENCE RECORDS FROM A TABLE           ###
###                                                                          ###
###                                                                          ###
###                Created by Danielle de O. Moreira                         ###
###                     created: 11 feb 2021                                 ###
###                     updated: 21 apr 2022                                 ###
################################################################################

# SET UP LIBRARIES AND DATA ----------------------------------------------------
#You might need to confirm to install the rnaturalearth package when loading CoordinateCleaner

library(data.table)
library(countrycode)
library(CoordinateCleaner)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(rgbif)
library(sp)
library(rgdal)
library(raster)

# Obtain data
#?fread
spp.gbif.table <- fread("./results/gbif/gbif-BHRD-all-0191124-210914110416597.csv")

### Checking the column names of our dataframe.
names(spp.gbif.table) #a lot of columns

#select columns of interest 
spp.gbif.table <- spp.gbif.table %>%
  dplyr::select(species, countryCode, stateProvince, locality, decimalLongitude, decimalLatitude, individualCount, gbifID, family, taxonRank, coordinateUncertaintyInMeters, year, basisOfRecord, institutionCode, collectionCode)

# remove records without coordinates 
spp.gbif.table <- spp.gbif.table %>%
  filter(!is.na(decimalLongitude)) %>%
  filter(!is.na(decimalLatitude))


# FLAG PROBLEMATIC RECORDS -----------------------------------------------------
# CoordinateCleaner Package - Removes or flags mismatches between geographic coordinates and additional country information.
# Will take a while:
data_clean <- clean_coordinates(spp.gbif.table, 
                                lon = "decimalLongitude",
                                lat = "decimalLatitude",
                                countries = "countryCode",
                                species = "species")

summary(data_clean)
names(data_clean)
dim(data_clean)

plot(data_clean, lon = "decimalLongitude", lat = "decimalLatitude")

# to filter only occurrences with no spacial problems
data_clean %>% 
  count(.summary)

data_clean_T <- data_clean %>% 
  filter(.summary == "TRUE")

data_clean_F <- data_clean %>% 
  filter(.summary == "FALSE")


# IMPROVING DATA QUALITY USING GBIF META-DATA ----------------------------------
# Remove records with low coordinate precision. A histogram of the coordinate precision in the dataset.
# For more, see this tutorial
# https://cran.r-project.org/web/packages/CoordinateCleaner/vignettes/Cleaning_GBIF_data_with_CoordinateCleaner.html 

hist(data_clean_T$coordinateUncertaintyInMeters / 1000, breaks = 20)

data_clean_T <- data_clean_T %>%
  filter(coordinateUncertaintyInMeters / 1000 <= 100 | is.na(coordinateUncertaintyInMeters))

# Remove unsuitable data sources 
table(data_clean_T$basisOfRecord)

data_clean_T <- filter(data_clean_T, basisOfRecord == "HUMAN_OBSERVATION" | 
                   basisOfRecord == "OBSERVATION" |
                   basisOfRecord == "PRESERVED_SPECIMEN")

# Remove records with suspicious individual counts. GBIF includes few records of absence (individual count = 0) and suspiciously high occurrence counts, which might indicate inappropriate data or data entry problems.
# Individual count
table(data_clean_T$individualCount)

data_clean_T <- data_clean_T%>%
  filter(individualCount > 0 | is.na(individualCount))%>%
  filter(individualCount < 99 | is.na(individualCount)) # high counts are not a problem

# Select columns of interest 
names(data_clean_T)

data_clean_T <- data_clean_T %>%
  dplyr::select(species, countryCode, stateProvince, locality, decimalLongitude, decimalLatitude, individualCount, gbifID, family, taxonRank, coordinateUncertaintyInMeters, year, basisOfRecord, institutionCode, collectionCode)

dim(data_clean_T)
names(data_clean_T)

## to write the new table
write.csv(data_clean_T, "./results/species_gbif_Brasil_clean.csv")


## TRANSFORM POINTS TO SPATIAL DATA --------------------------------------------
## Read the shapefile of BHRD
bhrd <- readOGR("./data/bhrd_wgs84.shp") #loading shapefile

## Check CRS information 
proj4string(bhrd)

# Transform csv in shp for spatial manipulation
data <- read.csv("./results/species_gbif_Brasil_clean.csv", 
               header = T, sep=",", dec=".",
               encoding="utf-8")
# If above does not work use this:
# data <- fread("./results/species_gbif_Brasil_clean.csv")

# Leaving only columns we want
data <- data %>%
  dplyr::select(species, countryCode, stateProvince, locality, decimalLongitude, decimalLatitude, individualCount, gbifID, family, taxonRank, coordinateUncertaintyInMeters, year, basisOfRecord, institutionCode, collectionCode)
names(data)

## Converting data.frame into a SpatialPointsDataFrame for spatial operations
### note that the lon and lat columns are in columns 5 and 6
### Get long and lat from your data.frame. Make sure that the order is in lon/lat.
xy2 <- data[,c(5, 6)]

### convert
spp.shp <- SpatialPointsDataFrame(coords = xy2, 
                                   data = data,
                                   proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

plot(spp.shp)

# CROP RECORDS TO A REGION -----------------------------------------------------
# Now let's crop the points into the shapefile limits we want to. In this case, the Doce River watershed
spp.shp.bhrd <- crop(spp.shp, bhrd) #cut by file

dim(spp.shp.bhrd)

# plot data to get an overview
plot(bhrd)
points(spp.shp.bhrd$decimalLongitude, spp.shp.bhrd$decimalLatitude, col = "red", cex = .1)

# Save table of species for BHRD
write.csv(spp.shp.bhrd, "./results/species_gbif_bhrd_clean.csv")


# CORRECT SPECIFIC POINTS FALLING IN THE WRONG PLACE  --------------------------
# Points Falling out from Reserva Natural da Vale, Linhares, ES

# Create a polygon representing the Reserva
y_coord <- c(-19.21564, -19.22470, -19.26003, -19.27045, -19.21564)
x_coord <- c(-40.02190, -39.91275, -39.90799, -40.02280, -40.02190)

# -40.02190,-19.21564
# -39.91275,-19.22470
# -39.90799,-19.26003
# -40.02280,-19.27045

## closing the polygon
x_coord[length(x_coord) + 1] <- x_coord[1]
y_coord[length(y_coord) + 1] <- y_coord[1]

## construct sf POLYGON for the Reserva Natural Vale
sf_poly <- sf::st_sf( geometry = sf::st_sfc(sf::st_polygon(x = list(matrix(c(x_coord, y_coord), ncol = 2)))) )

# Plot to a map
plot(sf_poly)
plot(bhrd, add = TRUE)
points(spp.shp.bhrd$decimalLongitude, spp.shp.bhrd$decimalLatitude, col = "red", cex = .3)

# construct sf points. When do it, columns for "lon" and "lat" are replaced to "geometry".
# Load csv data
dat_bhrd <- read.csv("./results/species_gbif_bhrd_clean.csv", 
                header = T, sep=",", dec=".",
                encoding="utf-8")
names(dat_bhrd)

# Rename column where names is "decimalLongitude.1" and "decimalLatitude.1"
names(dat_bhrd)[names(dat_bhrd) == "decimalLongitude.1"] <- "lon"
names(dat_bhrd)[names(dat_bhrd) == "decimalLatitude.1"] <- "lat"

dat_bhrd <- dat_bhrd[-c(1)] # exclude column X
names(dat_bhrd)

# Create sf points
sf_point <- sf::st_as_sf(dat_bhrd, coords = c("lon", "lat"))
names(sf_point)

# Select records designed as Reserva Natural Vale, with and without coordinates errors
sf_point2 <- sf_point %>%
  filter(str_detect(locality, 
                    regex("Reserva Natural Vale|Reserva Florestal da CVRD|Reserva Natural da Vale|Reserva Florestal CVRD", 
                          ignore_case = TRUE))
  )

# Records that fall within the polygon
sf_point_vale <- sf::st_intersection(sf_point, sf_poly)

# Exclude records that fall within the polygon from the sf_point2 dataframe
sf_point3 <- sf_point2 %>%
  filter(!sf_point2$gbifID %in% sf_point_vale$gbifID)

plot(sf_poly)
plot(bhrd, add = TRUE)
points(sf_point3$decimalLongitude, sf_point3$decimalLatitude, col = "red", cex = .5)
# No points inside the rectangle. Correct

# Fix coordinates 
# Duplicate data
sf_point4 <- sf_point3
names(sf_point4)

# Design correct coordinates for Reserva Natural Vale
#-39.95583,-19.23365 lon/lat
sf_point4$decimalLongitude[1:448] <- -39.95583
sf_point4$decimalLatitude[1:448] <- -19.23365

plot(sf_poly)
plot(bhrd, add = TRUE)
points(sf_point4$decimalLongitude, sf_point4$decimalLatitude, col = "red", cex = .5)

# Join the new corrected dataframe with the total species records
names(sf_point) # complete table
names(sf_point4) # Table with records from Reserva Vale. Names of both table are the same
total_join <- rbind(sf_point4, sf_point) #join

# To remove duplicate records (by GBIF ID)
final_table <- total_join[!duplicated(total_join$gbifID), ]

#Comapare both tables
nrow(sf_point)
nrow(final_table)

plot(bhrd)
points(final_table$decimalLongitude, final_table$decimalLatitude, col = "red", cex = .5)

plot(sf_poly)
plot(bhrd, add = TRUE)
points(final_table$decimalLongitude, final_table$decimalLatitude, col = "red", cex = .5)


# Save shp points of occurrences 
# Transform sf to SpatialPolygonsDataFrame
final_shp <- as(final_table, "Spatial")

writeOGR(final_shp, "./SIG", 
         "spp_gbif_bhrd_clean", driver="ESRI Shapefile", overwrite_layer = TRUE)








