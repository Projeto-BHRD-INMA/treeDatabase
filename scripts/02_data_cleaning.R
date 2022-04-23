################################################################################
###                                                                          ###
###             TO CLEAN INCORRECT OCCURRENCE RECORDS FROM A TABLE           ###
###                                                                          ###
###                                                                          ###
###                Created by Danielle de O. Moreira                         ###
###                     created: 11 feb 2021                                 ###
###                     updated: 22 apr 2022                                 ###
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

# remove records without coordinates and without locality information (white spaces)
spp.gbif.table <- spp.gbif.table %>%
  filter(!is.na(decimalLongitude)) %>%
  filter(!is.na(decimalLatitude)) %>% 
  filter(trimws(locality) !="") # need to change white spaces to blank space and then remove them


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
write.csv(data_clean_T, "./results/spp_gbif_Brasil_clean.csv")


## TRANSFORM POINTS TO SPATIAL DATA --------------------------------------------
## Read the shapefile of BHRD
bhrd <- readOGR("./data/bhrd_wgs84.shp") #loading shapefile

## Check CRS information 
proj4string(bhrd)

# Transform csv in shp for spatial manipulation
data <- read.csv("./results/spp_gbif_Brasil_clean.csv", 
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
write.csv(spp.shp.bhrd, "./results/spp_gbif_bhrd.csv")


# CORRECT SPECIFIC POINTS FALLING IN THE WRONG PLACE  --------------------------
# Load csv data
dat_bhrd <- read.csv("./results/spp_gbif_bhrd.csv", 
                     header = T, sep=",", dec=".",
                     encoding="utf-8")
names(dat_bhrd)

# Delete records for Reserva Biológica de Sooretama, Reserva Biológica de Comboios, Pontal do Ipiranga. Coordinates are wrong and these places are not inside our study area.
dat_bhrd1 <- dat_bhrd %>%
  filter(!str_detect(locality, 
                    regex("Sooretama|Comboios|Comboio|Pontal do Ipiranga|Ponta do Ipiranga", 
                          ignore_case = TRUE))
  )

# Correct points Falling out from Reserva Natural da Vale, Linhares, ES
# Create a polygon representing the Reserva
v_y_coord <- c(-19.21564, -19.22470, -19.26003, -19.27045, -19.21564)
v_x_coord <- c(-40.02190, -39.91275, -39.90799, -40.02280, -40.02190)

# -40.02190,-19.21564
# -39.91275,-19.22470
# -39.90799,-19.26003
# -40.02280,-19.27045

## closing the polygon
v_x_coord[length(v_x_coord) + 1] <- v_x_coord[1]
v_y_coord[length(v_y_coord) + 1] <- v_y_coord[1]

## construct sf POLYGON for the Reserva Natural Vale
v_sf_poly <- sf::st_sf( geometry = sf::st_sfc(sf::st_polygon(x = list(matrix(c(v_x_coord, v_y_coord), ncol = 2)))) )

# Plot to a map
plot(v_sf_poly)
plot(bhrd, add = TRUE)
points(spp.shp.bhrd$decimalLongitude, spp.shp.bhrd$decimalLatitude, col = "red", cex = .3)

# construct sf points. When do it, columns for "lon" and "lat" are replaced to "geometry".
# Rename column where names is "decimalLongitude.1" and "decimalLatitude.1"
names(dat_bhrd1)[names(dat_bhrd1) == "decimalLongitude.1"] <- "lon"
names(dat_bhrd1)[names(dat_bhrd1) == "decimalLatitude.1"] <- "lat"

dat_bhrd1 <- dat_bhrd1[-c(1, 19)] # exclude column X and optional
names(dat_bhrd1)

# Create sf points for the original BHRD table
sf_point_or <- sf::st_as_sf(dat_bhrd1, coords = c("lon", "lat"))
names(sf_point_or)

# Select records designed as Reserva Natural Vale, with and without coordinates errors
v_sf_point <- sf_point_or %>%
  filter(str_detect(locality, 
                    regex("Reserva Natural Vale|Reserva Natural de CVRD|Reserva da Natural do Vale do Rio|Reserva da Natural da Companhia Vale do Rio|Reserva Florestal da CVRD|Reseva Florestal da CVRD|Reserve Florestal da CVRD|Reserva Floresta da CVRD|Reserva Natural da Vale|Reserva Florestal CVRD|Reserva Florestal de CVRD|Reserva Natural de CVRD|Reserva Florestal do CVRD|Reserva da Vale|Reserva da CVRD|Reserva Natural da CVRD|Forest Reserve Companhia Vale do Rio Doce|Linhares. Forest Reserve. Companhia Vale do Rio Doce|Res. Fl. da CVRD|Res. Fl. CVRD|Reserva Florestal de Linhares|Reserva Florestal do Linhares|Reserva Florestal Linhares|Reserva F. Linhares|Reserva Florestal da Cia. Vale|Reserva Florestal da Companhia Vale do Rio Doce|Res. Flor. Linhares. CVRD|Reserva Florestal Vale do Rio Doce|Reserva da Companhia Vale do Rio Doce|Reserva Floresta da CVRD|Reserva Floresta da C.V.R.D.|Reserva Florestal, C.V.R.D.|Reserva Florestal, CVRD|Linhares Vale do rio Doce|Res. Fl. Linhares|Reserva da Companhia Vale do Ri Doce|Reserva Vale|Estrada do Flamengo, Km 1. Reserva Vale|Reserva do Vale de Rio Doce|Reserva da Campanhia Vale do Rio Doce|Reserva da Compania Vale do Rio Doce|Reserva Florestal de Linahres|Reserva Nat. Vale", 
                          ignore_case = TRUE))
  )

# Records that fall within the polygon
sf_point_vale <- sf::st_intersection(v_sf_point, v_sf_poly)

# Exclude records that fall within the polygon from the v_sf_point dataframe. I will have a df with wrong coordinates records for Reserva da Vale only.
v_sf_point2 <- v_sf_point %>%
  filter(!v_sf_point$gbifID %in% sf_point_vale$gbifID)

plot(v_sf_poly)
plot(bhrd, add = TRUE)
points(v_sf_point2$decimalLongitude, v_sf_point2$decimalLatitude, col = "red", cex = .5)
# No points inside the rectangle. Correct

# Fix coordinates 
# Duplicate data
v_sf_point3 <- v_sf_point2
names(v_sf_point3)

# Designate correct coordinates for Reserva Natural Vale
#-39.95583,-19.23365 lon/lat
v_sf_point3$decimalLongitude[1:783] <- -39.95583
v_sf_point3$decimalLatitude[1:783] <- -19.23365

plot(v_sf_poly)
plot(bhrd, add = TRUE)
points(v_sf_point3$decimalLongitude, v_sf_point3$decimalLatitude, col = "red", cex = .5)


# Exclude records from sf_point_or that are in the v_sf_point3 df to not generate duplicates records
v_sf_point_exc <- sf_point_or %>%
  filter(!sf_point_or$gbifID %in% v_sf_point3$gbifID)

# Join dataframes
names(v_sf_point_exc)
names(v_sf_point3) # Table with coordinates corrected. Names of both table are the same
v_total_join <- rbind(v_sf_point_exc, v_sf_point3) #join

#Comapare original and final tables. Same number of rows
nrow(sf_point_or)
nrow(v_total_join)

plot(bhrd)
points(v_total_join$decimalLongitude, v_total_join$decimalLatitude, col = "red", cex = .5)

plot(v_sf_poly)
plot(bhrd, add = TRUE)
points(v_total_join$decimalLongitude, v_total_join$decimalLatitude, col = "red", cex = .5)



####
# Points Falling out from Parque Estadual do Rio Doce (PERD), MG.

# Create a polygon representing the PERD
d_y_coord <- c(-19.49395, -19.48375, -19.82261, -19.81022, -19.49395)
d_x_coord <- c(-42.69826, -42.45705, -42.43373, -42.68587, -42.69826)

# -42.69826,-19.49395
# -42.45705,-19.48375
# -42.43373,-19.82261
# -42.68587,-19.81022

## closing the polygon
d_x_coord[length(d_x_coord) + 1] <- d_x_coord[1]
d_y_coord[length(d_y_coord) + 1] <- d_y_coord[1]

## construct sf POLYGON for the PERD
d_sf_poly <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(x = list(matrix(c(d_x_coord, d_y_coord), ncol = 2)))) )

# Plot to a map
plot(bhrd)
points(v_total_join$decimalLongitude, v_total_join$decimalLatitude, col = "red", cex = .3)
plot(d_sf_poly, add = TRUE)

# Select records designed as PERD, with and without coordinates errors
d_sf_point <- v_total_join %>%
  filter(str_detect(locality, 
                    regex("Parque Estadual do Rio Doce|Parque Estadual Rio Doce", 
                          ignore_case = TRUE)) |
           str_detect(locality, 
                      regex("PERD", 
                            ignore_case = FALSE))
  )

# Records that fall within the polygon
sf_point_perd <- sf::st_intersection(d_sf_point, d_sf_poly)

# Exclude records that fall within the polygon from the d_sf_point dataframe
d_sf_point2 <- d_sf_point %>%
  filter(!d_sf_point$gbifID %in% sf_point_perd$gbifID)

plot(d_sf_poly)
plot(bhrd, add = TRUE)
points(d_sf_point2$decimalLongitude, d_sf_point2$decimalLatitude, col = "red", cex = .5)
# No points inside the rectangle. Correct

# Fix coordinates 
# Duplicate data
d_sf_point3 <- d_sf_point2
names(d_sf_point3)

# Design correct coordinates for PERD
#-42.54716,-19.66219 lon/lat
d_sf_point3$decimalLongitude[1:76] <- -42.54716
d_sf_point3$decimalLatitude[1:76] <- -19.66219

plot(d_sf_poly)
plot(bhrd, add = TRUE)
points(d_sf_point3$decimalLongitude, d_sf_point3$decimalLatitude, col = "red", cex = .5)

# Exclude records from v_total_join that are in the d_sf_point3 df to not generate duplicates records
d_sf_point_exc <- v_total_join %>%
  filter(!v_total_join$gbifID %in% d_sf_point3$gbifID)

# Join dataframes
names(d_sf_point_exc)
names(d_sf_point3) # Table with coordinates corrected. Names of both table are the same
d_total_join <- rbind(d_sf_point_exc, d_sf_point3) #join

#Comapare original and final tables. Same number of rows
nrow(v_total_join)
nrow(d_total_join)

plot(bhrd)
points(d_total_join$decimalLongitude, d_total_join$decimalLatitude, col = "red", cex = .5)
plot(d_sf_poly, add = TRUE)

write.csv(d_total_join, "./results/spp_gbif_bhrd_clean.csv")


# SAVE SHP ---------------------------------------------------------------------
# Save shp points of occurrences === I think it's better to convert the spp_gbif_bhrd_clean.csv to a shapefile in QGis because the names of columns are not abreviated.

# Transform sf to dataframe
final_table <- as.data.frame(d_total_join)
final_table <- final_table[-c(15)] # exclude column geometry

## Converting data.frame into a SpatialPointsDataFrame
### note that the lon and lat columns are in columns 3 and 4
### Get long and lat from your data.frame. Make sure that the order is in lon/lat.
latlon <- final_table[,c(3, 4)]

### convert
final_shp <- SpatialPointsDataFrame(coords = latlon, 
                                  data = final_table,
                                  proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

plot(final_shp)

writeOGR(final_shp, "./SIG", 
         "spp_gbif_bhrd_clean", driver="ESRI Shapefile", overwrite_layer = TRUE)

