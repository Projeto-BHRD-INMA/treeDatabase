################################################################################
###                                                                          ###
###             TO CLEAN INCORRECT OCCURRENCE RECORDS FROM A TABLE           ###
###                                                                          ###
###                                                                          ###
###                Created by Danielle de O. Moreira                         ###
###                         19 Apr 2022                                      ###
### From tutorial                                                            ###
### https://cran.r-project.org/web/packages/CoordinateCleaner/vignettes/Cleaning_GBIF_data_with_CoordinateCleaner.html                                
################################################################################

# SET UP LIBRARIES AND DATA ----------------------------------------------------
#You might need to confirm to install the rnaturalearth package when loading CoordinateCleaner

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
dat <- read.csv("./results/species_gbif_bhrd_clean.csv", 
                        header = T, sep=",", dec=".",
                        encoding="utf-8")

# names(dat) #a lot of columns

#select columns of interest 
dat <- dat %>%
  dplyr::select(species, countryCode, stateProvince, locality, decimalLongitude, decimalLatitude, individualCount, datasetKey, family, taxonRank, coordinateUncertaintyInMeters, year, basisOfRecord, institutionCode, collectionCode)

# remove records without coordinates 
dat <- dat%>%
  filter(!is.na(decimalLongitude))%>%
  filter(!is.na(decimalLatitude))


# VISUALIZE THE DATA ON A MAP --------------------------------------------------
# plot data to get an overview
bhrd <- readOGR("/home/danimoreira/01_INMA_BHRD/GIS/bhrd_wgs84.shp") #loading shapefile
wm <- borders(bhrd, colour="gray50", fill="gray50")
ggplot()+ coord_fixed()+ wm +
  geom_point(data = dat, aes(x = decimalLongitude, y = decimalLatitude),
             colour = "darkred", size = 0.5)+
  theme_bw()


# FLAG PROBLEMATIC RECORDS -----------------------------------------------------
# Use CoordinateCleaner.

#convert country code from ISO2c to ISO3c
dat$countryCode <-  countrycode(dat$countryCode, origin =  'iso2c', destination = 'iso3c')

#flag problems
dat <- data.frame(dat)
flags <- clean_coordinates(x = dat, 
                           lon = "decimalLongitude", 
                           lat = "decimalLatitude",
                           countries = "countryCode",
                           species = "species",
                           tests = c("capitals", "centroids", "equal","gbif", "institutions",
                                     "zeros", "countries")) # most test are on by default

summary(flags)
plot(flags, lon = "decimalLongitude", lat = "decimalLatitude")

#Exclude problematic records
dat_cl <- dat[flags$.summary,]

#The flagged records
dat_fl <- dat[!flags$.summary,]


# IMPROVING DATA QUALITY USING GBIF META-DATA ----------------------------------
# Remove records with low coordinate precision. A histogram of the coordinate precision in the dataset.
hist(dat_cl$coordinateUncertaintyInMeters / 1000, breaks = 20)

dat_cl <- dat_cl %>%
  filter(coordinateUncertaintyInMeters / 1000 <= 100 | is.na(coordinateUncertaintyInMeters))

# Remove unsuitable data sources 
table(dat$basisOfRecord)

dat_cl <- filter(dat_cl, basisOfRecord == "HUMAN_OBSERVATION" | 
                   basisOfRecord == "OBSERVATION" |
                   basisOfRecord == "PRESERVED_SPECIMEN")

# Remove records with suspicious individual counts. GBIF includes few records of absence (individual count = 0) and suspiciously high occurrence counts, which might indicate inappropriate data or data entry problems.
# Individual count
table(dat_cl$individualCount)

dat_cl <- dat_cl%>%
  filter(individualCount > 0 | is.na(individualCount))%>%
  filter(individualCount < 99 | is.na(individualCount)) # high counts are not a problem

table(dat_cl$taxonRank)

# plot data to get an overview
# Rename column where names is "decimalLongitude" and "decimalLatitude"
dat_cl1 <- dat_cl
names(dat_cl1)[names(dat_cl1) == "decimalLongitude"] <- "lon"
names(dat_cl1)[names(dat_cl1) == "decimalLatitude"] <- "lat"

plot(bhrd)
points(dat_cl1$lon, dat_cl1$lat, col = "red", cex = .1)

# or
# coordinates(dat_cl1) <- c("decimalLongitude", "decimalLatitude") # tells r to use x and y collumns as the lat/long 
# proj4string(dat_cl1) <- CRS("+proj=longlat +ellps=WGS84 +no_defs")
# plot(dat_cl1)



# CORRECT SPECIFIC POINTS FALLING IN THE WRONG PLACE  -----------------------------------
# Points Falling out from Reserva Natural da Vale

# Create a polygon representing the Reserva
y_coord <- c(-19.21564, -19.22470, -19.26003, -19.27045, -19.21564)
x_coord <- c(-40.02190, -39.91275, -39.90799, -40.02280, -40.02190)
xym <- cbind(x_coord, y_coord)
xym

# -40.02190,-19.21564
# -39.91275,-19.22470
# -39.90799,-19.26003
# -40.02280,-19.27045

p = Polygon(xym)
ps = Polygons(list(p),1)
sps = SpatialPolygons(list(ps))
plot(sps)

# Assign coordinate reference system to the polygon
proj4string(sps) = CRS("+proj=longlat +ellps=WGS84 +no_defs")

# Transform to a spatial polygon dataframe
data = data.frame(f=99.9)
spdf = SpatialPolygonsDataFrame(sps,data)
spdf

# Plot to a map
plot(spdf)
plot(bhrd, add = TRUE)
points(dat_cl1$lon, dat_cl1$lat, col = "red", cex = .5)

# Select records designed as Reserva Natural Vale, but falling outside it
dat_cl2 <- dat_cl1 %>%
  filter(str_detect(locality, regex("Reserva Natural Vale|Reserva Florestal da CVRD|Reserva Natural da Vale|Reserva Florestal CVRD", 
                                    ignore_case = TRUE)))
dat_vale <- dat_cl2

plot(bhrd)
points(dat_vale$lon, dat_vale$lat, col = "red", cex = .5)

#Coordenadas para a Reserva Natural Vale
#-39.95583,-19.23365


# Subsetting rows of dt1 for values 0.5 to 1 of x1 −

t1 <- dat_vale[between(dat_vale$lat, y_coord)]

