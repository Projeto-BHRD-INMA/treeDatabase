################################################################################
###                                                                          ###
###             TO CLEAN INCORRECT OCCURRENCE RECORDS FROM A TABLE           ###
###                                                                          ###
###                                                                          ###
###                Created by Danielle de O. Moreira                         ###
###                         11 feb 2021                                      ###
###                                                                          ###
################################################################################


library(data.table)
#?fread
spp.gbif.table <- fread("./results/gbif/gbif-BHRD-all-0191124-210914110416597.csv")

###Checking the dimensions of the table
dim(spp.gbif.table)

### Checking the column names of our dataframe.
names(spp.gbif.table)

### Filter data for Espírito Santo and Minas Gerais
unique(spp.gbif.table$stateProvince)

#Identify Coordinates Outside their Reported Country -------------------------------------
# CoordinateCleaner Package - Removes or flags mismatches between geographic coordinates and additional country information.
#(usually this information is reliably reported with specimens). Such a mismatch can occur for example, if latitude and longitude are switched.

library(CoordinateCleaner)
# Will take a while:
data_clean <- clean_coordinates(spp.gbif.table, 
                                lon = "decimalLongitude",
                                lat = "decimalLatitude",)

summary(data_clean)
names(data_clean)
dim(data_clean)


# to filter only occurrences with no spacial problems
library(tidyverse)
data_clean %>% 
  count(.summary)

data_clean2 <- data_clean %>% 
  filter(.summary == "TRUE")
dim(data_clean2)

data_clean_F <- data_clean %>% 
  filter(.summary == "FALSE")

names(data_clean2)

# Remove some columns
data_clean3 <- data_clean2[-c(1,51:60)]

dim(data_clean3)
names(data_clean3)

## to write the new table
write.csv(data_clean3, "./SIG/species_gbif_Brasil_clean.csv")

## to write the new table
write.csv(data_clean_F, "./SIG/species_gbif_Brasil_false.csv")


## LET'S PLOT THE CORRECT OCCURRENCES WITH NON CORRECTED OCCuRRENCES----------------------
## Read the shapefile of BHRD
library(rgdal)
library(raster)

bhrd <- readOGR("/home/danimoreira/01_INMA_BHRD/GIS/bhrd_wgs84.shp") #loading shapefile

## Check CRS information 
proj4string(bhrd)

## to assign the crs information  
## reproject shp to geographic system WGS 1984
#crs(amesul) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "
#proj4string(amesul)

## Plot - Will get a while because of the number of occurrences
library(ggplot2)


figura <- ggplot() + 
  geom_polygon(data=bhrd, aes(x = long, y = lat, group = group), fill="grey40", 
               colour = "grey90", alpha = 1) + 
  labs(x="", y="", title="Occurrence points of plants in BHRD") + #labels
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
        plot.title = element_text(lineheight=.8, face="bold", vjust=1)) + # make title bold and add space
  #geom_point(aes(x = decimalLongitude.1, y = decimalLatitude.1, color = .summary),
  #data = data_clean, alpha = 1, size = 3, color = "grey20") +# to get outline
  geom_point(data = data_clean, aes(x = decimalLongitude, y = decimalLatitude, 
                                    color = .summary), size = 1) +
  coord_equal(ratio=1) # square plot to avoid the distortion

figura

#Save figure
# png(".figs/spp_bhrd.png", res = 300, width = 2400, height = 2400)
# figura
# dev.off()



# Crop points to a region -------------------------------------------------------------
library(rgdal)
library(raster)
# First we need to transform csv in shp for spatial manipulation
data <- read.csv("./results/species_gbif_Brasil_clean.csv", 
               header = T, sep=",", dec=".",
               encoding="utf-8")
names(data)
## Converting data.frame into a SpatialPointsDataFrame for spatial operations
### note that the lon and lat columns are in columns 23 and 22
### Get long and lat from your data.frame. Make sure that the order is in lon/lat.
xy2 <- data[,c(23,22)]


### convert
spp.shp2 <- SpatialPointsDataFrame(coords = xy2, 
                                   data = data,
                                   proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

plot(spp.shp2)


# CROP SPECIES RECORDS ----------------------------------------------------------

# Read the shapefile (the limits we want to crop pur data)
bhrd <- readOGR("/home/danimoreira/01_INMA_BHRD/GIS/bhrd_wgs84.shp") #loading shapefile

# Check CRS information 
crs(bhrd)
#proj4string(amesul)

# Now let's crop the points into the shapefile limits we want to. In this case, South America
spp.shp.bhrd <- crop(spp.shp2, bhrd) #cut by file

dim(spp.shp.bhrd)

##Plot spatial objects
plot(spp.shp.bhrd, cex = .3)
plot(bhrd, add=TRUE)

#Save table of species for BHRD
write.csv(spp.shp.bhrd, "./results/species_gbif_bhrd_clean.csv")

#Save shp occurrence points
writeOGR(spp.shp.bhrd, "./SIG", "spp_gbif_bhrd_clean", driver="ESRI Shapefile",
         overwrite_layer = TRUE)


#Clean Occurrence Records---------------------------------------------

#remotes::install_github("ropensci/scrubr")
library("scrubr")

#I couldn't run this because my section failed probably because of memory
spp.bhrd <- read.csv("./results/species_gbif_bhrd_clean.csv", 
                 header = T, sep=",", dec=".",
                 encoding="utf-8")

#Deduplicate
spp.cl.dup <- dframe(spp.bhrd) %>% dedup()
nrow(spp.cl.dup)




# Extra: Data manipulation: cleanning table -------------------------------------------

### Leaving only a few columns
data.clean.sub <- subset(spp.bhrd, select=c("species","infraspecificEpithet","countryCode","stateProvince","locality","decimalLongitude","decimalLatitude"))

### Combining columns species and infraspecificEpithet
#data.clean.sub$spp <- paste(data.clean.sub$species,data.clean.sub$infraspecificEpithet)

### now excluding columns species and infraspecificEpithet
names(data.clean.sub)
spp.points <- subset(data.clean.sub, select=c("species", "countryCode", "stateProvince", "locality", "decimalLongitude","decimalLatitude"))

names(spp.points)

### Rename column where names is "decimalLongitude" and "decimalLatitude"
names(spp.points)[names(spp.points) == "decimalLongitude"] <- "lon"
names(spp.points)[names(spp.points) == "decimalLatitude"] <- "lat"
names(spp.points)
colnames(spp.points)

unique(spp.points$species)

#Save table
write.csv(spp.points,file="./results/spp_gbif_bhrd_sub.csv")




