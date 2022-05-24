################################################################################
###                                                                          ###
###             CALCULATE AREA OF A RASTER IN EACH CELL GRID                 ###
###                                                                          ###
###                                                                          ###
###                Created by Danielle de O. Moreira                         ###
###                     created: 25 apr 2022                                 ###
###                     updated:                                             ###
################################################################################


# Source of raw data in:
# https://drive.google.com/drive/folders/19-U3KsGCN92vrBftjDdYWGfOtHwFSLsM?usp=sharing

# SET UP LIBRARIES AND DATA ----------------------------------------------------
library(rgdal)
library(raster)
library(tidyverse) #for data manipulation
library(ggplot2) #to make graphs


# longlat <- sp::CRS("+proj=longlat +datum=WGS84")
#albers <-sp::CRS("+proj=aea +lat_0=-32 +lon_0=-60 +lat_1=-5 +lat_2=-42 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs")

# Read the shapefile (the limits we want to crop our data)
grid <- readOGR("./SIG/grid_bhrd_10km_only-count-spp_albers.shp")

# Read the raster file of native vegetation (filtered from Mapbiomas Collection 6 data)
veg <- raster("/home/danimoreira/01_INMA_BHRD/mapbiomas/data/mapbiomas-col6-bhrd/raster/2020/natural-formation_albers.tif")

# Check datum (CRS)
# proj4string(grid)
# proj4string(veg)

#Shapefile reprojection
# grid <- spTransform(grid, crs(longlat))

plot(veg)
plot(grid, add = TRUE)

# COUNT AND CALCULATE RASTER AREA
# Counting and calculating pixel of vegetation area within each grid cell
# https://gis.stackexchange.com/questions/412422/counting-pixels-of-each-land-cover-type-within-each-polygon-from-raster-image-us

#create a blank dataframe to write into
jj = data.frame() 
for(i in 1:nrow(grid)){ #initate a loop for each cell grid
  tmp <- grid[i,] #subset to get one cell
  crp <- crop(veg,tmp) #crop the vegetation raster by grid
  msk <- mask(crp,tmp) #mask out values outside of grids
  tbl <- data.frame(freq(msk)) #get the count of vegetation pixels 
  tbl <- tbl[!is.na(tbl$value), ] #remove NA values
  tbl$ha <- tbl$count*0.09 #add in a ha column for extra info. Each grid is an 30 x 30 meters of area
  tbl$grid <- tmp$grid #add in grid information as a column 
  jj <- rbind(jj,tbl) #add rows to dataframe jj
  
  print(paste0("done with ", i, " of ", nrow(grid)))} #give progress report for loop

jj #look at the dataframe

jj2 <- jj %>% 
  filter(value == "1")

grid_df <- as.data.frame(grid)

#Save table of species for BHRD
write.csv(grid_df, "./results/natural_areas_ric.csv")


# GRAPH ------------------------------------------------------------------------
#load table
data_r <- read.csv("./results/natural_areas_rich.csv", 
                    header = T, sep=",", dec=".",
                    encoding="utf-8")

# Scatterplot graph
attach(data_r)
plot(veg_area_ha, NUMSPECIES, main="Scatterplot Example",
     xlab="area ", ylab="Species richness", pch=19)

# Add fit lines
abline(lm(NUMSPECIES~veg_area_ha), col="red") # regression line (y~x)
lines(lowess(veg_area_ha, NUMSPECIES), col="blue") # lowess line (x,y)

