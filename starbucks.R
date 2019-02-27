
#-----------------------Loading the packages
setwd("D:\\leaflet")
library(leaflet)
library(rgeos)
library(rgdal)
library(spatstat)
library(maptools)
library(sp)
devtools::install_github("rstudio/leaflet")

#-----------------------Read the dataset

#Read the data
starbucks <- read.csv('E:\\Data\\6.2\\starbucks.csv')
head(starbucks)

#-----------------------Data Subsetting

#We want to choose any one city and map the various starbucks shops in that city
#on the map

city_freq<-data.frame(table(starbucks$City))

library(dplyr)
city_freq<-arrange(city_freq,-Freq)
head(city_freq,40)
#You can choose any city. For the purpose of this analysis , let's choose "Vancouver"

city_freq %>% filter(Var1=="Vancouver")

v<- subset(starbucks, City == 'Vancouver') #27 data points

#-----------------------Use Leaflet to visualise data on a map

#There are different ways to illustrate your data.

#Use addtiles
#Use addmarkers

# In fact, there's many different styles of map we could make,
# just by choosing different tiles. Here's some examples:

leaflet(v) %>% addProviderTiles("Stamen.Watercolor") %>% addMarkers()
leaflet(v) %>% addProviderTiles("Stamen.Toner") %>% addCircles(radius=400,color='yellow')
leaflet(v) %>% addTiles() %>% addMarkers()

names(v)
names(v)[6]<-"Products"

#Add text
leaflet(v) %>% addTiles() %>% addMarkers(popup = v$Products)

leaflet(v) %>% addTiles() %>% 
  addMarkers(popup = paste("Location:",v$Name,"Product:",v$Products))


#You can also create polygons

# Create a SpatialPointsDataFrame from Starbucks data frame
# and then project the data to an appropriate projected coordinate system 
# for buffering (UTM Zone 14N) with the spTransform function

coords <- cbind(v$Longitude, v$Latitude)

## Spatial points w/the WGS84 datum
##Converting this into a spatial data object
sp_v <- SpatialPointsDataFrame(coords = coords, data = v, 
                               proj4string = CRS("+proj=longlat +datum=WGS84"))


##What CRS is it?
##Check the EPSG code

library(rgdal)
s<-make_EPSG() #Gives you a list of all the projects and the respective EPSG codes

tmp<-s[(grep("longlat",s$prj4)),] #Shortlist only the geographic projections ie it has lat long values

tmp[(grep("+datum=WGS84",tmp$prj4)),] #Shortlist from these, which code refers to +datum=WGS84
#code     note                                prj4
#249 4326 # WGS 84 +proj=longlat +datum=WGS84 +no_defs

#The data in Open Street Map database is stored in a gcs with units decimal degrees 
#& datum of wgs84. (EPSG: 4326)

str(sp_v)

sp_v@coords

## So we can quickly read in our processed data without having to re-process it.
saveRDS(sp_v, "starbucks_spatial_df.rds")


#write it out as a shape file
writeOGR(sp_v, dsn = "shapefiles", 
         layer = "starbucks-shapefile", 
         driver = "ESRI Shapefile", 
         overwrite_layer = TRUE)

#How to read shape files back in , refer to ggmap pre-class videos

sp_v@bbox

#Transforming geographic lat long information to projected coordinate system
#Transform to standard format
sp_v_proj <- spTransform(sp_v, CRS("+init=epsg:26914"))
sp_v_proj@bbox


# To Create 2km buffers with the gBuffer function from the rgeos package
# around each Starbucks location. 

# To show the data on a Leaflet map,convert back to 
# using XY coordinates so that the Leaflet package can read it in. 

# The map will show Starbucks 
# locations with markers and a pop-up and the buffers

library(rgeos)
buff2km <- gBuffer(sp_v_proj, byid = TRUE, width = 2000)

buff_xy <- spTransform(buff2km, CRS("+proj=longlat +datum=WGS84"))

leaflet() %>%
  addMarkers(data = v, 
             lat = ~ Latitude, 
             lng = ~ Longitude, 
             popup = v$Name) %>%
  addPolygons(data = buff_xy, color = "brown", fill = "brown") %>%
  addTiles()

##--------------------------Adding Polygons

# Each polygon represents the area that is nearest to the point that it contains for a 
# given study area ("window" in the code below). 
 
# Each polygon has a popup as well that shows its area in square kilometers.
# To create this polygon, use Dirichlet polygons()

#Collect the coordinates
v_coords <- sp_v_proj@coords
v_coords

## Create the window for the polygons
range_x_coord<-range(v_coords[,1])
range_y_coord<-range(v_coords[,2])

#Create a window using the range/distances
window <- owin(range_x_coord, range_y_coord)
?owin

## Create the polygons
library(spatstat)
k <- dirichlet(as.ppp(v_coords, window))
?dirichlet

## Convert to a SpatialPolygonsDataFrame and calculate an "area" field.  
sp <- as(k, "SpatialPolygons")

sp_df <- SpatialPolygonsDataFrame(sp, 
          data = data.frame(id = 1:length(sp@polygons)))

sp_df@data
class(sp_df)
#Change the CRS; this is just to calculate the area
proj4string(sp_df) <- CRS("+init=epsg:26914")

#Calculte the area
sp_df$area <- round((gArea(sp_df, byid = TRUE) / 1000000), 1)
gArea(sp_df)
gArea(sp_df, byid = TRUE)
gArea(sp_df, byid = TRUE)/1000000

sp_xy <- spTransform(sp_df, CRS("+proj=longlat +datum=WGS84"))


#Plot using Leaflet()
leaflet() %>%
  addMarkers(data = v, 
             lat = ~ Latitude, 
             lng = ~ Longitude, 
             popup = v$Name) %>%
  addPolygons(data = sp_xy, 
              color = "red", 
              fill = "red", 
              popup = paste0("Area: ", 
                             as.numeric(sp_xy$area), 
                             " square km")) %>%
  addTiles()