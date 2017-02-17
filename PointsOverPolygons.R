#     ------------------------------------------------------------------------
#   |                                                                         |
#   |  Points Over Polygons using Rgdal library                                     |
#   |                                                                         |
#   |  By:                                                                    |
#   |  Yifang Zhang                                                           |                            
#   |  University of Illinois at Urbana Chamapaign                            |
#   |                                                                         |
#     ------------------------------------------------------------------------

## state GA is not avaliable

points_over_polygon <-function(points_path, polygons_path, polygons_layer_name, default_projection, outputFilePath){
  
  ##################################################################
  ## Preliminaries
  
  ## This function will check if a package is installed, and if not, install it
  pkgTest <- function(x) {
    if (!require(x, character.only = TRUE))
    {
      install.packages(x, dep = TRUE)
      if(!require(x, character.only = TRUE)) stop("Package not found")
    }
  }
  
  ## These lines load the required packages
  packages <- c("readxl", "data.table", "rgdal", "sp", "rgeos", "tools")
  lapply(packages, pkgTest)
  
  ## assigning the variables ##
  pointFilePath <- points_path
  polygonFilePath <- polygons_path
  layerName <- polygons_layer_name
    #polygons_layer_name
  
  
  ## reading the points file
  pointFilePathExt <- file_ext(pointFilePath)
  if(pointFilePathExt == "rds"){
    points_raw <- readRDS(pointFilePath)
  } else{
    points_raw <- read.csv(pointFilePath)
  }
  
  
  points <- points_raw[which(!is.na(points_raw$PropertyAddressLongitude)),]
  points <- points[which(!is.na(points$PropertyAddressLatitude)),]
  
  ## reading state shapefile 
  shpName <- polygonFilePath
  shpDir <- path.expand(shpName)
  ogrInfo(dsn = shpDir, layer = layerName)
  shp_poly <- readOGR(shpDir, layerName)
  
  ## adding the indexes in front of precincts 
  shp_poly$addedID <- seq.int(nrow(shp_poly))
  
  ## performing the projection for points
  coordinates(points) <- ~ PropertyAddressLongitude + PropertyAddressLatitude
  proj4string(points) <- CRS("+proj=longlat")
  
  ## if the shapefile does not have projection, we will defaulting it to WGS 84
  if(is.na(proj4string(shp_poly))){
    proj4string(shp_poly) <- CRS(default_projection) ## "+proj=longlat +datum=WGS84 +no_defs"
  }
  
  
  ## transform the projection from points to the projection of the shapefile
  points <- spTransform(points, proj4string(shp_poly))
  proj4string(points) <- proj4string(shp_poly)
  
  ## perform the over function
  res <- over(points, shp_poly)
  
  ## optional: plotting the data in R
  #plot(shp_poly)
  #plot(points_res$PropertyAddressLatitude ~ points_res$PropertyAddressLongitude, col = "red", cex = 1)
  
  ## Appending the result information after the Hedonics Data
  points_res <- points_raw
  points_res <- points_res[which(!is.na(points_res$PropertyAddressLatitude)),]
  points_res <- points_res[which(!is.na(points_res$PropertyAddressLongitude)),]
  
  final_res <- cbind(points_res, res)
  
  saveRDS(final_res, outputFilePath)
  
  return(final_res)
  
}

## testing only ##
#merged_result <- points_over_polygon(points_path = "~/share/projects/VotingBehavior/production/NCHedonics.rds", 
#                                     polygons_path = "~/share/projects/VotingBehavior/store/NC/NC_Shapefiles/", 
#                                     polygons_layer_name = "nc_final", 
#                                     default_projection = "+proj=longlat +datum=WGS84 +no_defs",
#                                     outputFilePath = "~/share/projects/VotingBehavior/MI_Save/NC_over.rds")

points_over_polygon_both_shapefile <-function(points_path, points_layer_name, polygons_path, polygons_layer_name, default_projection, outputFilePath)
{
  ## This function will check if a package is installed, and if not, install it
  pkgTest <- function(x) {
    if (!require(x, character.only = TRUE))
    {
      install.packages(x, dep = TRUE)
      if(!require(x, character.only = TRUE)) stop("Package not found")
    }
  }
  
  ## These lines load the required packages
  packages <- c("readxl", "data.table", "rgdal", "sp", "rgeos", "tools")
  lapply(packages, pkgTest)
  
  ## reading point shapefile 
  pointName <- points_path
  pointDir <- path.expand(pointName)
  ogrInfo(dsn = pointDir, layer = points_layer_name)
  points_raw <- readOGR(pointDir, points_layer_name)
  points <- points_raw
  
  if(is.na(proj4string(points))){
    proj4string(points) <- CRS(default_projection) ## "+proj=longlat +datum=WGS84 +no_defs"
  }
  
  ## reading polygon shapefile 
  shpName <- polygons_path
  shpDir <- path.expand(shpName)
  ogrInfo(dsn = shpDir, layer = polygons_layer_name)
  shp_poly <- readOGR(shpDir, polygons_layer_name)
  
  if(is.na(proj4string(shp_poly))){
    proj4string(shp_poly) <- CRS(default_projection) ## "+proj=longlat +datum=WGS84 +no_defs"
  }
  
  ## transform the projection from points to the projection of the shapefile
  points <- spTransform(points, proj4string(shp_poly))
  proj4string(points) <- proj4string(shp_poly)
  
  ## perform the over function
  res <- over(points, shp_poly)
  
  ## Appending the result information after the Hedonics Data
  points_res <- as(points_raw, "data.frame")
  #points_res <- points_res[which(!is.na(points_res$PropertyAddressLatitude)),]
  #points_res <- points_res[which(!is.na(points_res$PropertyAddressLongitude)),]
  
  final_res <- cbind(points_res, res)
  
  saveRDS(final_res, outputFilePath)
  
  return(final_res)
}


## temp function ##
points_over_polygon_fieldNameForPoints <-function(points_path, polygons_path, polygons_layer_name, default_projection, outputFilePath){
  
  ##################################################################
  ## Preliminaries
  
  ## This function will check if a package is installed, and if not, install it
  pkgTest <- function(x) {
    if (!require(x, character.only = TRUE))
    {
      install.packages(x, dep = TRUE)
      if(!require(x, character.only = TRUE)) stop("Package not found")
    }
  }
  
  ## These lines load the required packages
  packages <- c("readxl", "data.table", "rgdal", "sp", "rgeos", "tools")
  lapply(packages, pkgTest)
  
  ## assigning the variables ##
  pointFilePath <- points_path
  polygonFilePath <- polygons_path
  layerName <- polygons_layer_name
  
  
  ## reading the points file
  pointFilePathExt <- file_ext(pointFilePath)
  if(pointFilePathExt == "rds"){
    points_raw <- readRDS(pointFilePath)
  } else{
    points_raw <- read.csv(pointFilePath)
  }
  
  
  points <- points_raw[which(!is.na(points_raw$Longitude)),]
  points <- points[which(!is.na(points$Latitude)),]
  
  ## reading state shapefile 
  shpName <- polygonFilePath
  shpDir <- path.expand(shpName)
  ogrInfo(dsn = shpDir, layer = layerName)
  shp_poly <- readOGR(shpDir, layerName)
  
  ## adding the indexes in front of precincts 
  shp_poly$addedID <- seq.int(nrow(shp_poly))
  
  ## performing the projection for points
  coordinates(points) <- ~ Longitude + Latitude
  proj4string(points) <- CRS("+proj=longlat")
  
  ## if the shapefile does not have projection, we will defaulting it to WGS 84
  if(is.na(proj4string(shp_poly))){
    proj4string(shp_poly) <- CRS(default_projection) ## "+proj=longlat +datum=WGS84 +no_defs"
  }
  
  
  ## transform the projection from points to the projection of the shapefile
  points <- spTransform(points, proj4string(shp_poly))
  proj4string(points) <- proj4string(shp_poly)
  
  ## perform the over function
  res <- over(points, shp_poly)
  
  ## optional: plotting the data in R
  #plot(shp_poly)
  #plot(points_res$PropertyAddressLatitude ~ points_res$PropertyAddressLongitude, col = "red", cex = 1)
  
  ## Appending the result information after the Hedonics Data
  points_res <- points_raw
  points_res <- points_res[which(!is.na(points_res$Latitude)),]
  points_res <- points_res[which(!is.na(points_res$Longitude)),]
  
  final_res <- cbind(points_res, res)
  
  saveRDS(final_res, outputFilePath)
  
  return(final_res)
  
}

get_layerName <- function(state)
{
  relative_path <- paste("store",state, paste(state, "_Shapefile/", state, "_final.shp", sep=""), sep = "/")
  return(ogrListLayers(relative_path)[1])
}

get_relative_path <- function(state)  # when already under the store folder, input the state name to get the path to the shapefile folder
{
    return (paste(state, "/", state, "_Shapefile/", sep=""))
}
yes <- 0;
no <- 0;
test_state_for_layername <- function(state) # test the state for the listlayer function
{
    # sapply(abbr, test_state_for_layername)  test all 50 states for this function
    wd <- paste(base, get_relative_path(state), sep="")
    folder <- tryCatch(setwd(wd), error=function(e) {print(paste("no folder for state", state))
                                              return(-1)})# setting the wd to state_Shapefile/
    if (folder == (-1))
    {
      no <- no + 1 
    }
    
    spath <- paste(state, "_final.shp", sep="")
    success <- tryCatch(ogrInfo(spath)$layer, error=function(e) {return(-1)} )#{state <- tolower(state)})
    if (success == (-1))
    {
      print(paste("lowering the letters for state", state))
      state2 <- tolower(state)
      spath2 <- paste(state2, "_final.shp", sep="")
      success2 <- tryCatch(ogrInfo(spath2)$layer, error=function(e) {no <- no + 1
                                                          print(paste("failed:", state))})
      if (success2 == (-1))
      {
        print("success: ", state)
      }
    }
    else
    {
      yes <- yes+1
      print(paste("success:", state))
    }
}