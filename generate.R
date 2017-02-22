################################
# inputting state


#################################
# loading libraries
source("~/share/projects/VotingBehavior/scripts/workflow_module/Zillow_Hedonics.R")
source("~/share/projects/VotingBehavior/scripts/workflow_module/PointsOverPolygons.R")
source("~/share/projects/VotingBehavior/scripts/workflow_module/FELM_prep.R")

on_rstudio <- TRUE
state <- "IL"

abbr <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI",
          "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", 
          "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", 
          "OK", "OR","PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", 
          "WV", "WI", "WY", "AS", "GU", "MP", "PR", "VI", "UM")
code <- c("01", "02", "04", "05", "06", "08", "09", 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22,23,24,25, 26,
          27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50,
          51, 53, 54, 55, 56, 60, 66, 69, 72, 78, 74)

###############
## specifying inputs

state_code <- get_code(state)
project_path <- "~/share/projects/VotingBehavior"
setwd(project_path)
hedonics_output <- get_hedonics_output_path(state)
polygon_input <- get_shape_file(state)
points_over_polygon_output <- get_matching_result_path(state)
output_path <- paste(project_path, "/production/", paste(state, "_generate.rds", sep=""), sep="")
#polygons_layer_name <- get_layerName(state)
#default_projection <- "+proj=longlat +datum=WGS84 +no_defs"


###################################
# calling steps
generate_hedonics(state_code, hedonics_output) ## could just use the return value as the input for POP
                                              # save the output to (state)_hedonics.rds
TD <- points_over_polygon(hedonics_output, polygon_input, points_over_polygon_output)  # save the output to (state)_matching.rds

TD <- felm_prep(points_over_polygon_output)  

saveRDS(TD, output_path) # save the output to (state)_generate.rds

# ###################################
# #felm_prep
# TD$date <- as.Date(TD$RecordingDate, format="%Y-%m-%d")                           # transform into "R date"
#   TD$datepos <- as.POSIXlt(TD$date)
#   TD$month <- as.factor(TD$datepos$mon+1)
#   TD$month2 <- as.numeric(TD$datepos$mon+1)
#   TD$year <- as.factor(TD$datepos$year+1900)
#   TD$year2 <- as.numeric(TD$datepos$year+1900)
#   TD$tm <- as.yearmon(paste(TD$year2,TD$month2,sep="-"))
#   TD$tm2 <- as.factor(TD$tm)
#   
#   TD$HHID <- as.factor(TD$ImportParcelID)
# 
#   #Exclude houses <$10,000 & >$10,000,000 (Follows Currie et al. (2015))
#   TD$price <- as.numeric(TD$SalesPriceAmount)    #
#   TD <- subset(TD, price>1000)  #Exclude houses with sales prices <$10,000
#   TD <- subset(TD, price<10000000)  #Exclude houses with sales prices >$10 mil  
#   
#   #Manipulating sale price data_________________________________________________________________________
#   # PETER - NEED TO INCLUDE CPI TO DEFLATE PRICES HERE
#   TD$logprice <- as.numeric(log(TD$SalesPriceAmount)) #calculate log of price
#   
#   #Subset Housing Price Data_________________________________________________________________________
#   #Exclude intrafamily transfers & tax exempt transactions
#   TD <- subset(TD, IntraFamilyTransferFlag!="Y")    
#   TD <- subset(TD, TransferTaxExemptFlag!="Y")      
#   
#   #Keeping only land use type RR101 (most common by far) COME BACK TO WHEN KNOW CODES
#   TD <- subset(TD, PropertyLandUseStndCode=="RR101")      
#   




####################################
# plot
