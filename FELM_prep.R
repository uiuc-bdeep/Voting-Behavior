
#     ------------------------------------------------------------------------
#   |                                                                         |
#   |  Creates Hedonics Dataset for Michigan                  |
#   |                                                                         |
#   |  By:                                                                    |
#   |  Peter Christensen                                             |    
#   |  Big Data for Environmental Economics and Policy                        |
#   |  University of Illinois at Urbana Chamapaign                            |
#   |                                                                         |
#     ------------------------------------------------------------------------


# Source of data  -------------------------------------------------------------

    # converted files are stored at:
    #    "~/share/projects/data/Flint/production/"

#install packages

## Preliminaries
#rm(list=ls())

## This function will check if a package is installed, and if not, install it
# pkgTest <- function(x) {
#   if (!require(x, character.only = TRUE))
#   {
#     install.packages(x, dep = TRUE)
#     if(!require(x, character.only = TRUE)) stop("Package not found")
#   }
# }
# 
# ## These lines load the required packages
# packages <- c("plyr", "dplyr", "lfe", "rdrobust", "stargazer", "ggplot2", "outliers", "doBy", "zoo")
# lapply(packages, pkgTest)
# 
# require(ggplot2)
# library(doBy)
# library(zoo)



# # Read Transaction Data -------------------------------------------------------------------
# 
# TD <- read.csv("~/share/projects/VotingBehavior/MI_matching.csv")
# 
# 
# ## Prepare Data_________________________________________________________________________________________ 
# 
#   #change date format
# 	TD$date <- as.Date(TD$RecordingDate, format="%Y-%m-%d")                           # transform into "R date"
# 	TD$datepos <- as.POSIXlt(TD$date)
# 	TD$month <- as.factor(TD$datepos$mon+1)
# 	TD$month2 <- as.numeric(TD$datepos$mon+1)
# 	TD$year <- as.factor(TD$datepos$year+1900)
# 	TD$year2 <- as.numeric(TD$datepos$year+1900)
# 	TD$tm <- as.yearmon(paste(TD$year2,TD$month2,sep="-"))
# 	TD$tm2 <- as.factor(TD$tm)
# 	
# 	TD$HHID <- as.factor(TD$ImportParcelID)
# 
# 	#Exclude houses <$10,000 & >$10,000,000 (Follows Currie et al. (2015))
# 	TD$price <- as.numeric(TD$SalesPriceAmount)    #
# 	TD <- subset(TD, price>1000)  #Exclude houses with sales prices <$10,000
# 	TD <- subset(TD, price<10000000)  #Exclude houses with sales prices >$10 mil	
# 	
# 	#Manipulating sale price data_________________________________________________________________________
# 	# PETER - NEED TO INCLUDE CPI TO DEFLATE PRICES HERE
# 	TD$logprice <- as.numeric(log(TD$SalesPriceAmount)) #calculate log of price
# 	
# 	#Subset Housing Price Data_________________________________________________________________________
# 	#Exclude intrafamily transfers & tax exempt transactions
# 	TD <- subset(TD, IntraFamilyTransferFlag!="Y")	  
# 	TD <- subset(TD, TransferTaxExemptFlag!="Y")	    
# 	
# 	#Keeping only land use type RR101 (most common by far) COME BACK TO WHEN KNOW CODES
# 	TD <- subset(TD, PropertyLandUseStndCode=="RR101")	    
	
felm_prep <- function(matching_file_path)
{
  
  pkgTest <- function(x) {
    if (!require(x, character.only = TRUE))
    {
      install.packages(x, dep = TRUE)
      if(!require(x, character.only = TRUE)) stop("Package not found")
    }
  }
  
  ## These lines load the required packages
  packages <- c("plyr", "dplyr", "lfe", "rdrobust", "stargazer", "ggplot2", "outliers", "doBy", "zoo")
  lapply(packages, pkgTest)
  
  
  TD <- readRDS(matching_file_path)
  
  
  TD$date <- as.Date(TD$RecordingDate, format="%Y-%m-%d")                           # transform into "R date"
  TD$datepos <- as.POSIXlt(TD$date)
  TD$month <- as.factor(TD$datepos$mon+1)
  TD$month2 <- as.numeric(TD$datepos$mon+1)
  TD$year <- as.factor(TD$datepos$year+1900)
  TD$year2 <- as.numeric(TD$datepos$year+1900)
  TD$tm <- as.yearmon(paste(TD$year2,TD$month2,sep="-"))
  TD$tm2 <- as.factor(TD$tm)
  
  TD$HHID <- as.factor(TD$ImportParcelID)
  
  #Exclude houses <$10,000 & >$10,000,000 (Follows Currie et al. (2015))
  TD$price <- as.numeric(TD$SalesPriceAmount)    #
  TD <- subset(TD, price>1000)  #Exclude houses with sales prices <$10,000
  TD <- subset(TD, price<10000000)  #Exclude houses with sales prices >$10 mil	
  
  #Manipulating sale price data_________________________________________________________________________
  # PETER - NEED TO INCLUDE CPI TO DEFLATE PRICES HERE
  TD$logprice <- as.numeric(log(TD$SalesPriceAmount)) #calculate log of price
  
  #Subset Housing Price Data_________________________________________________________________________
  #Exclude intrafamily transfers & tax exempt transactions
  TD <- subset(TD, IntraFamilyTransferFlag!="Y")	  
  TD <- subset(TD, TransferTaxExemptFlag!="Y")	    
  
  #Keeping only land use type RR101 (most common by far) COME BACK TO WHEN KNOW CODES
  TD <- subset(TD, PropertyLandUseStndCode=="RR101")
  
  
  return(TD)
}