##function list
#     setup_env
#     get_base
#     get_bldg
#     get_sqft
#     get_propTrans
#     get_propTrans
#
#
#
#

setup_env <- function() {
  ## Preliminaries
  #rm(list=ls())
  
  ## This function will check if a package is installed, and if not, install it
  pkgTest <- function(x) {
    if (!require(x, character.only = TRUE))
    {
      install.packages(x, dep = TRUE)
      if(!require(x, character.only = TRUE)) stop("Package not found")
    }
  }
  
  ## These lines load the required packages
  packages <- c("readxl", "data.table", "sp")
  lapply(packages, pkgTest)
  
  ## These lines set several options
  options(scipen = 999) # Do not print scientific notation
  options(stringsAsFactors = FALSE) ## Do not load strings as factors
}



get_base <- function(dir, rows2load, col_namesMain) {
  
  setup_env()
  ######################################################################
  # Pull address, geographic, lot size, and tax data from main table
  
  base <- read.table(file.path(dir, "ZAsmt/Main.txt"),
                     nrows = rows2load,
                     sep = '|',
                     header = FALSE,
                     stringsAsFactors = FALSE,
                     skipNul = TRUE,                            # tells R to treat two ajacent delimiters as dividing a column
                     comment.char="",                           # tells R not to read any symbol as a comment
                     quote = "",                                # this tells R not to read quotation marks as a special symbol
                     col.names = col_namesMain
  )
  
  base <- as.data.table(base)
  base <- base[ , list(RowID, ImportParcelID, LoadID,
                       FIPS, State, County,
                       PropertyFullStreetAddress,
                       PropertyHouseNumber, PropertyHouseNumberExt, PropertyStreetPreDirectional, PropertyStreetName, PropertyStreetSuffix, PropertyStreetPostDirectional,
                       PropertyCity, PropertyState, PropertyZip,
                       PropertyBuildingNumber, PropertyAddressUnitDesignator, PropertyAddressUnitNumber,
                       PropertyAddressLatitude, PropertyAddressLongitude, PropertyAddressCensusTractAndBlock,
                       NoOfBuildings,
                       LotSizeAcres, LotSizeSquareFeet,
                       TaxAmount, TaxYear)]
  ## we want this
  
  # Keep only one record for each ImportPropertyID.
  # ImportParcelID is the unique identifier of a parcel. Multiple entries for the same ImportParcelID are due to updated records.
  # The most recent record is identified by the greatest LoadID.
  #   **** This step may not be necessary for the published dataset as we intend to only publish the updated records, but due dilligence demands we check.
  
  length(unique(base$ImportParcelID))  # Number of unique ImportParcelIDs
  dim(base)[1]                         # Number of rows in the base dataset
  
  if( length(unique(base$ImportParcelID)) != dim(base)[1] ){
    
    #Example: Print all entries for parcels with at least two records.
    base[ImportParcelID %in% base[duplicated(ImportParcelID), ImportParcelID], ][order(ImportParcelID)]
    
    setkeyv(base, c("ImportParcelID", "LoadID"))  # Sets the index and also orders by ImportParcelID, then LoadID increasing
    keepRows <- base[ ,.I[.N], by = c("ImportParcelID")]   # Creates a table where the 1st column is ImportParcelID and the second column
    # gives the row number of the last row that ImportParcelID appears.
    base <- base[keepRows[[2]], ] # Keeps only those rows identified in previous step
    
  }
  return(base)
}

get_bldg <- function(dir, rows2load, col_namesBldg) {
  
  setup_env()
  ######################################################################
  #### Load most property attributes
  
  bldg <- read.table(file.path(dir, "ZAsmt/Building.txt"),
                     nrows = rows2load,                    # this is set just to test it out. Remove when code runs smoothly.
                     sep = '|',
                     header = FALSE,
                     stringsAsFactors = FALSE,
                     skipNul = TRUE,                            # tells R to treat two ajacent delimiters as dividing a column
                     comment.char="",                           # tells R not to read any symbol as a comment
                     quote = "",                                # this tells R not to read quotation marks as a special symbol
                     col.names = col_namesBldg
  )
  
  bldg <- as.data.table(bldg)
  
  bldg <- bldg[ , list(RowID, NoOfUnits, BuildingOrImprovementNumber,
                       YearBuilt, EffectiveYearBuilt, YearRemodeled,
                       NoOfStories, StoryTypeStndCode, TotalRooms, TotalBedrooms,
                       FullBath, ThreeQuarterBath, HalfBath, QuarterBath,
                       HeatingTypeorSystemStndCode,
                       PropertyLandUseStndCode)]
  
  
  #  Reduce bldg dataset to Single-Family Residence, Condo's, Co-opts (or similar)
  
  bldg <- bldg[PropertyLandUseStndCode %in% c('RR101',  # SFR
                                              'RR999',  # Inferred SFR
                                              # 'RR102',  # Rural Residence   (includes farm/productive land?)
                                              'RR104',  # Townhouse
                                              'RR105',  # Cluster Home
                                              'RR106',  # Condominium
                                              'RR107',  # Cooperative
                                              'RR108',  # Row House
                                              'RR109',  # Planned Unit Development
                                              'RR113',  # Bungalow
                                              'RR116',  # Patio Home
                                              'RR119',  # Garden Home
                                              'RR120'), # Landominium
               ]
    return(bldg)
}

get_sqft <- function(dir, rows2load, col_namesBldgA) {
  
  setup_env()
  ######################################################################
  #### Load building squarefoot data
  
  sqft <- read.table(file.path(dir, "ZAsmt/BuildingAreas.txt"),
                     nrows = rows2load,                    # this is set just to test it out. Remove when code runs smoothly.
                     sep = '|',
                     header = FALSE,
                     stringsAsFactors = FALSE,
                     skipNul = TRUE,                            # tells R to treat two ajacent delimiters as dividing a column
                     comment.char="",                           # tells R not to read any symbol as a comment
                     quote = "",                                # this tells R not to read quotation marks as a special symbol
                     col.names = col_namesBldgA
  )
  
  
  sqft <- as.data.table(sqft)
  
  # Counties report different breakdowns of building square footage and/or call similar concepts by different names.
  # The structure of this table is to keep all entries reported by the county as they are given. See 'Bldg Area' table in documentation.
  # The goal of this code is to determine the total square footage of each property.
  # We assume a simple logic to apply across all counties here. Different logic may be as or more valid.
  # The logic which generates square footage reported on our sites is more complex, sometimes county specific, and often influenced by user interaction and update.
  
  sqft <- sqft[BuildingAreaStndCode %in% c('BAL',  # Building Area Living
                                           'BAF',  # Building Area Finished
                                           'BAE',  # Effective Building Area
                                           'BAG',  # Gross Building Area
                                           'BAJ',  # Building Area Adjusted
                                           'BAT',  # Building Area Total
                                           'BLF'), # Building Area Finished Living
               ]
  
  table(sqft$BuildingOrImprovementNumber)  # BuildingOrImprovementNumber > 1  refers to additional buildings on the parcel.
  
  sqft <- sqft[ , list(sqfeet = max(BuildingAreaSqFt, na.rm = T)), by = c("RowID", "BuildingOrImprovementNumber")]
  return(sqft)
}

get_propTrans <- function(dir, rows2load, col_namesProp) {
  
  setup_env()
  ###############################################################################
  #   Load PropertyInfo table for later merge
  
  propTrans <- read.table(file.path(dir, "ZTrans/PropertyInfo.txt"),
                          nrows = rows2load,                    # this is set just to test it out. Remove when code runs smoothly.
                          sep = '|',
                          header = FALSE,
                          stringsAsFactors = FALSE,
                          skipNul = TRUE,                            # tells R to treat two ajacent delimiters as dividing a column
                          comment.char="",                           # tells R not to read any symbol as a comment
                          quote = "",                                # this tells R not to read quotation marks as a special symbol
                          col.names = col_namesProp
  )
  
  propTrans <- as.data.table(propTrans)
  
  propTrans <- propTrans[ , list(TransId, PropertySequenceNumber, LoadID, ImportParcelID)]
  
  ## we do that
  
  # Keep only one record for each TransID and PropertySequenceNumber.
  # TransID is the unique identifier of a transaction, which could have multiple properties sequenced by PropertySequenceNumber.
  # Multiple entries for the same TransID and PropertySequenceNumber are due to updated records.
  # The most recent record is identified by the greatest LoadID.
  #   **** This step may not be necessary for the published dataset as we intend to only publish most updated record.
  
  setkeyv(propTrans, c("TransId", "PropertySequenceNumber", "LoadID"))
  keepRows <- propTrans[ ,.I[.N], by = c("TransId", "PropertySequenceNumber")]
  propTrans <- propTrans[keepRows[[3]], ]
  propTrans[ , LoadID:= NULL]
  
  # Drop transactions of multiple parcels (transIDs associated with PropertySequenceNumber > 1)
  
  dropTrans <- unique(propTrans[PropertySequenceNumber > 1, TransId])
  propTrans <- propTrans[!(TransId %in% dropTrans), ]   # ! is "not"
  
  rm(keepRows)
  return(propTrans)
}

get_trans <- function(dir, rows2load, col_namesMainTr) {
  
  setup_env()
  #######################################################################################
  #  Load main table in Ztrans database, which provides information on real estate events
  
  trans <- read.table(file.path(dir, "ZTrans/Main.txt"),
                      nrows = rows2load,                    # this is set just to test it out. Remove when code runs smoothly.
                      sep = '|',
                      header = FALSE,
                      stringsAsFactors = FALSE,
                      skipNul = TRUE,                            # tells R to treat two ajacent delimiters as dividing a column
                      comment.char="",                           # tells R not to read any symbol as a comment
                      quote = "",                                # this tells R not to read quotation marks as a special symbol
                      col.names = col_namesMainTr
  )
  
  trans <- as.data.table(trans)
  
  trans <- trans[ , list(TransId, LoadID,
                         RecordingDate, DocumentDate, SignatureDate, EffectiveDate,
                         SalesPriceAmount, LoanAmount,
                         SalesPriceAmountStndCode, LoanAmountStndCode,
                         # These remaining variables may be helpful to, although possibly not sufficient for, data cleaning. See documentation for all possible variables.
                         DataClassStndCode, DocumentTypeStndCode,
                         PartialInterestTransferStndCode, IntraFamilyTransferFlag, TransferTaxExemptFlag,
                         PropertyUseStndCode, AssessmentLandUseStndCode,
                         OccupancyStatusStndCode)]
  
  # Keep only one record for each TransID.
  # TransID is the unique identifier of a transaction.
  # Multiple entries for the same TransID are due to updated records.
  # The most recent record is identified by the greatest LoadID.
  #   **** This step may not be necessary for the published dataset as we intend to only publish most updated record.
  
  setkeyv(trans, c("TransId", "LoadID"))
  keepRows <- trans[ ,.I[.N], by = "TransId"]
  trans <- trans[keepRows[[2]], ]
  trans[ , LoadID:= NULL]
  
  rm(keepRows)
  #  Keep only events which are deed transfers (excludes mortgage records, foreclosures, etc. See documentation.)
  
  trans <- trans[DataClassStndCode %in% c('D', 'H', 'F', 'M'), ] ## D: Deed Transfer, H: Mortage, F: foreclosure, M: mortage
  return(trans)
}