#setwd("~/share/projects/VotingBehavior/scripts/workflow_module/")
source("~/share/projects/VotingBehavior/scripts/workflow_module/Zillow_Functions.R")

##
##  Hedonics generating function from zillow


generate_hedonics <- function(state_code, output)
{
  setup_env()  # clear the environment, load libraries, and make some settings
  
  ## Setting the working directory
  setwd("~/share/projects/zillow/stores/")
  
  # Change directory to where you've stored ZTRAX
  dir <- paste("DB", state_code, sep="")#"DB17" 
  
  ## the state code can be find at:
  ## https://en.wikipedia.org/wiki/Federal_Information_Processing_Standard_state_code
  
  
  layoutZAsmt <- read_excel(file.path("~/share/projects/zillow/stores/", 'Layout.xlsx'), sheet = 1) #("../stores/", 'Layout.xlsx'), sheet = 1)
  layoutZTrans <- read_excel(file.path("~/share/projects/zillow/stores/", 'Layout.xlsx'),            #("../stores/", 'Layout.xlsx'),
                             sheet = 2,
                             col_types = c("text", "text", "numeric", "text", "text"))
  
  prototyping <- FALSE
  
  if(prototyping){
    rows2load <- 10000
  }else{
    rows2load <- -1
  }
  options(scipen = 999) # Do not print scientific notation
  options(stringsAsFactors = FALSE) ## Do not load strings as factors
  
  
  ######################################################################
  ###  Create property attribute table
  #    Need 3 tables
  #    1) Main table in assessor database
  #    2) Building table
  #    3) BuildingAreas
  
  col_namesMain <- t(layoutZAsmt[layoutZAsmt$TableName == 'utMain', 'FieldName'])
  col_namesBldg <- t(layoutZAsmt[layoutZAsmt$TableName == 'utBuilding', 'FieldName'])
  col_namesBldgA <- t(layoutZAsmt[layoutZAsmt$TableName == 'utBuildingAreas', 'FieldName'])
  
  base <- get_base(dir, rows2load, col_namesMain)
  bldg <- get_bldg(dir, rows2load, col_namesBldg)
  sqft <- get_sqft(dir, rows2load, col_namesBldgA)
  
  
  ###############################################################################
  #   Merge previous three datasets together to form attribute table
  
  attr <- merge(base, bldg, by = "RowID")
  attr <- merge(attr, sqft, by = c("RowID", "BuildingOrImprovementNumber"), all.x=TRUE)
  
  ## write an intermediate file ##
  #saveRDS(attr, "~/share/projects/VotingBehavior/store/NCIntermeidate.rds")
  
  rm(base)
  rm(bldg)
  rm(sqft)
  
  ###############################################################################
  ###############################################################################
  #  Load transaction dataset.
  #     Need two tables
  #      1) PropertyInfo table provided ImportParcelID to match transaction to assessor data loaded above
  #      2) Main table in Ztrans database provides information on real estate events
  
  col_namesProp <- t(layoutZTrans[layoutZTrans$TableName == 'utPropertyInfo', 'FieldName'])
  col_namesMainTr <-t(layoutZTrans[layoutZTrans$TableName == 'utMain', 'FieldName'])
  
  propTrans <- get_propTrans(dir, rows2load, col_namesProp)
  trans <- get_trans(dir, rows2load, col_namesMainTr)
  rm(col_namesProp)
  rm(col_namesMainTr)
  
  ###############################################################################
  #   Merge previous two datasets together to form transaction table
  
  transComplete <- merge(propTrans, trans, by = "TransId")
  rm(propTrans)
  rm(trans)
  
  ###############################################################################
  #   Merge the trans and azmt table together
  
  finalResult <- merge(transComplete, attr, by = "ImportParcelID")
  rm(transComplete)
  
  ###############################################################################
  #   write the the final merging result 
  
  WRITEFILE = TRUE
  
  if(WRITEFILE){
    saveRDS(finalResult, output)#"~/share/projects/VotingBehavior/production/ILHedonics.rds")
  }
}
## getting the code for the state given the statename, 
## using the 2 vectors "abbr" and "code" in the generate.R script
##  input:
##      state_name: string
##  output:
##      code: number 
get_code <- function(state_name) {
  
  dict <- as.list(code)
  names(dict) <- abbr
  return(dict[state_name][[1]])

}
