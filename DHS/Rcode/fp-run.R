
#############LIBRARIES##############
library(stats) # <- this IS needed for Rscript-driven runs.
library(dplyr)
library(tidyr)

#library(reshape2)  <- this is really outdated, what is it even used for?
#library(tools) ## <- why?
####################################

# these are utility functions

scripts <- c( "config.R", "utilities.R", "generateUnmet.R", 
  "categorization.R", "transform.R", "output.R")

script_paths <- file.path(PDUCodePath, scripts)

for (script in script_paths) source(script)

source_files <- dir(path='../source', pattern='\\.ZIP', full.names=TRUE, ignore.case=TRUE)
files_on_women <- pdhs::filter_file_names(source_files, latest=TRUE, country="KE", dataset="IR", 
  format = pdhs::get_file_format_code("Stata"))

pdhs::filtered_unzip(files_on_women, 'dta', dataPath)

dtaOnly <- dir(path=dataPath, pattern='^KEIR..FL\\.DTA', full.names=TRUE)
surveyCodes <- paste0(substr(basename(dtaOnly), 1,2), substr(basename(dtaOnly), 5,6)) %>%
  tolower

####################################

# Reclassify based on translation table and Write one .RData file per survey
# to the output directory.

for (surveyCode in surveyCodes) {
  outputFile <- file.path(outputPath, paste0(surveyCode, ".rds"))
  if (file.exists(outputFile)) {
    msg <- paste0("skipping '", surveyCode, "' due to file '", 
      outputFile, "' being present.")
    print(msg)
    next()
  }
  irData <- DHSReadDTA(dataPath, surveyCode, varToKeep)
  if (is.null(irData)) {
    msg <- paste0("input file for survey '", surveyCode, "', at path: '",
      dataPath, "', is not available. Skipping.")
    print(msg)
    next()
  }
  msg <- paste0("input file for survey '", surveyCode, "' found and output",
    " file not present, processing.")
  print(msg)
  reclassifiedData <- contraceptiveClassification(irData, methodClassification, 
    translationTable, surveyCode) %>% Unmet(surveyCode, specialSurveys)
 
  saveRDS(reclassifiedData, file = outputFile)
}


stop("This is as far as we need to go.")


# This was used to categorize Marital Status, Age:
  ir.data <- calculateNewVariables(ir.data)
  
